// Copyright (c) HashiCorp, Inc.
// SPDX-License-Identifier: BUSL-1.1

package lang

import (
	"fmt"

	"github.com/hashicorp/hcl/v2"
	"github.com/hashicorp/hcl/v2/ext/dynblock"
	"github.com/hashicorp/hcl/v2/hcldec"
	"github.com/zclconf/go-cty/cty"
	"github.com/zclconf/go-cty/cty/convert"

	"github.com/hashicorp/terraform/internal/addrs"
	"github.com/hashicorp/terraform/internal/configs/configschema"
	"github.com/hashicorp/terraform/internal/instances"
	"github.com/hashicorp/terraform/internal/lang/blocktoattr"
	"github.com/hashicorp/terraform/internal/lang/langrefs"
	"github.com/hashicorp/terraform/internal/tfdiags"
)

// ExpandBlock expands any "dynamic" blocks present in the given body. The
// result is a body with those blocks expanded, ready to be evaluated with
// EvalBlock.
//
// If the returned diagnostics contains errors then the result may be
// incomplete or invalid.
func (s *Scope) ExpandBlock(body hcl.Body, schema *configschema.Block) (hcl.Body, tfdiags.Diagnostics) {
	spec := schema.DecoderSpec()

	traversals := dynblock.ExpandVariablesHCLDec(body, spec)
	refs, diags := langrefs.References(s.ParseRef, traversals)

	ctx, ctxDiags := s.EvalContext(refs)
	diags = diags.Append(ctxDiags)

	return dynblock.Expand(body, ctx), diags
}

// EvalBlock evaluates the given body using the given block schema and returns
// a cty object value representing its contents. The type of the result conforms
// to the implied type of the given schema.
//
// This function does not automatically expand "dynamic" blocks within the
// body. If that is desired, first call the ExpandBlock method to obtain
// an expanded body to pass to this method.
//
// If the returned diagnostics contains errors then the result may be
// incomplete or invalid.
func (s *Scope) EvalBlock(body hcl.Body, schema *configschema.Block) (cty.Value, tfdiags.Diagnostics) {
	spec := schema.DecoderSpec()

	refs, diags := langrefs.ReferencesInBlock(s.ParseRef, body, schema)

	ctx, ctxDiags := s.EvalContext(refs)
	diags = diags.Append(ctxDiags)
	if diags.HasErrors() {
		// We'll stop early if we found problems in the references, because
		// it's likely evaluation will produce redundant copies of the same errors.
		return cty.UnknownVal(schema.ImpliedType()), diags
	}

	// HACK: In order to remain compatible with some assumptions made in
	// Terraform v0.11 and earlier about the approximate equivalence of
	// attribute vs. block syntax, we do a just-in-time fixup here to allow
	// any attribute in the schema that has a list-of-objects or set-of-objects
	// kind to potentially be populated instead by one or more nested blocks
	// whose type is the attribute name.
	body = blocktoattr.FixUpBlockAttrs(body, schema)

	val, _ := hcldec.Decode(body, spec, ctx)

	return val, diags
}

// EvalSelfBlock evaluates the given body only within the scope of the provided
// object and instance key data. References to the object must use self, and the
// key data will only contain count.index or each.key. The static values for
// terraform and path will also be available in this context.
func (s *Scope) EvalSelfBlock(body hcl.Body, self cty.Value, schema *configschema.Block, keyData instances.RepetitionData) (cty.Value, tfdiags.Diagnostics) {
	var diags tfdiags.Diagnostics

	spec := schema.DecoderSpec()

	vals := make(map[string]cty.Value)
	vals["self"] = self

	if !keyData.CountIndex.IsNull() {
		vals["count"] = cty.ObjectVal(map[string]cty.Value{
			"index": keyData.CountIndex,
		})
	}
	if !keyData.EachKey.IsNull() {
		vals["each"] = cty.ObjectVal(map[string]cty.Value{
			"key": keyData.EachKey,
		})
	}

	refs, refDiags := langrefs.References(s.ParseRef, hcldec.Variables(body, spec))
	diags = diags.Append(refDiags)

	terraformAttrs := map[string]cty.Value{}
	pathAttrs := map[string]cty.Value{}

	// We could always load the static values for Path and Terraform values,
	// but we want to parse the references so that we can get source ranges for
	// user diagnostics.
	for _, ref := range refs {
		// we already loaded the self value
		if ref.Subject == addrs.Self {
			continue
		}

	}

	vals["path"] = cty.ObjectVal(pathAttrs)
	vals["terraform"] = cty.ObjectVal(terraformAttrs)

	ctx := &hcl.EvalContext{
		Variables: vals,
		Functions: s.Functions(),
	}

	val, _ := hcldec.Decode(body, schema.DecoderSpec(), ctx)
	return val, diags
}

// EvalExpr evaluates a single expression in the receiving context and returns
// the resulting value. The value will be converted to the given type before
// it is returned if possible, or else an error diagnostic will be produced
// describing the conversion error.
//
// Pass an expected type of cty.DynamicPseudoType to skip automatic conversion
// and just obtain the returned value directly.
//
// If the returned diagnostics contains errors then the result may be
// incomplete, but will always be of the requested type.

// EvalReference evaluates the given reference in the receiving scope and
// returns the resulting value. The value will be converted to the given type before
// it is returned if possible, or else an error diagnostic will be produced
// describing the conversion error.
//
// Pass an expected type of cty.DynamicPseudoType to skip automatic conversion
// and just obtain the returned value directly.
//
// If the returned diagnostics contains errors then the result may be
// incomplete, but will always be of the requested type.
func (s *Scope) EvalReference(ref *addrs.Reference, wantType cty.Type) (cty.Value, tfdiags.Diagnostics) {
	var diags tfdiags.Diagnostics

	// We cheat a bit here and just build an EvalContext for our requested
	// reference with the "self" address overridden, and then pull the "self"
	// result out of it to return.
	ctx, ctxDiags := s.evalContext([]*addrs.Reference{ref}, ref.Subject)
	diags = diags.Append(ctxDiags)
	val := ctx.Variables["self"]
	if val == cty.NilVal {
		val = cty.DynamicVal
	}

	var convErr error
	val, convErr = convert.Convert(val, wantType)
	if convErr != nil {
		val = cty.UnknownVal(wantType)
		diags = diags.Append(&hcl.Diagnostic{
			Severity: hcl.DiagError,
			Summary:  "Incorrect value type",
			Detail:   fmt.Sprintf("Invalid expression value: %s.", tfdiags.FormatError(convErr)),
			Subject:  ref.SourceRange.ToHCL().Ptr(),
		})
	}

	return val, diags
}

// EvalContext constructs a HCL expression evaluation context whose variable
// scope contains sufficient values to satisfy the given set of references.
//
// Most callers should prefer to use the evaluation helper methods that
// this type offers, but this is here for less common situations where the
// caller will handle the evaluation calls itself.
func (s *Scope) EvalContext(refs []*addrs.Reference) (*hcl.EvalContext, tfdiags.Diagnostics) {
	return s.evalContext(refs, s.SelfAddr)
}

func (s *Scope) evalContext(refs []*addrs.Reference, selfAddr addrs.Referenceable) (*hcl.EvalContext, tfdiags.Diagnostics) {
	if s == nil {
		panic("attempt to construct EvalContext for nil Scope")
	}

	var diags tfdiags.Diagnostics
	vals := make(map[string]cty.Value)
	funcs := s.Functions()
	ctx := &hcl.EvalContext{
		Variables: vals,
		Functions: funcs,
	}

	if len(refs) == 0 {
		// Easy path for common case where there are no references at all.
		return ctx, diags
	}

	// First we'll do static validation of the references. This catches things
	// early that might otherwise not get caught due to unknown values being
	// present in the scope during planning.
	staticDiags := s.Data.StaticValidateReferences(refs, selfAddr, s.SourceAddr)
	diags = diags.Append(staticDiags)
	if staticDiags.HasErrors() {
		return ctx, diags
	}

	// The reference set we are given has not been de-duped, and so there can
	// be redundant requests in it for two reasons:
	//  - The same item is referenced multiple times
	//  - Both an item and that item's container are separately referenced.
	// We will still visit every reference here and ask our data source for
	// it, since that allows us to gather a full set of any errors and
	// warnings, but once we've gathered all the data we'll then skip anything
	// that's redundant in the process of populating our values map.
	dataResources := map[string]map[string]cty.Value{}
	managedResources := map[string]map[string]cty.Value{}
	ephemeralResources := map[string]map[string]cty.Value{}
	wholeModules := map[string]cty.Value{}
	inputVariables := map[string]cty.Value{}
	localValues := map[string]cty.Value{}
	outputValues := map[string]cty.Value{}
	pathAttrs := map[string]cty.Value{}
	terraformAttrs := map[string]cty.Value{}
	countAttrs := map[string]cty.Value{}
	forEachAttrs := map[string]cty.Value{}
	checkBlocks := map[string]cty.Value{}
	runBlocks := map[string]cty.Value{}
	var self cty.Value

	for _, ref := range refs {

		rawSubj := ref.Subject
		if rawSubj == addrs.Self {
			if selfAddr == nil {
				diags = diags.Append(&hcl.Diagnostic{
					Severity: hcl.DiagError,
					Summary:  `Invalid "self" reference`,
					// This detail message mentions some current practice that
					// this codepath doesn't really "know about". If the "self"
					// object starts being supported in more contexts later then
					// we'll need to adjust this message.
					Detail:  `The "self" object is not available in this context. This object can be used only in resource provisioner, connection, and postcondition blocks.`,
					Subject: ref.SourceRange.ToHCL().Ptr(),
				})
				continue
			}

			if selfAddr == addrs.Self {
				// Programming error: the self address cannot alias itself.
				panic("scope SelfAddr attempting to alias itself")
			}

			// self can only be used within a resource instance

			// Self is an exception in that it must always resolve to a
			// particular instance. We will still insert the full resource into
			// the context below.
			// We should always have a valid self index by this point, but in
			// the case of an error, self may end up as a cty.DynamicValue.
			continue
		}

		// This type switch must cover all of the "Referenceable" implementations
		// in package addrs, however we are removing the possibility of
		// Instances beforehand.
		switch addr := rawSubj.(type) {
		case addrs.ResourceInstance:
			rawSubj = addr.ContainingResource()
		case addrs.ModuleCallInstance:
			rawSubj = addr.Call
		case addrs.ModuleCallInstanceOutput:
			rawSubj = addr.Call.Call
		}

	}

	// Managed resources are exposed in two different locations. The primary
	// is at the top level where the resource type name is the root of the
	// traversal, but we also expose them under "resource" as an escaping
	// technique if we add a reserved name in a future language edition which
	// conflicts with someone's existing provider.
	for k, v := range buildResourceObjects(managedResources) {
		vals[k] = v
	}
	vals["resource"] = cty.ObjectVal(buildResourceObjects(managedResources))
	vals["ephemeral"] = cty.ObjectVal(buildResourceObjects(ephemeralResources))
	vals["data"] = cty.ObjectVal(buildResourceObjects(dataResources))
	vals["module"] = cty.ObjectVal(wholeModules)
	vals["var"] = cty.ObjectVal(inputVariables)
	vals["local"] = cty.ObjectVal(localValues)
	vals["path"] = cty.ObjectVal(pathAttrs)
	vals["terraform"] = cty.ObjectVal(terraformAttrs)
	vals["count"] = cty.ObjectVal(countAttrs)
	vals["each"] = cty.ObjectVal(forEachAttrs)

	// Checks, outputs, and run blocks are conditionally included in the
	// available scope, so we'll only write out their values if we actually have
	// something for them.
	if len(checkBlocks) > 0 {
		vals["check"] = cty.ObjectVal(checkBlocks)
	}

	if len(outputValues) > 0 {
		vals["output"] = cty.ObjectVal(outputValues)
	}

	if len(runBlocks) > 0 {
		vals["run"] = cty.ObjectVal(runBlocks)
	}

	if self != cty.NilVal {
		vals["self"] = self
	}

	return ctx, diags
}

func buildResourceObjects(resources map[string]map[string]cty.Value) map[string]cty.Value {
	vals := make(map[string]cty.Value)
	for typeName, nameVals := range resources {
		vals[typeName] = cty.ObjectVal(nameVals)
	}
	return vals
}
