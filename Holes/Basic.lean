import Lean
import Lean.Elab.Term
import Lean.Meta.Tactic.LibrarySearch

open Lean Meta Elab Term


/-! # Syntax -/

/--
  An empty hole.

  Unlike `_`, this elaborates to `sorry`.
-/
syntax (name := questionHole) "?" : term

/--
  Interactive hole
-/
syntax (name := termHole) "{!" (term)? "!}" : term

-- NOTE: Is this the best syntax for interactive editing in Lean? Perhaps there
-- are alternatives that might work better with intellisense and code actions.
--
-- Other options could be:
--
-- * `"?(" (term)? ")"`
-- * `"?{" (term)? "}"`


/-! # Hole information -/

/--
  Hole information, for use in the `InfoTree`
-/
structure HoleInfo where
  lctx : LocalContext
  localInsts : LocalInstances
  expectedType? : Option Expr := none
  contents? : Option (TSyntax `term) := none
  deriving TypeName

def HoleInfo.withLocals [MonadControlT MetaM n] [Monad n] (info : HoleInfo) : n α → n α :=
  withLCtx info.lctx info.localInsts

def HoleInfo.withExpectedType (info : HoleInfo) (x : Expr → TermElabM Expr) : TermElabM Expr :=
  Term.withExpectedType info.expectedType? x

def HoleInfo.ofTermInfo (info : TermInfo) : HoleInfo :=
  { info with localInsts := #[] }


/-! # Elaboration actions -/

/--
  Fill a hole with the provided contents
-/
def fillHole (info : HoleInfo) : TermElabM Format :=
  match info.contents? with
  | none => throwError "no contents in hole"
  | some stx =>
    info.withLocals do
      -- Ensure that the contents of the hole matches the expected type
      let _ ← elabTermEnsuringType stx info.expectedType? (catchExPostpone := false)
      PrettyPrinter.formatTerm stx

/--
  Fill a hole with the elaborated contents
-/
def elabFillHole (info : HoleInfo) : TermElabM Format :=
  match info.contents? with
  | none => throwError "no contents in hole"
  | some stx =>
    info.withLocals do
      -- Ensure that the contents of the hole matches the expected type
      let e ← elabTermEnsuringType stx info.expectedType? (catchExPostpone := false)
      let t ← Tactic.TryThis.delabToRefinableSyntax e
      monadLift (PrettyPrinter.ppTerm t)

/--
  Attempt to automatically fill a hole

  This will use the declaration in the local context with the same type as the
  the expected type of the hole.
-/
def autoFillHole (info : HoleInfo) : TermElabM Format :=
  info.withLocals do
    -- TODO: Return multiple suggestions
    -- TODO: Use suggestions
    -- TODO: More proof searching: https://agda.readthedocs.io/en/latest/tools/auto.html
    -- TODO: Look at `Lean.(Elab|Tactic).LibrarySearch` tactic for inspiration

    let e ← info.withExpectedType fun expectedType => do
      -- Look for solutions in the local context, returning the first one that
      -- has the same typee as the expected type
      for decl in info.lctx.decls do
        if let some decl := decl then
          if !decl.isImplementationDetail && (← isDefEq expectedType decl.type) then
            return decl.toExpr

      throwError m!"could not find solution for `{expectedType}`"

    let t ← Tactic.TryThis.delabToRefinableSyntax e
    monadLift (PrettyPrinter.ppTerm t)

/--
  Refine a hole
-/
def refineHole (_info : HoleInfo) : TermElabM Format :=
  -- TODO: See `C-c C-r` in https://agda.readthedocs.io/en/latest/tools/emacs-mode.html#commands-in-context-of-a-goal
  --
  -- If the contents of the hole matches the return type of the expected type:
  --    Replace with `e {! !} ... {!  !}` where the number of holes matches the
  --    number of parameters in the expected type
  -- If the hole is empty:
  --    Insert a lambda or a constructor
  throwError "failed to refine hole"

/--
  Split a hole
-/
def splitHole (_info : HoleInfo) : TermElabM Format :=
  -- TODO: See `C-c C-c` in https://agda.readthedocs.io/en/latest/tools/emacs-mode.html#commands-in-context-of-a-goal
  throwError "failed to split hole"

/--
  Make a new top-level definition with the same type as with the expected type,
  binding the declarations in the local context as parameters.
-/
def makeDefinition (_info : HoleInfo) : TermElabM Format :=
  throwError "failed to make definition"


/-! # Hole elaborators -/

private def holeImpl : TermElab := fun stx expectedType? => do
  withExpectedType expectedType? fun expectedType => do
    let lctx ← getLCtx
    let localInsts ← getLocalInstances

    -- Extract the contents of the hole
    let contents? :=
      match stx with
      | `({! $t !}) => some t
      | `(?) | `({! !}) | _ => none

    -- Add hole information to the info tree
    pushInfoLeaf <| .ofCustomInfo {
      stx := stx
      value := Dynamic.mk (α := HoleInfo) {
        lctx, localInsts, expectedType?, contents?
      }
    }

    -- Show goal and context in the infoview
    -- TODO: Proper widget, like the “Expected type” widget
    let locals :=
      lctx.decls.toList |> List.filterMap fun
        | some d =>
          if d.isImplementationDetail then none else
            match d.value? with
            | some v => some m!"{d.userName} : {d.type} := {v}\n"
            | none => some m!"{d.userName} : {d.type}\n"
        | none => none

    logInfo m!"{MessageData.joinSep locals MessageData.nil}⊢ {expectedType?}"

    mkSorry expectedType (synthetic := true)

@[term_elab questionHole]
private def questionHoleImpl : TermElab := holeImpl

@[term_elab termHole]
private def termHoleImpl : TermElab := holeImpl


/-! # Code action providers -/

open Server Lsp RequestM in
@[code_action_provider]
private def holeProvider : CodeActionProvider := fun params snap => do
  let doc ← readDoc
  let rc ← readThe RequestContext

  -- Traverse the info tree, looking for holes, constructing an array of code actions
  pure <| snap.infoTree.foldInfo (init := #[]) fun _ctx info result => Id.run do
    -- Find terms with hole information recorded
    let .ofCustomInfo { stx, value } := info | result
    let some holeInfo := value.get? HoleInfo | result

    -- Get the source code range of the hole
    let some stxRange := stx.getRange? | result
    let stxRange := doc.meta.text.utf8RangeToLspRange stxRange

  -- NOTE: Not sure what these do!
  -- Cargo culted from `Lean.Tactic.TryThis.tryThisProvider`
    unless stxRange.start.line ≤ params.range.end.line do return result
    unless params.range.start.line ≤ stxRange.end.line do return result

    let holeAction (title : String) (action : HoleInfo → TermElabM Format) : LazyCodeAction :=
      let eager : CodeAction := {
        title
        kind? := "quickfix"
      }

      let lazy : IO CodeAction := do
        let ft ← runTermElabM snap (action holeInfo) rc
          -- FIXME: Prints to the console... we need a user-facing error
          |> EIO.toIO (IO.userError ∘ RequestError.message)

        pure {
          eager with
          edit? := WorkspaceEdit.ofTextDocumentEdit {
            textDocument := doc.versionedIdentifier
            edits := #[{ range := stxRange, newText := s!"{ft}" }]
          }
        }

      { eager, lazy? := lazy }

    let mut result := result

    if holeInfo.contents?.isSome then
      result := result.push <| holeAction s!"Fill with contents" fillHole
      result := result.push <| holeAction s!"Fill with elaborated contents" elabFillHole

    result := result.push <| holeAction s!"Automatically fill hole" autoFillHole

    pure result

open Server Lsp RequestM in
@[hole_code_action]
private def placeholderProvider : CodeAction.HoleCodeAction := fun params snap _ctx termInfo => do
  let doc ← readDoc
  let rc ← readThe RequestContext

  -- Get the source code range of the hole
  let some stxRange := termInfo.stx.getRange? | pure #[]
  let stxRange := doc.meta.text.utf8RangeToLspRange stxRange

  -- NOTE: Not sure what these do!
  -- Cargo culted from `Lean.Tactic.TryThis.tryThisProvider`
  unless stxRange.start.line ≤ params.range.end.line do return #[]
  unless params.range.start.line ≤ stxRange.end.line do return #[]

  let eager : CodeAction := {
    title := s!"Automatically fill hole"
    kind? := "quickfix"
  }

  let lazy : IO CodeAction := do
    let ft ← runTermElabM snap (autoFillHole (HoleInfo.ofTermInfo termInfo)) rc
      -- FIXME: Prints to the console... we need a user-facing error
      |> EIO.toIO (IO.userError ∘ RequestError.message)

    pure {
      eager with
      edit? := WorkspaceEdit.ofTextDocumentEdit {
        textDocument := doc.versionedIdentifier
        edits := #[{ range := stxRange, newText := s!"{ft}" }]
      }
    }

  pure #[{ eager, lazy? := lazy }]
