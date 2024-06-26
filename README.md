# lean-holes

> [!WARNING]
> This is an experimental and incomplete proof-of-concept!

A syntax extension for [Lean 4](https://leanprover.github.io/) that adds support
for interactive holes[^1]. The goal is to make it easier to interactively
construct programs and direct-style proofs in a type directed way.

Holes can be introduced using following custom syntax:

```lean
syntax "?" : term
syntax "{!" (term)? "!}" : term
```

For examples, see [Examples/Sample.lean](Examples/Sample.lean).

A series of code actions are provided:

- [x] Fill with contents
- [x] Fill with elaborated contents
- [x] Automatically fill a hole (NOTE: rudimentary support[^3])
- [ ] Refine hole
- [ ] Split hole
- [ ] Make definition

The same actions have been added to `_`, `?_`, and `sorry`.

## Resources

This was inspired by [this conversation](https://mathstodon.xyz/@d_christiansen/112262886640861767)
with David Christiansen on Mastodon.

Agda docs:

- [Agda: A Taste of Agda – Holes and case splitting](https://agda.readthedocs.io/en/latest/getting-started/a-taste-of-agda.html#holes-and-case-splitting)
- [Agda: Lexical structure – Holes](https://agda.readthedocs.io/en/latest/language/lexical-structure.html#holes)
- [Agda: Emacs Mode – Commands in the context of a goal](https://agda.readthedocs.io/en/latest/tools/emacs-mode.html#commands-in-context-of-a-goal)
- [Agda: Automatic Proof Search (Auto)](https://agda.readthedocs.io/en/latest/tools/auto.html)

Idris:

- [Idris 2: Interactive Editing](https://idris2.readthedocs.io/en/latest/tutorial/interactive.html)
- [Idris 2: The IDE Protocol](https://idris2.readthedocs.io/en/latest/implementation/ide-protocol.html)
- [Idris 2: TTImp.Interactive.ExprSearch](https://github.com/idris-lang/Idris2/blob/main/src/TTImp/Interactive/ExprSearch.idr)

[^1]: As seen in Agda and Idris. I’m not exactly sure when interactive holes
      where first introduced however... possibly in Epigram?
[^3]: Auto-filling currently just looks up the local context for a matching declaration.
