import Holes

/-!
  This demonstrates the code actions added to Lean’s existing placeholder syntax.

  Try moving the cursor in your editor to the placeholders, and notice the code
  actions that appear (in VS Code these can be accessed via the lightbulb icon),
  and the goal information rendered in the infoview.

  Note that there is currently no infoview support for existing placeholders,
  unlike with the custom holes syntax.
-/

example (n : Nat) : Nat :=
  _

example (n : Nat) : Nat :=
  ?_

example (n : Nat) : Nat :=
  sorry
