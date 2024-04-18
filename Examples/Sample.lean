import Holes

/-!
  This demonstrates the code actions and infoview support added by this library.

  Try moving the cursor in your editor to the placeholders, and notice the code
  actions that appear (in VS Code these can be accessed via the lightbulb icon),
  and the goal information rendered in the infoview.
-/

example (m : Unit) (n : Nat) (o : Float) : Nat :=
  ?

example (m : Unit) (n : Nat) (o : Float) : Nat × Unit × Float :=
  ? -- FIXME: Fails silently, logging to the console

example : Nat :=
  ? -- FIXME: Fails silently, logging to the console

example (n : Nat) : Nat :=
  {! 3 + n !}

example (n : Nat) : Nat :=
  {! ((3 + 1)) + n !}

example (n : Nat) : Nat :=
  {! () !} -- FIXME: Fails silently, logging to the console

inductive Vect (α : Type u) : Nat → Type u where
  | nil : Vect α 0
  | cons : α → Vect α n → Vect α (n + 1)

def append (xs : Vect α n) (ys : Vect α m) : Vect α (n + m) :=
  {!  !}
