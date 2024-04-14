import Lake
open Lake DSL

package holes where
  -- add package configuration options here

lean_lib Holes where
  -- add library configuration options here

@[default_target]
lean_exe holes where
  root := `Main
