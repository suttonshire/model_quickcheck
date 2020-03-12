# Model_quickcheck: Model-based testing for imperative OCaml code

Model_quickcheck is useful for automatically generating many tests for stateful OCaml code. With stateful code it is often difficult to tease out which sequence of actions need to be applied to a system to test interesting behavior. Model_quickcheck allows you to think more about the "properties" of the system and takes care of generating the sequences of actions to apply.

With Model_quickcheck you specify a few main components to setup a test configuration:

1. A unit under test (UUT)
2. A deterministic model of the unit under test
3. A set of actions that transition the state of the uut and the model
4. A set of functions that validate the uut against the model

The implementation of the model is generally much simpler than the implementation of the unit under test . It is important that the model be deterministic. If it is not then it is possible that different executions of the test suite with the same initial conditions will yield different test results.

An action represents a possible state change of the UUT. An action is implemented as a module that has a function to transition the uut state and the model in an analogous way.

Model_quickcheck can generate a sequence of actions, applies each to the UUT and checks that the user provided predicate is satisfied for that action.

## Example

Let's take a look at an example that test one of the simplest parts of imperative OCaml, the int ref. Here we model the int ref as a int and specify two actions, `Deref` and `Set`. We're testing the `Deref` action and we want to verify that the returned value and the model are equivalent.

```ocaml
open Base
open Base_quickcheck

module Model = struct
  type t = int

  let create () = 0
end

module Uut = struct
  type t = int ref

  let create () = ref 0
  let cleanup _ = ()
end

module TestSetup = Model_quickcheck.Setup (Model) (Uut)

module Set = struct
  type arg = int [@@deriving sexp_of, quickcheck]
  type ret = unit [@@deriving sexp_of]

  let name = "set"
  let update_model _ arg = arg
  let update_uut uut arg = uut, (uut := arg)
  let precondition = TestSetup.true_precondition
end

module Deref = struct
  type arg = unit [@@deriving sexp_of, quickcheck]
  type ret = int [@@deriving sexp_of]

  let name = "deref"
  let update_model model () = model
  let update_uut uut () = uut, if !uut = 32 then 1 else !uut
  let precondition = TestSetup.true_precondition
end

let expect action prop =
  let f arg ret model =
    if prop arg ret model then Or_error.return () else Or_error.error_string ""
  in
  TestSetup.check [ 0.5, (module Set); 0.5, (module Deref) ] action f
  |> Or_error.sexp_of_t Unit.sexp_of_t
  |> Stdio.print_s
;;

let%expect_test "ref example" =
  expect (module Deref) (fun () ret model -> ret = model);
  [%expect {||}]
;;
```

After running the test we see that Model_quickcheck has found a sequence of actions invalidates the specified property, a bug. It reports the smallest sequence that can invoke this bug.

```sh
$ dune runtest examples
Done: 33/35 (jobs: 1)File "examples/ref.ml", line 1, characters 0-0:
         git (internal) (exit 1)
(cd _build/default && /usr/bin/git diff --no-index --color=always -u examples/ref.ml examples/ref.ml.corrected)
diff --git a/examples/ref.ml b/examples/ref.ml.corrected
index 2bb0b4b..ad8ce55 100644
--- a/examples/ref.ml
+++ b/examples/ref.ml.corrected
@@ -47,5 +47,8 @@ let expect action prop =

 let%expect_test "ref example" =
   expect (module Deref) (fun () ret model -> ret = model);
-  [%expect {||}]
+  [%expect {|
+    (Error
+     ("Model_quickcheck.check: property falsified"
+      (sequence ((set 32) (deref ()))) (error ""))) |}]
 ;;
```
