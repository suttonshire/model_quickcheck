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
  [%expect {|
    (Error
     ("Model_quickcheck: property falsified" (sequence ((set 32) (deref ())))
      (error ""))) |}]
;;
