(* The config is used *)
(* Create is called the correct amount
    - Create a model and uut whose state contains the number of times an action has been applied
    - Get the model and utt states at the end of the test and make sure that they were called the correct number of times
    - We'll need a way to expose the model and uut states at the end of sequences and at the end of tests
*)
open Base
open Base_quickcheck
open Model_quickcheck

module Model = struct
  type t = int

  let create () = 0
end

module Uut = struct
  type t = int ref

  let create () = ref 0
  let cleanup _ = ()
end

module TestSetup = Setup (Model) (Uut)

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

let%expect_test "Uut.create should be called once for each test sequence" =
  let cnt = ref 0 in
  let module Uut = struct
    type t = unit

    let create () = Int.incr cnt
    let cleanup () = ()
  end
  in
  let module Model = struct
    type t = unit

    let create () = ()
  end
  in
  let module A = struct
    type arg = unit [@@deriving sexp_of, quickcheck]
    type ret = unit [@@deriving sexp_of]

    let name = "A"
    let update_model model _ = model
    let update_uut uut _ = uut, ()
    let precondition = TestSetup.true_precondition
  end
  in
  let module T = Setup (Model) (Uut) in
  let prop _ _ _ = Or_error.return () in
  Or_error.ok_exn (T.check [ 1.0, (module A) ] (module A) prop);
  Stdio.printf "%d" !cnt;
  [%expect {| 1024 |}]
;;

let%expect_test "Uut.cleanup should be called once for each test sequence" =
  let cnt = ref 0 in
  let module Uut = struct
    type t = unit

    let create () = ()
    let cleanup () = Int.incr cnt
  end
  in
  let module Model = struct
    type t = unit

    let create () = ()
  end
  in
  let module A = struct
    type arg = unit [@@deriving sexp_of, quickcheck]
    type ret = unit [@@deriving sexp_of]

    let name = "A"
    let update_model model _ = model
    let update_uut uut _ = uut, ()
    let precondition = TestSetup.true_precondition
  end
  in
  let module T = Setup (Model) (Uut) in
  let prop _ _ _ = Or_error.return () in
  Or_error.ok_exn (T.check [ 1.0, (module A) ] (module A) prop);
  Stdio.printf "%d" !cnt;
  [%expect {| 1024 |}]
;;

let%test "Cleanup should be called even if an exception is thrown in an action" =
  let created = ref 0 in
  let cleanedup = ref 0 in
  let module Uut = struct
    type t = unit

    let create () = Int.incr created
    let cleanup () = Int.incr cleanedup
  end
  in
  let module Model = struct
    type t = unit

    let create () = ()
  end
  in
  let module A = struct
    type arg = unit [@@deriving sexp_of, quickcheck]
    type ret = unit [@@deriving sexp_of]

    let name = "A"
    let update_model model _ = model

    let update_uut uut _ =
      if !created = 10 then raise (Invalid_argument "test") else uut, ()
    ;;

    let precondition = TestSetup.true_precondition
  end
  in
  let module T = Setup (Model) (Uut) in
  let prop _ _ _ = Or_error.return () in
  let config = { T.Config.default with shrink_count = 0 } in
  match T.check ~config [ 1.0, (module A) ] (module A) prop with
  | Error _ -> !created = !cleanedup
  | Ok _ -> false
;;

let%test "On success an action should be called sequence_count * sequence_length times" =
  let module Uut = struct
    type t = int

    let create () = 0
    let cleanup _ = ()
  end
  in
  let module Model = struct
    type t = int

    let create () = 0
  end
  in
  let module A = struct
    type arg = unit [@@deriving sexp_of, quickcheck]
    type ret = unit [@@deriving sexp_of]

    let name = "A"
    let update_model model _ = model + 1
    let update_uut uut _ = uut + 1, ()
    let precondition = TestSetup.true_precondition
  end
  in
  let module T = Setup (Model) (Uut) in
  let prop _ _ _ = Or_error.return () in
  let config = { T.Config.default with shrink_count = 0 } in
  match T.check_states ~config [ 1.0, (module A) ] (module A) prop with
  | Error _ -> false
  | Ok seq ->
    Sequence.length seq = config.sequence_count
    && Sequence.count seq ~f:(fun (model, uut) ->
           Int.(model = uut && model = config.sequence_length))
       = config.sequence_count
;;

let%expect_test "Return error if an action is duplicated" =
  let module Uut = struct
    type t = int

    let create () = 0
    let cleanup _ = ()
  end
  in
  let module Model = struct
    type t = int

    let create () = 0
  end
  in
  let module A = struct
    type arg = unit [@@deriving sexp_of, quickcheck]
    type ret = unit [@@deriving sexp_of]

    let name = "A"
    let update_model model _ = model + 1
    let update_uut uut _ = uut + 1, ()
    let precondition = TestSetup.true_precondition
  end
  in
  let module T = Setup (Model) (Uut) in
  let prop _ _ _ = Or_error.return () in
  let config = { T.Config.default with shrink_count = 0 } in
  T.check ~config [ 1.0, (module A); 1.0, (module A) ] (module A) prop
  |> Or_error.sexp_of_t Unit.sexp_of_t
  |> Stdio.print_s;
  [%expect {| (Error "Model_quickcheck: Action A is duplicated in action list") |}]
;;

let%expect_test "Return error if an action is duplicated" =
  let module Uut = struct
    type t = int

    let create () = 0
    let cleanup _ = ()
  end
  in
  let module Model = struct
    type t = int

    let create () = 0
  end
  in
  let module A = struct
    type arg = unit [@@deriving sexp_of, quickcheck]
    type ret = unit [@@deriving sexp_of]

    let name = "A"
    let update_model model _ = model + 1
    let update_uut uut _ = uut + 1, ()
    let precondition = TestSetup.true_precondition
  end
  in
  let module B = struct
    type arg = unit [@@deriving sexp_of, quickcheck]
    type ret = unit [@@deriving sexp_of]

    let name = "B"
    let update_model model _ = model + 1
    let update_uut uut _ = uut + 1, ()
    let precondition = TestSetup.true_precondition
  end
  in
  let module T = Setup (Model) (Uut) in
  let prop _ _ _ = Or_error.return () in
  let config = { T.Config.default with shrink_count = 0 } in
  T.check ~config [ 1.0, (module A) ] (module B) prop
  |> Or_error.sexp_of_t Unit.sexp_of_t
  |> Stdio.print_s;
  [%expect {| (Error "Model_quickcheck: Action B is not in the action list") |}]
;;

(* Exception is thrown if an invalid set of action is given *)
(* Failure is returned *)
(* Success is returned *)
