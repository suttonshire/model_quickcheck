open Base
open Base_quickcheck

module Shrinker = struct
  include Shrinker

  let filter t ~f = create (fun x -> Sequence.filter ~f (shrink t x))
end

module type Model = sig
  type t

  val create : unit -> t
end

module type Uut = sig
  type t

  val create : unit -> t
  val cleanup : t -> unit
end

module type S = sig
  type model
  type uut

  module Action : sig
    module type S = sig
      type arg
      type ret

      val name : string
      val quickcheck_generator_arg : arg Base_quickcheck.Generator.t
      val quickcheck_shrinker_arg : arg Base_quickcheck.Shrinker.t
      val sexp_of_arg : arg -> Base.Sexp.t
      val sexp_of_ret : ret -> Base.Sexp.t
      val update_model : model -> arg -> model
      val update_uut : uut -> arg -> uut * ret
      val precondition : model -> arg -> bool
    end
  end

  val true_precondition : 'a -> 'b -> bool

  val check
    :  (float * (module Action.S)) list
    -> (module Action.S with type arg = 'a and type ret = 'b)
    -> ('a -> 'b -> model -> unit Base.Or_error.t)
    -> unit Base.Or_error.t
end

module Setup (Model : Model) (Uut : Uut) = struct
  type model = Model.t
  type uut = Uut.t

  module Action = struct
    module type S = sig
      type arg
      type ret

      val name : string
      val quickcheck_generator_arg : arg Base_quickcheck.Generator.t
      val quickcheck_shrinker_arg : arg Base_quickcheck.Shrinker.t
      val sexp_of_arg : arg -> Sexplib0.Sexp.t
      val sexp_of_ret : ret -> Sexplib0.Sexp.t
      val update_model : model -> arg -> model
      val update_uut : uut -> arg -> uut * ret
      val precondition : model -> arg -> bool
    end
  end

  module Action_instance = struct
    module type S = sig
      module Action : Action.S

      val arg : Action.arg
      val property : Action.arg -> Action.ret -> model -> unit Or_error.t
    end

    type t = (module S)

    let sexp_of_t t =
      let module A = (val t : S) in
      Sexp.List [ String.sexp_of_t A.Action.name; A.Action.sexp_of_arg A.arg ]
    ;;

    let make (type a b) (module A : Action.S with type arg = a and type ret = b) arg prop =
      (module struct
        module Action = A

        let arg = arg
        let property = prop
      end : S)
    ;;

    let generator
        (type a b)
        (module A : Action.S with type arg = a and type ret = b)
        model
        prop
      =
      Generator.filter A.quickcheck_generator_arg ~f:(A.precondition model)
      |> Generator.map ~f:(fun arg -> A.update_model model arg, make (module A) arg prop)
    ;;

    let weighted_generator
        (type a b)
        actions
        (module TestAction : Action.S with type arg = a and type ret = b)
        model
        prop
      =
      Generator.weighted_union
        (List.map
           ~f:(fun (weight, a) ->
             let module A = (val a : Action.S) in
             let instance =
               if String.(A.name = TestAction.name)
               then generator (module TestAction) model prop
               else generator (module A) model (fun _ _ _ -> Or_error.return ())
             in
             weight, instance)
           actions)
    ;;

    let shrinker =
      let f ai =
        let module A = (val ai : S) in
        let arg_seq = Shrinker.shrink A.Action.quickcheck_shrinker_arg A.arg in
        Sequence.map ~f:(fun arg -> make (module A.Action) arg A.property) arg_seq
      in
      Shrinker.create f
    ;;
  end

  module Action_seq = struct
    type t = Action_instance.t list

    let sexp_of_t (t : t) = List.sexp_of_t Action_instance.sexp_of_t t

    let generator actions action model prop ~len : t Generator.t =
      let open Generator.Let_syntax in
      let rec loop len model res =
        match len with
        | 0 -> Generator.return (List.rev res)
        | _ ->
          let%bind model, action =
            Action_instance.weighted_generator actions action model prop
          in
          loop (len - 1) model (action :: res)
      in
      loop len model []
    ;;

    let shrinker : t Shrinker.t =
      Shrinker.list Action_instance.shrinker
      |> Shrinker.filter ~f:(fun actions ->
             List.fold_until
               ~finish:(fun _ -> true)
               ~init:(Model.create ())
               ~f:(fun model action ->
                 let module A = (val action : Action_instance.S) in
                 if A.Action.precondition model A.arg
                 then Continue_or_stop.Continue (A.Action.update_model model A.arg)
                 else Continue_or_stop.Stop false)
               actions)
    ;;
  end

  let apply_actions_and_test actions model uut =
    let open Or_error.Let_syntax in
    let%map (_ : model), (_ : uut) =
      List.fold_result actions ~init:(model, uut) ~f:(fun (model, uut) action ->
          let module A = (val action : Action_instance.S) in
          let uut', ret = A.Action.update_uut uut A.arg in
          let%map () = A.property A.arg ret model in
          let new_model = A.Action.update_model model A.arg in
          new_model, uut')
    in
    ()
  ;;

  let shrink_error ~shrinker ~config ~f input error =
    let rec loop ~shrink_count ~alternates input error =
      match shrink_count with
      | 0 -> input, error
      | _ ->
        let shrink_count = shrink_count - 1 in
        (match Sequence.next alternates with
        | None -> input, error
        | Some (alternate, alternates) ->
          (match f alternate with
          | Ok () -> loop ~shrink_count ~alternates input error
          | Error error ->
            let alternates = Shrinker.shrink shrinker alternate in
            loop ~shrink_count ~alternates alternate error))
    in
    let shrink_count = Test.Config.shrink_count config in
    let alternates = Shrinker.shrink shrinker input in
    loop ~shrink_count ~alternates input error
  ;;

  let run_sequence actions action prop config =
    let quickcheck_generator =
      Action_seq.generator actions action (Model.create ()) prop ~len:1024
    in
    let f actions =
      Or_error.try_with_join ~backtrace:(Backtrace.Exn.am_recording ()) (fun () ->
          let model = Model.create () in
          let uut = Uut.create () in
          let res = apply_actions_and_test actions model uut in
          Uut.cleanup uut;
          res)
    in
    Test.with_sample quickcheck_generator ~config ~f:(fun sequence ->
        match
          Sequence.fold_result sequence ~init:() ~f:(fun () input ->
              match f input with
              | Ok () -> Ok ()
              | Error error -> Error (input, error))
        with
        | Ok () -> Ok ()
        | Error (input, error) ->
          let shrinker = Action_seq.shrinker in
          let input, error = shrink_error ~shrinker ~config ~f input error in
          Or_error.error_s
            (Sexp.message
               "Model_quickcheck.check: property falsified"
               [ "sequence", Action_seq.sexp_of_t input; "error", Error.sexp_of_t error ]))
  ;;

  let true_precondition _ _ = true

  let check
      (type a b)
      actions
      (module TestAction : Action.S with type arg = a and type ret = b)
      prop
    =
    let config =
      { Test.default_config with test_count = 1024; shrink_count = 1024 * 1024 }
    in
    List.find actions ~f:(fun (_, m) ->
        let module Action = (val m : Action.S) in
        String.(Action.name = TestAction.name))
    |> function
    | None ->
      let err_s =
        Printf.sprintf
          "Model_quickcheck.check: Action %s is not in the action list"
          TestAction.name
      in
      Or_error.error_string err_s
    | Some _ -> run_sequence actions (module TestAction) prop config
  ;;
end
