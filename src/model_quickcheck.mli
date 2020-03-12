module type Model = sig
  (** The type that models the system under test *)
  type t

  (** [create ()] should return a new t. [create ()] is called at a couple points during a
      test. Rirst before generating a test sequence and again before applying each
      action in the generated test sequence *)
  val create : unit -> t
end

module type Uut = sig
  (** The type of system that will be tested *)
  type t

  (** Create an instance of the system to test. [create ()] is called before applying the
      actions in a sequence *)
  val create : unit -> t

  (** [cleanup t] should release any resource held by [t]. [cleanup t] is called after
      applying each action in a sequence *)
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

  (** [check actions test_action property] generates many sequences of actions and checks
      [property] after applying [test_action]. [actions] is a list of (weight,
      action) pairs. The occurence of an action from [actions] is weighted by the weight
      fields. If an action in a generated sequence has the same name as [test_action]
      then [property] is checked. The sequences contain 1024 actions.

      [property] takes the argument used in the action, and the return type of applying the action to the UUT and the state of the model before applying the action.

      Fails if no action in [actions] has the same name as [test_action]. Fails if [property] returns [Error _] or raises.

      Weights must be non-negative and must have a strictly positive sum.*)
  val check
    :  (float * (module Action.S)) list
    -> (module Action.S with type arg = 'a and type ret = 'b)
    -> ('a -> 'b -> model -> unit Base.Or_error.t)
    -> unit Base.Or_error.t
end

module Setup (Model : Model) (Uut : Uut) :
  S with type model = Model.t and type uut = Uut.t
