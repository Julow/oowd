(** The two main concepts are {!type-elt} and {!type-attr}. *)

type +'a elt
(** An element represents an object (often a graphical widget). The encapsulated
    object is instantiated immediately.

    Use {{!type-attr} attributes} for setting up reactive properties.

    See {{!childs} children} for container/child relations. *)

type -'a attr
(** Attributes depend on a reactive value and set a property of an {!type-elt}.
    They operate directly on the encapsulated object. *)

val elt : 'a -> 'a attr list -> 'a elt
(** Make an element from an object and a list of attributes. *)

val attr : ('a -> unit Lwd.t) -> 'a attr
(** Make an attribute from a function operating on an object. *)

val app : ('a -> unit) -> 'a attr
(** An attribute that execute a function on an object when it is just created. *)

val join : ('a -> 'b -> unit) -> 'a elt -> 'b attr
(** An attribute that operates on two elements: the element it is attached to
    and a second element passed as argument. *)

val apply_attrs : 'a -> 'a attr list -> unit Lwd.t
(** Apply attributes to any object. Useful to define attributes that themselves
    take attributes. *)

val root : 'a elt -> 'a Lwd.root
(** Used to define the main loop. *)

(** {1:childs Container/child relations} *)

(** {2:ordered_child Ordered children} *)

val ordered_childs :
  insert:('p -> int -> 'c -> unit) ->
  remove:('p -> 'c -> unit) ->
  'c elt Lwd_seq.t Lwd.t ->
  'p attr
(** Maintain a sequence of children using two functions:

    - [insert parent index child] Inserts an element at a specific position. An
      [index] of [0] means before the first element, an [index] equal to the
      number of elements means at the end.
    - [remove parent child] Removes an element from the parent.

    Children are maintained in the same order as they appear in the sequence. A
    moved children is first removed then inserted again. *)

(** {2:positionned_child Self-positionned children} *)

type ('p, 'c) positionned_child
(** Positionned children are elements that position themselves inside their
    parent. See {!positionned_childs}. *)

val positionned_child :
  add:('p -> 'c -> unit) -> 'c elt -> ('p, 'c) positionned_child
(** Create a new {!type-positionned_child}. Each children has its own [add]
    function. *)

val positionned_childs :
  remove:('p -> 'c -> unit) ->
  ('p, 'c) positionned_child Lwd_seq.t Lwd.t ->
  'p attr
(** Calls to the [add] functions can happen in any order. A children that
    changes ({!val-positionned_child} is called again with the same element) is
    not treated differently from an element that was removed while an other is
    added. *)
