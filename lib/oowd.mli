type +'a elt
type -'a attr

val app : ('a -> unit) -> 'a attr
val root : 'a elt -> 'a Lwd.root
val elt : 'a -> 'a attr list -> 'a elt
val attr : ('a -> unit Lwd.t) -> 'a attr
val join : ('a -> 'b -> unit) -> 'a elt -> 'b attr

type ('p, 'c) positionned_child

val positionned_child :
  add:('p -> 'c -> unit) -> 'c elt -> ('p, 'c) positionned_child

val positionned_childs :
  remove:('p -> 'c -> unit) ->
  ('p, 'c) positionned_child Lwd_seq.t Lwd.t ->
  'p attr
(** Manage children. Each child has its own [add] function. Changed children
    are removed from and added to the parent. Useful when absolute position are
    defined for each childre, for example tables in Gtk3. *)
