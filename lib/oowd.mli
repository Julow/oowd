type +'a elt
type -'a attr

val app : ('a -> unit) -> 'a attr
val root : 'a elt -> 'a Lwd.root
val elt : 'a -> 'a attr list -> 'a elt
val attr : ('a -> unit Lwd.t) -> 'a attr
val join : ('a -> 'b -> unit) -> 'a elt -> 'b attr
