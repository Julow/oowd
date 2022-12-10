type +'a elt = unit Lwd.t * 'a
type -'a attr = 'a -> unit Lwd.t

let app f elt =
  f elt;
  Lwd.return ()

let root (ct, ce) = Lwd.observe (Lwd.map ~f:(fun () -> ce) ct)

let apply_attrs elm attrs =
  Lwd_utils.pack ((), fun () () -> ()) (List.map (fun attr -> attr elm) attrs)

let elt elm attrs = (apply_attrs elm attrs, elm)
let attr f = f

let join f (ct, ce) parent =
  f ce parent;
  ct

include Ordered_childs
include Positionned_childs
