type +'a elt = unit Lwd.t * 'a
type -'a attr = 'a -> unit Lwd.t

let app f elt =
  f elt;
  Lwd.return ()

let root (t, elt) = Lwd.observe (Lwd.map ~f:(fun () -> elt) t)

let elt elm attrs =
  ( Lwd_utils.pack ((), fun () () -> ()) (List.map (fun attr -> attr elm) attrs),
    elm )

let attr f = f

let join f (ct, ce) elm =
  f ce elm;
  ct
