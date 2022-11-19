open Oowd

let remove parent child = parent#remove child

let connect f : < connect : 'a ; .. > attr =
  Oowd.app (fun elt -> ignore (f elt#connect))

let button attrs = elt (GButton.button ()) attrs
let toggle_button attrs = elt (GButton.toggle_button ()) attrs
let label v = attr (fun e -> Lwd.map ~f:e#set_label v)
let table attrs = elt (GPack.table ()) attrs

let attach ~left ~top ?right ?bottom ?expand ?fill ?shrink ?xpadding ?ypadding c
    =
  join
    (fun c t ->
      t#attach ~left ~top ?right ?bottom ?expand ?fill ?shrink ?xpadding
        ?ypadding
        (c :> GObj.widget))
    c

let scrolled_window attrs = elt (GBin.scrolled_window ()) attrs
let border_width v = attr (fun e -> Lwd.map ~f:e#set_border_width v)
let hpolicy v = attr (fun e -> Lwd.map ~f:e#set_hpolicy v)

let add_with_viewport c =
  join (fun c t -> t#add_with_viewport (c :> GObj.widget)) c

let dialog attrs contents =
  let add_contents c (w : _ GWindow.dialog) =
    w#vbox#pack ~expand:true (c :> GObj.widget)
  in
  elt (GWindow.dialog ()) (join add_contents contents :: attrs)

let title v = attr (fun e -> Lwd.map ~f:e#set_title v)

let resize v =
  attr (fun e -> Lwd.map ~f:(fun (w, h) -> e#resize ~width:w ~height:h) v)

let width v = attr (fun e -> Lwd.map ~f:e#set_width v)
let height v = attr (fun e -> Lwd.map ~f:e#set_height v)
let add c = join (fun c t -> t#add (c :> GObj.widget)) c
let grab_default () = attr (fun e -> Lwd.return (e#grab_default ()))
let show () = attr (fun e -> Lwd.return (e#show ()))
let set_relief v = attr (fun e -> Lwd.map ~f:e#set_relief v)

let main window =
  let root = Oowd.root window in
  (Lwd.quick_sample root)#show ();
  ignore
  @@ GMain.Idle.add (fun () ->
         ignore (Lwd.quick_sample root);
         true)
