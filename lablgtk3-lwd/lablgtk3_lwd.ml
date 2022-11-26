open Oowd
open Internal

let () = ignore (GMain.init ())

(** This module is meant to be opened. *)

module A = struct
  (** Attributes. *)

  (** Widget *)

  let width_request width =
    attr (fun elm -> Lwd.map ~f:elm#set_width_request width)

  let height_request height =
    attr (fun elm -> Lwd.map ~f:elm#set_height_request height)

  (** Buttons *)

  let label v = attr (fun e -> Lwd.map ~f:e#set_label v)
  let grab_default () = attr (fun e -> Lwd.return (e#grab_default ()))
  let relief v = attr (fun e -> Lwd.map ~f:e#set_relief v)
  let on_clicked f = app (fun w -> ignore @@ w#connect#clicked ~callback:f)

  (** General containers *)

  let add c = join (fun c t -> t#add (c :> GObj.widget)) c

  (** Table *)

  let table_attach ~left ~top c =
    let add (t : GPack.table) c = t#attach ~left ~top c in
    positionned_child ~add (c :> GObj.widget elt)

  let rows v = attr (fun e -> Lwd.map ~f:e#set_rows v)
  let columns v = attr (fun e -> Lwd.map ~f:e#set_columns v)
  let border_width v = attr (fun e -> Lwd.map ~f:e#set_border_width v)
  let hpolicy v = attr (fun e -> Lwd.map ~f:e#set_hpolicy v)

  (** Grid *)

  let grid_attach ~left ~top ?width ?height c =
    let add (g : GPack.grid) c = g#attach ~left ~top ?width ?height c in
    positionned_child ~add (c :> GObj.widget elt)

  let hexpand x = attr (fun e -> Lwd.map ~f:e#set_hexpand x)
  let valign x = attr (fun e -> Lwd.map ~f:e#set_valign x)
  let col_spacings x = attr (fun elm -> Lwd.map ~f:elm#set_col_spacings x)
  let row_spacings x = attr (fun elm -> Lwd.map ~f:elm#set_row_spacings x)

  (** Dialog *)

  let action_area attrs =
    attr (fun dialog -> apply_attrs dialog#action_area attrs)

  (** Window *)

  let title v = attr (fun e -> Lwd.map ~f:e#set_title v)

  let resize v =
    attr (fun e -> Lwd.map ~f:(fun (w, h) -> e#resize ~width:w ~height:h) v)

  let width v = attr (fun e -> Lwd.map ~f:e#set_width v)
  let height v = attr (fun e -> Lwd.map ~f:e#set_height v)
  let show () = attr (fun e -> Lwd.return (e#show ()))
  let on_destroy f = app (fun w -> ignore @@ w#connect#destroy ~callback:f)

  (** Style Context *)

  let style_context attrs =
    attr (fun elm -> apply_attrs elm#misc#style_context attrs)

  let pure_add_class cls = app (fun sctx -> sctx#add_class cls)

  let add_provider ?(priority = 800) css =
    join (fun provider sctx -> sctx#add_provider provider priority) css

  let add_provider_for_screen ?(priority = 800) css =
    join
      (fun provider win ->
        GtkData.StyleContext.add_provider_for_screen win#screen
          provider#as_css_provider priority)
      css
end

module E = struct
  (** Gtk3 objects. *)

  let button attrs = elt (GButton.button ()) attrs
  let toggle_button attrs = elt (GButton.toggle_button ()) attrs

  let table attrs c =
    elt (GPack.table ()) (positionned_childs ~remove c :: attrs)

  let grid attrs contents =
    elt (GPack.grid ()) (positionned_childs ~remove contents :: attrs)

  let scrolled_window attrs = elt (GBin.scrolled_window ()) attrs

  let dialog attrs contents =
    let add_contents c (w : _ GWindow.dialog) =
      w#vbox#pack ~expand:true (c :> GObj.widget)
    in
    elt (GWindow.dialog ()) (join add_contents contents :: attrs)

  let window attrs contents = elt (GWindow.window ()) (A.add contents :: attrs)

  let css_provider_from_string css =
    let provider = GObj.css_provider () in
    provider#load_from_data css;
    elt provider []
end

module Gtk_lwd = struct
  let main window =
    let root = Oowd.root window in
    (Lwd.quick_sample root)#show ();
    ignore
    @@ GMain.Idle.add (fun () ->
           ignore (Lwd.quick_sample root);
           true);
    GMain.main ()
end
