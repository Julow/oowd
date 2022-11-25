(** Library of helper function to make the examples shorter. *)

open! Lwd_infix
module G = Lablgtk3_lwd

let () = ignore (GMain.init ())

(** Create a dialog window and show it. Run Gtk's main loop. *)
let show_in_dialog ?(a = []) elt =
  let dialog =
    G.dialog
      (a
      @ [
          G.title (Lwd.pure "dialog");
          G.border_width (Lwd.pure 10);
          G.resize (Lwd.pure (300, 300));
          G.action_area_add
            (G.button
               [
                 G.label (Lwd.pure "close");
                 G.grab_default ();
                 G.on_clicked GMain.quit;
               ]);
          G.connect (fun c -> c#destroy ~callback:GMain.quit);
        ])
      elt
  in
  G.main dialog;
  GMain.main ()
