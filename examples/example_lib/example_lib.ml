(** Library of helper function to make the examples shorter. *)

open! Lwd_infix
open Lablgtk3_lwd

(** Create a dialog window and show it. Run Gtk's main loop. *)
let show_in_dialog ?(a = []) elt =
  let dialog =
    E.dialog
      (a
      @ [
          A.title (Lwd.pure "dialog");
          A.border_width (Lwd.pure 10);
          A.resize (Lwd.pure (300, 300));
          A.action_area
            [
              A.add
                (E.button
                   [
                     A.label (Lwd.pure "close");
                     A.grab_default ();
                     A.on_clicked GMain.quit;
                   ]);
            ];
          A.on_destroy GMain.quit;
        ])
      elt
  in
  Gtk_lwd.main dialog
