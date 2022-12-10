open! Lwd_infix
open Lablgtk3_lwd

(** Show a variable list of element and insert elements at any position.
    Elements have reactive states themselves. *)

let pure = Lwd.pure

let () =
  let mk_block row n =
    let updt_var inc () = n $= Lwd.peek n + inc in
    let add_before () = ignore (Lwd_table.before ~set:(Lwd.var 0) row) in
    let remove_row () = Lwd_table.remove row in
    Lwd_seq.element
    @@ E.hbox
         [
           A.border_width (pure 50);
           A.width_request (pure 50);
           A.height_request (pure 50);
           A.add (E.button [ A.label (Lwd.map ~f:string_of_int (Lwd.get n)) ]);
           A.add (E.button [ A.label (pure "+"); A.on_clicked (updt_var ~+1) ]);
           A.add (E.button [ A.label (pure "-"); A.on_clicked (updt_var ~-1) ]);
           A.add
             (E.button [ A.label (pure "add above"); A.on_clicked add_before ]);
           A.add (E.button [ A.label (pure "remove"); A.on_clicked remove_row ]);
         ]
  in

  let data = Lwd_table.make () in
  let blocks = Lwd_table.map_reduce mk_block Lwd_seq.monoid data in
  let append_block () = ignore (Lwd_table.append ~set:(Lwd.var 0) data) in

  Example_lib.show_in_dialog
    ~a:
      [
        A.action_area
          [
            A.add (E.button [ A.label (pure "add"); A.on_clicked append_block ]);
          ];
      ]
    (E.scrolled_window [ A.add (E.vbox [ A.box_childs blocks ]) ])
