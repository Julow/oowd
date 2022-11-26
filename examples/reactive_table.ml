open! Lwd_infix
open Lablgtk3_lwd

(* A table with a variable number of elements. *)

let () =
  let data = Lwd_table.make () in
  let n_data = Lwd_table.map_reduce (fun _ _ -> 1) (0, ( + )) data in

  let table =
    let mk_button row i =
      let left = i / 5 and top = i mod 5 in
      let label =
        let$ n_data in
        Printf.sprintf "button %d/%d (%d,%d)\n" i n_data left top
      in
      let btn =
        E.button
          [ A.label label; A.on_clicked (fun () -> Lwd_table.remove row) ]
      in
      Printf.printf "mk_button %d\n%!" i;
      Lwd_seq.element (A.table_attach ~left ~top btn)
    in
    E.table [] @@ Lwd_table.map_reduce mk_button Lwd_seq.monoid data
  in

  let button_i = ref 0 in
  let add_button () =
    ignore (Lwd_table.append ~set:!button_i data);
    incr button_i
  and remove_button () =
    Option.iter Lwd_table.remove (Lwd_table.last data);
    decr button_i
  in

  Example_lib.show_in_dialog
    ~a:
      [
        A.action_area
          [
            A.add
              (E.button [ A.label (Lwd.pure "add"); A.on_clicked add_button ]);
            A.add
              (E.button
                 [ A.label (Lwd.pure "del"); A.on_clicked remove_button ]);
          ];
      ]
    (E.scrolled_window
       [
         A.border_width (Lwd.pure 10);
         A.hpolicy (Lwd.pure `AUTOMATIC);
         A.add table;
       ])
