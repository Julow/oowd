open! Lwd_infix
module G = Lablgtk3_lwd

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
        G.button
          [ G.label label; G.on_clicked (fun () -> Lwd_table.remove row) ]
      in
      Printf.printf "mk_button %d\n%!" i;
      Lwd_seq.element (G.attach ~left ~top btn)
    in
    G.table [] @@ Lwd_table.map_reduce mk_button Lwd_seq.monoid data
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
        G.action_area_add
          (G.button [ G.label (Lwd.pure "add"); G.on_clicked add_button ]);
        G.action_area_add
          (G.button [ G.label (Lwd.pure "del"); G.on_clicked remove_button ]);
      ]
    (G.scrolled_window
       [
         G.border_width (Lwd.pure 10);
         G.hpolicy (Lwd.pure `AUTOMATIC);
         G.add_with_viewport table;
       ])
