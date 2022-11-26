open! Lwd_infix
open Lablgtk3_lwd

(* Table children are [positionned_child]. Play with moving one around. *)

let () =
  let pos = Lwd.var 0 in

  let moving_btn =
    let btn =
      E.button
        [
          A.label (Lwd.map ~f:string_of_int (Lwd.get pos));
          A.on_clicked (fun () -> pos $= (Lwd.peek pos + 1) mod 5);
        ]
    in
    let$ pos = Lwd.get pos in
    Lwd_seq.element (A.table_attach ~left:2 ~top:pos btn)
  in

  let static_elements =
    let mk_btn i =
      let label =
        let$ pos = Lwd.get pos in
        if pos = i then "->" else ""
      in
      A.table_attach ~left:1 ~top:i (E.button [ A.label label ])
    in
    List.init 5 mk_btn |> Lwd_seq.of_list
  in

  let childs =
    let$ moving_btn in
    Lwd_seq.concat static_elements moving_btn
  in

  Example_lib.show_in_dialog (E.table [] childs)
