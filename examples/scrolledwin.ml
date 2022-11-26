open Lwd_infix
open Lablgtk3_lwd

(* Rewrite of the 'scrolledwin' example in lablgtk using Oowd. The toggle
   button is rewritten using Lwd and a standard button. *)

let toggle_button on attrs =
  let relief =
    let$ on = Lwd.get on in
    if on then `NORMAL else `NONE
  and toggle () = on $= not (Lwd.peek on) in
  E.button (A.on_clicked toggle :: A.relief relief :: attrs)

let () =
  let table =
    List.init 10 (fun i ->
        List.init 10 (fun j ->
            let on = Lwd.var false in
            let label =
              let$ on = Lwd.get on in
              Printf.sprintf "button (%d,%d,%b)\n" i j on
            in
            let btn = toggle_button on [ A.label label ] in
            A.table_attach ~left:i ~top:j btn))
    |> List.flatten |> Lwd_seq.of_list |> Lwd.return |> E.table []
  in

  Example_lib.show_in_dialog
    (E.scrolled_window
       [
         A.border_width (Lwd.pure 10);
         A.hpolicy (Lwd.pure `AUTOMATIC);
         A.add table;
       ])
