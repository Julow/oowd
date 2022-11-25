open Lwd_infix
module G = Lablgtk3_lwd

(* Rewrite of the 'scrolledwin' example in lablgtk using Oowd. The toggle
   button is rewritten using Lwd and a standard button. *)

let toggle_button on attrs =
  let relief =
    let$ on = Lwd.get on in
    if on then `NORMAL else `NONE
  and toggle () = on $= not (Lwd.peek on) in
  G.button (G.on_clicked toggle :: G.set_relief relief :: attrs)

let () =
  let table =
    List.init 10 (fun i ->
        List.init 10 (fun j ->
            let on = Lwd.var false in
            let label =
              let$ on = Lwd.get on in
              Printf.sprintf "button (%d,%d,%b)\n" i j on
            in
            let btn = toggle_button on [ G.label label ] in
            G.attach ~left:i ~top:j btn))
    |> List.flatten |> Lwd_seq.of_list |> Lwd.return |> G.table []
  in

  Example_lib.show_in_dialog
    (G.scrolled_window
       [
         G.border_width (Lwd.pure 10);
         G.hpolicy (Lwd.pure `AUTOMATIC);
         G.add_with_viewport table;
       ])
