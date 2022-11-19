open Lwd_infix
module G = Lablgtk3_lwd

(* Rewrite of the 'scrolledwin' example in lablgtk using Oowd. The toggle
   button is rewritten using Lwd and a standard button. *)

let toggle_button on attrs =
  let relief =
    let$ on = Lwd.get on in
    if on then `NORMAL else `NONE
  and on_click =
    Oowd.app (fun btn ->
        ignore
        @@ btn#connect#clicked ~callback:(fun () -> on $= not (Lwd.peek on)))
  in
  G.button (on_click :: G.set_relief relief :: attrs)

let window () =
  let table =
    List.init 10 (fun i ->
        List.init 10 (fun j ->
            let on = Lwd.var false in
            let label =
              let$ on = Lwd.get on in
              Printf.sprintf "button (%d,%d,%b)\n" i j on
            in
            let btn = toggle_button on [ G.label label ] in
            G.attach ~left:i ~top:j ~expand:`BOTH btn))
    |> List.flatten |> G.table
  in

  G.dialog
    [
      G.title (Lwd.pure "dialog");
      G.border_width (Lwd.pure 10);
      G.resize (Lwd.pure (300, 300));
      Oowd.join
        (fun c w -> w#action_area#add (c :> GObj.widget))
        (G.button
           [
             G.label (Lwd.pure "close");
             G.grab_default ();
             G.connect (fun c -> c#clicked ~callback:GMain.quit);
           ]);
      G.connect (fun c -> c#destroy ~callback:GMain.quit);
    ]
    (G.scrolled_window
       [
         G.border_width (Lwd.pure 10);
         G.hpolicy (Lwd.pure `AUTOMATIC);
         G.add_with_viewport table;
       ])

let () =
  ignore (GMain.init ());
  G.main (window ());
  GMain.main ()
