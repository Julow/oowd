open! Lwd_infix
module G = Lablgtk3_lwd

(* A table with a variable number of elements. *)

let window () =
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
          [
            G.label label;
            Oowd.app (fun btn ->
                ignore
                @@ btn#connect#clicked ~callback:(fun () ->
                       Lwd_table.remove row));
          ]
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

  G.dialog
    [
      G.title (Lwd.pure "dialog");
      G.border_width (Lwd.pure 10);
      G.resize (Lwd.pure (300, 300));
      Oowd.join
        (fun c w -> w#action_area#add (c :> GObj.widget))
        (G.button
           [
             G.label (Lwd.pure "add");
             G.connect (fun c -> c#clicked ~callback:add_button);
           ]);
      Oowd.join
        (fun c w -> w#action_area#add (c :> GObj.widget))
        (G.button
           [
             G.label (Lwd.pure "del");
             G.connect (fun c -> c#clicked ~callback:remove_button);
           ]);
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
