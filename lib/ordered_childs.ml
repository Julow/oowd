open Lwd_infix
module R = Lwd_seq.Reducer

(** Make sure children's Lwd documents are part of the parent's document. *)
let children_seq childs =
  Lwd_seq.bind childs (fun (ct, ce) ->
      let elt = Lwd_seq.element ce in
      let$ () = ct in
      elt)

type 'c synced_childs =
  | Leaf of {
      mutable state : bool;  (** Whether the child is added to the parent. *)
      elt : 'c;
    }
  | Concat of {
      mutable width : int;
          (** Number of element inside the tree. [0] when unsynced. *)
      left : 'c synced_childs;
      right : 'c synced_childs;
    }

let _states_considered = ref 0
let _dom_updates = ref 0

let _log_stats () =
  Printf.printf "Oowd.ordered_childs: %d states considered, %d dom updates\n%!"
    !_states_considered !_dom_updates;
  _states_considered := 0;
  _dom_updates := 0

(** Returns [0] if the node is not synced. *)
let width = function
  | Leaf { state = true; _ } -> 1
  | Leaf { state = false; _ } -> 0
  | Concat { width; _ } -> width

let ordered_childs ~insert ~remove childs parent =
  let map elt = Leaf { state = false; elt }
  and reduce left right =
    incr _states_considered;
    (* As an optimisation, compute the [width] immediately if both subtrees are
       already synced. *)
    let wl = width left and wr = width right in
    let width = if wl <> 0 && wr <> 0 then wl + wr else 0 in
    Concat { width; left; right }
  in
  let reducer = ref (R.make ~map ~reduce) in
  let rec sync_childs index = function
    | Leaf leaf ->
        incr _states_considered;
        if not leaf.state then (
          incr _dom_updates;
          leaf.state <- true;
          insert parent index leaf.elt);
        index + 1
    | Concat ({ width; left; right } as concat) ->
        if width = 0 then (
          incr _states_considered;
          let right_ends = sync_childs (sync_childs index left) right in
          concat.width <- right_ends - index;
          right_ends)
        else index + width
  in
  let sync_childs tree = ignore (sync_childs 0 tree) in
  let remove_dropped c () =
    match c with
    | Leaf { elt; _ } ->
        incr _dom_updates;
        remove parent elt
    | Concat _ -> assert false
  in
  let$ childs = children_seq childs in
  let dropped, r = R.update_and_get_dropped !reducer childs in
  reducer := r;
  R.fold_dropped `Map remove_dropped dropped ();
  Option.iter sync_childs (R.reduce r);
  _log_stats ()
