open Lwd_infix
module R = Lwd_seq.Reducer

type ('p, 'c) leaf_state =
  | Added
  | New of ('p -> 'c -> unit)
      (** The [add] function is lost as soon as the element is first added. *)

(** Constructed while reducing. In the shape of a tree to avoid quadratic list
      concatenation. *)
type ('p, 'c) synced_childs =
  | Leaf of { mutable state : ('p, 'c) leaf_state; elt : 'c }
  | Concat of {
      mutable synced : bool;  (** Whether the tree is uptodate. *)
      left : ('p, 'c) synced_childs;
      right : ('p, 'c) synced_childs;
    }

type ('p, 'c) positionned_child = ('p, 'c) synced_childs Lwd_seq.t Lwd.t

let positionned_child ~add (ct, ce) =
  (* Depend on [ct] but also share the state to avoid remove then add.
     [Lwd_seq.lift] is implemented using [bind], wrap in [Lwd_seq.element] in
     advance to optimise the document and allow the reducer to reuse old values.
     This depends on internal details of [Lwd_seq.bind] and the reducer, which
     uses [==] to reuse old values. *)
  let leaf = Lwd_seq.element (Leaf { state = New add; elt = ce }) in
  let$ () = ct in
  leaf

let _states_considered = ref 0
let _dom_updates = ref 0

(** Useful to assert the efficiency of the DOM update function. The first number
      is how much of the internal data structure needs to be looked at. *)
let _log_stats () =
  Printf.printf
    "Oowd.positionned_childs: %d states considered, %d dom updates\n%!"
    !_states_considered !_dom_updates;
  _states_considered := 0;
  _dom_updates := 0

let positionned_childs ~remove childs parent =
  let map x = x
  and reduce left right = Concat { synced = false; left; right } in
  let reducer = ref (R.make ~map ~reduce) in
  let rec update_childs = function
    | Leaf leaf -> (
        incr _states_considered;
        match leaf.state with
        | Added -> ()
        | New add ->
            incr _dom_updates;
            leaf.state <- Added;
            add parent leaf.elt)
    | Concat concat ->
        incr _states_considered;
        if not concat.synced then (
          update_childs concat.left;
          update_childs concat.right;
          concat.synced <- true (* Set last to resume after an exception. *))
  in
  let remove_dropped c () =
    match c with
    | Leaf leaf ->
        incr _dom_updates;
        remove parent leaf.elt
    | Concat _ -> assert false
  in
  let$ childs = Lwd_seq.bind childs Fun.id in
  let dropped, r = R.update_and_get_dropped !reducer childs in
  reducer := r;
  R.fold_dropped `Map remove_dropped dropped ();
  Option.iter update_childs (R.reduce r);
  _log_stats ()
