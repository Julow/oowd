open Lwd_infix

type +'a elt = unit Lwd.t * 'a
type -'a attr = 'a -> unit Lwd.t

let app f elt =
  f elt;
  Lwd.return ()

let root (ct, ce) = Lwd.observe (Lwd.map ~f:(fun () -> ce) ct)

let elt elm attrs =
  ( Lwd_utils.pack ((), fun () () -> ()) (List.map (fun attr -> attr elm) attrs),
    elm )

let attr f = f

let join f (ct, ce) parent =
  f ce parent;
  ct

module R = Lwd_seq.Reducer

type ('p, 'c) leaf_state =
  | Added
  | Dropped
      (** This state allow to avoid remove then add again when the document
          updates. This state should never be used if the smart sharing in
          [positionned_child] works. This depends on internal details of
          [Lwd_seq.bind] and the reducer. *)
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
     advance to optimise the document and allow the reducer to reuse old
     values. *)
  let leaf = Lwd_seq.element (Leaf { state = New add; elt = ce }) in
  let$ () = ct in
  leaf

let positionned_childs ~remove childs parent =
  let map x = x
  and reduce left right = Concat { synced = false; left; right } in
  let reducer = ref (R.make ~map ~reduce) in
  let rec update_childs = function
    | Leaf leaf -> (
        match leaf.state with
        | Added -> ()
        | Dropped ->
            (* Was changed in the sequence, avoid remove then add. *)
            leaf.state <- Added
        | New add ->
            leaf.state <- Added;
            add parent leaf.elt)
    | Concat concat ->
        if not concat.synced then (
          update_childs concat.left;
          update_childs concat.right;
          concat.synced <- true (* Set last to resume after an exception. *))
  in
  let set_dropped c () =
    match c with Leaf leaf -> leaf.state <- Dropped | Concat _ -> assert false
  in
  let remove_dropped c () =
    match c with
    | Leaf leaf -> if leaf.state = Dropped then remove parent leaf.elt
    | Concat _ -> assert false
  in
  let$ childs = Lwd_seq.bind childs Fun.id in
  let dropped, r = R.update_and_get_dropped !reducer childs in
  reducer := r;
  R.fold_dropped `Map set_dropped dropped ();
  Option.iter update_childs (R.reduce r);
  R.fold_dropped `Map remove_dropped dropped ()
