(* Demonstrates the correct behaviour of resizable weak arrays. *)

module W = Res.Weak
module Array = W  (* allows more convenient array access *)

class foo = object end

let ra = W.empty ()

let _ =
  W.add_one ra (Some (new foo));
  match ra.(0) with
  | Some _ -> print_endline "Correctly allocated!"
  | _ -> print_endline "Already deallocated??"

let _ =
  Gc.full_major ();
  match ra.(0) with
  | Some _ -> print_endline "Still not deallocated?"
  | _ -> print_endline "Correctly deallocated!"
