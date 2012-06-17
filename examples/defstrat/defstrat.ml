(* Demonstration of the default reallocation strategy in action *)

open Res.Array

let info v r = Printf.printf "virtual length: %3d  real length: %3d\n" v r

let _ =
  let ar = empty () in
  for _i = 1 to 100 do info (length ar) (real_length ar); add_one ar 42 done;
  for _i = 1 to 20 do info (length ar) (real_length ar); remove_n ar 5 done;
  info (length ar) (real_length ar)
