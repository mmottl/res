(* Reads a file given as first argument into a buffer and prints it
   out again. Uses an exponentially growing read-ahead during reading
   (just for demonstration). *)

let _ =
  let buf = Res.Buffer.empty ()
  and file = open_in Sys.argv.(1) in
  Res.Buffer.add_full_channel_f buf file 50000 (( * ) 2);
  Res.Buffer.output_buffer stdout buf
