module Bytes = struct
  type t = String.t
  let length = String.length

  let create = String.create
  let make = String.make

  let sub_string = String.sub

  let blit = String.blit
  let blit_string = String.blit

  let unsafe_get = String.unsafe_get
  let unsafe_set = String.unsafe_set
  let unsafe_blit = String.unsafe_blit
end  (* Bytes *)
