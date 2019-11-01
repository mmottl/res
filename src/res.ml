(*
   RES - Automatically Resizing Contiguous Memory for OCaml

   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

module DefStrat = struct
  type t = float * float * int

  let default = 1.5, 0.5, 16

  let grow (waste, _, min_size) new_len =
    max (truncate (float new_len *. waste)) min_size

  let shrink (waste, shrink_trig, min_size) ~real_len ~new_len =
    if real_len > min_size && truncate (float real_len *. shrink_trig) > new_len
    then max (truncate (float new_len *. waste)) min_size
    else -1
end

module BitDefStrat = struct
  include DefStrat
  let default = 1.5, 0.5, 1024
end

module Array_impl = struct
  type 'a t = 'a array

  let name = "Res.Array"
  let length = Array.length
  let make = Array.make
  let unsafe_get = Array.get
  let unsafe_set = Array.set
end

module Unsafe_float_impl = struct
  include Float.Array

  type el = float

  let unsafe_blit (ar1 : t) ofs1 ar2 ofs2 len =
    if ofs1 < ofs2 then
      for i = len - 1 downto 0 do
        unsafe_set ar2 (ofs2 + i) (unsafe_get ar1 (ofs1 + i)) done
    else
      for i = 0 to len - 1 do
        unsafe_set ar2 (ofs2 + i) (unsafe_get ar1 (ofs1 + i)) done
end

module Float_impl = struct
  include Unsafe_float_impl

  let name = "Res.Floats"
  let unsafe_get = get
  let unsafe_set = set

  let unsafe_blit ar1 ofs1 ar2 ofs2 len =
    if
      len < 0 || ofs1 < 0
      || ofs1 > length ar1 - len
      || ofs2 < 0 || ofs2 > length ar2 - len
    then invalid_arg "Res.Floats.blit"
    else unsafe_blit ar1 ofs1 ar2 ofs2 len
end

(* TODO: create safe version *)
(* Code of the Bit-module due to Jean-Christophe Filliatre *)
module Bit_impl = struct
  type el = bool
  type t = { length : int; bits : int array }

  let name = "Res.Bits"

  let length v = v.length

  let bpi = Sys.word_size - 2

  let bit_j = Array.init bpi (fun j -> 1 lsl j)
  let bit_not_j = Array.init bpi (fun j -> max_int - bit_j.(j))

  let low_mask = Array.make (bpi + 1) 0

  let () =
    for i = 1 to bpi do
      low_mask.(i) <- low_mask.(i-1) lor bit_j.(i - 1)
    done

  let keep_lowest_bits a j = a land low_mask.(j)

  let high_mask = Array.init (bpi + 1) (fun j -> low_mask.(j) lsl (bpi-j))

  let keep_highest_bits a j = a land high_mask.(j)

  let make n b =
    let initv = if b then max_int else 0 in
    let r = n mod bpi in
    if r = 0 then { length = n; bits = Array.make (n / bpi) initv }
    else begin
      let s = n / bpi in
      let b = Array.make (s + 1) initv in
      b.(s) <- b.(s) land low_mask.(r);
      { length = n; bits = b }
    end

  let create n = make n false

  let pos n =
    let i = n / bpi in
    let j = n mod bpi in
    if j < 0 then (i - 1, j + bpi) else (i,j)

  let unsafe_get v n =
    let (i,j) = pos n in
    ((Array.unsafe_get v.bits i) land (Array.unsafe_get bit_j j)) > 0

  let unsafe_set v n b =
    let (i,j) = pos n in
    if b then
      Array.unsafe_set v.bits i
        ((Array.unsafe_get v.bits i) lor (Array.unsafe_get bit_j j))
    else
      Array.unsafe_set v.bits i
        ((Array.unsafe_get v.bits i) land (Array.unsafe_get bit_not_j j))

  let blit_bits a i m v n =
    let (i',j) = pos n in
    if j == 0 then
      Array.unsafe_set v i'
        ((keep_lowest_bits (a lsr i) m) lor
         (keep_highest_bits (Array.unsafe_get v i') (bpi - m)))
    else
      let d = m + j - bpi in
      if d > 0 then begin
        Array.unsafe_set v i'
          (((keep_lowest_bits (a lsr i) (bpi - j)) lsl j) lor
           (keep_lowest_bits (Array.unsafe_get v i') j));
        Array.unsafe_set v (i' + 1)
          ((keep_lowest_bits (a lsr (i + bpi - j)) d) lor
           (keep_highest_bits (Array.unsafe_get v (i' + 1)) (bpi - d)))
      end else
        Array.unsafe_set v i'
          (((keep_lowest_bits (a lsr i) m) lsl j) lor
           ((Array.unsafe_get v i') land (low_mask.(j) lor high_mask.(-d))))

  let blit_int a v n =
    let (i,j) = pos n in
    if j == 0 then
      Array.unsafe_set v i a
    else begin
      Array.unsafe_set v i
        ( (keep_lowest_bits (Array.unsafe_get v i) j) lor
         ((keep_lowest_bits a (bpi - j)) lsl j));
      Array.unsafe_set v (i + 1)
        ((keep_highest_bits (Array.unsafe_get v (i + 1)) (bpi - j)) lor
         (a lsr (bpi - j)))
    end

  let unsafe_blit v1 ofs1 v2 ofs2 len =
    let (bi,bj) = pos ofs1 in
    let (ei,ej) = pos (ofs1 + len - 1) in
    if bi == ei then
      blit_bits (Array.unsafe_get v1.bits bi) bj len v2.bits ofs2
    else begin
      blit_bits (Array.unsafe_get v1.bits bi) bj (bpi - bj) v2.bits ofs2;
      let n = ref (ofs2 + bpi - bj) in
      for i = bi + 1 to ei - 1 do
        blit_int (Array.unsafe_get v1.bits i) v2.bits !n;
        n := !n + bpi
      done;
      blit_bits (Array.unsafe_get v1.bits ei) 0 (ej + 1) v2.bits !n
    end
end

module Buffer_impl = struct
  type el = char
  type t = Bytes.t

  let length = Bytes.length
  let create = Bytes.create
  let make = Bytes.make

  let name = "Res.Buffer"
  let unsafe_get = Bytes.unsafe_get
  let unsafe_set = Bytes.unsafe_set
  let unsafe_blit = Bytes.unsafe_blit
end

module MakeArray (S : Strat.T) = Pres_impl.Make (S) (Array_impl)
module MakeFloats (S : Strat.T) = Nopres_impl.Make (S) (Float_impl)
module MakeBits (S : Strat.T) = Nopres_impl.Make (S) (Bit_impl)
module MakeWeak (S : Strat.T) = Weak_impl.Make (S)

module MakeBuffer (S : Strat.T) = struct
  module B = Nopres_impl.Make (S) (Buffer_impl)
  include B

  let create _ = empty ()
  let contents buf = Bytes.sub_string buf.ar 0 (length buf)
  let reset = clear
  let add_char = add_one

  let add_string buf str =
    let old_buf_len = length buf in
    let len = String.length str in
    maybe_grow_ix buf (buf.vlix + len);
    Bytes.blit_string str 0 buf.ar old_buf_len len

  let add_substring buf str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      invalid_arg "add_substring";
    let old_buf_len = length buf in
    maybe_grow_ix buf (buf.vlix + len);
    Bytes.blit_string str ofs buf.ar old_buf_len len

  let add_buffer b1 b2 =
    let len = length b2 in
    let old_buf_len = length b1 in
    maybe_grow_ix b1 (b1.vlix + len);
    Bytes.blit b2.ar 0 b1.ar old_buf_len len

  let add_channel buf ch len =
    let old_buf_len = length buf in
    maybe_grow_ix buf (buf.vlix + len);
    try really_input ch buf.ar old_buf_len len with
    | End_of_file ->
        buf.vlix <- old_buf_len - 1;
        enforce_strategy buf

  let rec add_full_channel_f_aux buf ch len adjust =
    if len > 0 then begin
      let old_buf_len = length buf in
      maybe_grow_ix buf (buf.vlix + len);
      let r = input ch buf.ar old_buf_len len in
      if r > 0 then begin
        let diff = len - r in
        if diff > 0 then begin
          buf.vlix <- buf.vlix - diff;
          add_full_channel_f_aux buf ch len adjust end
        else add_full_channel_f_aux buf ch (adjust len) adjust end
      else buf.vlix <- buf.vlix - len end

  let add_full_channel_f buf ch len adjust =
    add_full_channel_f_aux buf ch len adjust; enforce_strategy buf

  let add_full_channel buf ch = add_full_channel_f buf ch 4096 (fun n -> n)

  let output_buffer ch buf = output ch buf.ar 0 (length buf)

  let sof_string strategy str =
    sinit strategy (String.length str) (fun i -> String.unsafe_get str i)

  let of_string = sof_string Strategy.default
end

module Array = MakeArray (DefStrat)
module Floats = MakeFloats (DefStrat)
module Bits = MakeBits (BitDefStrat)
module Weak = MakeWeak (DefStrat)
module Buffer = MakeBuffer (DefStrat)
