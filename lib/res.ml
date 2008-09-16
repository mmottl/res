(*
   RES - Automatically Resizing Contiguous Memory for OCaml

   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* $Id: res.ml,v 1.18 2005/11/07 20:25:28 mottl Exp $ *)

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

module Array_Impl = struct
  type 'a t = 'a array

  let name = "Res.Array"
  let length = Array.length
  let make = Array.make
  let unsafe_get = Array.unsafe_get
  let unsafe_set = Array.unsafe_set
end

module Int_Impl = struct
  type el = int
  type t = el array

  let name = "Res.Ints"
  let length = Array.length
  let create n = Array.create n 0
  let make = Array.make
  let unsafe_get = Array.unsafe_get
  let unsafe_set = Array.unsafe_set

  let unsafe_blit ar1 ofs1 ar2 ofs2 len =
    if ofs1 < ofs2 then
      for i = pred len downto 0 do
        unsafe_set ar2 (ofs2 + i) (unsafe_get ar1 (ofs1 + i)) done
    else
      for i = 0 to pred len do
        unsafe_set ar2 (ofs2 + i) (unsafe_get ar1 (ofs1 + i)) done
end

module Float_Impl = struct
  type el = float
  type t = el array

  let name = "Res.Floats"
  let length = Array.length
  let create n = Array.create n 0.0
  let make = Array.make
  let unsafe_get = Array.unsafe_get
  let unsafe_set = Array.unsafe_set

  let unsafe_blit ar1 ofs1 ar2 ofs2 len =
    if ofs1 < ofs2 then
      for i = pred len downto 0 do
        unsafe_set ar2 (ofs2 + i) (unsafe_get ar1 (ofs1 + i)) done
    else
      for i = 0 to pred len do
        unsafe_set ar2 (ofs2 + i) (unsafe_get ar1 (ofs1 + i)) done
end

(* Code of the Bit-module due to Jean-Christophe Filliatre *)
module Bit_Impl = struct
  type el = bool
  type t = { length : int; bits : int array }

  let name = "Res.Bits"

  let length v = v.length

  let bpi = Sys.word_size - 2

  let max_length = Sys.max_array_length * bpi

  let bit_j = Array.init bpi (fun j -> 1 lsl j)
  let bit_not_j = Array.init bpi (fun j -> max_int - bit_j.(j))

  let low_mask = Array.create (succ bpi) 0
  let _ =
    for i = 1 to bpi do low_mask.(i) <- low_mask.(i-1) lor bit_j.(pred i) done

  let keep_lowest_bits a j = a land low_mask.(j)

  let high_mask = Array.init (succ bpi) (fun j -> low_mask.(j) lsl (bpi-j))

  let keep_highest_bits a j = a land high_mask.(j)

  let make n b =
    let initv = if b then max_int else 0 in
    let r = n mod bpi in
    if r = 0 then { length = n; bits = Array.create (n / bpi) initv }
    else begin
      let s = n / bpi in
      let b = Array.create (succ s) initv in
      b.(s) <- b.(s) land low_mask.(r);
      { length = n; bits = b }
    end

  let create n = make n false

  let pos n =
    let i = n / bpi and j = n mod bpi in
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
        Array.unsafe_set v (succ i')
          ((keep_lowest_bits (a lsr (i + bpi - j)) d) lor
           (keep_highest_bits (Array.unsafe_get v (succ i')) (bpi - d)))
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
      Array.unsafe_set v (succ i)
        ((keep_highest_bits (Array.unsafe_get v (succ i)) (bpi - j)) lor
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
      for i = succ bi to pred ei do
        blit_int (Array.unsafe_get v1.bits i) v2.bits !n;
        n := !n + bpi
      done;
      blit_bits (Array.unsafe_get v1.bits ei) 0 (succ ej) v2.bits !n
    end
end

module Buffer_Impl = struct
  type el = char
  type t = string

  let name = "Res.Buffer"
  let length = String.length
  let create = String.create
  let make = String.make
  let unsafe_get = String.unsafe_get
  let unsafe_set = String.unsafe_set
  let unsafe_blit = String.unsafe_blit
end

module MakeArray (S : Strat.T) = Pres_impl.Make (S) (Array_Impl)
module MakeInts (S : Strat.T) = Nopres_impl.Make (S) (Int_Impl)
module MakeFloats (S : Strat.T) = Nopres_impl.Make (S) (Float_Impl)
module MakeBits (S : Strat.T) = Nopres_impl.Make (S) (Bit_Impl)
module MakeWeak (S : Strat.T) = Weak_impl.Make (S)

module MakeBuffer (S : Strat.T) = struct
  module B = Nopres_impl.Make (S) (Buffer_Impl)
  include B

  let create _ = empty ()
  let contents buf = String.sub buf.ar 0 (length buf)
  let reset = clear
  let add_char = add_one

  let add_string buf str =
    let old_buf_len = length buf
    and len = String.length str in
    maybe_grow_ix buf (buf.vlix + len);
    String.blit str 0 buf.ar old_buf_len len

  let add_substring buf str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      invalid_arg "add_substring";
    let old_buf_len = length buf in
    maybe_grow_ix buf (buf.vlix + len);
    String.blit str ofs buf.ar old_buf_len len

  let add_buffer b1 b2 = add_substring b1 b2.ar 0 (length b2)

  let add_channel buf ch len =
    let old_buf_len = length buf in
    maybe_grow_ix buf (buf.vlix + len);
    try really_input ch buf.ar old_buf_len len with
    | End_of_file -> buf.vlix <- pred old_buf_len; enforce_strategy buf

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
module Ints = MakeInts (DefStrat)
module Floats = MakeFloats (DefStrat)
module Bits = MakeBits (BitDefStrat)
module Weak = MakeWeak (DefStrat)
module Buffer = MakeBuffer (DefStrat)
