(*
   RES - Automatically Resizing Contiguous Memory for OCaml

   Copyright (C) 1999-2002  Markus Mottl
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

(* $Id: nopres_impl.ml,v 1.14 2005/11/07 20:25:28 mottl Exp $ *)

module type Implementation = sig
  type el
  type t

  val name : string
  val length : t -> int
  val create : int -> t
  val make : int -> el -> t
  val unsafe_get : t -> int -> el
  val unsafe_set : t -> int -> el -> unit
  val unsafe_blit : t -> int -> t -> int -> int -> unit
end

module Make (S : Strat.T) (Impl : Implementation) = struct
  module Strategy = S

  type strategy = Strategy.t

  type el = Impl.el

  type t = {mutable ar : Impl.t;
            mutable vlix : int;
            mutable strategy : strategy}

  let name = Impl.name

  let invalid_arg str = invalid_arg (name ^ "." ^ str)
  let failwith str = failwith (name ^ "." ^ str)

  let length ra = succ ra.vlix
  let lix ra = ra.vlix

  let real_length ra = Impl.length ra.ar
  let real_lix ra = pred (real_length ra)

  let unsafe_get ra ix = Impl.unsafe_get ra.ar ix
  let unsafe_set ra ix el = Impl.unsafe_set ra.ar ix el

  let get ra n =
    if n > ra.vlix or n < 0 then invalid_arg "get" else unsafe_get ra n

  let set ra n el =
    if n > ra.vlix or n < 0 then invalid_arg "set" else unsafe_set ra n el

  let creator = Impl.create

  let dummy_loc = 0
  let no_obj () = Obj.magic dummy_loc

  let screate strategy n =
    let res = {ar = no_obj (); vlix = pred n; strategy = strategy} in
    Strategy.grow strategy (fun rlen -> res.ar <- creator rlen) n; res

  let smake strategy n x =
    let res = {ar = no_obj (); vlix = pred n; strategy = strategy} in
    Strategy.grow strategy (fun rlen -> res.ar <- Impl.make rlen x) n; res

  let create_fresh n = screate Strategy.default n

  let create_from ra =
    {ar = creator (length ra); vlix = ra.vlix; strategy = ra.strategy}

  let sempty strategy =
    let res = {ar = no_obj (); vlix = -1; strategy = strategy} in
    Strategy.grow strategy (fun rlen -> res.ar <- creator rlen) 0; res

  let empty () = sempty Strategy.default

  let create = screate Strategy.default
  let make = smake Strategy.default

  let sinit strategy n f =
    let res = smake strategy n (f 0) in
    for i = 1 to pred n do unsafe_set res i (f i) done; res

  let init n f = sinit Strategy.default n f

  let get_strategy ra = ra.strategy

  let resizer some_lix ra len =
    let ar = creator len in
    for i = 0 to some_lix do Impl.unsafe_set ar i (unsafe_get ra i) done;
    ra.ar <- ar

  let enforce_strategy ra =
    let real_len = real_length ra and len = length ra in
    Strategy.shrink ra.strategy (resizer ra.vlix ra) real_len len

  let set_strategy ra strategy = ra.strategy <- strategy; enforce_strategy ra
  let put_strategy ra strategy = ra.strategy <- strategy

  let unsafe_blit_on_other ra1 ofs1 ra2 = Impl.unsafe_blit ra1.ar ofs1 ra2.ar

  let copy ra =
    let len = length ra in let ar = Impl.create len in
    Impl.unsafe_blit ra.ar 0 ar 0 len; {ra with ar = ar}

  let append ra1 ra2 = match ra1.vlix, ra2.vlix with
    | -1, -1 -> empty ()
    | _, -1 -> copy ra1
    | -1, _ -> copy ra2
    | _ -> let len1 = length ra1
           and len2 = length ra2 in
           let res = create_fresh (len1 + len2) in
           unsafe_blit_on_other ra1 0 res 0 len1;
           unsafe_blit_on_other ra2 0 res len1 len2; res

  let rec concat_aux res offset = function
    | [] -> res
    | h::t -> if h.vlix < 0 then concat_aux res offset t
              else let len = length h in
                   unsafe_blit_on_other h 0 res offset len;
                   concat_aux res (offset + len) t

  let concat l =
    let len = List.fold_left (fun a el -> a + length el) 0 l in
    if len = 0 then empty ()
    else concat_aux (create_fresh len) 0 l

  let unsafe_sub ra ofs len =
    let res = create_fresh len in unsafe_blit_on_other ra ofs res 0 len; res

  let sub ra ofs len =
    if ofs < 0 or len < 0 or ofs + len > length ra then invalid_arg "sub"
    else unsafe_sub ra ofs len

  let guarantee_ix ra ix =
    if real_lix ra < ix then
      Strategy.grow ra.strategy (resizer ra.vlix ra) (succ ix)

  let maybe_grow_ix ra new_lix = guarantee_ix ra new_lix; ra.vlix <- new_lix

  let add_one ra x = let n = length ra in maybe_grow_ix ra n; unsafe_set ra n x

  let unsafe_remove_one ra = ra.vlix <- pred ra.vlix; enforce_strategy ra

  let remove_one ra =
    if ra.vlix < 0 then failwith "remove_one" else unsafe_remove_one ra

  let unsafe_remove_n ra n = ra.vlix <- ra.vlix - n; enforce_strategy ra

  let remove_n ra n =
    if n > length ra || n < 0 then invalid_arg "remove_n"
    else unsafe_remove_n ra n

  let unsafe_remove_range ra ofs len =
    unsafe_blit_on_other ra (ofs + len) ra ofs (length ra - len);
    unsafe_remove_n ra len

  let remove_range ra ofs len =
    if ofs < 0 or len < 0 or ofs + len > length ra then
      invalid_arg "remove_range"
    else unsafe_remove_range ra ofs len

  let clear ra = ra.vlix <- -1; enforce_strategy ra

  let unsafe_swap ra n m =
    let tmp = unsafe_get ra n in
    unsafe_set ra n (unsafe_get ra m); unsafe_set ra m tmp

  let swap ra n m =
    if n > ra.vlix or m > ra.vlix or n < 0 or m < 0 then invalid_arg "swap"
    else unsafe_swap ra n m

  let unsafe_swap_in_last ra n =
    unsafe_set ra n (unsafe_get ra ra.vlix);
    unsafe_remove_one ra

  let swap_in_last ra n =
    if n > ra.vlix or n < 0 then invalid_arg "swap_in_last"
    else unsafe_swap_in_last ra n

  let unsafe_fill ra ofs len x =
    let last = ofs + pred len in
    guarantee_ix ra (max last ra.vlix);
    for i = ofs to last do unsafe_set ra i x done

  let fill ra ofs len x =
    if ofs < 0 or len < 0 or ofs > length ra then invalid_arg "fill"
    else unsafe_fill ra ofs len x

  let unsafe_blit ra1 ofs1 ra2 ofs2 len =
    guarantee_ix ra2 (ofs2 + pred len);
    unsafe_blit_on_other ra1 ofs1 ra2 ofs2 len

  let blit ra1 ofs1 ra2 ofs2 len =
    if len < 0 or ofs1 < 0 or ofs2 < 0 or ofs1 + len > length ra1 or
       ofs2 > length ra2 then invalid_arg "blit"
    else unsafe_blit ra1 ofs1 ra2 ofs2 len

  let rec to_list_aux ra i accu =
    if i < 0 then accu else to_list_aux ra (pred i) (unsafe_get ra i :: accu)

  let to_list ra = to_list_aux ra ra.vlix []

  let rec of_list_aux res i = function
    | [] -> res
    | h::t -> unsafe_set res i h; of_list_aux res (succ i) t

  let of_list l = of_list_aux (create_fresh (List.length l)) 0 l

  let sof_list strategy l = of_list_aux (screate strategy (List.length l)) 0 l

  let to_array ra = Array.init (length ra) (unsafe_get ra)

  let sof_array strategy ar =
    sinit strategy (Array.length ar) (Array.unsafe_get ar)

  let of_array ar = sof_array Strategy.default ar

  let iter f ra = for i = 0 to ra.vlix do f (unsafe_get ra i) done

  let map f ra =
    let res = create_from ra in
    for i = 0 to res.vlix do unsafe_set res i (f (unsafe_get ra i)) done; res

  let iteri f ra = for i = 0 to ra.vlix do f i (unsafe_get ra i) done

  let mapi f ra =
    let res = create_from ra in
    for i = 0 to res.vlix do unsafe_set res i (f i (unsafe_get ra i)) done; res

  let fold_left f accu ra =
    let res = ref accu in
    for i = 0 to ra.vlix do res := f !res (unsafe_get ra i) done; !res

  let fold_right f ra accu =
    let res = ref accu in
    for i = ra.vlix downto 0 do res := f (unsafe_get ra i) !res done; !res

  let rec for_all_aux i p ra =
    if i > ra.vlix then true
    else if p (unsafe_get ra i) then for_all_aux (succ i) p ra else false

  let for_all p ra = for_all_aux 0 p ra

  let rec exists_aux i p ra =
    if i > ra.vlix then false
    else if p (unsafe_get ra i) then true else exists_aux (succ i) p ra

  let exists p ra = exists_aux 0 p ra

  let rec mem_aux i x ra =
    if i > ra.vlix then false
    else if unsafe_get ra i = x then true else mem_aux (succ i) x ra

  let mem x ra = mem_aux 0 x ra

  let rec memq_aux i x ra =
    if i > ra.vlix then false
    else if unsafe_get ra i == x then true else memq_aux (succ i) x ra

  let memq x ra = memq_aux 0 x ra

  let rec pos_aux i x ra =
    if i > ra.vlix then None
    else if unsafe_get ra i = x then Some i else pos_aux (succ i) x ra

  let pos x ra = pos_aux 0 x ra

  let rec posq_aux i x ra =
    if i > ra.vlix then None
    else if unsafe_get ra i == x then Some i else posq_aux (succ i) x ra

  let posq x ra = posq_aux 0 x ra

  let rec find_aux i p ra =
    if i > ra.vlix then raise Not_found
    else let el = unsafe_get ra i in if p el then el else find_aux (succ i) p ra

  let find p ra = find_aux 0 p ra

  let rec find_index_aux p ra i =
    if i > ra.vlix then raise Not_found
    else if p (unsafe_get ra i) then i else find_index_aux p ra (succ i)

  let find_index p ra i =
    if i < 0 then invalid_arg "find_index" else find_index_aux p ra i

  let filter p ra =
    let res = sempty ra.strategy in
    for i = 0 to ra.vlix do
      let el = unsafe_get ra i in if p el then add_one res el done;
    res

  let find_all = filter

  let filter_in_place p ra =
    let dest = ref 0
    and pos = ref 0 in
    while !pos <= ra.vlix do
      let el = unsafe_get ra !pos in
      if p el then begin unsafe_set ra !dest el; incr dest end;
      incr pos done;
    unsafe_remove_n ra (!pos - !dest)

  let partition p ra =
    let res1, res2 as res = sempty ra.strategy, sempty ra.strategy in
    for i = 0 to ra.vlix do
      let el = unsafe_get ra i in
      if p el then add_one res1 el else add_one res2 el done;
    res
end
