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

module type Implementation = sig
  type 'a t
  val name : string
  val length : 'a t -> int
  val make : int -> 'a -> 'a t
  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit
end

module Make (S : Strat.T) (Impl : Implementation) = struct
  module Strategy = S

  type strategy = Strategy.t

  type 'a t = {
    mutable ar : 'a option Impl.t;
    mutable vlix : int;
    mutable strategy : strategy
  }

  let name = Impl.name

  let invalid_arg str = invalid_arg (name ^ "." ^ str)
  let failwith str = failwith (name ^ "." ^ str)

  let length ra = ra.vlix + 1
  let lix ra = ra.vlix

  let real_length ra = Impl.length ra.ar
  let real_lix ra = real_length ra - 1

  let unsafe_get_ar ar ix =
    match Impl.unsafe_get ar ix with
    | None -> failwith "unsafe_get_ar: element undefined - concurrent access?"
    | Some el -> el

  let unsafe_get ra ix = unsafe_get_ar ra.ar ix

  let unsafe_set_ar ar ix el = Impl.unsafe_set ar ix (Some el)
  let unsafe_set ra ix el = unsafe_set_ar ra.ar ix el

  let get ra n =
    if n > ra.vlix || n < 0 then invalid_arg "get"
    else unsafe_get ra n

  let set ra n el =
    if n > ra.vlix || n < 0 then invalid_arg "set"
    else unsafe_set ra n el

  let creator n = Impl.make n None

  let screate_fresh strategy n =
    let res = { ar = creator 0; vlix = n - 1; strategy = strategy } in
    res.ar <- creator (Strategy.grow strategy n);
    res

  let create_fresh n = screate_fresh Strategy.default n

  let create_from ra =
    { ar = creator (length ra); vlix = ra.vlix; strategy = ra.strategy }

  let sempty strategy =
    let res = { ar = creator 0; vlix = -1; strategy = strategy } in
    res.ar <- creator (Strategy.grow strategy 0);
    res

  let empty () = sempty Strategy.default

  let screate strategy n x =
    let res = screate_fresh strategy n in
    let res_ar = res.ar in
    let el = Some x in
    for i = 0 to n - 1 do Impl.unsafe_set res_ar i el done;
    res

  let smake = screate

  let create n = smake Strategy.default n

  let make = create

  let sinit strategy n f =
    let res = screate_fresh strategy n in
    let res_ar = res.ar in
    for i = 0 to n - 1 do unsafe_set_ar res_ar i (f i) done;
    res

  let init n f = sinit Strategy.default n f

  let to_array ({ ar = ar } as ra) =
    Array.init (length ra) (fun i -> unsafe_get_ar ar i)

  let sof_array strategy ar =
    sinit strategy (Array.length ar) (fun i -> Array.unsafe_get ar i)

  let of_array ar = sof_array Strategy.default ar

  let get_strategy ra = ra.strategy

  let resizer some_lix ra len =
    let ar = creator len in
    let old_ar = ra.ar in
    for i = 0 to some_lix do
      Impl.unsafe_set ar i (Impl.unsafe_get old_ar i)
    done;
    ra.ar <- ar

  let enforce_strategy ra =
    let real_len = real_length ra in
    let new_len = length ra in
    let new_real_len = Strategy.shrink ra.strategy ~real_len ~new_len in
    if new_real_len <> -1 then resizer ra.vlix ra new_real_len

  let set_strategy ra strategy = ra.strategy <- strategy; enforce_strategy ra
  let put_strategy ra strategy = ra.strategy <- strategy

  let make_matrix sx sy init =
    let res = create_fresh sx in
    let res_ar = res.ar in
    for i = 0 to res.vlix do
      unsafe_set_ar res_ar i (make sy init)
    done;
    res

  let copy ({ ar = ar } as ra) =
    let new_ar = Impl.make (real_length ra) (Impl.unsafe_get ar 0) in
    for i = 1 to real_lix ra do
      Impl.unsafe_set new_ar i (Impl.unsafe_get ar i)
    done;
    { ra with ar = new_ar }

  let unsafe_blit_on_other { ar = ar1 } ofs1 { ar = ar2 } ofs2 len =
    let ofs_diff = ofs2 - ofs1 in
    for i = ofs1 to ofs1 + len - 1 do
      Impl.unsafe_set ar2 (i + ofs_diff) (Impl.unsafe_get ar1 i)
    done

  let append ra1 ra2 =
    match ra1.vlix, ra2.vlix with
    | -1, -1 -> empty ()
    | _, -1 -> copy ra1
    | -1, _ -> copy ra2
    | _ ->
        let len1 = length ra1 in
        let len2 = length ra2 in
        let res = create_fresh (len1 + len2) in
        unsafe_blit_on_other ra1 0 res 0 len1;
        unsafe_blit_on_other ra2 0 res len1 len2;
        res

  let rec concat_aux res offset = function
    | [] -> res
    | h::t ->
        if h.vlix < 0 then concat_aux res offset t
        else
          let len = length h in
          unsafe_blit_on_other h 0 res offset len;
          concat_aux res (offset + len) t

  let concat l =
    let len = List.fold_left (fun a el -> a + length el) 0 l in
    if len = 0 then empty ()
    else concat_aux (create_fresh len) 0 l

  let unsafe_sub ra ofs len =
    let res = create_fresh len in
    unsafe_blit_on_other ra ofs res 0 len;
    res

  let sub ra ofs len =
    if ofs < 0 || len < 0 || ofs + len > length ra then invalid_arg "sub"
    else unsafe_sub ra ofs len

  let guarantee_ix ra ix =
    if real_lix ra < ix then
      resizer ra.vlix ra (Strategy.grow ra.strategy (ix + 1))

  let maybe_grow_ix ra new_lix = guarantee_ix ra new_lix; ra.vlix <- new_lix

  let add_one ra x =
    let n = length ra in
    maybe_grow_ix ra n;
    unsafe_set ra n x

  let unsafe_remove_one ra =
    Impl.unsafe_set ra.ar ra.vlix None;
    ra.vlix <- ra.vlix - 1;
    enforce_strategy ra

  let remove_one ra =
    if ra.vlix < 0 then failwith "remove_one"
    else unsafe_remove_one ra

  let unsafe_remove_n ra n =
    let old_vlix = ra.vlix in
    let old_ar = ra.ar in
    ra.vlix <- old_vlix - n;
    enforce_strategy ra;
    if old_ar == ra.ar then
      for i = ra.vlix + 1 to old_vlix do
        Impl.unsafe_set old_ar i None
      done

  let remove_n ra n =
    if n > length ra || n < 0 then invalid_arg "remove_n"
    else unsafe_remove_n ra n

  let unsafe_remove_range ra ofs len =
    let ofs_len = ofs + len in
    unsafe_blit_on_other ra ofs_len ra ofs (length ra - ofs_len);
    unsafe_remove_n ra len

  let remove_range ra ofs len =
    if ofs < 0 || len < 0 || ofs + len > length ra then
      invalid_arg "remove_range"
    else unsafe_remove_range ra ofs len

  let clear ra = unsafe_remove_n ra (length ra)

  let unsafe_swap ra n m =
    let tmp = unsafe_get ra n in
    unsafe_set ra n (unsafe_get ra m);
    unsafe_set ra m tmp

  let swap ra n m =
    if n > ra.vlix || m > ra.vlix || n < 0 || m < 0 then invalid_arg "swap"
    else unsafe_swap ra n m

  let unsafe_swap_in_last ({ ar = ar } as ra) n =
    Impl.unsafe_set ar n (Impl.unsafe_get ar ra.vlix);
    unsafe_remove_one ra

  let swap_in_last ra n =
    if n > ra.vlix || n < 0 then invalid_arg "swap_in_last"
    else unsafe_swap_in_last ra n

  let unsafe_fill ra ofs len x =
    let last = ofs + len - 1 in
    maybe_grow_ix ra (max last ra.vlix);
    let el = Some x in
    let ar = ra.ar in
    for i = ofs to last do Impl.unsafe_set ar i el done

  let fill ra ofs len x =
    if ofs < 0 || len < 0 || ofs > length ra then invalid_arg "fill"
    else unsafe_fill ra ofs len x

  let unsafe_blit { ar = ar1 } ofs1 ({ ar = ar2 } as ra2) ofs2 len =
    guarantee_ix ra2 (ofs2 + len - 1);
    if ofs1 < ofs2 then
      for i = len - 1 downto 0 do
        Impl.unsafe_set ar2 (ofs2 + i) (Impl.unsafe_get ar1 (ofs1 + i))
      done
    else
      for i = 0 to len - 1 do
        Impl.unsafe_set ar2 (ofs2 + i) (Impl.unsafe_get ar1 (ofs1 + i))
      done

  let blit ra1 ofs1 ra2 ofs2 len =
    if
      len < 0 || ofs1 < 0 || ofs2 < 0
      || ofs1 + len > length ra1 || ofs2 > length ra2
    then invalid_arg "blit"
    else unsafe_blit ra1 ofs1 ra2 ofs2 len

  let rec to_list_aux ar i accu =
    if i < 0 then accu
    else to_list_aux ar (i - 1) (unsafe_get_ar ar i :: accu)

  let to_list ra = to_list_aux ra.ar ra.vlix []

  let rec of_list_aux res_ar i = function
    | [] -> ()
    | h::t ->
        unsafe_set_ar res_ar i h;
        of_list_aux res_ar (i + 1) t

  let of_list l =
    let res = create_fresh (List.length l) in
    of_list_aux res.ar 0 l;
    res

  let sof_list s l =
    let res = screate_fresh s (List.length l) in
    of_list_aux res.ar 0 l;
    res

  let iter f ({ ar = ar } as ra) =
    for i = 0 to ra.vlix do f (unsafe_get_ar ar i) done

  let map f ({ ar = ar } as ra) =
    let { ar = res_ar } as res = create_from ra in
    for i = 0 to res.vlix do
      unsafe_set_ar res_ar i (f (unsafe_get_ar ar i))
    done;
    res

  let iteri f ({ ar = ar } as ra) =
    for i = 0 to ra.vlix do f i (unsafe_get_ar ar i) done

  let mapi f ({ ar = ar } as ra) =
    let { ar = res_ar } as res = create_from ra in
    for i = 0 to res.vlix do
      unsafe_set_ar res_ar i (f i (unsafe_get_ar ar i))
    done;
    res

  let fold_left f accu ({ ar = ar } as ra) =
    let res = ref accu in
    for i = 0 to ra.vlix do
      res := f !res (unsafe_get_ar ar i)
    done;
    !res

  let fold_right f ({ ar = ar } as ra) accu =
    let res = ref accu in
    for i = ra.vlix downto 0 do
      res := f (unsafe_get_ar ar i) !res
    done;
    !res

  let rec for_all_aux i p ra =
    i > ra.vlix || p (unsafe_get ra i) && for_all_aux (i + 1) p ra

  let for_all p ra = for_all_aux 0 p ra

  let rec exists_aux i p ra =
    i <= ra.vlix && (p (unsafe_get ra i) || exists_aux (i + 1) p ra)

  let exists p ra = exists_aux 0 p ra

  let rec mem_aux i x ra =
    i <= ra.vlix && (unsafe_get ra i = x || mem_aux (i + 1) x ra)

  let mem x ra = mem_aux 0 x ra

  let rec memq_aux i x ra =
    i <= ra.vlix && (unsafe_get ra i == x || memq_aux (i + 1) x ra)

  let memq x ra = memq_aux 0 x ra

  let rec pos_aux i x ra =
    if i > ra.vlix then None
    else if unsafe_get ra i = x then Some i
    else pos_aux (i + 1) x ra

  let pos x ra = pos_aux 0 x ra

  let rec posq_aux i x ra =
    if i > ra.vlix then None
    else if unsafe_get ra i == x then Some i
    else posq_aux (i + 1) x ra

  let posq x ra = posq_aux 0 x ra

  let rec find_aux i p ra =
    if i > ra.vlix then raise Not_found
    else
      let el = unsafe_get ra i in
      if p el then el
      else find_aux (i + 1) p ra

  let find p ra = find_aux 0 p ra

  let rec find_index_aux p ra i =
    if i > ra.vlix then raise Not_found
    else if p (unsafe_get ra i) then i
    else find_index_aux p ra (i + 1)

  let find_index p ra i =
    if i < 0 then invalid_arg "find_index"
    else find_index_aux p ra i

  let filter p ({ ar = ar } as ra) =
    let res = sempty ra.strategy in
    for i = 0 to ra.vlix do
      let el = unsafe_get_ar ar i in
      if p el then add_one res el
    done;
    res

  let find_all = filter

  let filter_in_place p ({ ar = ar } as ra) =
    let dest = ref 0 in
    let pos = ref 0 in
    while !pos <= ra.vlix do
      let el = unsafe_get_ar ar !pos in
      if p el then begin
        unsafe_set_ar ar !dest el;
        incr dest
      end;
      incr pos
    done;
    unsafe_remove_n ra (!pos - !dest)

  let partition p ({ ar = ar } as ra) =
    let res1, res2 as res = sempty ra.strategy, sempty ra.strategy in
    for i = 0 to ra.vlix do
      let el = unsafe_get_ar ar i in
      if p el then add_one res1 el
      else add_one res2 el
    done;
    res
end
