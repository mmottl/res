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

  type t = {
    mutable ar : Impl.t;
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

  let unsafe_get ra ix = Impl.unsafe_get ra.ar ix
  let unsafe_set ra ix el = Impl.unsafe_set ra.ar ix el

  let get ra n =
    if n > ra.vlix || n < 0 then invalid_arg "get"
    else unsafe_get ra n

  let set ra n el =
    if n > ra.vlix || n < 0 then invalid_arg "set"
    else unsafe_set ra n el

  let creator = Impl.create
  let empty_ar = Impl.create 0

  let screate strategy n =
    let res = { ar = empty_ar; vlix = n - 1; strategy = strategy } in
    res.ar <- creator (Strategy.grow strategy n);
    res

  let smake strategy n x =
    let res = { ar = empty_ar; vlix = n - 1; strategy = strategy } in
    res.ar <- Impl.make (Strategy.grow strategy n) x;
    res

  let create_fresh n = screate Strategy.default n

  let create_from ra =
    { ar = creator (length ra); vlix = ra.vlix; strategy = ra.strategy }

  let sempty strategy =
    let res = { ar = empty_ar; vlix = -1; strategy = strategy } in
    res.ar <- creator (Strategy.grow strategy 0);
    res

  let empty () = sempty Strategy.default
  let create = screate Strategy.default
  let make = smake Strategy.default

  let sinit strategy n f =
    let res = smake strategy n (f 0) in
    let ar = res.ar in
    for i = 1 to n - 1 do Impl.unsafe_set ar i (f i) done;
    res

  let init n f = sinit Strategy.default n f

  let get_strategy ra = ra.strategy

  let resizer some_lix ({ ar = ar} as ra) len =
    let new_ar = creator len in
    for i = 0 to some_lix do
      Impl.unsafe_set new_ar i (Impl.unsafe_get ar i)
    done;
    ra.ar <- new_ar

  let enforce_strategy ra =
    let real_len = real_length ra in
    let new_len = length ra in
    let new_real_len = Strategy.shrink ra.strategy ~real_len ~new_len in
    if new_real_len <> -1 then resizer ra.vlix ra new_real_len

  let set_strategy ra strategy = ra.strategy <- strategy; enforce_strategy ra
  let put_strategy ra strategy = ra.strategy <- strategy

  let unsafe_blit_on_other ra1 ofs1 ra2 = Impl.unsafe_blit ra1.ar ofs1 ra2.ar

  let copy ra =
    let len = length ra in
    let ar = Impl.create len in
    Impl.unsafe_blit ra.ar 0 ar 0 len;
    { ra with ar = ar }

  let append ra1 ra2 = match ra1.vlix, ra2.vlix with
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

  let unsafe_remove_one ra = ra.vlix <- ra.vlix - 1; enforce_strategy ra

  let remove_one ra =
    if ra.vlix < 0 then failwith "remove_one"
    else unsafe_remove_one ra

  let unsafe_remove_n ra n = ra.vlix <- ra.vlix - n; enforce_strategy ra

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

  let clear ra = ra.vlix <- -1; enforce_strategy ra

  let unsafe_swap { ar = ar } n m =
    let tmp = Impl.unsafe_get ar n in
    Impl.unsafe_set ar n (Impl.unsafe_get ar m);
    Impl.unsafe_set ar m tmp

  let swap ra n m =
    if n > ra.vlix || m > ra.vlix || n < 0 || m < 0 then invalid_arg "swap"
    else unsafe_swap ra n m

  let unsafe_swap_in_last ({ ar = ar } as ra) n =
    Impl.unsafe_set ar n (Impl.unsafe_get ar ra.vlix);
    unsafe_remove_one ra

  let swap_in_last ra n =
    if n > ra.vlix || n < 0 then invalid_arg "swap_in_last"
    else unsafe_swap_in_last ra n

  let unsafe_fill ({ ar = ar } as ra) ofs len x =
    let last = ofs + len - 1 in
    maybe_grow_ix ra (max last ra.vlix);
    for i = ofs to last do Impl.unsafe_set ar i x done

  let fill ra ofs len x =
    if ofs < 0 || len < 0 || ofs > length ra then invalid_arg "fill"
    else unsafe_fill ra ofs len x

  let unsafe_blit ra1 ofs1 ra2 ofs2 len =
    guarantee_ix ra2 (ofs2 + len - 1);
    unsafe_blit_on_other ra1 ofs1 ra2 ofs2 len

  let blit ra1 ofs1 ra2 ofs2 len =
    if
      len < 0 || ofs1 < 0 || ofs2 < 0
      || ofs1 + len > length ra1 || ofs2 > length ra2
    then invalid_arg "blit"
    else unsafe_blit ra1 ofs1 ra2 ofs2 len

  let rec to_list_aux ar i accu =
    if i < 0 then accu
    else to_list_aux ar (i - 1) (Impl.unsafe_get ar i :: accu)

  let to_list ra = to_list_aux ra.ar ra.vlix []

  let rec of_list_aux ar i = function
    | [] -> ()
    | h::t -> Impl.unsafe_set ar i h; of_list_aux ar (i + 1) t

  let of_list l =
    let ra = create_fresh (List.length l) in
    of_list_aux ra.ar 0 l;
    ra

  let sof_list strategy l =
    let ra = screate strategy (List.length l) in
    of_list_aux ra.ar 0 l;
    ra

  let to_array ({ ar = ar } as ra) =
    Array.init (length ra) (fun i -> Impl.unsafe_get ar i)

  let sof_array strategy ar =
    sinit strategy (Array.length ar) (fun i -> Array.unsafe_get ar i)

  let of_array ar = sof_array Strategy.default ar

  let iter f ({ ar = ar } as ra) =
    for i = 0 to ra.vlix do f (Impl.unsafe_get ar i) done

  let map f ({ ar = ar } as ra) =
    let res = create_from ra in
    let res_ar = res.ar in
    for i = 0 to res.vlix do
      Impl.unsafe_set res_ar i (f (Impl.unsafe_get ar i))
    done;
    res

  let iteri f ({ ar = ar } as ra) =
    for i = 0 to ra.vlix do f i (Impl.unsafe_get ar i) done

  let mapi f ({ ar = ar } as ra) =
    let { ar = res_ar } as res = create_from ra in
    for i = 0 to res.vlix do
      Impl.unsafe_set res_ar i (f i (Impl.unsafe_get ar i))
    done;
    res

  let fold_left f accu ({ ar = ar } as ra) =
    let res = ref accu in
    for i = 0 to ra.vlix do
      res := f !res (Impl.unsafe_get ar i)
    done;
    !res

  let fold_right f ({ ar = ar } as ra) accu =
    let res = ref accu in
    for i = ra.vlix downto 0 do
      res := f (Impl.unsafe_get ar i) !res
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
      let el = Impl.unsafe_get ar i in
      if p el then add_one res el
    done;
    res

  let find_all = filter

  let filter_in_place p ({ ar = ar } as ra) =
    let dest = ref 0 in
    let pos = ref 0 in
    while !pos <= ra.vlix do
      let el = Impl.unsafe_get ar !pos in
      if p el then begin
        Impl.unsafe_set ar !dest el;
        incr dest
      end;
      incr pos
    done;
    unsafe_remove_n ra (!pos - !dest)

  let partition p ra =
    let res1, res2 as res = sempty ra.strategy, sempty ra.strategy in
    for i = 0 to ra.vlix do
      let el = unsafe_get ra i in
      if p el then add_one res1 el
      else add_one res2 el
    done;
    res
end
