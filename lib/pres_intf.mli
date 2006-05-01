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

(* $Id: pres_intf.mli,v 1.17 2005/11/07 20:25:28 mottl Exp $ *)

(** Interface to parameterized resizable arrays *)
module type T = sig
  (** {6 Signatures and types} *)

  (** Module implementing the reallocation strategy *)
  module Strategy : Strat.T

  type strategy = Strategy.t (** Type of reallocation strategy *)
  type 'a t (** Type of parameterized resizable arrays *)


  (** {6 Index and length information} *)

  val length : 'a t -> int
  (** [length ra] @return (virtual) length of resizable array [ra]
      excluding the reserved space. *)

  val lix : 'a t -> int
  (** [lix ra] @return (virtual) last index of resizable array [ra]
      excluding the reserved space. *)

  val real_length : 'a t -> int
  (** [real_length ra] @return (real) length of resizable array [ra]
      including the reserved space. *)

  val real_lix : 'a t -> int
  (** [real_lix ra] @return (real) last index of resizable array [ra]
      including the reserved space. *)


  (** {6 Getting and setting} *)

  val get : 'a t -> int -> 'a
  (** [get ra n] @return the [n]th element of [ra].
      @raise Invalid_argument if index out of bounds. *)

  val set : 'a t -> int -> 'a -> unit
  (** [set ra n] sets the [n]th element of [ra].
      @raise Invalid_argument if index out of bounds. *)


  (** {6 Creation of resizable arrays} *)

  val sempty : strategy -> 'a t
  (** [sempty s] @return an empty resizable array using strategy [s]. *)

  val empty : unit -> 'a t
  (** [empty ()] same as [sempty] but uses default strategy. *)

  val screate : strategy -> int -> 'a -> 'a t
  (** [screate s n el] @return a resizable array of length [n] containing
      element [el] only using strategy [s]. *)

  val create : int -> 'a -> 'a t
  (** [create n el] same as [screate] but uses default strategy. *)

  val smake : strategy -> int -> 'a -> 'a t
  (** [smake s n el] same as [screate]. *)

  val make : int -> 'a -> 'a t
  (** [make n el] same as [create]. *)

  val sinit : strategy -> int -> (int -> 'a) -> 'a t
  (** [sinit s n f] @return an array of length [n] containing
      elements that were created by applying function [f] to the index,
      using strategy [s]. *)

  val init : int -> (int -> 'a) -> 'a t
  (** [init n f] sames as [sinit] but uses default strategy. *)


  (** {6 Strategy handling} *)

  val get_strategy : 'a t -> strategy
  (** [get_strategy ra] @return the reallocation strategy used by
      resizable array [ra]. *)

  val set_strategy : 'a t -> strategy -> unit
  (** [set_strategy ra s] sets the reallocation strategy of
      resizable array [ra] to [s], possibly causing an immediate
      reallocation. *)

  val put_strategy : 'a t -> strategy -> unit
  (** [put_strategy ra s] sets the reallocation strategy of
      resizable array [ra] to [s]. Reallocation is only done at later
      changes in size. *)

  val enforce_strategy : 'a t -> unit
  (** [enforce_strategy ra] forces a reallocation if necessary
      (e.g. after a [put_strategy]. *)


  (** {6 Matrix functions} *)

  val make_matrix : int -> int -> 'a -> 'a t t
  (** [make_matrix sx sy el] creates a (resizable) matrix of
      dimensions [sx] and [sy] containing element [el] only. Both
      dimensions are controlled by the default strategy. *)


  (** {6 Copying, blitting and range extraction} *)

  val copy : 'a t -> 'a t
  (** [copy ra] @return a copy of resizable array [ra]. The two
      arrays share the same strategy! *)

  val sub : 'a t -> int -> int -> 'a t
  (** [sub ra ofs len] @return a resizable subarray of length [len]
      from resizable array [ra] starting at offset [ofs] using the
      default strategy.
      @raise Invalid_argument if parameters do not denote a correct
      subarray. *)

  val fill : 'a t -> int -> int -> 'a -> unit
  (** [fill ra ofs len el] fills resizable array [ra] from offset
      [ofs] with [len] elements [el], possibly adding elements at the
      end. Raises [Invalid_argument] if offset [ofs] is larger than the
      length of the array. *)

  val blit : 'a t -> int -> 'a t -> int -> int -> unit
  (** [blit ra1 ofs1 ra2 ofs2 len] blits resizable array [ra1] onto
      [ra2] reading [len] elements from offset [ofs1] and writing them
      to [ofs2], possibly adding elements at the end of ra2. Raises
      [Invalid_argument] if [ofs1] and [len] do not designate a valid
      subarray of [ra1] or if [ofs2] is larger than the length of
      [ra2]. *)


  (** {6 Combining resizable arrays} *)

  val append : 'a t -> 'a t -> 'a t
  (** [append ra1 ra2] @return a new resizable array using the
      default strategy and copying [ra1] and [ra2] in this order onto
      it. *)

  val concat : 'a t list -> 'a t
  (** [concat l] @return a new resizable array using the default
      strategy and copying all resizable arrays in [l] in their respective
      order onto it. *)


  (** {6 Adding and removing elements} *)

  val add_one : 'a t -> 'a -> unit
  (** [add_one ra el] adds element [el] to resizable array [ra],
      possibly causing a reallocation. *)

  val remove_one : 'a t -> unit
  (** [remove_one ra] removes the last element of resizable array
      [ra], possibly causing a reallocation.
      @raise Failure if the array is empty. *)

  val remove_n : 'a t -> int -> unit
  (** [remove_n ra n] removes the last n elements of resizable
      array [ra], possibly causing a reallocation.
      @raise Invalid_arg if there are not enough elements or [n < 0]. *)

  val remove_range : 'a t -> int -> int -> unit
  (** [remove_range ra ofs len] removes [len] elements from resizable
      array [ra] starting at [ofs] and possibly causing a
      reallocation.
      @raise Invalid_argument if range is invalid. *)

  val clear : 'a t -> unit
  (** [clear ra] removes all elements from resizable array [ra],
      possibly causing a reallocation. *)


  (** {6 Swapping} *)

  val swap : 'a t -> int -> int -> unit
  (** [swap ra n m] swaps elements at indices [n] and [m].
      @raise Invalid_argument if any index is out of range. *)

  val swap_in_last : 'a t -> int -> unit
  (** [swap_in_last ra n] swaps the last element with the one at
      position [n].
      @raise Invalid_argument if index [n] is out of range. *)


  (** {6 Array conversions} *)

  val to_array : 'a t -> 'a array
  (** [to_array ra] converts a resizable array to a standard one. *)

  val sof_array : strategy -> 'a array -> 'a t
  (** [sof_array s ar] converts a standard array to a resizable one,
      using strategy [s]. *)

  val of_array : 'a array -> 'a t
  (** [of_array ar] converts a standard array to a resizable one
      using the default strategy. *)


  (** {6 List conversions} *)

  val to_list : 'a t -> 'a list
  (** [to_list ra] converts resizable array [ra] to a list. *)

  val sof_list : strategy -> 'a list -> 'a t
  (** [sof_list s l] creates a resizable array using strategy [s] and
      the elements in list [l]. *)

  val of_list : 'a list -> 'a t
  (** [of_list l] creates a resizable array using the default
      strategy and the elements in list [l]. *)


  (** {6 Iterators} *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f ra] applies the unit-function [f] to each element in
      resizable array [ra]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f ra] @return a resizable array using the strategy of
      [ra] and mapping each element in [ra] to its corresponding position
      in the new array using function [f]. *)

  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** [iteri f ra] applies the unit-function [f] to each index and
      element in resizable array [ra]. *)

  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  (** [mapi f ra] @return a resizable array using the strategy of
      [ra] and mapping each element in [ra] to its corresponding
      position in the new array using function [f] and the index
      position. *)

  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** [fold_left f a ra] left-folds values in resizable array [ra]
      using function [f] and start accumulator [a]. *)

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold_right f a ra] right-folds values in resizable array [ra]
      using function [f] and start accumulator [a]. *)


  (** {6 Scanning of resizable arrays} *)

  val for_all : ('a -> bool) -> 'a t -> bool
  (** [for_all p ra] @return [true] if all elements in resizable
      array [ra] satisfy the predicate [p], [false] otherwise. *)

  val exists : ('a -> bool) -> 'a t -> bool
  (** [exists p ra] @return [true] if at least one element in
      resizable array [ra] satisfies the predicate [p], [false]
      otherwise. *)

  val mem : 'a -> 'a t -> bool
  (** [mem el ra] @return [true] if element [el] is logically equal
      to any element in resizable array [ra], [false] otherwise. *)

  val memq : 'a -> 'a t -> bool
  (** [memq el ra] @return [true] if element [el] is physically equal
      to any element in resizable array [ra], [false] otherwise. *)

  val pos : 'a -> 'a t -> int option
  (** [pos el ra] @return [Some index] if [el] is logically
      equal to the element at [index] in [ra], [None] otherwise.  [index]
      is the index of the first element that matches. *)

  val posq : 'a -> 'a t -> int option
  (** [posq el ra] @return [Some index] if [el] is physically
      equal to the element at [index] in [ra], [None] otherwise.  [index]
      is the index of the first element that matches. *)


  (** {6 Searching of resizable arrays} *)

  val find : ('a -> bool) -> 'a t -> 'a
  (** [find p ra] @return the first element in resizable array [ra]
      that satisfies predicate [p].
      @raise Not_found if there is no such element. *)

  val find_index : ('a -> bool) -> 'a t -> int -> int
  (** [find_index p ra pos] @return the index of the first element
      that satisfies predicate [p] in resizable array [ra], starting
      search at index [pos].
      @raise Not_found if there is no such element or if [pos] is larger
      than the highest index.
      @raise Invalid_argument if [pos] is negative. *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter p ra] @return a new resizable array by filtering
      out all elements in [ra] that satisfy predicate [p] using the same
      strategy as [ra]. *)

  val find_all : ('a -> bool) -> 'a t -> 'a t
  (** [find_all p ra] is the same as [filter] *)

  val filter_in_place : ('a -> bool) -> 'a t -> unit
  (** [filter_in_place p ra] as [filter], but filters in place. *)

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [partition p ra] @return a pair of resizable arrays, the
      left part containing only elements of [ra] that satisfy predicate
      [p], the right one only those that do not satisfy it. Both returned
      arrays are created using the strategy of [ra]. *)


  (** {6 {b UNSAFE STUFF - USE WITH CAUTION!}} *)

  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit

  val unsafe_sub : 'a t -> int -> int -> 'a t

  val unsafe_fill : 'a t -> int -> int -> 'a -> unit
  val unsafe_blit : 'a t -> int -> 'a t -> int -> int -> unit

  val unsafe_remove_one : 'a t -> unit
  val unsafe_remove_n : 'a t -> int -> unit

  val unsafe_swap : 'a t -> int -> int -> unit
  val unsafe_swap_in_last : 'a t -> int -> unit
end
