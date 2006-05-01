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

(* $Id: nopres_intf.mli,v 1.18 2005/11/07 20:25:28 mottl Exp $ *)

(** Interfaces to unparameterized resizable arrays and buffers *)

(** Interface to unparameterized resizable arrays *)
module type T = sig
  (** {6 Signatures and types} *)

  (** Module implementing the reallocation strategy *)
  module Strategy : Strat.T

  type strategy = Strategy.t (** Type of reallocation strategy *)
  type t (** Type of resizable arrays *)
  type el (** Type of the elements in the resizable array *)


  (** {6 Index and length information} *)

  val length : t -> int
  (** [length ra] @return (virtual) length of resizable array [ra]
      excluding the reserved space. *)

  val lix : t -> int
  (** [lix ra] @return (virtual) last index of resizable array [ra]
      excluding the reserved space. *)

  val real_length : t -> int
  (** [real_length ra] @return (real) length of resizable array [ra]
      including the reserved space. *)

  val real_lix : t -> int
  (** [real_lix ra] @return (real) last index of resizable array [ra]
      including the reserved space. *)


  (** {6 Getting and setting} *)

  val get : t -> int -> el
  (** [get ra n] @return the [n]th element of [ra].
      @raise Invalid_argument if index out of bounds. *)

  val set : t -> int -> el -> unit
  (** [set ra n] sets the [n]th element of [ra].
      @raise Invalid_argument if index out of bounds. *)


  (** {6 Creation of resizable arrays} *)

  val sempty : strategy -> t
  (** [sempty s] @return an empty resizable array using strategy [s]. *)

  val empty : unit -> t
  (** [empty ()] same as [sempty] but uses default strategy. *)

  val screate : strategy -> int -> t
  (** [screate s n] @return a resizable array with strategy [s]
      containing [n] arbitrary elements.

      {e Attention: the contents is {b not} specified!} *)

  val create : int -> t
  (** [create n] same as [screate] but uses default strategy. *)

  val smake : strategy -> int -> el -> t
  (** [smake s n el] @return a resizable array of length [n]
      containing element [el] only using strategy [s]. *)

  val make : int -> el -> t
  (** [make n el] same as [smake] but uses default strategy. *)

  val sinit : strategy -> int -> (int -> el) -> t
  (** [sinit s n f] @return an array of length [n] containing
      elements that were created by applying function [f] to the index,
      using strategy [s]. *)

  val init : int -> (int -> el) -> t
  (** [init n f] sames as [sinit] but uses default strategy. *)


  (** {6 Strategy handling} *)

  val get_strategy : t -> strategy
  (** [get_strategy ra] @return the reallocation strategy used by
      resizable array [ra]. *)

  val set_strategy : t -> strategy -> unit
  (** [set_strategy ra s] sets the reallocation strategy of
      resizable array [ra] to [s], possibly causing an immediate
      reallocation. *)

  val put_strategy : t -> strategy -> unit
  (** [put_strategy ra s] sets the reallocation strategy of
      resizable array [ra] to [s]. Reallocation is only done at later
      changes in size. *)

  val enforce_strategy : t -> unit
  (** [enforce_strategy ra] forces a reallocation if necessary
      (e.g. after a [put_strategy]. *)


  (** {6 Copying, blitting and range extraction} *)

  val copy : t -> t
  (** [copy ra] @return a copy of resizable array [ra]. The two
      arrays share the same strategy! *)

  val sub : t -> int -> int -> t
  (** [sub ra ofs len] @return a resizable subarray of length [len]
      from resizable array [ra] starting at offset [ofs] using the
      default strategy.
      @raise Invalid_argument if parameters do not denote a correct
      subarray. *)

  val fill : t -> int -> int -> el -> unit
  (** [fill ra ofs len el] fills resizable array [ra] from offset
      [ofs] with [len] elements [el], possibly adding elements at the
      end. Raises [Invalid_argument] if offset [ofs] is larger than the
      length of the array. *)

  val blit : t -> int -> t -> int -> int -> unit
  (** [blit ra1 ofs1 ra2 ofs2 len] blits resizable array [ra1] onto
      [ra2] reading [len] elements from offset [ofs1] and writing them
      to [ofs2], possibly adding elements at the end of ra2. Raises
      [Invalid_argument] if [ofs1] and [len] do not designate a valid
      subarray of [ra1] or if [ofs2] is larger than the length of
      [ra2]. *)


  (** {6 Combining resizable arrays} *)

  val append : t -> t -> t
  (** [append ra1 ra2] @return a new resizable array using the
      default strategy and copying [ra1] and [ra2] in this order onto
      it. *)

  val concat : t list -> t
  (** [concat l] @return a new resizable array using the default
      strategy and copying all resizable arrays in [l] in their respective
      order onto it. *)


  (** {6 Adding and removing elements} *)

  val add_one : t -> el -> unit
  (** [add_one ra el] adds element [el] to resizable array [ra],
      possibly causing a reallocation. *)

  val remove_one : t -> unit
  (** [remove_one ra] removes the last element of resizable array
      [ra], possibly causing a reallocation.
      @raise Failure if the array is empty. *)

  val remove_n : t -> int -> unit
  (** [remove_n ra n] removes the last n elements of resizable
      array [ra], possibly causing a reallocation.
      @raise Invalid_arg if there are not enough elements or [n < 0]. *)

  val remove_range : t -> int -> int -> unit
  (** [remove_range ra ofs len] removes [len] elements from resizable
      array [ra] starting at [ofs] and possibly causing a
      reallocation.
      @raise Invalid_argument if range is invalid. *)

  val clear : t -> unit
  (** [clear ra] removes all elements from resizable array [ra],
      possibly causing a reallocation. *)


  (** {6 Swapping} *)

  val swap : t -> int -> int -> unit
  (** [swap ra n m] swaps elements at indices [n] and [m].
      @raise Invalid_argument if any index is out of range. *)

  val swap_in_last : t -> int -> unit
  (** [swap_in_last ra n] swaps the last element with the one at
      position [n].
      @raise Invalid_argument if index [n] is out of range. *)


  (** {6 Array conversions} *)

  val to_array : t -> el array
  (** [to_array ra] converts a resizable array to a standard one. *)

  val sof_array : strategy -> el array -> t
  (** [sof_array s ar] converts a standard array to a resizable one,
      using strategy [s]. *)

  val of_array : el array -> t
  (** [of_array ar] converts a standard array to a resizable one
      using the default strategy. *)


  (** {6 List conversions} *)

  val to_list : t -> el list
  (** [to_list ra] converts resizable array [ra] to a list. *)

  val sof_list : strategy -> el list -> t
  (** [sof_list s l] creates a resizable array using strategy [s] and
      the elements in list [l]. *)

  val of_list : el list -> t
  (** [of_list l] creates a resizable array using the default
      strategy and the elements in list [l]. *)


  (** {6 Iterators} *)

  val iter : (el -> unit) -> t -> unit
  (** [iter f ra] applies the unit-function [f] to each element in
      resizable array [ra]. *)

  val map : (el -> el) -> t -> t
  (** [map f ra] @return a resizable array using the strategy of
      [ra] and mapping each element in [ra] to its corresponding position
      in the new array using function [f]. *)

  val iteri : (int -> el -> unit) -> t -> unit
  (** [iteri f ra] applies the unit-function [f] to each index and
      element in resizable array [ra]. *)

  val mapi : (int -> el -> el) -> t -> t
  (** [mapi f ra] @return a resizable array using the strategy of
      [ra] and mapping each element in [ra] to its corresponding
      position in the new array using function [f] and the index
      position. *)

  val fold_left : ('a -> el -> 'a) -> 'a -> t -> 'a
  (** [fold_left f a ra] left-folds values in resizable array [ra]
      using function [f] and start accumulator [a]. *)

  val fold_right : (el -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_right f a ra] right-folds values in resizable array [ra]
      using function [f] and start accumulator [a]. *)


  (** {6 Scanning of resizable arrays} *)

  val for_all : (el -> bool) -> t -> bool
  (** [for_all p ra] @return [true] if all elements in resizable
      array [ra] satisfy the predicate [p], [false] otherwise. *)

  val exists : (el -> bool) -> t -> bool
  (** [exists p ra] @return [true] if at least one element in
      resizable array [ra] satisfies the predicate [p], [false]
      otherwise. *)

  val mem : el -> t -> bool
  (** [mem el ra] @return [true] if element [el] is logically equal
      to any element in resizable array [ra], [false] otherwise. *)

  val memq : el -> t -> bool
  (** [memq el ra] @return [true] if element [el] is physically equal
      to any element in resizable array [ra], [false] otherwise. *)

  val pos : el -> t -> int option
  (** [pos el ra] @return [Some index] if [el] is logically
      equal to the element at [index] in [ra], [None] otherwise.  [index]
      is the index of the first element that matches. *)

  val posq : el -> t -> int option
  (** [posq el ra] @return [Some index] if [el] is physically
      equal to the element at [index] in [ra], [None] otherwise.  [index]
      is the index of the first element that matches. *)


  (** {6 Searching of resizable arrays} *)

  val find : (el -> bool) -> t -> el
  (** [find p ra] @return the first element in resizable array [ra]
      that satisfies predicate [p].
      @raise Not_found if there is no such element. *)

  val find_index : (el -> bool) -> t -> int -> int
  (** [find_index p ra pos] @return the index of the first element
      that satisfies predicate [p] in resizable array [ra], starting
      search at index [pos].
      @raise Not_found if there is no such element or if [pos] is larger
      than the highest index.
      @raise Invalid_argument if [pos] is negative. *)

  val filter : (el -> bool) -> t -> t
  (** [filter p ra] @return a new resizable array by filtering
      out all elements in [ra] that satisfy predicate [p] using the same
      strategy as [ra]. *)

  val find_all : (el -> bool) -> t -> t
  (** [find_all p ra] is the same as [filter] *)

  val filter_in_place : (el -> bool) -> t -> unit
  (** [filter_in_place p ra] as [filter], but filters in place. *)

  val partition : (el -> bool) -> t -> t * t
  (** [partition p ra] @return a pair of resizable arrays, the
      left part containing only elements of [ra] that satisfy predicate
      [p], the right one only those that do not satisfy it. Both returned
      arrays are created using the strategy of [ra]. *)


  (** {6 {b UNSAFE STUFF - USE WITH CAUTION!}} *)

  val unsafe_get : t -> int -> el
  val unsafe_set : t -> int -> el -> unit

  val unsafe_sub : t -> int -> int -> t

  val unsafe_fill : t -> int -> int -> el -> unit
  val unsafe_blit : t -> int -> t -> int -> int -> unit

  val unsafe_remove_one : t -> unit
  val unsafe_remove_n : t -> int -> unit

  val unsafe_swap : t -> int -> int -> unit
  val unsafe_swap_in_last : t -> int -> unit
end


(** Extended interface to buffers (resizable strings) *)
module type Buffer = sig
  include T
  (** Includes all functions that exist in non-parameterized arrays. *)


  (** {6 String conversions} *)

  val sof_string : strategy -> string -> t
  (** [sof_string s ar] converts a string to a resizable buffer
      using strategy [s]. *)

  val of_string : string -> t
  (** [of_string ar] converts a string to a resizable buffer using
      the default strategy. *)


  (** {6 Functions found in the standard [Buffer]-module} *)

  (** Note that the function [create n] ignores the parameter [n] and
      uses the default strategy instead. You can supply a different
      strategy with [creates s n] as described above. *)

  val contents : t -> string
  (** [contents b] @return a copy of the current contents of the
      buffer [b]. *)

  val reset : t -> unit
  (** [reset b] just clears the buffer, possibly resizing it. *)

  val add_char : t -> char -> unit
  (** [add_char b c] appends the character [c] at the end of
      the buffer [b]. *)

  val add_string : t -> string -> unit
  (** [add_string b s] appends the string [s] at the end of
      the buffer [b]. *)

  val add_substring : t -> string -> int -> int -> unit
  (** [add_substring b s ofs len] takes [len] characters from offset
      [ofs] in string [s] and appends them at the end of the buffer
      [b]. *)

  val add_buffer : t -> t -> unit
  (** [add_buffer b1 b2] appends the current contents of buffer [b2]
      at the end of buffer [b1]. [b2] is not modified. *)

  val add_channel : t -> in_channel -> int -> unit
  (** [add_channel b ic n] reads exactly [n] character from the
      input channel [ic] and stores them at the end of buffer [b].
      @raise End_of_file if the channel contains fewer than [n]
      characters. *)

  val output_buffer : out_channel -> t -> unit
  (** [output_buffer oc b] writes the current contents of buffer [b]
      on the output channel [oc]. *)


  (** {6 Additional buffer functions} *)

  val add_full_channel : t -> in_channel -> unit
  (* [add_full_channel b ic] reads the whole channel [ic] into
     buffer [b]. *)

  val add_full_channel_f : t -> in_channel -> int -> (int -> int) -> unit
  (* [add_full_channel_f b ic n f] reads the whole channel [ic] into
     buffer [b], starting with read-ahead [n] and using function [f] to
     calculate the next read-ahead if end-of-file was still not found. *)
end
