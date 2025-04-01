open! Core
open! Int_repr

module I = Iobuf

module type D = sig
  module Field_name : String_id.S

  val (?:) : string -> Field_name.t

  module Typ : sig
    type 't t

    module Raw : sig
      val bit8 : uint8 t
      val bit16 : uint16 t
      val bit32 : uint32 t
      val bit64 : uint64 t
    end

    module Int : sig
      val uint8 : uint8 t
      val uint16 : uint16 t
      val uint32 : uint32 t
      val uint64 : uint64 t
    end

    val char : char t
  end

  module Computation : sig
    type t

    val offsetof : Field_name.t -> t

    val (+) : t -> t -> t
    val (-) : t -> t -> t
  end

  module Constraint : sig
    type 'a t

    val constant : 'a -> 'a t

    (* CR smuenzel: add type conversion? *)
    val computation : Computation.t -> 'a t
  end

  type entry

  val field
    : 'a Typ.t
    -> ?constraint_:'a Constraint.t
    -> Field_name.t
    -> entry

  val group
    : ?name:string
    -> entry list
    -> entry

  val select
    : Field_name.t
    -> 'a Typ.t
    -> ('a * entry) list
    -> entry

  val message_name
    : ?scope:string
    -> string
    -> entry
    -> entry
end

module Make(D : D) = struct
  open! D

  let header =
    group
      ~name:"header"
      [ field Typ.char ?:"sync_char_1"
          ~constraint_:(Constraint.constant '\xb5')
      ; field Typ.char ?:"sync_char_2"
          ~constraint_:(Constraint.constant '\x62')
      ; field Typ.Raw.bit8 ?:"class_id"
      ; field Typ.Raw.bit8 ?:"message_id"
      ; field Typ.Int.uint16 ?:"length"
          ~constraint_:(Constraint.computation
                          Computation.((offsetof ?:"ck_a") - (offsetof ?:"class_id"))
                       )
      ]

  let trailer =
    group
      ~name:"trailer"
      [ field Typ.Int.uint8 ?:"ck_a"
      ; field Typ.Int.uint8 ?:"ck_b"
      ]

  let messages_ack =
    group
      [ select ?:"message_id" Typ.Raw.bit8
          [ Uint8.of_base_int_exn 0x01, message_name "ack" (group [])
          ; Uint8.of_base_int_exn 0x00, message_name "nack" (group [])
          ]
      ; field Typ.Raw.bit8 ?:"ack_class_id"
      ; field Typ.Raw.bit8 ?:"ack_message_id"
      ]
    |> message_name ~scope:"class" "ack"

  let messages_nav =
    group
      [ select ?:"message_id" Typ.Raw.bit8
          [ Uint8.of_base_int_exn 0x01, message_name "clock"
              (group
                 [ field Typ.Int.uint32 ?:"time_of_week"
                 ; field Typ.Int.int32 ?:"bias"
                 ; field Typ.Int.int32 ?:"drift"
                 ; field Typ.Int.uint32 ?:"time_accuracy"
                 ; field Typ.Int.uint32 ?:"frequency_accuracy"
                 ])
          ]
      ]
    |> message_name ~scope:"class" "nav"

  let messages =
    select ?:"class_id" Typ.Raw.bit8
      [ Uint8.of_base_int_exn 0x05, messages_ack
      ; Uint8.of_base_int_exn 0x01, messages_nav
      ]

  let full_message =
    group
      [ header
      ; messages
      ; trailer
      ]

end
