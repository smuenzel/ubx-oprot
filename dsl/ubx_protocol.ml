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

      val int8 : int8 t
      val int16 : int16 t
      val int32 : int32 t
      val int64 : int64 t
    end

    val char : char t

    val fixed_array : int -> 'a t -> 'a array t
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
    :  ?constraint_:'a Constraint.t
    -> 'a Typ.t
    -> Field_name.t
    -> entry

  val field_reserved
    :  ?constraint_:'a Constraint.t
    -> 'a Typ.t
    -> entry

  val group
    :  ?optional:bool
    -> ?name:string
    -> entry list
    -> entry

  val select
    : ?default:entry
    -> Field_name.t
    -> 'a Typ.t
    -> ('a * entry) list
    -> entry

  val message_name
    : ?scope:string
    -> string
    -> entry
    -> entry

  val repeating_group
    :  ?count:int Constraint.t
    -> name:string
    -> entry list
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

  let messages_cfg =
    group
      [ select ?:"message_id" Typ.Raw.bit8
          [ Uint8.of_base_int_exn 0x13, message_name "ant"
              (group
                 [ field Type.Int.uint16 ?:"antenna_flag_mask"
                 ; field Type.Int.uint16 ?:"antenna_pin_configuration"
                 ])
          ; Uint8.of_base_int_exn 0x09, message_name "cfg"
              (group
                 [ field Type.Int.uint32 ?:"clear_mask"
                 ; field Type.Int.uint32 ?:"save_mask"
                 ; field Type.Int.uint32 ?:"load_mask"
                 ; group ~optional:true ~name:"device"
                     [ field Type.Int.uint8 ?:"device_mask"
                     ]
                 ])
          ; Uint8.of_base_int_exn 0x04, message_name "rst"
              (group
                [ field Type.Int.uint16 ?:"nav_bbr_mask"
                ; field Type.Int.uint8 ?:"reset_mode"
                ; field_reserved Type.Int.uint8
                ])
          ; Uint8.of_base_int_exn 0x8c, message_name "valdel"
              (group
                 [ field Type.Int.uint8 ?:"message_version"
                 ; select ?:"message_version" Type.Int.uint8
                     [ Uint8.of_base_int_exn 0x00,
                       group 
                         [ field Type.Int.uint8 ?:"layers"
                         ; field_reserved Type.Int.uint8
                         ]
                     ; Uint8.of_base_int_exn 0x01,
                       group
                         [ field Type.Int.uint8 ?:"layers"
                         ; field Type.Int.uint8 ?:"transaction"
                         ]
                     ]
                 ; field_reserved Type.Int.uint8
                 ; repeating_group ~name:"items"
                     [ field Type.Int.uint32 ?:"key"
                     ]
                 ])
          ]
      ]
    |> message_name ~scope:"class" "cfg"

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
          ; Uint8.of_base_int_exn 0x13, message_name "high_precision_solution_ecef"
              (group
                 [ field Typ.Int.uint8 ?:"message_version"
                 ; select ?:"message_version" Typ.Int.uint8
                     [ Uint8.of_base_int_exn 0x00, group [] ]
                 ; field_reserved (Typ.fixed_array 3 Typ.Int.uint8)
                 ; field Typ.Int.uint32 ?:"time_of_week"
                 ; field Typ.Int.int32 ?:"ecef_x"
                 ; field Typ.Int.int32 ?:"ecef_y"
                 ; field Typ.Int.int32 ?:"ecef_z"
                 ; field Typ.Int.int8 ?:"ecef_x_high_precision"
                 ; field Typ.Int.int8 ?:"ecef_y_high_precision"
                 ; field Typ.Int.int8 ?:"ecef_z_high_precision"
                 ; field Typ.Raw.bit8 ?:"flags"
                 ; field Typ.Int.uint32 ?:"position_accuracy"
                 ])
          ; Uint8.of_base_int_exn 0x14, message_name "high_precision_solution_geodetic"
              (group
                 [ field Typ.Int.uint8 ?:"message_version"
                 ; select ?:"message_version" Typ.Int.uint8
                     [ Uint8.of_base_int_exn 0x00, group [] ]
                 ; field_reserved (Typ.fixed_array 2 Typ.Int.uint8)
                 ; field Typ.Raw.bit8 ?:"flags"
                 ; field Typ.Int.uint32 ?:"time_of_week"
                 ; field Typ.Int.int32 ?:"longitude"
                 ; field Typ.Int.int32 ?:"latitude"
                 ; field Typ.Int.int32 ?:"height"
                 ; field Typ.Int.int32 ?:"height_msl"
                 ; field Typ.Int.int8 ?: "longitude_high_precision"
                 ; field Typ.Int.int8 ?: "latitude_high_precision"
                 ; field Typ.Int.int8 ?: "height_high_precision"
                 ; field Typ.Int.int8 ?: "height_msl_high_precision"
                 ; field Typ.Int.uint32 ?: "horizontal_accuracy"
                 ; field Typ.Int.uint32 ?: "vertical_accuracy"
                 ])
          ]
      ]
    |> message_name ~scope:"class" "nav"

  let messages =
    select ?:"class_id" Typ.Raw.bit8
      [ Uint8.of_base_int_exn 0x05, messages_ack
      ; Uint8.of_base_int_exn 0x06, messages_cfg
      ; Uint8.of_base_int_exn 0x01, messages_nav
      ]

  let full_message =
    group
      [ header
      ; messages
      ; trailer
      ]

end
