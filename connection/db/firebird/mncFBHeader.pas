unit mncFBHeader;
{**
 *  This file is part of the "Mini Connections"
 *
 *
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  Initial ported from IBX, Ibase
 *
 *}

{$M+}{$H+}
{$mode objfpc}
{$macro on}
{$ifdef WINDOWS}
  {$define isc_export:=stdcall}
  {$define isc_export_vararg:=cdecl}
{$else}
  {$define isc_export:=cdecl}
  {$define isc_export_vararg:=cdecl}
{$endif}

interface

uses
  mncFBTypes;

{$define FB_API_VER 25}
{$define isc_version4}

const
  FB_Version = 25;

  ISC_TRUE = 1;
  ISC_FALSE = 0;

  DSQL_close = 1;
  DSQL_drop = 2;
  DSQL_unprepare = 4;  

  METADATALENGTH = 68;

  {$ifdef WINDOWS}
  FBClient_DLL = 'fbclient.dll';
  FBEmbed_DLL = 'fbembed.dll';

  REGSTR_PATH_FB='Software\Firebird Project\Firebird Server';

  {$else}
  FBClient_DLL = 'fbclient.'+sharedsuffix;;
  FBEmbed_DLL = 'fbembed.'+sharedsuffix;;
  {$endif}

  FB_DefaultInstance = 'DefaultInstance';

type
  //typedef int (*FB_SHUTDOWN_CALLBACK)(const int reason, const int mask, void* arg);
  TFB_SHUTDOWN_CALLBACK = function(Reason: Integer; Mask: Integer; Arg: Pointer): Integer;

  (**********************************)
  (** Firebird Handle Definitions **)
  (**********************************)
  TISC_ATT_HANDLE = FB_API_HANDLE;
  PISC_ATT_HANDLE = ^TISC_ATT_HANDLE;
  TISC_BLOB_HANDLE = FB_API_HANDLE;
  PISC_BLOB_HANDLE = ^TISC_BLOB_HANDLE;
  TISC_DB_HANDLE = FB_API_HANDLE;
  PISC_DB_HANDLE = ^TISC_DB_HANDLE;
  TISC_FORM_HANDLE = FB_API_HANDLE;
  PISC_FORM_HANDLE = ^TISC_FORM_HANDLE;
  TISC_REQ_HANDLE = FB_API_HANDLE;
  PISC_REQ_HANDLE = ^TISC_REQ_HANDLE;
  TISC_STMT_HANDLE = FB_API_HANDLE;
  PISC_STMT_HANDLE = ^TISC_STMT_HANDLE;
  TISC_SVC_HANDLE = FB_API_HANDLE;
  PISC_SVC_HANDLE = ^TISC_SVC_HANDLE;
  TISC_TR_HANDLE = FB_API_HANDLE;
  PISC_TR_HANDLE = ^TISC_TR_HANDLE;
  TISC_WIN_HANDLE = FB_API_HANDLE;
  PISC_WIN_HANDLE = ^TISC_WIN_HANDLE;
  TISC_CALLBACK = procedure;
  ISC_SVC_HANDLE = ISC_LONG;
               
  TISC_PRINT_CALLBACK = procedure (P: Pointer; param2:ISC_SHORT; param3:PChar);
  TISC_VERSION_CALLBACK = procedure (P: Pointer; Buffer:PChar);
  TISC_EVENT_CALLBACK = procedure  (P: Pointer; param2:ISC_USHORT; param3:PISC_UCHAR);

  (*******************************************************************)
  (* Time & Date Support                                             *)
  (*******************************************************************)
const
  TIME_SECONDS_PRECISION = 10000;
  TIME_SECONDS_PRECISION_SCALE = -4;

type
  ISC_DATE = Long;
  PISC_DATE = ^ISC_DATE;
  ISC_TIME = ULong;
  PISC_TIME = ^ISC_TIME;
  TISC_TIMESTAMP = record
    timestamp_date: ISC_DATE;
    timestamp_time: ISC_TIME;
  end;
  PISC_TIMESTAMP = ^TISC_TIMESTAMP;

  (*********************************************************************)
  (** Blob id structure                                               **)
  (*********************************************************************)
  TGDS_QUAD = record
    gds_quad_high: ISC_LONG;
    gds_quad_low: UISC_LONG;
  end;

  TGDS__QUAD = TGDS_QUAD;
  TISC_QUAD = TGDS_QUAD;
  PGDS_QUAD = ^TGDS_QUAD;
  PGDS__QUAD = ^TGDS__QUAD;
  PISC_QUAD = ^TISC_QUAD;

  TISC_ARRAY_BOUND = record
    array_bound_lower: Short;
    array_bound_upper: Short;
  end;

  PISC_ARRAY_BOUND = ^TISC_ARRAY_BOUND;
  
  TISC_ARRAY_DESC = record
    array_desc_dtype: UChar;
    array_desc_scale: Char;
    array_desc_length: UShort;
    array_desc_field_name: array[0..31] of Char;
    array_desc_relation_name: array[0..31] of Char;
    array_desc_dimensions: Short;
    array_desc_flags: Short;
    array_desc_bounds: array[0..15] of TISC_ARRAY_BOUND;
  end; // TISC_ARRAY_DESC
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;

type
  TISC_BLOB_DESC = record
    blob_desc_subtype: Short;
    blob_desc_charset: Short;
    blob_desc_segment_size: Short;
    blob_desc_field_name: array[0..31] of UChar;
    blob_desc_relation_name: array[0..31] of UChar;
  end; // TISC_BLOB_DESC
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;

type
  (*****************************)
  (** Blob control structure  **)
  (*****************************)
  TISC_BLOB_CTL_SOURCE_FUNCTION = function: ISC_STATUS; // ISC_FAR
  PISC_BLOB_CTL = ^TISC_BLOB_CTL; // ISC_FAR
  TISC_BLOB_CTL = record
    (** Source filter **)
    ctl_source: TISC_BLOB_CTL_SOURCE_FUNCTION;
    (** Argument to pass to source filter **)
    ctl_source_handle: PISC_BLOB_CTL;
    ctl_to_sub_type: Short; (** Target type **)
    ctl_from_sub_type: Short; (** Source type **)
    ctl_buffer_length: UShort; (** Length of buffer **)
    ctl_segment_length: UShort; (** Length of current segment **)
    ctl_bpb_length: UShort; (** Length of blob parameter **)
      (** block **)
    ctl_bpb: PChar; (** Address of blob parameter **)
      (** block **)
    ctl_buffer: PUChar; (** Address of segment buffer **)
    ctl_max_segment: ISC_LONG; (** Length of longest segment **)
    ctl_number_segments: ISC_LONG; (** Total number of segments **)
    ctl_total_length: ISC_LONG; (** Total length of blob **)
    ctl_status: PISC_STATUS; (** Address of status vector **)
    ctl_data: array[0..7] of long; (** Application specific data **)
  end;
  
  (*****************************)
  (** Blob stream definitions **)
  (*****************************)
  TBSTREAM = record
    bstr_blob: PVoid; (** Blob handle **)
    bstr_buffer: PChar; (** Address of buffer **)
    bstr_ptr: PChar; (** Next character **)
    bstr_length: Short; (** Length of buffer **)
    bstr_cnt: Short; (** Characters in buffer **)
    bstr_mode: Char; (** (mode) ? OUTPUT : INPUT **)
  end;
  PBSTREAM = ^TBSTREAM;
(* moved to FBExternal.pas

// Blob passing structure

// This enum applies to parameter "mode" in blob_lseek
  TBlob_LSeek_Mode = (blb_seek_relative = 1, blb_seek_from_tail = 2);
// This enum applies to the value returned by blob_get_segment */
  TBlob_Get_Result = (blb_got_fragment = -1, blb_got_eof = 0, blb_got_full_segment = 1);

  TBlob_get_segment = function (hnd:PVoid; buffer:PISC_UCHAR; buf_size:ISC_USHORT; result_len:PISC_USHORT):short;
  TBlob_put_segment = procedure (hnd:PVoid; const buffer:PISC_UCHAR; buf_size:ISC_USHORT);
  TBlob_lseek = function (hnd:PVoid; mode:ISC_USHORT; offset:ISC_LONG ):ISC_LONG;

  PBlobCallback = ^TBlobCallback;//zaher be shure
  TBlobCallback = record
    blob_get_segment: TBlob_get_segment;
    blob_handle:PVoid;

    blob_number_segments:ISC_LONG	;
    blob_max_segment:ISC_LONG	;
    blob_total_length:ISC_LONG	;

    blob_put_segment: TBlob_put_segment;
    blob_lseek : TBlob_lseek;
  end;
*)

(****************************************
 ****************************************
 * Older XSQLDA strucutre.
 * NOTE: This is kept only for backward
 * compatability. Please refrain from
 * using these old structures.
 * It is strongly  recomended
 * to use the newer SQLDA version
 * and related XSQLVAR structure.
 ****************************************)
  TXSQLVAR = record
    sqltype: Short; (** datatype of field **)
    sqlscale: Short; (** scale factor **)
    sqlsubtype: Short; (** datatype subtype - BLOBs **)
          (** & text types only **)
    sqllen: Short; (** length of data area **)
    sqldata: Pointer; (** address of data **)
    sqlind: PShort; (** address of indicator **)
                                             (** variable **)
    sqlname_length: Short; (** length of sqlname field **)
    (** name of field, name length + space for NULL **)
    sqlname: array[0..31] of Char;
    relname_length: Short; (** length of relation name **)
    (** field's relation name + space for NULL **)
    relname: array[0..31] of Char;
    ownname_length: Short; (** length of owner name **)
    (** relation's owner name + space for NULL **)
    ownname: array[0..31] of Char;
    aliasname_length: Short; (** length of alias name **)
    (** relation's alias name + space for NULL **)
    aliasname: array[0..31] of Char;
  end; // TXSQLVAR
  PXSQLVAR = ^TXSQLVAR;

  TXSQLDA = record
    version: Short; (** version of this XSQLDA **)
    (** XSQLDA name field **)
    sqldaid: array[0..7] of Char;
    sqldabc: ISC_LONG; (** length in bytes of SQLDA **)
    sqln: Short; (** number of fields allocated **)
    sqld: Short; (** actual number of fields **)
    (** first field address **)
    sqlvar: array[0..0] of TXSQLVAR;
  end; // TXSQLDA
  PXSQLDA = ^TXSQLDA;

(*********************)
(** SQL definitions **)
(*********************)
const
  SQL_VARYING = 448;
  SQL_TEXT = 452;
  SQL_DOUBLE = 480;
  SQL_FLOAT = 482;
  SQL_LONG = 496;
  SQL_SHORT = 500;
  SQL_TIMESTAMP = 510;
  SQL_BLOB = 520;
  SQL_D_FLOAT = 530;
  SQL_ARRAY = 540;
  SQL_QUAD = 550;
  SQL_TYPE_TIME = 560;
  SQL_TYPE_DATE = 570;
  SQL_INT64 = 580;
  SQL_BOOLEAN = 590; //zaher huh
  SQL_NULL = 32766;

  SQLDA_VERSION1 = 1;
  
  SQL_DIALECT_V5 = 1;
  SQL_DIALECT_V6_TRANSITION = 2;
  SQL_DIALECT_V6 = 3;
  SQL_DIALECT_CURRENT = SQL_DIALECT_V6; 

type
(********************************************************)
(** This record type is for passing arguments to       **)
(** isc_start_transaction (See docs)                   **)
(********************************************************)
  TISC_START_TRANS = record
    db_handle: PISC_DB_HANDLE;
    tpb_length: UShort;
    tpb_address: PChar;
  end;

(********************************************************)
(** This record type is for passing arguments to       **)
(** isc_start_multiple (see docs)                      **)
(********************************************************)
  TISC_TEB = record
    db_handle: PISC_DB_HANDLE;
    tpb_length: Long;
    tpb_address: PChar;
  end;
  PISC_TEB = ^TISC_TEB;
  TISC_TEB_ARRAY = array[0..0] of TISC_TEB;
  PISC_TEB_ARRAY = ^TISC_TEB_ARRAY;

(*****************************)
(** OSRI database functions **)
(*****************************)

  Tisc_attach_database = function(status_vector: PISC_STATUS; db_name_length: Short; db_name: PChar;  db_handle: PISC_DB_HANDLE; parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS; isc_export;

  Tisc_array_gen_sdl = function(status_vector: PISC_STATUS; isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort; isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS; isc_export;

  Tisc_array_get_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS; isc_export;

  Tisc_array_lookup_bounds = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar; descriptor: PISC_ARRAY_DESC): ISC_STATUS; isc_export;

  Tisc_array_lookup_desc = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar; descriptor: PISC_ARRAY_DESC): ISC_STATUS; isc_export;

  Tisc_array_set_desc = function(status_vector: PISC_STATUS; table_name: PChar; column_name: PChar; sql_dtype, sql_length, sql_dimensions: PShort; descriptor: PISC_ARRAY_DESC): ISC_STATUS; isc_export;

  Tisc_array_put_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS; isc_export;

  Tisc_blob_default_desc = procedure(descriptor: PISC_BLOB_DESC; table_name: PUChar; column_name: PUChar); isc_export;

  Tisc_blob_gen_bpb = function(status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC; bpb_buffer_length: UShort; bpb_buffer: PUChar; bpb_length: PUShort): ISC_STATUS; isc_export;

  Tisc_blob_info = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short; item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar): ISC_STATUS; isc_export;

  Tisc_blob_lookup_desc = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar; descriptor: PISC_BLOB_DESC; global: PUChar): ISC_STATUS; isc_export;

  Tisc_blob_set_desc = function(status_vector: PISC_STATUS; table_name, column_name: PChar; subtype, charset, segment_size: Short; descriptor: PISC_BLOB_DESC): ISC_STATUS; isc_export;

  Tisc_cancel_blob = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS; isc_export; Tisc_cancel_events = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS; isc_export;

  Tisc_close_blob = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS; isc_export; Tisc_commit_retaining = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; isc_export;

  Tisc_commit_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; isc_export; Tisc_create_blob = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD): ISC_STATUS; isc_export;

  Tisc_create_blob2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short; bpb_address: PChar): ISC_STATUS; isc_export;

  Tisc_create_database = function(status_vector: PISC_STATUS; isc_arg2: Short; isc_arg3: PChar; db_handle: PISC_DB_HANDLE; isc_arg5: Short; isc_arg6: PChar; isc_arg7: Short): ISC_STATUS; isc_export;

  Tisc_database_info = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short; item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar): ISC_STATUS; isc_export;

  Tisc_decode_date = procedure(ib_date: PISC_QUAD; tm_date: PCTimeStructure); isc_export; Tisc_decode_sql_date = procedure(ib_date: PISC_DATE; tm_date: PCTimeStructure); isc_export;

  Tisc_decode_sql_time = procedure(ib_time: PISC_TIME; tm_date: PCTimeStructure); isc_export;

  Tisc_decode_timestamp = procedure(ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure); isc_export;

  Tisc_detach_database = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS; isc_export;

  Tisc_drop_database = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS; isc_export;

  Tisc_dsql_allocate_statement = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS; isc_export;

  Tisc_dsql_alloc_statement2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS; isc_export;

  Tisc_dsql_describe = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_dsql_describe_bind = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_dsql_exec_immed2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: UShort; statement: PChar; dialect: UShort; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_dsql_execute = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_dsql_execute2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_dsql_execute_immediate = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: UShort; statement: PChar; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_dsql_fetch = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; isc_export; Tisc_dsql_finish = function(db_handle: PISC_DB_HANDLE): ISC_STATUS; isc_export;

  Tisc_dsql_free_statement = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; options: UShort): ISC_STATUS; isc_export;

  Tisc_dsql_insert = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; arg3: UShort; xsqlda: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_dsql_prepare = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; length: UShort; statement: PChar; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_dsql_set_cursor_name = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; cursor_name: PChar; _type: UShort): ISC_STATUS; isc_export;

  Tisc_dsql_sql_info = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PChar; buffer_length: Short; buffer: PChar): ISC_STATUS; isc_export;

  Tisc_encode_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_QUAD); isc_export;

  Tisc_encode_sql_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_DATE); isc_export;

  Tisc_encode_sql_time = procedure(tm_date: PCTimeStructure; ib_time: PISC_TIME); isc_export;

  Tisc_encode_timestamp = procedure(tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP); isc_export;

  Tisc_event_block = function(event_buffer: PPChar; result_buffer: PPChar; id_count: UShort; var event_list: array of PChar): ISC_LONG; isc_export_vararg;

  Tisc_event_block_a = function(event_buffer: PPChar; result_buffer: PPChar; id_count: UShort; event_list: PPChar): ISC_LONG; isc_export;

  Tisc_event_block_s = function(event_buffer: PPChar; result_buffer: PPChar; id_count: UShort; event_list: PPChar; arg1: PUShort): ISC_LONG; isc_export;

  Tisc_event_counts = procedure(status_vector: PISC_STATUS; buffer_length: Short; event_buffer: PChar; result_buffer: PChar); isc_export;

  Tisc_modify_dpb = function(dpb: PPChar; isc_arg2, isc_arg3: PShort; isc_arg4: UShort; isc_arg5: PChar; isc_arg6: Short): Int; isc_export;

  Tisc_free = function(isc_arg1: PChar): ISC_LONG; isc_export;

  Tisc_get_segment = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PUShort; seg_buffer_length: UShort; seg_buffer: PChar): ISC_STATUS; isc_export;

  Tisc_get_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PISC_QUAD; isc_arg5: Short; isc_arg6: PChar; isc_arg7: Short; isc_arg8: PISC_LONG; isc_arg9: ISC_LONG; isc_arg10: PVoid; isc_arg11: PISC_LONG): ISC_STATUS; isc_export;

  Tisc_interprete = function(buffer: PChar; status_vector: PPISC_STATUS): ISC_STATUS; isc_export; deprecated;

  Tfb_interpret = function(buffer: PChar; length:short; const status_vector: PPISC_STATUS): ISC_STATUS; isc_export;

  Tisc_open_blob = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD): ISC_STATUS; isc_export;

  Tisc_open_blob2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short; bpb_buffer: PChar): ISC_STATUS; isc_export;

  Tisc_prepare_transaction2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; msg_length: Short; msg: PChar): ISC_STATUS; isc_export;

  Tisc_print_sqlerror = procedure(sqlcode: Short; status_vector: PISC_STATUS); isc_export;

  Tisc_print_status = function(status_vector: PISC_STATUS): ISC_STATUS; isc_export;

  Tisc_put_segment = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: UShort; seg_buffer: PChar): ISC_STATUS; isc_export;

  Tisc_put_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PISC_QUAD; isc_arg5: Short; isc_arg6: PChar; isc_arg7: Short; isc_arg8: PISC_LONG; isc_arg9: ISC_LONG; isc_arg10: PVoid): ISC_STATUS; isc_export;

  Tisc_que_events = function( status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short; event_buffer: PChar; event_function: TISC_EVENT_CALLBACK; event_function_arg: PVoid): ISC_STATUS; isc_export;

  Tisc_release_savepoint = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; tran_name: PChar): ISC_STATUS; isc_export;

  Tisc_rollback_retaining = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; isc_export;

  Tisc_rollback_savepoint = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; tran_name: PChar; Option: UShort): ISC_STATUS; isc_export;

  Tisc_rollback_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; isc_export;

  Tisc_start_multiple = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS; isc_export;

  Tisc_start_savepoint = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; tran_name: PChar): ISC_STATUS; isc_export;

  Tisc_start_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; db_handle_count: Short; db_handle: PISC_DB_HANDLE; tpb_length: UShort; tpb_address: PChar): ISC_STATUS; isc_export_vararg;

  Tisc_sqlcode = function(status_vector: PISC_STATUS): ISC_LONG; isc_export;

  Tisc_sqlcode_s = function(status_vector: PISC_STATUS; arg1: PULong): ISC_LONG; isc_export;

  Tisc_sql_interprete = procedure(sqlcode: Short; buffer: PChar; buffer_length: Short); isc_export;

  Tisc_transaction_info = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; item_list_buffer_length: Short; item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar): ISC_STATUS; isc_export;

  Tisc_transact_request = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: UShort; isc_arg5: PChar; isc_arg6: UShort; isc_arg7: PChar; isc_arg8: UShort; isc_arg9: PChar): ISC_STATUS; isc_export;

  Tisc_vax_integer = function(buffer: PChar; length: Short): ISC_LONG; isc_export; Tisc_portable_integer = function(buffer: PChar; length: Short): ISC_INT64; isc_export;

(***************************************)
(** Security Functions and structures **)
(***************************************)

const
  sec_uid_spec = $01;
  sec_gid_spec = $02;
  sec_server_spec = $04;
  sec_password_spec = $08;
  sec_group_name_spec = $10;
  sec_first_name_spec = $20;
  sec_middle_name_spec = $40;
  sec_last_name_spec = $80;
  sec_dba_user_name_spec = $100;
  sec_dba_password_spec = $200;

  sec_protocol_tcpip = 1;
  sec_protocol_netbeui = 2;
  sec_protocol_spx = 3; // -- Deprecated Protocol. Declaration retained for compatibility   
  sec_protocol_local = 4;

type
  TUserSecData = record
    sec_flags: Short; (** which fields are specified **)
    uid: Int; (** the user's id **)
    gid: int; (** the user's group id **)
    protocol: Int; (** protocol to use for connection **)
    server: PChar; (** server to administer **)
    user_name: PChar; (** the user's name **)
    password: PChar; (** the user's password **)
    group_name: PChar; (** the group name **)
    first_name: PChar; (** the user's first name **)
    middle_name: PChar; (** the user's middle name **)
    last_name: PChar; (** the user's last name **)
    dba_user_name: PChar; (** the dba user name **)
    dba_password: PChar; (** the dba password **)
  end;
  PUserSecData = ^TUserSecData;

  Tisc_add_user = function(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS; isc_export;

  Tisc_delete_user = function(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS; isc_export;

  Tisc_modify_user = function(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS; isc_export;

(************************************)
(**  Other OSRI functions          **)
(************************************)

  Tisc_compile_request = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; request_handle: PISC_REQ_HANDLE; isc_arg4: Short; isc_arg5: PChar): ISC_STATUS; isc_export;

  Tisc_compile_request2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; request_handle: PISC_REQ_HANDLE; isc_arg4: Short; isc_arg5: PChar): ISC_STATUS; isc_export;

  Tisc_ddl = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: Short; isc_arg5: PChar): ISC_STATUS; isc_export;

  Tisc_prepare_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; isc_export;

  Tisc_receive = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3, isc_arg4: Short; isc_arg5: PVoid; isc_arg6: Short): ISC_STATUS; isc_export;

  Tisc_receive2 = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3, isc_arg4: Short; isc_arg5: PVoid; isc_arg6, isc_arg7: Short; isc_arg8: Long): ISC_STATUS; isc_export;

  Tisc_reconnect_transaction = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: Short; isc_arg5: PChar): ISC_STATUS; isc_export;

  Tisc_release_request = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE): ISC_STATUS; isc_export;

  Tisc_request_info = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3: Short; isc_arg4: Short; isc_arg5: PChar; isc_arg6: Short; isc_arg7: PChar): ISC_STATUS; isc_export;

  Tisc_seek_blob = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; isc_arg3: Short; isc_arg4: ISC_LONG; isc_arg5: PISC_LONG): ISC_STATUS; isc_export;

  Tisc_send = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3, isc_arg4: Short; isc_arg5: PVoid; isc_arg6: Short): ISC_STATUS; isc_export;

  Tisc_start_and_send = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4, isc_arg5: Short; isc_arg6: PVoid; isc_arg7: Short): ISC_STATUS; isc_export;

  Tisc_start_request = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: Short): ISC_STATUS; isc_export;

  Tisc_unwind_request = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: Short): ISC_STATUS; isc_export;

  Tisc_wait_for_event = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; length: Short; event_buffer, result_buffer: PChar): ISC_STATUS; isc_export;

(*******************************)
(** Other Sql functions       **)
(*******************************)

  Tisc_close = function(status_vector: PISC_STATUS; isc_arg2: PChar): ISC_STATUS; isc_export;

  Tisc_declare = function(status_vector: PISC_STATUS; isc_arg2, isc_arg3: PChar): ISC_STATUS; isc_export;

  Tisc_describe = function(status_vector: PISC_STATUS; isc_arg2: PChar; isc_arg3: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_describe_bind = function(status_vector: PISC_STATUS; isc_arg2: PChar; isc_arg3: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_execute = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PChar; isc_arg4: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_execute_immediate = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PShort; isc_arg5: PChar): ISC_STATUS; isc_export;

  Tisc_fetch = function(status_vector: PISC_STATUS; isc_arg2: PChar; isc_arg3: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_open = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PChar; isc_arg4: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_prepare = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PChar; isc_arg5: PShort; isc_arg6: PChar; isc_arg7: PXSQLDA): ISC_STATUS; isc_export;

(***************************************)
(** Other Dynamic sql functions       **)
(***************************************)

  Tisc_dsql_execute_m = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE; isc_arg4: UShort; isc_arg5: PChar; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PChar): ISC_STATUS; isc_export;

  Tisc_dsql_execute2_m = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE;
                                  isc_arg4: UShort; isc_arg5: PChar; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PChar; isc_arg9: UShort; isc_arg10: PChar;
                                  isc_arg11: UShort; isc_arg12: UShort; isc_arg13: PChar): ISC_STATUS; isc_export;

  Tisc_dsql_execute_immediate_m = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
                                            isc_arg4: UShort; isc_arg5: PChar; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PChar; isc_arg9: UShort; isc_arg10: UShort;
                                            isc_arg11: PChar): ISC_STATUS; isc_export;

  Tisc_dsql_exec_immed3_m = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
                                     isc_arg4: UShort; isc_arg5: PChar; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PChar; isc_arg9: UShort;
                                     isc_arg10: UShort; isc_arg11: PChar; isc_arg12: UShort; isc_arg13: PChar; isc_arg14: UShort;
                                     isc_arg15: UShort; isc_arg16: PChar): ISC_STATUS; isc_export;

  Tisc_dsql_fetch_m = function(status_vector: PISC_STATUS; statement_handle: PISC_STMT_HANDLE; isc_arg3: UShort; isc_arg4: PChar; isc_arg5: UShort; isc_arg6: UShort; isc_arg7: PChar): ISC_STATUS; isc_export;

  Tisc_dsql_insert_m = function(status_vector: PISC_STATUS; statement_handle: PISC_STMT_HANDLE; isc_arg3: UShort; isc_arg4: PChar; isc_arg5: UShort; isc_arg6: UShort; isc_arg7: PChar): ISC_STATUS; isc_export;

  Tisc_dsql_prepare_m = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE; isc_arg4: UShort; isc_arg5: PChar; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PChar; isc_arg9: UShort; isc_arg10: PChar): ISC_STATUS; isc_export;

  Tisc_dsql_release = function(status_vector: PISC_STATUS; isc_arg2: PChar): ISC_STATUS; isc_export;

  Tisc_embed_dsql_close = function(status_vector: PISC_STATUS; isc_arg2: PChar): ISC_STATUS; isc_export;

  Tisc_embed_dsql_declare = function(status_vector: PISC_STATUS; isc_arg2: PChar; isc_arg3: PChar): ISC_STATUS; isc_export;

  Tisc_embed_dsql_describe = function(status_vector: PISC_STATUS; isc_arg2: PChar; isc_arg3: UShort; isc_arg4: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_describe_bind = function(status_vector: PISC_STATUS; isc_arg2: PChar; isc_arg3: UShort; isc_arg4: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_execute = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PChar; isc_arg4: UShort; isc_arg5: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_execute2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PChar; isc_arg4: UShort; isc_arg5: PXSQLDA; isc_arg6: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_execute_immed = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: UShort; isc_arg5: PChar; isc_arg6: UShort; isc_arg7: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_fetch = function(status_vector: PISC_STATUS; isc_arg2: PChar; isc_arg3: UShort; isc_arg4: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_fetch_a = function(status_vector: PISC_STATUS; isc_arg2: PInt; isc_arg3: PChar; isc_arg4: UShort; isc_arg5: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_length = procedure(arg1: PChar; arg2: PUShort); isc_export; Tisc_embed_dsql_open = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PChar; isc_arg4: UShort; isc_arg5: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_open2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PChar; isc_arg4: UShort; isc_arg5: PXSQLDA; isc_arg6: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_insert = function(status_vector: PISC_STATUS; isc_arg2: PChar; isc_arg3: UShort; isc_arg4: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_prepare = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PChar; isc_arg5: UShort; isc_arg6: PChar; isc_arg7: UShort; isc_arg8: PXSQLDA): ISC_STATUS; isc_export;

  Tisc_embed_dsql_release = function(status_vector: PISC_STATUS; isc_arg2: PChar): ISC_STATUS; isc_export;

(********************************)
(** Other Blob functions       **)
(********************************)

  TBLOB_open = function(blob_handle: TISC_BLOB_HANDLE; isc_arg2: PChar; isc_arg3: int): PBSTREAM; isc_export;

  TBLOB_put = function(isc_arg1: char; isc_arg2: PBSTREAM): Int; isc_export;

  TBLOB_close = function(isc_arg1: PBSTREAM): Int; isc_export;

  TBLOB_get = function(isc_arg1: PBSTREAM): Int; isc_export;

  TBLOB_display = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PChar): Int; isc_export;

  TBLOB_dump = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PChar): Int; isc_export;

  TBLOB_edit = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PChar): Int; isc_export;

  TBLOB_load = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PChar): Int; isc_export;

  TBLOB_text_dump = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PChar): Int; isc_export;

  TBLOB_text_load = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PChar): Int; isc_export;

  TBopen = function(blob_id: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; mode: PChar): PBSTREAM; isc_export;

  TBclose = function(Stream: PBSTREAM): PISC_STATUS; isc_export;

(********************************)
(** Other Misc functions       **)
(********************************)

  Tisc_ftof = function(isc_arg1: PChar; isc_arg2: UShort; isc_arg3: PChar; isc_arg4: UShort): ISC_LONG; isc_export;

  Tisc_print_blr = function(isc_arg1: PChar; isc_arg2: TISC_PRINT_CALLBACK; isc_arg3: PVoid; isc_arg4: Short): ISC_STATUS; isc_export;

  Tisc_set_debug = procedure(isc_arg1: Int); isc_export;

  Tisc_qtoq = procedure(isc_arg1: PISC_QUAD; isc_arg2: PISC_QUAD); isc_export;

  Tisc_vtof = procedure(isc_arg1: PChar; isc_arg2: PChar; isc_arg3: UShort); isc_export;

  Tisc_vtov = procedure(isc_arg1: PChar; isc_arg2: PChar; isc_arg3: Short); isc_export;

  Tisc_version = function(db_handle: PISC_DB_HANDLE; isc_arg2: TISC_VERSION_CALLBACK; isc_arg3: PVoid): Int; isc_export;

  Tisc_reset_fpe = function(isc_arg1: UShort): ISC_LONG; isc_export;

  Tisc_baddress = function(isc_arg1: PChar): uintptr_t; isc_export;

  Tisc_baddress_s = procedure(isc_arg1: PChar; isc_arg2: uintptr_t); isc_export;

(*******************************************)
(** Service manager functions             **)
(*******************************************)

  Tisc_service_attach = function(status_vector: PISC_STATUS; isc_arg2: UShort; isc_arg3: PChar; service_handle: PISC_SVC_HANDLE; isc_arg5: UShort; isc_arg6: PChar): ISC_STATUS; isc_export;

  Tisc_service_detach = function(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE): ISC_STATUS; isc_export;

  Tisc_service_query = function(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort; isc_arg5: PChar; isc_arg6: UShort; isc_arg7: PChar; isc_arg8: UShort; isc_arg9: PChar): ISC_STATUS; isc_export;

  Tisc_service_start = function(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort; isc_arg5: PChar): ISC_STATUS; isc_export;

(********************************)
(* Client information functions *)
(********************************)

{  Tfb_shutdown= function (arg1: UInt; arg2: int): Int;

  ISC_STATUS isc_export fb_shutdown_callback(ISC_STATUS*,
  										   FB_SHUTDOWN_CALLBACK,
  										   const int,
  										   void*);

  ISC_STATUS isc_export fb_cancel_operation(ISC_STATUS*,
  										  isc_db_handle*,
  										  ISC_USHORT);

}
(********************************)
(* Client information functions *)
(********************************)

  Tisc_get_client_version = procedure(buffer: PChar); isc_export;
  Tisc_get_client_major_version = function: Integer; isc_export;
  Tisc_get_client_minor_version = function: Integer; isc_export;

(*****************************************************)
(** Actions to pass to the blob filter (ctl_source) **)
(*****************************************************)

const
  isc_blob_filter_open = 0;
  isc_blob_filter_get_segment = 1;
  isc_blob_filter_close = 2;
  isc_blob_filter_create = 3;
  isc_blob_filter_put_segment = 4;
  isc_blob_filter_alloc = 5;
  isc_blob_filter_free = 6;
  isc_blob_filter_seek = 7;

(*********************)
(** Blr definitions **)
(*********************)

  blr_text                = 14;
  blr_text2               = 15;
  blr_short               = 7;
  blr_long                = 8;
  blr_quad                = 9;
  blr_float               = 10;
  blr_double              = 27;
  blr_d_float             = 11;
  blr_timestamp           = 35;
  blr_varying             = 37;
  blr_varying2            = 38;
  blr_blob                = 261;
  blr_cstring             = 40;
  blr_cstring2            = 41;
  blr_blob_id             = 45;
  blr_sql_date            = 12;
  blr_sql_time            = 13;
  blr_int64               = 16;
  blr_blob2               = 17;
  blr_domain_name         = 18;
  blr_domain_name2        = 19;
  blr_not_nullable        = 20;
  blr_column_name         = 21;
  blr_column_name2        = 22;

  blr_domain_type_of      = 0;
  blr_domain_full         = 1;

  blr_date                = blr_timestamp;

  blr_inner               = 0;
  blr_left                = 1;
  blr_right               = 2;
  blr_full                = 3;

  blr_gds_code            = 0;
  blr_sql_code            = 1;
  blr_exception           = 2;
  blr_trigger_code        = 3;
  blr_default_code        = 4;
  blr_raise               = 5;
  blr_exception_msg       = 6;

  blr_version4            = 4;
  blr_version5            = 5;
  blr_eoc                 = 76;
  blr_end                 = 255;

  blr_assignment          = 1;
  blr_begin               = 2;
  blr_dcl_variable        = 3;
  blr_message             = 4;
  blr_erase               = 5;
  blr_fetch               = 6;
  blr_for                 = 7;
  blr_if                  = 8;
  blr_loop                = 9;
  blr_modify              = 10;
  blr_handler             = 11;
  blr_receive             = 12;
  blr_select              = 13;
  blr_send                = 14;
  blr_store               = 15;
  blr_label               = 17;
  blr_leave               = 18;
  blr_store2              = 19;
  blr_post                = 20;
  blr_literal             = 21;
  blr_dbkey               = 22;
  blr_field               = 23;
  blr_fid                 = 24;
  blr_parameter           = 25;
  blr_variable            = 26;
  blr_average             = 27;
  blr_count               = 28;
  blr_maximum             = 29;
  blr_minimum             = 30;
  blr_total               = 31;

  blr_add                 = 34;
  blr_subtract            = 35;
  blr_multiply            = 36;
  blr_divide              = 37;
  blr_negate              = 38;
  blr_concatenate         = 39;
  blr_substring           = 40;
  blr_parameter2          = 41;
  blr_from                = 42;
  blr_via                 = 43;
  blr_parameter2_old      = 44;
  blr_user_name           = 44;
  blr_null                = 45;

  blr_equiv               = 46;
  blr_eql                 = 47;
  blr_neq                 = 48;
  blr_gtr                 = 49;
  blr_geq                 = 50;
  blr_lss                 = 51;
  blr_leq                 = 52;
  blr_containing          = 53;
  blr_matching            = 54;
  blr_starting            = 55;
  blr_between             = 56;
  blr_or                  = 57;
  blr_and                 = 58;
  blr_not                 = 59;
  blr_any                 = 60;
  blr_missing             = 61;
  blr_unique              = 62;
  blr_like                = 63;

  blr_rse                 = 67;
  blr_first               = 68;
  blr_project             = 69;
  blr_sort                = 70;
  blr_boolean             = 71;
  blr_ascending           = 72;
  blr_descending          = 73;
  blr_relation            = 74;
  blr_rid                 = 75;
  blr_union               = 76;
  blr_map                 = 77;
  blr_group_by            = 78;
  blr_aggregate           = 79;
  blr_join_type           = 80;

  blr_agg_count           = 83;
  blr_agg_max             = 84;
  blr_agg_min             = 85;
  blr_agg_total           = 86;
  blr_agg_average         = 87;
  blr_parameter3          = 88;
  blr_run_max             = 89;
  blr_run_min             = 90;
  blr_run_total           = 91;
  blr_run_average         = 92;
  blr_agg_count2          = 93;
  blr_agg_count_distinct  = 94;
  blr_agg_total_distinct  = 95;
  blr_agg_average_distinct = 96;

  blr_function            = 100;
  blr_gen_id              = 101;
  blr_prot_mask           = 102;
  blr_upcase              = 103;
  blr_lock_state          = 104;
  blr_value_if            = 105;
  blr_matching2           = 106;
  blr_index               = 107;
  blr_ansi_like           = 108;

  blr_seek                = 112;

  blr_continue            = 0;
  blr_forward             = 1;
  blr_backward            = 2;
  blr_bof_forward         = 3;
  blr_eof_backward        = 4;

  blr_run_count           = 118;
  blr_rs_stream           = 119;
  blr_exec_proc           = 120;

  blr_procedure           = 124;
  blr_pid                 = 125;
  blr_exec_pid            = 126;
  blr_singular            = 127;
  blr_abort               = 128;
  blr_block               = 129;
  blr_error_handler       = 130;

  blr_cast                = 131;

  blr_start_savepoint     = 134;
  blr_end_savepoint       = 135;

  blr_plan                = 139;
  blr_merge               = 140;
  blr_join                = 141;
  blr_sequential          = 142;
  blr_navigational        = 143;
  blr_indices             = 144;
  blr_retrieve            = 145;

  blr_relation2           = 146;
  blr_rid2                = 147;

  blr_set_generator       = 150;

  blr_ansi_any            = 151;
  blr_exists              = 152;

  blr_record_version      = 154;
  blr_stall               = 155;

  blr_ansi_all            = 158;

  blr_extract             = 159;

  blr_extract_year                = 0;
  blr_extract_month               = 1;
  blr_extract_day                 = 2;
  blr_extract_hour                = 3;
  blr_extract_minute              = 4;
  blr_extract_second              = 5;
  blr_extract_weekday             = 6;
  blr_extract_yearday             = 7;
  blr_extract_millisecond         = 8;
  blr_extract_week                = 9;

  blr_current_date        = 160;
  blr_current_timestamp   = 161;
  blr_current_time        = 162;

  blr_post_arg            = 163;
  blr_exec_into           = 164;
  blr_user_savepoint      = 165;
  blr_dcl_cursor          = 166;
  blr_cursor_stmt         = 167;
  blr_current_timestamp2  = 168;
  blr_current_time2       = 169;
  blr_agg_list            = 170;
  blr_agg_list_distinct   = 171;
  blr_modify2             = 172;

  blr_current_role        = 174;
  blr_skip                = 175;

  blr_exec_sql            = 176;
  blr_internal_info       = 177;
  blr_nullsfirst          = 178;
  blr_writelock           = 179;
  blr_nullslast           = 180;

  blr_lowcase                     = 181;
  blr_strlen                      = 182;

  blr_strlen_bit          = 0;
  blr_strlen_char         = 1;
  blr_strlen_octet        = 2;

  blr_trim                        = 183;

  blr_trim_both           = 0;
  blr_trim_leading        = 1;
  blr_trim_trailing       = 2;

  blr_trim_spaces         = 0;
  blr_trim_characters     = 1;

  blr_savepoint_set       = 0;
  blr_savepoint_release   = 1;
  blr_savepoint_undo      = 2;
  blr_savepoint_release_single    = 3;

  blr_cursor_open                 = 0;
  blr_cursor_close                = 1;
  blr_cursor_fetch                = 2;

  blr_init_variable       = 184;
  blr_recurse                     = 185;
  blr_sys_function        = 186;

  blr_auto_trans          = 187;
  blr_similar                     = 188;
  blr_exec_stmt           = 189;

  blr_exec_stmt_inputs            =  1;
  blr_exec_stmt_outputs           =  2;
  blr_exec_stmt_sql                       =  3;
  blr_exec_stmt_proc_block        =  4;
  blr_exec_stmt_data_src          =  5;
  blr_exec_stmt_user                      =  6;
  blr_exec_stmt_pwd                       =  7;
  blr_exec_stmt_tran              =  8;
  blr_exec_stmt_tran_clone        =  9;
  blr_exec_stmt_privs                     =  10;
  blr_exec_stmt_in_params         =  11;
  blr_exec_stmt_in_params2        =  12;
  blr_exec_stmt_out_params        =  13;
  blr_exec_stmt_role                      =  14;

  blr_stmt_expr                           =  190;
  blr_derived_expr                        =  191;

(************************************)
(** Database parameter block stuff **)
(************************************)

  isc_dpb_version1 = 1;
  isc_dpb_cdd_pathname = 1;
  isc_dpb_allocation = 2;
  isc_dpb_journal = 3;
  isc_dpb_page_size = 4;
  isc_dpb_num_buffers = 5;
  isc_dpb_buffer_length = 6;
  isc_dpb_debug = 7;
  isc_dpb_garbage_collect = 8;
  isc_dpb_verify = 9;
  isc_dpb_sweep = 10;
  isc_dpb_enable_journal = 11;
  isc_dpb_disable_journal = 12;
  isc_dpb_dbkey_scope = 13;
  isc_dpb_number_of_users = 14;
  isc_dpb_trace = 15;
  isc_dpb_no_garbage_collect = 16;
  isc_dpb_damaged = 17;
  isc_dpb_license = 18;
  isc_dpb_sys_user_name = 19;
  isc_dpb_encrypt_key = 20;
  isc_dpb_activate_shadow = 21;
  isc_dpb_sweep_interval = 22;
  isc_dpb_delete_shadow = 23;
  isc_dpb_force_write = 24;
  isc_dpb_begin_log = 25;
  isc_dpb_quit_log = 26;
  isc_dpb_no_reserve = 27;
  isc_dpb_user_name = 28;
  isc_dpb_password = 29;
  isc_dpb_password_enc = 30;
  isc_dpb_sys_user_name_enc = 31;
  isc_dpb_interp = 32;
  isc_dpb_online_dump = 33;
  isc_dpb_old_file_size = 34;
  isc_dpb_old_num_files = 35;
  isc_dpb_old_file = 36;
  isc_dpb_old_start_page = 37;
  isc_dpb_old_start_seqno = 38;
  isc_dpb_old_start_file = 39;
  isc_dpb_drop_walfile = 40;
  isc_dpb_old_dump_id = 41;
  isc_dpb_wal_backup_dir = 42;
  isc_dpb_wal_chkptlen = 43;
  isc_dpb_wal_numbufs = 44;
  isc_dpb_wal_bufsize = 45;
  isc_dpb_wal_grp_cmt_wait = 46;
  isc_dpb_lc_messages = 47;
  isc_dpb_lc_ctype = 48;
  isc_dpb_cache_manager = 49;
  isc_dpb_shutdown = 50;
  isc_dpb_online = 51;
  isc_dpb_shutdown_delay = 52;
  isc_dpb_reserved = 53;
  isc_dpb_overwrite = 54;
  isc_dpb_sec_attach = 55;
  isc_dpb_disable_wal = 56;
  isc_dpb_connect_timeout = 57;
  isc_dpb_dummy_packet_interval = 58;
  isc_dpb_gbak_attach = 59;
  isc_dpb_sql_role_name = 60;
  isc_dpb_set_page_buffers = 61;
  isc_dpb_working_directory = 62;
  isc_dpb_SQL_dialect = 63;
  isc_dpb_set_db_readonly = 64;
  isc_dpb_set_db_SQL_dialect = 65;
  isc_dpb_gfix_attach = 66;
  isc_dpb_gstat_attach = 67;
  isc_dpb_set_db_charset = 68;
  isc_dpb_gsec_attach = 69;
  isc_dpb_address_path= 70;
  isc_dpb_process_id  = 71;
  isc_dpb_no_db_trigge= 72;
  isc_dpb_trusted_auth = 73;
  isc_dpb_process_name = 74;
  isc_dpb_trusted_role  = 75;
  isc_dpb_org_filename = 76;
  isc_dpb_utf8_filename = 77;
  isc_dpb_ext_call_depth = 78;

  isc_dpb_last_dpb_constant = isc_dpb_ext_call_depth;


  isc_dpb_address  = 1;
  
  isc_dpb_addr_protocol = 1;
  isc_dpb_addr_endpoint = 2;

(***********************************)
(** isc_dpb_verify specific flags **)
(***********************************)

  isc_dpb_pages = 1;
  isc_dpb_records = 2;
  isc_dpb_indices = 4;
  isc_dpb_transactions = 8;
  isc_dpb_no_update = 16;
  isc_dpb_repair = 32;
  isc_dpb_ignore = 64;

(*************************************)
(** isc_dpb_shutdown specific flags **)
(*************************************)

  isc_dpb_shut_cache = 1;
  isc_dpb_shut_attachment = 2;
  isc_dpb_shut_transaction = 4;
  isc_dpb_shut_force = 8;
  isc_dpb_shut_mode_mask = $70;

  isc_dpb_shut_default = $0;
  isc_dpb_shut_normal = $10;
  isc_dpb_shut_multi = $20;
  isc_dpb_shut_single = $30;
  isc_dpb_shut_full = $40;

(****************************************)
(** Bit assignments in RDB$SYSTEM_FLAG **)
(****************************************)

  RDB_system = 1;
  RDB_id_assigned = 2;

(***************************************)
(** Transaction parameter block stuff **)
(***************************************)

  isc_tpb_version1 = 1;
  isc_tpb_version3 = 3;
  isc_tpb_consistency = 1;
  isc_tpb_concurrency = 2;
  isc_tpb_shared = 3;
  isc_tpb_protected = 4;
  isc_tpb_exclusive = 5;
  isc_tpb_wait = 6;
  isc_tpb_nowait = 7;
  isc_tpb_read = 8;
  isc_tpb_write = 9;
  isc_tpb_lock_read = 10;
  isc_tpb_lock_write = 11; //it must pass the table name
  isc_tpb_verb_time = 12;
  isc_tpb_commit_time = 13;
  isc_tpb_ignore_limbo = 14;
  isc_tpb_read_committed = 15;
  isc_tpb_autocommit = 16;
  isc_tpb_rec_version = 17;
  isc_tpb_no_rec_version = 18;
  isc_tpb_restart_requests = 19;
  isc_tpb_no_auto_undo = 20;
  isc_tpb_lock_timeout = 21;

  isc_tpb_last_tpb_constant = isc_tpb_lock_timeout;

(**************************)
(** Blob Parameter Block **)
(**************************)

  isc_bpb_version1 = 1;
  isc_bpb_source_type = 1;
  isc_bpb_target_type = 2;
  isc_bpb_type = 3;
  isc_bpb_source_interp = 4;
  isc_bpb_target_interp = 5;
  isc_bpb_filter_parameter = 6;
  isc_bpb_storage = 7;

  isc_bpb_type_segmented = 0;
  isc_bpb_type_stream = 1;
  isc_bpb_storage_main = 0;
  isc_bpb_storage_temp = 2;

(***********************************)
(** Service parameter block stuff **)
(***********************************)

  c_spb_version1                  = 1;
  isc_spb_current_version         = 2;
  isc_spb_version                 = isc_spb_current_version;
  isc_spb_user_name               = isc_dpb_user_name;
  isc_spb_sys_user_name           = isc_dpb_sys_user_name;
  isc_spb_sys_user_name_enc       = isc_dpb_sys_user_name_enc;
  isc_spb_password                = isc_dpb_password;
  isc_spb_password_enc            = isc_dpb_password_enc;
  isc_spb_command_line            = 105;
  isc_spb_dbname                  = 106;
  isc_spb_verbose                 = 107;
  isc_spb_options                 = 108;
  isc_spb_address_path            = 109;
  isc_spb_process_id              = 110;
  isc_spb_trusted_auth            = 111;
  isc_spb_process_name            = 112;
  isc_spb_trusted_role            = 113;

  isc_spb_connect_timeout         = isc_dpb_connect_timeout;
  isc_spb_dummy_packet_interval   = isc_dpb_dummy_packet_interval;
  isc_spb_sql_role_name           = isc_dpb_sql_role_name;

(*****************************************)
(* Service action items                 **)
(*****************************************)

  isc_action_svc_backup = 1;
  isc_action_svc_restore = 2;
  isc_action_svc_repair = 3;
  isc_action_svc_add_user  = 4;
  isc_action_svc_delete_user = 5;
  isc_action_svc_modify_user = 6;
  isc_action_svc_display_user = 7;
  isc_action_svc_properties = 8;
  isc_action_svc_add_license = 9;
  isc_action_svc_remove_license = 10;
  isc_action_svc_db_stats = 11;
  //isc_action_svc_get_ib_log = 12;
  isc_action_svc_get_fb_log = 12;
  isc_action_svc_nbak = 20;
  isc_action_svc_nrest = 21;
  isc_action_svc_trace_start = 22;
  isc_action_svc_trace_stop = 23;
  isc_action_svc_trace_suspend = 24;
  isc_action_svc_trace_resume = 25;
  isc_action_svc_trace_list = 26;
  isc_action_svc_set_mapping = 27;
  isc_action_svc_drop_mapping = 28;
  isc_action_svc_display_user_adm = 29;
  isc_action_svc_last = 30;

(*****************************************)
(** Service information items           **)
(*****************************************)

  isc_info_svc_svr_db_info = 50; { Retrieves the number of attachments and databases }
  isc_info_svc_get_license = 51; { Retrieves all license keys and IDs from the license file }
  isc_info_svc_get_license_mask = 52; { Retrieves a bitmask representing licensed options on the server }
  isc_info_svc_get_config = 53; { Retrieves the parameters and values for FB_CONFIG }
  isc_info_svc_version = 54; { Retrieves the version of the services manager }
  isc_info_svc_server_version = 55; { Retrieves the version of the Firebird server }
  isc_info_svc_implementation = 56; { Retrieves the implementation of the Firebird server }
  isc_info_svc_capabilities = 57; { Retrieves a bitmask representing the server's capabilities }
  isc_info_svc_user_dbpath = 58; { Retrieves the path to the security database in use by the server }
  isc_info_svc_get_env = 59; { Retrieves the setting of $Firebird }
  isc_info_svc_get_env_lock = 60; { Retrieves the setting of $Firebird_LCK }
  isc_info_svc_get_env_msg = 61; { Retrieves the setting of $Firebird_MSG }
  isc_info_svc_line = 62; { Retrieves 1 line of service output per call }
  isc_info_svc_to_eof = 63; { Retrieves as much of the server output as will fit in the supplied buffer }
  isc_info_svc_timeout = 64; { Sets / signifies a timeout value for reading service information }
  isc_info_svc_get_licensed_users = 65; { Retrieves the number of users licensed for accessing the server }
  isc_info_svc_limbo_trans = 66; { Retrieve the limbo transactions }
  isc_info_svc_running = 67; { Checks to see if a service is running on an attachment }
  isc_info_svc_get_users = 68; { Returns the user information from isc_action_svc_display_users }

(*****************************************)
(* Parameters for isc_action_{add|delete|modify)_user *)
(*****************************************)

  isc_spb_sec_userid = 5;
  isc_spb_sec_groupid = 6;
  isc_spb_sec_username = 7;
  isc_spb_sec_password = 8;
  isc_spb_sec_groupname = 9;
  isc_spb_sec_firstname = 10;
  isc_spb_sec_middlename = 11;
  isc_spb_sec_lastname = 12;
  isc_spb_sec_admin = 13;

  isc_spb_lic_key              = 5;
  isc_spb_lic_id               = 6;
  isc_spb_lic_desc             = 7;

(*****************************************)
(* Parameters for isc_action_svc_backup  *)
(*****************************************)

  isc_spb_bkp_file = 5;
  isc_spb_bkp_factor = 6;
  isc_spb_bkp_length = 7;
  isc_spb_bkp_ignore_checksums = $01;
  isc_spb_bkp_ignore_limbo = $02;
  isc_spb_bkp_metadata_only = $04;
  isc_spb_bkp_no_garbage_collect = $08;
  isc_spb_bkp_old_descriptions = $10;
  isc_spb_bkp_non_transportable = $20;
  isc_spb_bkp_convert = $40;
  isc_spb_bkp_expand = $80;
  isc_spb_bkp_no_triggers = $8000;

(*****************************************)
(* Parameters for isc_action_svc_properties *)
(*****************************************)

  isc_spb_prp_page_buffers = 5;
  isc_spb_prp_sweep_interval = 6;
  isc_spb_prp_shutdown_db = 7;
  isc_spb_prp_deny_new_attachments = 9;
  isc_spb_prp_deny_new_transactions = 10;
  isc_spb_prp_reserve_space = 11;
  isc_spb_prp_write_mode = 12;
  isc_spb_prp_access_mode = 13;
  isc_spb_prp_set_sql_dialect = 14;
  isc_spb_prp_activate = $0100;
  isc_spb_prp_db_online = $0200;
  isc_spb_prp_force_shutdown            =  41;
  isc_spb_prp_attachments_shutdown      =  42;
  isc_spb_prp_transactions_shutdown     =  43;
  isc_spb_prp_shutdown_mode             =  44;
  isc_spb_prp_online_mode                = 45;

  isc_spb_prp_sm_normal          = 0;
  isc_spb_prp_sm_multi           = 1;
  isc_spb_prp_sm_single          = 2;
  isc_spb_prp_sm_full            = 3;

  isc_spb_prp_res_use_full = 35;
  isc_spb_prp_res = 36;

(*****************************************)
(* Parameters for isc_spb_prp_write_mode  *)
(*****************************************)

  isc_spb_prp_wm_async = 37;
  isc_spb_prp_wm_sync = 38;

(*****************************************)
(* Parameters for isc_spb_prp_access_mode *)
(*****************************************)

  isc_spb_prp_am_readonly = 39;
  isc_spb_prp_am_readwrite = 40;

(*****************************************)
(* Parameters for isc_action_svc_repair  *)
(*****************************************)

  isc_spb_rpr_commit_trans = 15;
  isc_spb_rpr_rollback_trans = 34;
  isc_spb_rpr_recover_two_phase = 17;
  isc_spb_tra_id = 18;
  isc_spb_single_tra_id = 19;
  isc_spb_multi_tra_id = 20;
  isc_spb_tra_state = 21;
  isc_spb_tra_state_limbo = 22;
  isc_spb_tra_state_commit = 23;
  isc_spb_tra_state_rollback = 24;
  isc_spb_tra_state_unknown = 25;
  isc_spb_tra_host_site = 26;
  isc_spb_tra_remote_site = 27;
  isc_spb_tra_db_path = 28;
  isc_spb_tra_advise = 29;
  isc_spb_tra_advise_commit = 30;
  isc_spb_tra_advise_rollback = 31;
  isc_spb_tra_advise_unknown = 33;
  isc_spb_rpr_validate_db = $01;
  isc_spb_rpr_sweep_db = $02;
  isc_spb_rpr_mend_db = $04;
  isc_spb_rpr_list_limbo_trans = $08;
  isc_spb_rpr_check_db = $10;
  isc_spb_rpr_ignore_checksum = $20;
  isc_spb_rpr_kill_shadows = $40;
  isc_spb_rpr_full = $80;

(*****************************************)
(* Parameters for isc_action_svc_restore  *)
(*****************************************)

  isc_spb_res_buffers = 9;
  isc_spb_res_page_size = 10;
  isc_spb_res_length = 11;
  isc_spb_res_access_mode = 12;
  isc_spb_res_fix_fss_data = 13;
  isc_spb_res_fix_fss_metadata = 14;
  isc_spb_res_metadata_only = isc_spb_bkp_metadata_only;
  isc_spb_res_deactivate_idx = $0100;
  isc_spb_res_no_shadow = $0200;
  isc_spb_res_no_validity = $0400;
  isc_spb_res_one_at_a_time = $0800;
  isc_spb_res_replace = $1000;
  isc_spb_res_create = $2000;
  isc_spb_res_use_all_space = $4000;

(*****************************************)
(* Parameters for isc_spb_res_access_mode  *)
(*****************************************)

  isc_spb_res_am_readonly = isc_spb_prp_am_readonly;
  isc_spb_res_am_readwrite = isc_spb_prp_am_readwrite;

(*****************************************)
(* Parameters for isc_info_svc_svr_db_info *)
(*****************************************)

  isc_spb_num_att = 5;
  isc_spb_num_db = 6;

(*****************************************)
(* Parameters for isc_info_svc_db_stats  *)
(*****************************************)

  isc_spb_sts_data_pages = $01;
  isc_spb_sts_db_log = $02;
  isc_spb_sts_hdr_pages = $04;
  isc_spb_sts_idx_pages = $08;
  isc_spb_sts_sys_relations = $10;
  isc_spb_sts_record_versions = $20;
  isc_spb_sts_table = $40;
  isc_spb_sts_nocreation = $80;

  isc_spb_nbk_level = 5;
  isc_spb_nbk_file = 6;
  isc_spb_nbk_direct = 7;
  isc_spb_nbk_no_triggers = $01;

  isc_spb_trc_id = 1;
  isc_spb_trc_name = 2;
  isc_spb_trc_cfg = 3;

(************************************************)
(** Dynamic Data Definition Language operators **)
(************************************************)

(********************)
(** Version number **)
(********************)

  isc_dyn_version_1 = 1;
  isc_dyn_eoc = 255;

(********************************)
(** Operations (may be nested) **)
(********************************)

  isc_dyn_begin = 2;
  isc_dyn_end = 3;
  isc_dyn_if = 4;
  isc_dyn_def_database = 5;
  isc_dyn_def_global_fld = 6;
  isc_dyn_def_local_fld = 7;
  isc_dyn_def_idx = 8;
  isc_dyn_def_rel = 9;
  isc_dyn_def_sql_fld = 10;
  isc_dyn_def_view = 12;
  isc_dyn_def_trigger = 15;
  isc_dyn_def_security_class = 120;
  isc_dyn_def_dimension = 140;
  isc_dyn_def_generator = 24;
  isc_dyn_def_function = 25;
  isc_dyn_def_filter = 26;
  isc_dyn_def_function_arg = 27;
  isc_dyn_def_shadow = 34;
  isc_dyn_def_trigger_msg = 17;
  isc_dyn_def_file = 36;
  isc_dyn_mod_database = 39;
  isc_dyn_mod_rel = 11;
  isc_dyn_mod_global_fld = 13;
  isc_dyn_mod_idx = 102;
  isc_dyn_mod_local_fld = 14;
  isc_dyn_mod_sql_fld = 216;
  isc_dyn_mod_view = 16;
  isc_dyn_mod_security_class = 122;
  isc_dyn_mod_trigger = 113;
  isc_dyn_mod_trigger_msg = 28;
  isc_dyn_delete_database = 18;
  isc_dyn_delete_rel = 19;
  isc_dyn_delete_global_fld = 20;
  isc_dyn_delete_local_fld = 21;
  isc_dyn_delete_idx = 22;
  isc_dyn_delete_security_class = 123;
  isc_dyn_delete_dimensions = 143;
  isc_dyn_delete_trigger = 23;
  isc_dyn_delete_trigger_msg = 29;
  isc_dyn_delete_filter = 32;
  isc_dyn_delete_function = 33;
  isc_dyn_delete_shadow = 35;
  isc_dyn_grant = 30;
  isc_dyn_revoke = 31;
  isc_dyn_def_primary_key = 37;
  isc_dyn_def_foreign_key = 38;
  isc_dyn_def_unique = 40;
  isc_dyn_def_procedure = 164;
  isc_dyn_delete_procedure = 165;
  isc_dyn_def_parameter = 135;
  isc_dyn_delete_parameter = 136;

  isc_dyn_mod_procedure = 175;

  isc_dyn_def_exception = 181;
  isc_dyn_mod_exception = 182;
  isc_dyn_del_exception = 183;

  isc_dyn_def_difference =           220;
  isc_dyn_drop_difference =          221;
  isc_dyn_begin_backup =             222;
  isc_dyn_end_backup =               223;
  isc_dyn_debug_info =               240;

  isc_dyn_view_blr = 43;
  isc_dyn_view_source = 44;
  isc_dyn_view_relation = 45;
  isc_dyn_view_context = 46;
  isc_dyn_view_context_name = 47;

(************************)
(** Generic attributes **)
(************************)

  isc_dyn_rel_name = 50;
  isc_dyn_fld_name = 51;
  isc_dyn_new_fld_name = 215;
  isc_dyn_idx_name = 52;
  isc_dyn_description = 53;
  isc_dyn_security_class = 54;
  isc_dyn_system_flag = 55;
  isc_dyn_update_flag = 56;
  isc_dyn_prc_name = 166;
  isc_dyn_prm_name = 137;
  isc_dyn_sql_object = 196;
  isc_dyn_fld_character_set_name = 174;

(**********************************)
(** Relation specific attributes **)
(**********************************)

  isc_dyn_rel_dbkey_length = 61;
  isc_dyn_rel_store_trig = 62;
  isc_dyn_rel_modify_trig = 63;
  isc_dyn_rel_erase_trig = 64;
  isc_dyn_rel_store_trig_source = 65;
  isc_dyn_rel_modify_trig_source = 66;
  isc_dyn_rel_erase_trig_source = 67;
  isc_dyn_rel_ext_file = 68;
  isc_dyn_rel_sql_protection = 69;
  isc_dyn_rel_constraint = 162;
  isc_dyn_delete_rel_constraint = 163;

(**************************************)
(** Global field specific attributes **)
(**************************************)

  isc_dyn_fld_type = 70;
  isc_dyn_fld_length = 71;
  isc_dyn_fld_scale = 72;
  isc_dyn_fld_sub_type = 73;
  isc_dyn_fld_segment_length = 74;
  isc_dyn_fld_query_header = 75;
  isc_dyn_fld_edit_string = 76;
  isc_dyn_fld_validation_blr = 77;
  isc_dyn_fld_validation_source = 78;
  isc_dyn_fld_computed_blr = 79;
  isc_dyn_fld_computed_source = 80;
  isc_dyn_fld_missing_value = 81;
  isc_dyn_fld_default_value = 82;
  isc_dyn_fld_query_name = 83;
  isc_dyn_fld_dimensions = 84;
  isc_dyn_fld_not_null = 85;
  isc_dyn_fld_precision = 86;
  isc_dyn_fld_char_length = 172;
  isc_dyn_fld_collation = 173;
  isc_dyn_fld_default_source = 193;
  isc_dyn_del_default = 197;
  isc_dyn_del_validation = 198;
  isc_dyn_single_validation = 199;
  isc_dyn_fld_character_set = 203;
  isc_dyn_del_computed = 242;

(*************************************)
(** Local field specific attributes **)
(*************************************)

  isc_dyn_fld_source = 90;
  isc_dyn_fld_base_fld = 91;
  isc_dyn_fld_position = 92;
  isc_dyn_fld_update_flag = 93;

(*******************************)
(** Index specific attributes **)
(*******************************)

  isc_dyn_idx_unique = 100;
  isc_dyn_idx_inactive = 101;
  isc_dyn_idx_type = 103;
  isc_dyn_idx_foreign_key = 104;
  isc_dyn_idx_ref_column = 105;
  isc_dyn_idx_statistic = 204;

(*********************************)
(** Trigger specific attributes **)
(*********************************)

  isc_dyn_trg_type = 110;
  isc_dyn_trg_blr = 111;
  isc_dyn_trg_source = 112;
  isc_dyn_trg_name = 114;
  isc_dyn_trg_sequence = 115;
  isc_dyn_trg_inactive = 116;
  isc_dyn_trg_msg_number = 117;
  isc_dyn_trg_msg = 118;

(****************************************)
(** Security Class specific attributes **)
(****************************************)

  isc_dyn_scl_acl = 121;
  isc_dyn_grant_user = 130;
  isc_dyn_grant_proc = 186;
  isc_dyn_grant_trig = 187;
  isc_dyn_grant_view = 188;
  isc_dyn_grant_options = 132;
  isc_dyn_grant_user_group = 205;
  isc_dyn_grant_role = 218;
  isc_dyn_grant_grantor = 245;

(************************************)
(** Dimension specific information **)
(************************************)

  isc_dyn_dim_lower = 141;
  isc_dyn_dim_upper = 142;

(******************************)
(** File specific attributes **)
(******************************)

  isc_dyn_file_name = 125;
  isc_dyn_file_start = 126;
  isc_dyn_file_length = 127;
  isc_dyn_shadow_number = 128;
  isc_dyn_shadow_man_auto = 129;
  isc_dyn_shadow_conditional = 130;


(**********************************)
(** Function specific attributes **)
(**********************************)

  isc_dyn_function_name = 145;
  isc_dyn_function_type = 146;
  isc_dyn_func_module_name = 147;
  isc_dyn_func_entry_point = 148;
  isc_dyn_func_return_argument = 149;
  isc_dyn_func_arg_position = 150;
  isc_dyn_func_mechanism = 151;
  isc_dyn_filter_in_subtype = 152;
  isc_dyn_filter_out_subtype = 153;

  isc_dyn_description2 = 154;
  isc_dyn_fld_computed_source2 = 155;
  isc_dyn_fld_edit_string2 = 156;
  isc_dyn_fld_query_header2 = 157;
  isc_dyn_fld_validation_source2 = 158;
  isc_dyn_trg_msg2 = 159;
  isc_dyn_trg_source2 = 160;
  isc_dyn_view_source2 = 161;
  isc_dyn_xcp_msg2 = 184;

(***********************************)
(** Generator specific attributes **)
(***********************************)

  isc_dyn_generator_name = 95;
  isc_dyn_generator_id = 96;

(***********************************)
(** Procedure specific attributes **)
(***********************************)

  isc_dyn_prc_inputs = 167;
  isc_dyn_prc_outputs = 168;
  isc_dyn_prc_source = 169;
  isc_dyn_prc_blr = 170;
  isc_dyn_prc_source2 = 171;
  isc_dyn_prc_type = 239;

  isc_dyn_prc_t_selectable        =  1;
  isc_dyn_prc_t_executable        =  2;

(***********************************)
(** Parameter specific attributes **)
(***********************************)

  isc_dyn_prm_number = 138;
  isc_dyn_prm_type = 139;
  isc_dyn_prm_mechanism = 241;

(**********************************)
(** Relation specific attributes **)
(**********************************)

  isc_dyn_xcp_msg = 185;

(************************************************)
(** Cascading referential integrity values     **)
(************************************************)
  isc_dyn_foreign_key_update = 205;
  isc_dyn_foreign_key_delete = 206;
  isc_dyn_foreign_key_cascade = 207;
  isc_dyn_foreign_key_default = 208;
  isc_dyn_foreign_key_null = 209;
  isc_dyn_foreign_key_none = 210;

(*************************)
(** SQL role values     **)
(*************************)

  isc_dyn_def_sql_role = 211;
  isc_dyn_sql_role_name = 212;
  isc_dyn_grant_admin_options = 213;
  isc_dyn_del_sql_role = 214;

(**********************************************)
(* Generators again                           *)
(**********************************************)

  isc_dyn_delete_generator         = 217;

  isc_dyn_mod_function             = 224;
  isc_dyn_mod_filter               = 225;
  isc_dyn_mod_generator            = 226;
  isc_dyn_mod_sql_role             = 227;
  isc_dyn_mod_charset              = 228;
  isc_dyn_mod_collation            = 229;
  isc_dyn_mod_prc_parameter        = 230;

(***********************)
(* collation values    *)
(***********************)

  isc_dyn_def_collation					 = 231;
  isc_dyn_coll_for_charset				 = 232;
  isc_dyn_coll_from						 = 233;
  isc_dyn_coll_attribute					 = 234;
  isc_dyn_coll_specific_attributes_charset = 235;
  isc_dyn_coll_specific_attributes		 = 236;
  isc_dyn_del_collation					 = 237;

  isc_dyn_mapping = 243;
  isc_dyn_map_role = 1;
  isc_dyn_unmap_role = 2;
  isc_dyn_map_user = 3;
  isc_dyn_unmap_user = 4;
  isc_dyn_automap_role = 5;
  isc_dyn_autounmap_role = 6;

  isc_dyn_user = 244;
  isc_dyn_user_add = 1;
  isc_dyn_user_mod = 2;
  isc_dyn_user_del = 3;
  isc_dyn_user_passwd = 4;
  isc_dyn_user_first = 5;
  isc_dyn_user_middle = 6;
  isc_dyn_user_last = 7;
  isc_dyn_user_admin = 8;
  isc_user_end = 0;

  isc_dyn_last_dyn_value           = 247;

(********************************************)
(** Array slice description language (SDL) **)
(********************************************)

  isc_sdl_version1 = 1;
  isc_sdl_eoc = -1;
  isc_sdl_relation = 2;
  isc_sdl_rid = 3;
  isc_sdl_field = 4;
  isc_sdl_fid = 5;
  isc_sdl_struct = 6;
  isc_sdl_variable = 7;
  isc_sdl_scalar = 8;
  isc_sdl_tiny_integer = 9;
  isc_sdl_short_integer = 10;
  isc_sdl_long_integer = 11;
  isc_sdl_literal = 12;
  isc_sdl_add = 13;
  isc_sdl_subtract = 14;
  isc_sdl_multiply = 15;
  isc_sdl_divide = 16;
  isc_sdl_negate = 17;
  isc_sdl_eql = 18;
  isc_sdl_neq = 19;
  isc_sdl_gtr = 20;
  isc_sdl_geq = 21;
  isc_sdl_lss = 22;
  isc_sdl_leq = 23;
  isc_sdl_and = 24;
  isc_sdl_or = 25;
  isc_sdl_not = 26;
  isc_sdl_while = 27;
  isc_sdl_assignment = 28;
  isc_sdl_label = 29;
  isc_sdl_leave = 30;
  isc_sdl_begin = 31;
  isc_sdl_end = 32;
  isc_sdl_do3 = 33;
  isc_sdl_do2 = 34;
  isc_sdl_do1 = 35;
  isc_sdl_element = 36;

(**********************************************)
(** International text interpretation values **)
(**********************************************)

  isc_interp_eng_ascii = 0;
  isc_interp_jpn_sjis = 5;
  isc_interp_jpn_euc = 6;

(*******************)
(** Blob Subtypes **)
(*******************)

(** types less than zero are reserved for customer use **)

  isc_blob_untyped = 0;

(** internal subtypes **)

  isc_blob_text = 1;
  isc_blob_blr = 2;
  isc_blob_acl = 3;
  isc_blob_ranges = 4;
  isc_blob_summary = 5;
  isc_blob_format = 6;
  isc_blob_tra = 7;
  isc_blob_extfile = 8;
  isc_blob_debug_info = 9;
  isc_blob_max_predefined_subtype   = 10;

(** the range 20-30 is reserved for dBASE and Paradox types **)

  isc_blob_formatted_memo = 20;
  isc_blob_paradox_ole = 21;
  isc_blob_graphic = 22;
  isc_blob_dbase_ole = 23;
  isc_blob_typed_binary = 24;

  isc_info_db_SQL_dialect = 62;
  isc_info_db_read_only = 63;
  isc_info_db_size_in_pages = 64;

  fb_shut_confirmation                     = 1;
  fb_shut_preproviders                     = 2;
  fb_shut_postproviders                    = 4;
  fb_shut_finish                                   = 8;

  fb_shutrsn_svc_stopped                    =-1;
  fb_shutrsn_no_connection                  =-2;
  fb_shutrsn_app_stopped                    =-3;
  fb_shutrsn_device_removed                 =-4;
  fb_shutrsn_signal                                 =-5;
  fb_shutrsn_services                               =-6;
  fb_shutrsn_exit_called                    =-7;

  fb_cancel_disable                                = 1;
  fb_cancel_enable                                 = 2;
  fb_cancel_raise                                  = 3;
  fb_cancel_abort = 4;

  fb_dbg_version = 1;
  fb_dbg_end = 255;
  fb_dbg_map_src2blr = 2;
  fb_dbg_map_varname = 3;
  fb_dbg_map_argument = 4;

  fb_dbg_arg_input = 0;
  fb_dbg_arg_output = 1;

{*********************************}
{  Information call declarations  }
{*********************************}

  isc_info_end = 1;
  isc_info_truncated = 2;
  isc_info_error = 3;
  isc_info_data_not_ready = 4;
  isc_info_length = 126;
  isc_info_flag_end = 127;

(********************************)
(** Database information items **)
(********************************)

  isc_info_db_id = 4;
  isc_info_reads = 5;
  isc_info_writes = 6;
  isc_info_fetches = 7;
  isc_info_marks = 8;

  isc_info_implementation = 11;
  isc_info_version = 12;
  isc_info_base_level = 13;
  isc_info_svr_maj_ver = isc_info_base_level;
  isc_info_page_size = 14;
  isc_info_num_buffers = 15;
  isc_info_limbo = 16;
  isc_info_current_memory = 17;
  isc_info_max_memory = 18;
  isc_info_window_turns = 19;
  isc_info_license = 20;

  isc_info_allocation = 21;
  isc_info_attachment_id = 22;
  isc_info_read_seq_count = 23;
  isc_info_read_idx_count = 24;
  isc_info_insert_count = 25;
  isc_info_update_count = 26;
  isc_info_delete_count = 27;
  isc_info_backout_count = 28;
  isc_info_purge_count = 29;
  isc_info_expunge_count = 30;
  
  isc_info_sweep_interval = 31;
  isc_info_ods_version = 32;
  isc_info_ods_minor_version = 33;
  isc_info_no_reserve = 34;
  
  isc_info_logfile = 35;
  isc_info_cur_logfile_name = 36;
  isc_info_cur_log_part_offset = 37;
  isc_info_num_wal_buffers = 38;
  isc_info_wal_buffer_size = 39;
  isc_info_wal_ckpt_length = 40;

  isc_info_wal_cur_ckpt_interval = 41;
  isc_info_wal_prv_ckpt_fname = 42;
  isc_info_wal_prv_ckpt_poffset = 43;
  isc_info_wal_recv_ckpt_fname = 44;
  isc_info_wal_recv_ckpt_poffset = 45;
  isc_info_wal_grpc_wait_usecs = 47;
  isc_info_wal_num_io = 48;
  isc_info_wal_avg_io_size = 49;
  isc_info_wal_num_commits = 50;
  isc_info_wal_avg_grpc_size = 51;
  isc_info_forced_writes = 52;
  isc_info_user_names = 53;
  isc_info_page_errors = 54;
  isc_info_record_errors = 55;
  isc_info_bpage_errors = 56;
  isc_info_dpage_errors = 57;
  isc_info_ipage_errors = 58;
  isc_info_ppage_errors = 59;
  isc_info_tpage_errors = 60;
  isc_info_set_page_buffers = 61;

  frb_info_att_charset = 101;
  isc_info_db_class = 102;
  isc_info_firebird_version = 103;
  isc_info_oldest_transaction = 104;
  isc_info_oldest_active = 105;
  isc_info_oldest_snapshot = 106;
  isc_info_next_transaction = 107;
  isc_info_db_provider = 108;
  isc_info_active_transactions = 109;
  isc_info_active_tran_count = 110;
  isc_info_creation_date = 111;
  isc_info_db_file_size = 112;
  fb_info_page_contents = 113;

  isc_info_db_last_value  = fb_info_page_contents;

(****************************************)
(** Database information return values **)
(****************************************)

  isc_info_db_impl_rdb_vms = 1;
  isc_info_db_impl_rdb_eln = 2;
  isc_info_db_impl_rdb_eln_dev = 3;
  isc_info_db_impl_rdb_vms_y = 4;
  isc_info_db_impl_rdb_eln_y = 5;
  isc_info_db_impl_jri = 6;
  isc_info_db_impl_jsv = 7;

  isc_info_db_impl_isc_apl_68K = 25;
  isc_info_db_impl_isc_vax_ultr = 26;
  isc_info_db_impl_isc_vms = 27;
  isc_info_db_impl_isc_sun_68k = 28;
  isc_info_db_impl_isc_os2 = 29;
  isc_info_db_impl_isc_sun4 = 30;
  isc_info_db_impl_isc_hp_ux = 31;
  isc_info_db_impl_isc_sun_386i = 32;
  isc_info_db_impl_isc_vms_orcl = 33;
  isc_info_db_impl_isc_mac_aux = 34;
  isc_info_db_impl_isc_rt_aix = 35;
  isc_info_db_impl_isc_mips_ult = 36;
  isc_info_db_impl_isc_xenix = 37;
  isc_info_db_impl_isc_dg = 38;
  isc_info_db_impl_isc_hp_mpexl = 39;
  isc_info_db_impl_isc_hp_ux68K = 40;
  isc_info_db_impl_isc_sgi = 41;
  isc_info_db_impl_isc_sco_unix = 42;
  isc_info_db_impl_isc_cray = 43;
  isc_info_db_impl_isc_imp = 44;
  isc_info_db_impl_isc_delta = 45;

  isc_info_db_impl_isc_next = 46;
  isc_info_db_impl_isc_dos = 47;
  isc_info_db_impl_m88K = 48;
  isc_info_db_impl_unixware = 49;
  isc_info_db_impl_isc_winnt_x86 = 50;
  isc_info_db_impl_isc_epson = 51;
  isc_info_db_impl_alpha_osf = 52;
  isc_info_db_impl_alpha_vms = 53;
  isc_info_db_impl_netware_386 = 54;
  isc_info_db_impl_win_only = 55;
  isc_info_db_impl_ncr_3000 = 56;
  isc_info_db_impl_winnt_ppc = 57;
  isc_info_db_impl_dg_x86 = 58;
  isc_info_db_impl_sco_ev = 59;
  isc_info_db_impl_i386 = 60;

  isc_info_db_impl_freebsd = 61;
  isc_info_db_impl_netbsd = 62;
  isc_info_db_impl_darwin = 63;
  isc_info_db_impl_sinixz = 64;

  isc_info_db_impl_linux_sparc = 65;
  isc_info_db_impl_linux_amd64 = 66;

  isc_info_db_impl_freebsd_amd64 = 67;

  isc_info_db_impl_winnt_amd64 = 68;

  isc_info_db_impl_linux_ppc = 69;
  isc_info_db_impl_darwin_x86 = 70;
  isc_info_db_impl_linux_mipsel = 71;
  isc_info_db_impl_linux_mips = 72;
  isc_info_db_impl_darwin_x64 = 73;
  isc_info_db_impl_sun_amd64 = 74;

  isc_info_db_impl_linux_arm = 75;
  isc_info_db_impl_linux_ia64 = 76;

  isc_info_db_impl_darwin_ppc64 = 77;
  isc_info_db_impl_linux_s390x = 78;
  isc_info_db_impl_linux_s390 = 79;

  isc_info_db_impl_linux_sh = 80;
  isc_info_db_impl_linux_sheb = 81;
  isc_info_db_impl_linux_hppa = 82;
  isc_info_db_impl_linux_alpha = 83;

  isc_info_db_impl_last_value = isc_info_db_impl_linux_alpha;

  isc_info_db_class_access = 1;
  isc_info_db_class_y_valve = 2;
  isc_info_db_class_rem_int = 3;
  isc_info_db_class_rem_srvr = 4;
  isc_info_db_class_pipe_int = 7;
  isc_info_db_class_pipe_srvr = 8;
  isc_info_db_class_sam_int = 9;
  isc_info_db_class_sam_srvr = 10;
  isc_info_db_class_gateway = 11;
  isc_info_db_class_cache = 12;
  isc_info_db_class_classic_access = 13;
  isc_info_db_class_server_access = 14;

  isc_info_db_class_last_value = isc_info_db_class_server_access;

//  info_db_provider
  isc_info_db_code_rdb_eln = 1;
  isc_info_db_code_rdb_vms = 2;
  isc_info_db_code_interbase = 3;
  isc_info_db_code_firebird = 4;

  isc_info_db_code_last_value = isc_info_db_code_firebird;

(*******************************)
(** Request information items **)
(*******************************)

  isc_info_number_messages = 4;
  isc_info_max_message = 5;
  isc_info_max_send = 6;
  isc_info_max_receive = 7;
  isc_info_state = 8;
  isc_info_message_number = 9;
  isc_info_message_size = 10;
  isc_info_request_cost = 11;
  isc_info_access_path = 12;
  isc_info_req_select_count = 13;
  isc_info_req_insert_count = 14;
  isc_info_req_update_count = 15;
  isc_info_req_delete_count = 16;

(***********************)
(** Access path items **)
(***********************)

  isc_info_rsb_end = 0;
  isc_info_rsb_begin = 1;
  isc_info_rsb_type = 2;
  isc_info_rsb_relation = 3;
  isc_info_rsb_plan = 4;

(***************)
(** Rsb types **)
(***************)

  isc_info_rsb_unknown = 1;
  isc_info_rsb_indexed = 2;
  isc_info_rsb_navigate = 3;
  isc_info_rsb_sequential = 4;
  isc_info_rsb_cross = 5;
  isc_info_rsb_sort = 6;
  isc_info_rsb_first = 7;
  isc_info_rsb_boolean = 8;
  isc_info_rsb_union = 9;
  isc_info_rsb_aggregate = 10;
  isc_info_rsb_merge = 11;
  isc_info_rsb_ext_sequential = 12;
  isc_info_rsb_ext_indexed = 13;
  isc_info_rsb_ext_dbkey = 14;
  isc_info_rsb_left_cross = 15;
  isc_info_rsb_select = 16;
  isc_info_rsb_sql_join = 17;
  isc_info_rsb_simulate = 18;
  isc_info_rsb_sim_cross = 19;
  isc_info_rsb_once = 20;
  isc_info_rsb_procedure = 21;
  isc_info_rsb_skip = 22;
  isc_info_rsb_virt_sequential = 23;
  isc_info_rsb_recursive = 24;

(************************)
(** Bitmap expressions **)
(************************)

  isc_info_rsb_and = 1;
  isc_info_rsb_or = 2;
  isc_info_rsb_dbkey = 3;
  isc_info_rsb_index = 4;

  isc_info_req_active = 2;
  isc_info_req_inactive = 3;
  isc_info_req_send = 4;
  isc_info_req_receive = 5;
  isc_info_req_select = 6;
  isc_info_req_sql_stall = 7;

(****************************)
(** Blob information items **)
(****************************)

  isc_info_blob_num_segments = 4;
  isc_info_blob_max_segment = 5;
  isc_info_blob_total_length = 6;
  isc_info_blob_type = 7;

(***********************************)
(** Transaction information items **)
(***********************************)

  isc_info_tra_id = 4;
  isc_info_tra_oldest_interesting        = 5;
  isc_info_tra_oldest_snapshot           = 6;
  isc_info_tra_oldest_active  = 7;
  isc_info_tra_isolation = 8;
  isc_info_tra_access = 9;
  isc_info_tra_lock_timeout = 10;

  isc_info_tra_consistency = 1;
  isc_info_tra_concurrency = 2;
  isc_info_tra_read_committed = 3;

  isc_info_tra_no_rec_version = 0;
  isc_info_tra_rec_version = 1;

  isc_info_tra_readonly = 0;
  isc_info_tra_readwrite = 1;

(***************************)
(** SQL information items **)
(***************************)

  isc_info_sql_select = 4;
  isc_info_sql_bind = 5;
  isc_info_sql_num_variables = 6;
  isc_info_sql_describe_vars = 7;
  isc_info_sql_describe_end = 8;
  isc_info_sql_sqlda_seq = 9;
  isc_info_sql_message_seq = 10;
  isc_info_sql_type = 11;
  isc_info_sql_sub_type = 12;
  isc_info_sql_scale = 13;
  isc_info_sql_length = 14;
  isc_info_sql_null_ind = 15;
  isc_info_sql_field = 16;
  isc_info_sql_relation = 17;
  isc_info_sql_owner = 18;
  isc_info_sql_alias = 19;
  isc_info_sql_sqlda_start = 20;
  isc_info_sql_stmt_type = 21;
  isc_info_sql_get_plan = 22;
  isc_info_sql_records = 23;
  isc_info_sql_batch_fetch = 24;
  isc_info_sql_relation_alias = 25;

(***********************************)
(** SQL information return values **)
(***********************************)

  isc_info_sql_stmt_select = 1;
  isc_info_sql_stmt_insert = 2;
  isc_info_sql_stmt_update = 3;
  isc_info_sql_stmt_delete = 4;
  isc_info_sql_stmt_ddl = 5;
  isc_info_sql_stmt_get_segment = 6;
  isc_info_sql_stmt_put_segment = 7;
  isc_info_sql_stmt_exec_procedure = 8;
  isc_info_sql_stmt_start_trans = 9;
  isc_info_sql_stmt_commit = 10;
  isc_info_sql_stmt_rollback = 11;
  isc_info_sql_stmt_select_for_upd = 12;
  isc_info_sql_stmt_set_generator = 13;
  isc_info_sql_stmt_savepoint = 14;


{ Error codes from  }

  isc_facility = 20;
  isc_err_base = 335544320;
  isc_err_factor = 1;
  isc_arg_end = 0;
  isc_arg_gds = 1;
  isc_arg_string = 2;
  isc_arg_cstring = 3;
  isc_arg_number = 4;
  isc_arg_interpreted = 5;
  isc_arg_vms = 6;
  isc_arg_unix = 7;
  isc_arg_domain = 8;
  isc_arg_dos = 9;
  isc_arg_mpexl = 10;
  isc_arg_mpexl_ipc = 11;
  isc_arg_next_mach = 15;
  isc_arg_netware = 16;
  isc_arg_win32 = 17;
  isc_arg_warning = 18;
  isc_arg_sql_state		= 19;

  XSQLVAR_SIZE = sizeof(TXSQLVAR);

function XSQLDA_LENGTH(n: Long): Long;

(*
#define ADD_SPB_LENGTH(p, length)	{*(p)++ = (length); \
          *(p)++ = (length) >> 8;}

#define ADD_SPB_NUMERIC(p, data)	{*(p)++ = (data); \
          *(p)++ = (data) >> 8; \
      *(p)++ = (data) >> 16; \
      *(p)++ = (data) >> 24;}
*)
procedure add_spb_length(var p: PChar; length: integer);
procedure add_spb_numeric(var p: PChar; data: integer);

implementation

function XSQLDA_LENGTH(n: Long): Long;
(*  The C-macro reads like this:
   XSQLDA_LENGTH(n)	(sizeof (XSQLDA) + (n-1) * sizeof (XSQLVAR)) *)
begin
  Result := SizeOf(TXSQLDA) + ((n - 1) * SizeOf(TXSQLVAR));
end;

(*******************************************)
(** Service manager functions             **)
(*******************************************)

procedure add_spb_length(var p: PChar; length: integer);
(*
#define ADD_SPB_LENGTH(p, length)	{*(p)++ = (length); \
          *(p)++ = (length) >> 8;}
*)
begin
  p^ := char(length);
  Inc(p);
  p^ := char(length shr 8);
  Inc(p);
end;

procedure add_spb_numeric(var p: PChar; data: integer);
(*
#define ADD_SPB_NUMERIC(p, data)	{*(p)++ = (data); \
          *(p)++ = (data) >> 8; \
      *(p)++ = (data) >> 16; \
      *(p)++ = (data) >> 24;}
*)
begin
  p^ := char(data);
  Inc(p);
  p^ := char(data shr 8);
  Inc(p);
  p^ := char(data shr 16);
  Inc(p);
  p^ := char(data shr 24);
  Inc(p);
end;

end.

