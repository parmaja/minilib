{* ibase.h *}
{*
 *	MODULE:		ibase.h
 *	DESCRIPTION:	OSRI entrypoints and defines
 *
 * The contents of this file are subject to the Interbase Public
 * License Version 1.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy
 * of the License at http://www.Inprise.com/IPL.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
 * or implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code was created by Inprise Corporation
 * and its predecessors. Portions created by Inprise Corporation are
 * Copyright (C) Inprise Corporation.
 *
 * All Rights Reserved.
 * Contributor(s): ______________________________________.
 *
 * 2001.07.28: John Bellardo:  Added blr_skip
 * 2001.09.18: Ann Harrison:   New info codes
 * 17-Oct-2001 Mike Nordell: CPU affinity
 * 2001-04-16 Paul Beach: ISC_TIME_SECONDS_PRECISION_SCALE modified for HP10
 * Compiler Compatibility
 * 2002.02.15 Sean Leyne - Code Cleanup, removed obsolete ports:
 *                          - EPSON, XENIX, MAC (MAC_AUX), Cray and OS/2
 * 2002.10.29 Nickolay Samofatov: Added support for savepoints
 *
 * 2002.10.29 Sean Leyne - Removed support for obsolete IPX/SPX Protocol
 *
 * 2006.09.06 Steve Boyd - Added various prototypes required by Cobol ESQL
 *                         isc_embed_dsql_length
 *                         isc_event_block_a
 *                         isc_sqlcode_s
 *                         isc_embed_dsql_fetch_a
 *                         isc_event_block_s
 *                         isc_baddress
 *                         isc_baddress_s
 *
 *}

const
  FB_API_VER = 40;
  {$define isc_version4}

  ISC_TRUE = 1;
  ISC_FALSE = 0;

{$define ISC_FAR}

{$i mncFBTypes.inc}

type
  TISC_CALLBACK = procedure; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  isc_resv_handle = ISC_LONG;

  TISC_PRINT_CALLBACK = procedure(P: Pointer; param2:ISC_SHORT; param3:PByte); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TISC_VERSION_CALLBACK = procedure(P: Pointer; Buffer: PByte); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TISC_EVENT_CALLBACK = procedure(P: Pointer; param2: ISC_USHORT; param3: PISC_UCHAR); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {*******************************************************************}
  {* Blob id structure                                               *}
  {*******************************************************************}

  TISC_ARRAY_BOUND = record
    array_bound_lower: SmallInt;
    array_bound_upper: SmallInt;
  end;
  PISC_ARRAY_BOUND = ^TISC_ARRAY_BOUND;

  TISC_ARRAY_DESC = record
    array_desc_dtype: Byte;
    array_desc_scale: AnsiChar;
    array_desc_length: Word;
    array_desc_field_name: array[0..31] of AnsiChar;
    array_desc_relation_name: array[0..31] of AnsiChar;
    array_desc_dimensions: SmallInt;
    array_desc_flags: SmallInt;
    array_desc_bounds: array[0..15] of TISC_ARRAY_BOUND;
  end;
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;

  TISC_BLOB_DESC = record
    blob_desc_subtype: SmallInt;
    blob_desc_charset: SmallInt;
    blob_desc_segment_size: SmallInt;
    blob_desc_field_name: array[0..31] of Byte;
    blob_desc_relation_name: array[0..31] of Byte;
  end; // TISC_BLOB_DESC
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;

  {***************************}
  {* Blob control structure  *}
  {***************************}

  TISC_BLOB_CTL_SOURCE_FUNCTION = function: ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  PISC_BLOB_CTL = ^TISC_BLOB_CTL;

  TISC_BLOB_CTL = record
    ctl_source: TISC_BLOB_CTL_SOURCE_FUNCTION; (** Source filter **)
    ctl_source_handle: PISC_BLOB_CTL; (** Argument to pass to source filter **)
    ctl_to_sub_type: SmallInt; (** Target type **)
    ctl_from_sub_type: SmallInt; (** Source type **)
    ctl_buffer_length: Word; (** Length of buffer **)
    ctl_segment_length: Word; (** Length of current segment **)
    ctl_bpb_length: Word; (** Length of blob parameter **)
    ctl_bpb: PByte; (** Address of blob parameter **)
    ctl_buffer: PByte; (** Address of segment buffer **)
    ctl_max_segment: ISC_LONG; (** Length of longest segment **)
    ctl_number_segments: ISC_LONG; (** Total number of segments **)
    ctl_total_length: ISC_LONG; (** Total length of blob **)
    ctl_status: PISC_STATUS; (** Address of status vector **)
    ctl_data: array[0..7] of long; (** Application specific data **)
  end;

  {***************************}
  {* Blob stream definitions *}
  {***************************}
  TBSTREAM = record
    bstr_blob: PPointer; (** Blob handle **)
    bstr_buffer: PByte; (** Address of buffer **)
    bstr_ptr: PByte; (** Next character **)
    bstr_length: SmallInt; (** Length of buffer **)
    bstr_cnt: SmallInt; (** Characters in buffer **)
    bstr_mode: Char; (** (mode) ? OUTPUT : INPUT **)
  end;
  PBSTREAM = ^TBSTREAM;

{
  /* Three ugly macros, one even using octal radix... sigh... */
  #define getb(p)	(--(p)->bstr_cnt >= 0 ? *(p)->bstr_ptr++ & 0377: BLOB_get (p))
  #define putb(x, p) (((x) == '\n' || (!(--(p)->bstr_cnt))) ? BLOB_put ((x),p) : ((int) (*(p)->bstr_ptr++ = (unsigned) (x))))
  #define putbx(x, p) ((!(--(p)->bstr_cnt)) ? BLOB_put ((x),p) : ((int) (*(p)->bstr_ptr++ = (unsigned) (x))))
}

{********************************************************************}
{* CVC: Public blob interface definition held in val.h.             *}
{* For some unknown reason, it was only documented in langRef       *}
{* and being the structure passed by the engine to UDFs it never    *}
{* made its way into this public definitions file.                  *}
{* Being its original name "blob", I renamed it blobcallback here.  *}
{* I did the full definition with the proper parameters instead of  *}
{* the weak C declaration with any number and type of parameters.   *}
{* Since the first parameter -BLB- is unknown outside the engine,   *}
{* it's more accurate to use void* than int* as the blob pointer    *}
{********************************************************************}

  {* Blob passing structure *}

  {* This enum applies to parameter "mode" in blob_lseek *}
  TBlob_LSeek_Mode = (blb_seek_relative = 1, blb_seek_from_tail = 2);
  {* This enum applies to the value returned by blob_get_segment *}
  TBlob_Get_Result = (blb_got_fragment = -1, blb_got_eof = 0, blb_got_full_segment = 1);


  TBlob_get_segment = function (hnd: Pointer; buffer: PISC_UCHAR; buf_size: ISC_USHORT; result_len: PISC_USHORT): SmallInt; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBlob_put_segment = procedure (hnd: Pointer; const buffer: PISC_UCHAR; buf_size: ISC_USHORT); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBlob_lseek = function (hnd: Pointer; mode: ISC_USHORT; offset: ISC_LONG): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  TBlobCallback = record
    blob_get_segment: TBlob_get_segment;
    blob_handle: PPointer;

    blob_number_segments:ISC_LONG;
    blob_max_segment:ISC_LONG;
    blob_total_length: ISC_LONG;

    blob_put_segment: TBlob_put_segment;
    blob_lseek : TBlob_lseek;
  end;
  PBlobCallback = ^TBlobCallback;

  {********************************************************************}
  {* CVC: Public descriptor interface held in dsc2.h.                 *}
  {* We need it documented to be able to recognize NULL in UDFs.      *}
  {* Being its original name "dsc", I renamed it paramdsc here.       *}
  {* Notice that I adjust to the original definition: contrary to     *}
  {* other cases, the typedef is the same struct not the pointer.     *}
  {* I included the enumeration of dsc_dtype possible values.         *}
  {* Ultimately, dsc2.h should be part of the public interface.       *}
  {********************************************************************}

  {* This is the famous internal descriptor that UDFs can use, too. *}

  TParamDsc = record
      dsc_dtype: ISC_UCHAR;
      dsc_scale: ShortInt;
      dsc_length: ISC_USHORT;
      dsc_sub_type: SmallInt;
      dsc_flags: ISC_USHORT;
      dsc_address: PISC_UCHAR;
  end;

  {* This is a helper struct to work with varchars. *}
  TParamVary = record
    vary_length: ISC_USHORT;
    vary_string:array[0..0] of ISC_UCHAR;
  end;

  {$i mncFBDsc.inc}

  {***************************}
  {* Dynamic SQL definitions *}
  {***************************}

  {$i mncFBSQLDA.inc}

  {***************************}
  {* OSRI database functions *}
  {***************************}

type

  { Not exist in original API }

  (********************************************************)
  (** This record type is for passing arguments to       **)
  (** isc_start_transaction (See docs)                   **)
  (********************************************************)
    TISC_START_TRANS = record
      db_handle: PISC_DB_HANDLE;
      tpb_length: Word;
      tpb_address: PByte;
    end;

  (********************************************************)
  (** This record type is for passing arguments to       **)
  (** isc_start_multiple (see docs)                      **)
  (********************************************************)
    TISC_TEB = record
      db_handle: PISC_DB_HANDLE;
      tpb_length: Long;
      tpb_address: PByte;
    end;
    PISC_TEB = ^TISC_TEB;
    TISC_TEB_ARRAY = array[0..0] of TISC_TEB;
    PISC_TEB_ARRAY = ^TISC_TEB_ARRAY;

type

  Tisc_attach_database = function(status_vector: PISC_STATUS; db_name_length: SmallInt; db_name: PByte;  db_handle: PISC_DB_HANDLE; parm_buffer_length: SmallInt; parm_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_gen_sdl = function(status_vector: PISC_STATUS; isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PSmallInt; isc_arg4: PByte; isc_arg5: PSmallInt): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_get_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; dest_array: PPointer; slice_length: ISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_lookup_bounds = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_ARRAY_DESC): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_lookup_desc = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_ARRAY_DESC): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_set_desc = function(status_vector: PISC_STATUS; table_name: PByte; column_name: PByte; sql_dtype, sql_length, sql_dimensions: PSmallInt; descriptor: PISC_ARRAY_DESC): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_put_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; source_array: PPointer; slice_length: PISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_default_desc = procedure(descriptor: PISC_BLOB_DESC; table_name: PByte; column_name: PByte); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_gen_bpb = function(status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC; bpb_buffer_length: Word; bpb_buffer: PByte; bpb_length: PWord): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_info = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: SmallInt; item_list_buffer: PByte; result_buffer_length: SmallInt; result_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_lookup_desc = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_BLOB_DESC; global: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_set_desc = function(status_vector: PISC_STATUS; table_name, column_name: PByte; subtype, charset, segment_size: SmallInt; descriptor: PISC_BLOB_DESC): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_cancel_blob = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_cancel_events = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_close_blob = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_commit_retaining = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_commit_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_create_blob = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_create_blob2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: SmallInt; bpb_address: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_create_database = function(status_vector: PISC_STATUS; isc_arg2: SmallInt; isc_arg3: PByte; db_handle: PISC_DB_HANDLE; isc_arg5: SmallInt; isc_arg6: PByte; isc_arg7: SmallInt): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_database_info = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; item_list_buffer_length: SmallInt; item_list_buffer: PByte; result_buffer_length: SmallInt; result_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_decode_date = procedure(ib_date: PISC_QUAD; tm_date: PCTimeStructure); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_decode_sql_date = procedure(ib_date: PISC_DATE; tm_date: PCTimeStructure); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_decode_sql_time = procedure(ib_time: PISC_TIME; tm_date: PCTimeStructure); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_decode_timestamp = procedure(ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_detach_database = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_drop_database = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_allocate_statement = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_alloc_statement2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_describe = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_describe_bind = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_exec_immed2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word; statement: PByte; dialect: Word; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute_immediate = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word; statement: PByte; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_fetch = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_finish = function(db_handle: PISC_DB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_free_statement = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_insert = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; arg3: Word; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_prepare = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; length: Word; statement: PByte; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_set_cursor_name = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; cursor_name: PByte; _type: Word): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_sql_info = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; item_length: SmallInt; items: PByte; buffer_length: SmallInt; buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tfb_dsql_set_timeout  = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; Timeout: ISC_ULONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}
  {*ISC_STATUS ISC_EXPORT fb_get_statement_interface(ISC_STATUS*,
                                                    FB_API_HANDLE*,
                                                    void**);}
  Tisc_encode_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_QUAD); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_encode_sql_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_DATE); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_encode_sql_time = procedure(tm_date: PCTimeStructure; ib_time: PISC_TIME); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_encode_timestamp = procedure(tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_event_block = function(event_buffer: PPByte; result_buffer: PPByte; id_count: Word; var event_list: array of PByte): ISC_LONG; cdecl;
  Tisc_event_block_a = function(event_buffer: PPByte; result_buffer: PPByte; id_count: Word; event_list: PPByte): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_event_block_s = function(event_buffer: PPByte; result_buffer: PPByte; id_count: Word; event_list: PPByte; arg1: PWord): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_event_counts = procedure(status_vector: PISC_STATUS; buffer_length: SmallInt; event_buffer: PByte; result_buffer: PByte); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  //Tisc_modify_dpb = function(dpb: PPByte; isc_arg2, isc_arg3: PSmallInt; isc_arg4: Word; isc_arg5: PByte; isc_arg6: SmallInt): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; deprecated;
  Tisc_free = function(isc_arg1: PByte): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_get_segment = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord; seg_buffer_length: Word; seg_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_get_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PISC_QUAD; isc_arg5: SmallInt; isc_arg6: PByte; isc_arg7: SmallInt; isc_arg8: PISC_LONG; isc_arg9: ISC_LONG; isc_arg10: PPointer; isc_arg11: PISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  {* CVC: This non-const signature is needed for compatibility, see gds.cpp. *}
  Tisc_interprete = function(buffer: PByte; var status_vector: PISC_STATUS): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  {* This const params version used in the engine and other places. *}
  Tfb_interpret = function(buffer: PByte; length: SmallInt; const status_vector: PPISC_STATUS): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_open_blob = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_open_blob2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: SmallInt; bpb_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_prepare_transaction2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; msg_length: SmallInt; msg: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_print_sqlerror = procedure(sqlcode: SmallInt; status_vector: PISC_STATUS); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_print_status = function(status_vector: PISC_STATUS): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_put_segment = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_put_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PISC_QUAD; isc_arg5: SmallInt; isc_arg6: PByte; isc_arg7: SmallInt; isc_arg8: PISC_LONG; isc_arg9: ISC_LONG; isc_arg10: PPointer): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_que_events = function( status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: SmallInt; event_buffer: PByte; event_function: TISC_EVENT_CALLBACK; event_function_arg: PPointer): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
/////  Tisc_release_savepoint = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; tran_name: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_rollback_retaining = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
////  Tisc_rollback_savepoint = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; tran_name: PByte; Option: Word): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_rollback_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_start_multiple = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; db_handle_count: SmallInt; teb_vector_address: PISC_TEB): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
///  Tisc_start_savepoint = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; tran_name: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_start_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; db_handle_count: SmallInt; db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PByte): ISC_STATUS; cdecl;
  Tfb_disconnect_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}
  Tisc_sqlcode = function(status_vector: PISC_STATUS): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_sqlcode_s = function(status_vector: PISC_STATUS; arg1: PISC_ULONG): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tfb_sqlstate = procedure(State: PAnsiChar; status_vector: PISC_STATUS); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}
  Tisc_sql_interprete = procedure(sqlcode: SmallInt; buffer: PByte; buffer_length: SmallInt); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_transaction_info = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; item_list_buffer_length: SmallInt; item_list_buffer: PByte; result_buffer_length: SmallInt; result_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_transact_request = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: Word; isc_arg5: PByte; isc_arg6: Word; isc_arg7: PByte; isc_arg8: Word; isc_arg9: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_vax_integer = function(buffer: PByte; length: SmallInt): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_portable_integer = function(buffer: PByte; length: SmallInt): ISC_INT64; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {*************************************}
  {* Security Functions and structures *}
  {*************************************}

const
  sec_uid_spec           = $01;
  sec_gid_spec           = $02;
  sec_server_spec        = $04;
  sec_password_spec      = $08;
  sec_group_name_spec    = $10;
  sec_first_name_spec    = $20;
  sec_middle_name_spec   = $40;
  sec_last_name_spec     = $80;
  sec_dba_user_name_spec = $100;
  sec_dba_password_spec  = $200;

  sec_protocol_tcpip     = 1;
  sec_protocol_netbeui   = 2;
  sec_protocol_spx       = 3; // -- Deprecated Protocol. Declaration retained for compatibility
  sec_protocol_local     = 4;

type
  TUserSecData = record
    sec_flags: SmallInt; (* which fields are specified *)
    uid: Integer; (* the user's id *)
    gid: Integer; (* the user's group id *)
    protocol: Integer; (* protocol to use for connection *)
    server: PISC_SCHAR; (* server to administer *)
    user_name: ISC_SCHAR; (* the user's name *)
    password: ISC_SCHAR; (* the user's password *)
    group_name: ISC_SCHAR; (* the group name *)
    first_name: ISC_SCHAR; (* the user's first name *)
    middle_name: ISC_SCHAR; (* the user's middle name *)
    last_name: ISC_SCHAR; (* the user's last name *)
    dba_user_name: ISC_SCHAR; (* the dba user name *)
    dba_password: ISC_SCHAR; (* the dba password *)
  end;
  PUserSecData = ^TUserSecData;

  Tisc_add_user = function(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_delete_user = function(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_modify_user = function(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {**********************************}
  {*  Other OSRI functions          *}
  {**********************************}

  Tisc_compile_request = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; request_handle: PISC_REQ_HANDLE; isc_arg4: SmallInt; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_compile_request2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; request_handle: PISC_REQ_HANDLE; isc_arg4: SmallInt; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  // This function always returns error since FB 3.0.
  // Tisc_ddl = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: SmallInt; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; deprecated;
  Tisc_prepare_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_receive = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3, isc_arg4: SmallInt; isc_arg5: PPointer; isc_arg6: SmallInt): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  Tisc_reconnect_transaction = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: SmallInt; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_release_request = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_request_info = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3: SmallInt; isc_arg4: SmallInt; isc_arg5: PByte; isc_arg6: SmallInt; isc_arg7: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_seek_blob = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; isc_arg3: SmallInt; isc_arg4: ISC_LONG; isc_arg5: PISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_send = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3, isc_arg4: SmallInt; isc_arg5: PPointer; isc_arg6: SmallInt): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_start_and_send = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4, isc_arg5: SmallInt; isc_arg6: PPointer; isc_arg7: SmallInt): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_start_request = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: SmallInt): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_unwind_request = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: SmallInt): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_wait_for_event = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; length: SmallInt; event_buffer, result_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {*****************************}
  {* Other Sql functions       *}
  {*****************************}

  Tisc_close = function(status_vector: PISC_STATUS; isc_arg2: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_declare = function(status_vector: PISC_STATUS; isc_arg2, isc_arg3: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_describe = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_describe_bind = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_execute = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_execute_immediate = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PSmallInt; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_fetch = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_open = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_prepare = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PByte; isc_arg5: PSmallInt; isc_arg6: PByte; isc_arg7: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {*************************************}
  {* Other Dynamic sql functions       *}
  {*************************************}

  Tisc_dsql_execute_m = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE; isc_arg4: Word; isc_arg5: PByte; isc_arg6: Word; isc_arg7: Word; isc_arg8: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute2_m = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE;
                                  isc_arg4: Word; isc_arg5: PByte; isc_arg6: Word; isc_arg7: Word; isc_arg8: PByte; isc_arg9: Word; isc_arg10: PByte;
                                  isc_arg11: Word; isc_arg12: Word; isc_arg13: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute_immediate_m = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
                                            isc_arg4: Word; isc_arg5: PByte; isc_arg6: Word; isc_arg7: Word; isc_arg8: PByte; isc_arg9: Word; isc_arg10: Word;
                                            isc_arg11: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_fetch_m = function(status_vector: PISC_STATUS; statement_handle: PISC_STMT_HANDLE; isc_arg3: Word; isc_arg4: PByte; isc_arg5: Word; isc_arg6: Word; isc_arg7: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_insert_m = function(status_vector: PISC_STATUS; statement_handle: PISC_STMT_HANDLE; isc_arg3: Word; isc_arg4: PByte; isc_arg5: Word; isc_arg6: Word; isc_arg7: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_prepare_m = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE; isc_arg4: Word; isc_arg5: PByte; isc_arg6: Word; isc_arg7: Word; isc_arg8: PByte; isc_arg9: Word; isc_arg10: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_release = function(status_vector: PISC_STATUS; isc_arg2: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_close = function(status_vector: PISC_STATUS; isc_arg2: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_declare = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_describe = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: Word; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_describe_bind = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: Word; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_execute = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: Word; isc_arg5: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_execute2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: Word; isc_arg5: PXSQLDA; isc_arg6: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_execute_immed = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: Word; isc_arg5: PByte; isc_arg6: Word; isc_arg7: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_fetch = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: Word; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_fetch_a = function(status_vector: PISC_STATUS; isc_arg2: PInteger; isc_arg3: PByte; isc_arg4: Word; isc_arg5: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_length = procedure(arg1: PByte; arg2: PISC_UShort); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_open = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: Word; isc_arg5: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_open2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: Word; isc_arg5: PXSQLDA; isc_arg6: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_insert = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: Word; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_prepare = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PByte; isc_arg5: Word; isc_arg6: PByte; isc_arg7: Word; isc_arg8: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_release = function(status_vector: PISC_STATUS; isc_arg2: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {******************************}
  {* Other Blob functions       *}
  {******************************}

  TBLOB_open = function(blob_handle: TISC_BLOB_HANDLE; isc_arg2: PByte; isc_arg3: Integer): PBSTREAM; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_put = function(isc_arg1: byte; isc_arg2: PBSTREAM): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_close = function(isc_arg1: PBSTREAM): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_get = function(isc_arg1: PBSTREAM): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_display = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_dump = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_edit = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_load = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_text_dump = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_text_load = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBopen = function(blob_id: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; mode: PByte): PBSTREAM; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  /////TBclose = function(Stream: PBSTREAM): PISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {******************************}
  {* Other Misc functions       *}
  {******************************}

  Tisc_ftof = function(isc_arg1: PByte; isc_arg2: Word; isc_arg3: PByte; isc_arg4: Word): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_print_blr = function(isc_arg1: PByte; isc_arg2: TISC_PRINT_CALLBACK; isc_arg3: PPointer; isc_arg4: SmallInt): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tfb_print_blr = function(isc_arg1: PByte; isc_arg2: ISC_ULONG; isc_arg3: TISC_PRINT_CALLBACK; isc_arg4: PPointer; isc_arg5: SmallInt): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}
  Tisc_set_debug = procedure(isc_arg1: Integer); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_qtoq = procedure(isc_arg1: PISC_QUAD; isc_arg2: PISC_QUAD); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_vtof = procedure(isc_arg1: PByte; isc_arg2: PByte; isc_arg3: Word); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_vtov = procedure(isc_arg1: PByte; isc_arg2: PByte; isc_arg3: SmallInt); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_version = function(db_handle: PISC_DB_HANDLE; isc_arg2: TISC_VERSION_CALLBACK; isc_arg3: PPointer): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  //Tisc_reset_fpe = function(isc_arg1: Word): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};deprecated;
  Tisc_baddress = function(isc_arg1: PByte): uintptr; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_baddress_s = procedure(isc_arg1: PByte; isc_arg2: uintptr); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {*****************************************}
  {* Service manager functions             *}
  {*****************************************}

type
  Tisc_service_attach = function(status_vector: PISC_STATUS; isc_arg2: Word; isc_arg3: PByte; service_handle: PISC_SVC_HANDLE; isc_arg5: Word; isc_arg6: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_service_detach = function(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_service_query = function(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE; isc_arg4: Word; isc_arg5: PByte; isc_arg6: Word; isc_arg7: PByte; isc_arg8: Word; isc_arg9: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_service_start = function(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE; isc_arg4: Word; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {***********************}
  {* Shutdown and cancel *}
  {***********************}

  Tfb_shutdown= function(arg1: Cardinal; arg2: Integer): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}
  Tfb_shutdown_callback = function(status_vector: PISC_STATUS; shutdown_callback: TFB_Shutdown_Callback_Proc; arg2: Integer; arg3: PPointer): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}
  Tfb_cancel_operation = function(status_vector: PISC_STATUS; db_handle: TISC_DB_HANDLE; arg2: ISC_USHORT): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}

  {***********************}
  {* Ping the connection *}
  {***********************}

  Tfb_ping = function(status_vector: PISC_STATUS; db_handle: TISC_DB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}

  {********************}
  {* Object interface *}
  {********************}

  Tfb_get_database_handle = function(status_vector: PISC_STATUS; db_handle: TISC_DB_HANDLE; arg3: PPointer): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}
  Tfb_get_transaction_handle = function(status_vector: PISC_STATUS; db_handle: TISC_DB_HANDLE; arg3: PPointer): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}
  Tfb_get_transaction_interface = function(status_vector: PISC_STATUS; arg2: PPointer; db_handle: Tisc_tr_handle): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}
  Tfb_get_statement_interface = function(status_vector: PISC_STATUS; arg2: PPointer; db_handle: Tisc_stmt_handle): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}

  {********************************}
  {* Client information functions *}
  {********************************}

  Tisc_get_client_version = procedure(buffer: PByte); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_get_client_major_version = function: Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_get_client_minor_version = function: Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {*******************************************}
  {* Set callback for database crypt plugins *}
  {*******************************************}

  Tfb_database_crypt_callback = function(status_vector: PISC_STATUS; arg2: PPointer): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; {new}

  {***************************************************}
  {* Actions to pass to the blob filter (ctl_source) *}
  {***************************************************}

const
  isc_blob_filter_open                     = 0;
  isc_blob_filter_get_segment              = 1;
  isc_blob_filter_close                    = 2;
  isc_blob_filter_create                   = 3;
  isc_blob_filter_put_segment              = 4;
  isc_blob_filter_alloc                    = 5;
  isc_blob_filter_free                     = 6;
  isc_blob_filter_seek                     = 7;

  {*******************}
  {* Blr definitions *}
  {*******************}

  {$i mncFBBlr.inc}

  {$i mncFBConsts.inc}

  {*********************************}
  {* Information call declarations *}
  {*********************************}

  {$i mncFBInf.inc}

  {$i mncFBError.inc}

