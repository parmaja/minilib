unit mncFBHeader;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Belal Hamed <belalhamed at gmail dot com>
 * @author    Zaher Dirkey zaherdirkey on internet
 * @comment   for Firebird 4
 *}

interface

uses
  Types, Classes, SysUtils, mnLibraries;

type
  Long = Longint;

  { C Date/Time Structure }
  TCTimeStructure = record
    tm_sec : integer;   { Seconds }
    tm_min : integer;   { Minutes }
    tm_hour : integer;  { Hour (0--23) }
    tm_mday : integer;  { Day of month (1--31) }
    tm_mon : integer;   { Month (0--11) }
    tm_year : integer;  { Year (calendar year minus 1900) }
    tm_wday : integer;  { Weekday (0--6) Sunday = 0) }
    tm_yday : integer;  { Day of year (0--365) }
    tm_isdst : integer; { 0 if daylight savings time is not in effect) }
  end;

  PCTimeStructure = ^TCTimeStructure;
  TM              = TCTimeStructure;
  PTM             = ^TM;

{$i mncIBase.inc}

type
  TStatusVector = array[0..19] of ISC_STATUS;
  PStatusVector = ^TStatusVector;

type

  { TmncFBLib }

  TmncFBLib = class(TmnLibrary)
  protected
    procedure Link; override;
  public
    BLOB_open: TBLOB_open;
    BLOB_put: TBLOB_put;
    BLOB_close: TBLOB_close;
    BLOB_get: TBLOB_get;
    Bopen: TBopen;
    //Bclose: TBclose;

    isc_sqlcode: Tisc_sqlcode;
    isc_sql_interprete: Tisc_sql_interprete;
    fb_interpret: Tfb_interpret;
    isc_interprete: Tisc_interprete; //deprecated;
    isc_vax_integer: Tisc_vax_integer;
    isc_portable_integer: Tisc_portable_integer;
    isc_blob_info: Tisc_blob_info;
    isc_open_blob2: Tisc_open_blob2;
    isc_close_blob: Tisc_close_blob;
    isc_get_segment: Tisc_get_segment;
    isc_put_segment: Tisc_put_segment;
    isc_create_blob2: Tisc_create_blob2;
    isc_array_gen_sdl: Tisc_array_gen_sdl;
    isc_array_get_slice: Tisc_array_get_slice;
    isc_array_lookup_bounds: Tisc_array_lookup_bounds;
    isc_array_lookup_desc: Tisc_array_lookup_desc;
    isc_array_set_desc: Tisc_array_set_desc;
    isc_array_put_slice: Tisc_array_put_slice;
    isc_blob_default_desc: Tisc_blob_default_desc;
    isc_blob_gen_bpb: Tisc_blob_gen_bpb;
    isc_blob_lookup_desc: Tisc_blob_lookup_desc;
    isc_blob_set_desc: Tisc_blob_set_desc;
    isc_cancel_blob: Tisc_cancel_blob;

    isc_version: Tisc_version;

    isc_service_attach: Tisc_service_attach;
    isc_service_detach: Tisc_service_detach;
    isc_service_query: Tisc_service_query;
    isc_service_start: Tisc_service_start;
    isc_decode_date: Tisc_decode_date;
    isc_decode_sql_date: Tisc_decode_sql_date;
    isc_decode_sql_time: Tisc_decode_sql_time;
    isc_decode_timestamp: Tisc_decode_timestamp;
    isc_encode_date: Tisc_encode_date;
    isc_encode_sql_date: Tisc_encode_sql_date;
    isc_encode_sql_time: Tisc_encode_sql_time;
    isc_encode_timestamp: Tisc_encode_timestamp;
    isc_dsql_free_statement: Tisc_dsql_free_statement;
    isc_dsql_execute2: Tisc_dsql_execute2;
    isc_dsql_execute: Tisc_dsql_execute;
    isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
    isc_dsql_fetch: Tisc_dsql_fetch;
    isc_dsql_sql_info: Tisc_dsql_sql_info;
    isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
    isc_dsql_prepare: Tisc_dsql_prepare;
    isc_dsql_describe_bind: Tisc_dsql_describe_bind;
    isc_dsql_describe: Tisc_dsql_describe;
    isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
    //isc_dsql_execute_immed2: Tisc_dsql_execute_immed2;
    isc_drop_database: Tisc_drop_database;
    isc_detach_database: Tisc_detach_database;
    isc_attach_database: Tisc_attach_database;
    isc_database_info: Tisc_database_info;
    isc_start_multiple: Tisc_start_multiple;
    isc_commit_transaction: Tisc_commit_transaction;
    isc_commit_retaining: Tisc_commit_retaining;
    isc_rollback_transaction: Tisc_rollback_transaction;
    isc_rollback_retaining: Tisc_rollback_retaining;
    isc_cancel_events: Tisc_cancel_events;
    isc_que_events: Tisc_que_events;
    isc_event_counts: Tisc_event_counts;
    isc_event_block: Tisc_event_block;
    isc_free: Tisc_free;
    isc_add_user: Tisc_add_user;
    isc_delete_user: Tisc_delete_user;
    isc_modify_user: Tisc_modify_user;
    isc_prepare_transaction: Tisc_prepare_transaction;
    isc_prepare_transaction2: Tisc_prepare_transaction2;

    isc_get_client_version: Tisc_get_client_version;
    isc_get_client_major_version: Tisc_get_client_major_version;
    isc_get_client_minor_version: Tisc_get_client_minor_version;

  end;

function XSQLDA_LENGTH(n: Long): Long;
procedure add_spb_length(var p: PByte; length: integer);
procedure add_spb_numeric(var p: PByte; data: integer);

var
  FBLib: TmncFBLib = nil;

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

procedure add_spb_length(var p: PByte; length: integer);
(*
#define ADD_SPB_LENGTH(p, length)	{*(p)++ = (length); \
          *(p)++ = (length) >> 8;}
*)
begin
  p^ := length;
  Inc(p);
  p^ := length shr 8;
  Inc(p);
end;

procedure add_spb_numeric(var p: PByte; data: integer);
(*
#define ADD_SPB_NUMERIC(p, data)	{*(p)++ = (data); \
          *(p)++ = (data) >> 8; \
      *(p)++ = (data) >> 16; \
      *(p)++ = (data) >> 24;}
*)
begin
  p^ := data;
  Inc(p);
  p^ := data shr 8;
  Inc(p);
  p^ := data shr 16;
  Inc(p);
  p^ := data shr 24;
  Inc(p);
end;

{ TmncFBLib }

procedure TmncFBLib.Link;
begin
  BLOB_open := GetAddress('BLOB_open');
  BLOB_get := GetAddress('BLOB_get');
  BLOB_put := GetAddress('BLOB_put');
  BLOB_close := GetAddress('BLOB_close');
  Bopen := GetAddress('Bopen');
  //Bclose := GetAddress('BLOB_close');

  isc_sqlcode := GetAddress('isc_sqlcode');
  isc_sql_interprete := GetAddress('isc_sql_interprete');
  fb_interpret := GetAddress('fb_interpret');
  //isc_interprete := GetAddress('isc_interprete');
  isc_vax_integer := GetAddress('isc_vax_integer');
  isc_portable_integer := GetAddress('isc_portable_integer');
  isc_blob_info := GetAddress('isc_blob_info');
  isc_open_blob2 := GetAddress('isc_open_blob2');
  isc_close_blob := GetAddress('isc_close_blob');
  isc_get_segment := GetAddress('isc_get_segment');
  isc_put_segment := GetAddress('isc_put_segment');
  isc_create_blob2 := GetAddress('isc_create_blob2');
  isc_cancel_blob := GetAddress('isc_cancel_blob');

  isc_version := GetAddress('isc_version');

  isc_array_gen_sdl := GetAddress('isc_array_gen_sdl');
  isc_array_get_slice := GetAddress('isc_array_get_slice');
  isc_array_lookup_bounds := GetAddress('isc_array_lookup_bounds');
  isc_array_lookup_desc := GetAddress('isc_array_lookup_desc');
  isc_array_set_desc := GetAddress('isc_array_set_desc');
  isc_array_put_slice := GetAddress('isc_array_put_slice');
  isc_blob_default_desc := GetAddress('isc_blob_default_desc');
  isc_blob_gen_bpb := GetAddress('isc_blob_gen_bpb');
  isc_blob_lookup_desc := GetAddress('isc_blob_lookup_desc');
  isc_blob_set_desc := GetAddress('isc_blob_set_desc');
  isc_decode_date := GetAddress('isc_decode_date');
  isc_encode_date := GetAddress('isc_encode_date');
  isc_dsql_free_statement := GetAddress('isc_dsql_free_statement');
  isc_dsql_execute2 := GetAddress('isc_dsql_execute2');
  isc_dsql_execute := GetAddress('isc_dsql_execute');
  isc_dsql_set_cursor_name := GetAddress('isc_dsql_set_cursor_name');
  isc_dsql_fetch := GetAddress('isc_dsql_fetch');
  isc_dsql_sql_info := GetAddress('isc_dsql_sql_info');
  isc_dsql_alloc_statement2 := GetAddress('isc_dsql_alloc_statement2');
  isc_dsql_prepare := GetAddress('isc_dsql_prepare');
  isc_dsql_describe_bind := GetAddress('isc_dsql_describe_bind');
  isc_dsql_describe := GetAddress('isc_dsql_describe');
  isc_dsql_execute_immediate := GetAddress('isc_dsql_execute_immediate');
  //isc_dsql_execute_immed2 := GetAddress('isc_dsql_exec_immed2');
  isc_drop_database := GetAddress('isc_drop_database');
  isc_detach_database := GetAddress('isc_detach_database');
  isc_attach_database := GetAddress('isc_attach_database', true);
  isc_database_info := GetAddress('isc_database_info');
  isc_start_multiple := GetAddress('isc_start_multiple');
  isc_commit_transaction := GetAddress('isc_commit_transaction');
  isc_commit_retaining := GetAddress('isc_commit_retaining');
  isc_rollback_transaction := GetAddress('isc_rollback_transaction');
  isc_cancel_events := GetAddress('isc_cancel_events');
  isc_que_events := GetAddress('isc_que_events');
  isc_event_counts := GetAddress('isc_event_counts');
  isc_event_block := GetAddress('isc_event_block');
  isc_free := GetAddress('isc_free');
  isc_add_user := GetAddress('isc_add_user');
  isc_delete_user := GetAddress('isc_delete_user');
  isc_modify_user := GetAddress('isc_modify_user');
  isc_prepare_transaction := GetAddress('isc_prepare_transaction');
  isc_prepare_transaction2 := GetAddress('isc_prepare_transaction2');

  isc_rollback_retaining := GetAddress('isc_rollback_retaining');
  isc_service_attach := GetAddress('isc_service_attach');
  isc_service_detach := GetAddress('isc_service_detach');
  isc_service_query := GetAddress('isc_service_query');
  isc_service_start := GetAddress('isc_service_start');
  isc_decode_sql_date := GetAddress('isc_decode_sql_date');
  isc_decode_sql_time := GetAddress('isc_decode_sql_time');
  isc_decode_timestamp := GetAddress('isc_decode_timestamp');
  isc_encode_sql_date := GetAddress('isc_encode_sql_date');
  isc_encode_sql_time := GetAddress('isc_encode_sql_time');
  isc_encode_timestamp := GetAddress('isc_encode_timestamp');
  isc_get_client_version := GetAddress('isc_get_client_version');
  isc_get_client_major_version := GetAddress('isc_get_client_major_version');
  isc_get_client_minor_version := GetAddress('isc_get_client_minor_version');
  //FClientVersion := isc_get_client_major_version + (isc_get_client_minor_version / 10);
end;

initialization
  FBLib := TmncFBLib.Create('fbclient');
finalization
  FreeAndNil(FBLib);
end.

