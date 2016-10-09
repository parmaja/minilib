unit mncFBClient;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{$DEFINE FREEFBCLIENT}

interface

uses
  SysUtils, Classes, dynlibs, mncFBHeader, mncFBTypes;

type
  EFBClientError = class(Exception)
  end;

  TCustomFBClient = class(TObject)
  private
    FLibrary: TLibHandle;
    FClientVersion: Single;
    FBLOB_get: TBLOB_get;
    FBLOB_put: TBLOB_put;
    FBopen: TBopen;
    FBclose: TBclose;

    Fisc_sqlcode: Tisc_sqlcode;
    Fisc_sql_interprete: Tisc_sql_interprete;
    Ffb_interpret: Tfb_interpret;
    Fisc_interprete: Tisc_interprete;
    Fisc_vax_integer: Tisc_vax_integer;
    Fisc_portable_integer: Tisc_portable_integer;
    Fisc_blob_info: Tisc_blob_info;
    Fisc_open_blob2: Tisc_open_blob2;
    Fisc_close_blob: Tisc_close_blob;
    Fisc_get_segment: Tisc_get_segment;
    Fisc_put_segment: Tisc_put_segment;
    Fisc_create_blob2: Tisc_create_blob2;
    Fisc_array_gen_sdl: Tisc_array_gen_sdl;
    Fisc_array_get_slice: Tisc_array_get_slice;
    Fisc_array_lookup_bounds: Tisc_array_lookup_bounds;
    Fisc_array_lookup_desc: Tisc_array_lookup_desc;
    Fisc_array_set_desc: Tisc_array_set_desc;
    Fisc_array_put_slice: Tisc_array_put_slice;
    Fisc_blob_default_desc: Tisc_blob_default_desc;
    Fisc_blob_gen_bpb: Tisc_blob_gen_bpb;
    Fisc_blob_lookup_desc: Tisc_blob_lookup_desc;
    Fisc_blob_set_desc: Tisc_blob_set_desc;
    Fisc_cancel_blob: Tisc_cancel_blob;

    Fisc_version:Tisc_version;

    Fisc_service_attach: Tisc_service_attach;
    Fisc_service_detach: Tisc_service_detach;
    Fisc_service_query: Tisc_service_query;
    Fisc_service_start: Tisc_service_start;
    Fisc_decode_date: Tisc_decode_date;
    Fisc_decode_sql_date: Tisc_decode_sql_date;
    Fisc_decode_sql_time: Tisc_decode_sql_time;
    Fisc_decode_timestamp: Tisc_decode_timestamp;
    Fisc_encode_date: Tisc_encode_date;
    Fisc_encode_sql_date: Tisc_encode_sql_date;
    Fisc_encode_sql_time: Tisc_encode_sql_time;
    Fisc_encode_timestamp: Tisc_encode_timestamp;
    Fisc_dsql_free_statement: Tisc_dsql_free_statement;
    Fisc_dsql_execute2: Tisc_dsql_execute2;
    Fisc_dsql_execute: Tisc_dsql_execute;
    Fisc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
    Fisc_dsql_fetch: Tisc_dsql_fetch;
    Fisc_dsql_sql_info: Tisc_dsql_sql_info;
    Fisc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
    Fisc_dsql_prepare: Tisc_dsql_prepare;
    Fisc_dsql_describe_bind: Tisc_dsql_describe_bind;
    Fisc_dsql_describe: Tisc_dsql_describe;
    Fisc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
    Fisc_drop_database: Tisc_drop_database;
    Fisc_detach_database: Tisc_detach_database;
    Fisc_attach_database: Tisc_attach_database;
    Fisc_database_info: Tisc_database_info;
    Fisc_start_multiple: Tisc_start_multiple;
    Fisc_commit_transaction: Tisc_commit_transaction;
    Fisc_commit_retaining: Tisc_commit_retaining;
    Fisc_rollback_transaction: Tisc_rollback_transaction;
    Fisc_rollback_retaining: Tisc_rollback_retaining;
    Fisc_cancel_events: Tisc_cancel_events;
    Fisc_que_events: Tisc_que_events;
    Fisc_event_counts: Tisc_event_counts;
    Fisc_event_block: Tisc_event_block;
    Fisc_free: Tisc_free;
    Fisc_add_user: Tisc_add_user;
    Fisc_delete_user: Tisc_delete_user;
    Fisc_modify_user: Tisc_modify_user;
    Fisc_prepare_transaction: Tisc_prepare_transaction;
    Fisc_prepare_transaction2: Tisc_prepare_transaction2;

    Fisc_get_client_version: Tisc_get_client_version;
    Fisc_get_client_major_version: Tisc_get_client_major_version;
    Fisc_get_client_minor_version: Tisc_get_client_minor_version;
    FInstancePath: string;

    FName: string;
  protected
    procedure LoadClientLibrary;
    procedure FreeClientLibrary;
    function LoadClient: Boolean;
    procedure CheckLoaded;
    function GetIsEmbed: Boolean; virtual; abstract;
    function GetInstanceName: string; virtual; abstract;
    function GetClientName: string; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function isc_attach_database(status_vector: PISC_STATUS; db_name_length: Short;
      db_name: PAnsiChar; db_handle: PISC_DB_HANDLE;
      parm_buffer_length: Short; parm_buffer: PAnsiChar): ISC_STATUS;
    function isc_array_get_slice(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
      descriptor: PISC_ARRAY_DESC; dest_array: PVoid;
      slice_length: ISC_LONG): ISC_STATUS;
    function isc_array_lookup_bounds(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      trans_handle: PISC_TR_HANDLE; table_name, column_name: PAnsiChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_lookup_desc(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      trans_handle: PISC_TR_HANDLE; table_name, column_name: PAnsiChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_set_desc(status_vector: PISC_STATUS; table_name: PAnsiChar;
      column_name: PAnsiChar; sql_dtype, sql_length, sql_dimensions: PShort;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_put_slice(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
      descriptor: PISC_ARRAY_DESC; source_array: PVoid;
      slice_length: PISC_LONG): ISC_STATUS;
    procedure isc_blob_default_desc(descriptor: PISC_BLOB_DESC; table_name: PUChar;
      column_name: PUChar);
    function isc_blob_gen_bpb(status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC;
      bpb_buffer_length: UShort; bpb_buffer: PUChar;
      bpb_length: PUShort): ISC_STATUS;
    function isc_blob_info(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
      item_list_buffer_length: Short; item_list_buffer: PAnsiChar;
      result_buffer_length: Short; result_buffer: PAnsiChar): ISC_STATUS;
    function isc_blob_lookup_desc(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      trans_handle: PISC_TR_HANDLE; table_name, column_name: PAnsiChar;
      descriptor: PISC_BLOB_DESC; global: PUChar): ISC_STATUS;
    function isc_blob_set_desc(status_vector: PISC_STATUS; table_name, column_name: PAnsiChar;
      subtype, charset, segment_size: Short; descriptor: PISC_BLOB_DESC): ISC_STATUS;
    function isc_cancel_blob(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_cancel_events(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      event_id: PISC_LONG): ISC_STATUS;
    function isc_close_blob(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_commit_retaining(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_transaction(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_create_blob2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE;
      blob_id: PISC_QUAD; bpb_length: Short; bpb_address: PAnsiChar): ISC_STATUS;
    function isc_database_info(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      item_list_buffer_length: Short; item_list_buffer: PAnsiChar;
      result_buffer_length: Short; result_buffer: PAnsiChar): ISC_STATUS;
    procedure isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    procedure isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
    procedure isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
    procedure isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure);
    function isc_detach_database(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_drop_database(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_dsql_alloc_statement2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_describe(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
      dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_describe_bind(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
      dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
      stmt_handle: PISC_STMT_HANDLE; dialect: UShort;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute2(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
      stmt_handle: PISC_STMT_HANDLE; dialect: UShort;
      in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute_immediate(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      tran_handle: PISC_TR_HANDLE; length: UShort;
      statement: PAnsiChar; dialect: UShort;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_fetch(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
      dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_free_statement(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
      options: UShort): ISC_STATUS;
    function isc_dsql_prepare(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
      stmt_handle: PISC_STMT_HANDLE; length: UShort;
      statement: PAnsiChar; dialect: UShort;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_set_cursor_name(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
      cursor_name: PAnsiChar; _type: UShort): ISC_STATUS;
    function isc_dsql_sql_info(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
      item_length: Short; items: PAnsiChar; buffer_length: Short;
      buffer: PAnsiChar): ISC_STATUS;
    procedure isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    procedure isc_encode_sql_date(tm_date: PCTimeStructure; ib_date: PISC_DATE);
    procedure isc_encode_sql_time(tm_date: PCTimeStructure; ib_time: PISC_TIME);
    procedure isc_encode_timestamp(tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP);
    function isc_event_block(event_buffer: PPAnsiChar; result_buffer: PPAnsiChar;
      id_count: UShort; event_list: array of PAnsiChar): ISC_LONG;
    procedure isc_event_counts(status_vector: PISC_STATUS; buffer_length: Short;
      event_buffer: PAnsiChar; result_buffer: PAnsiChar);
    function isc_free(isc_arg1: PAnsiChar): ISC_LONG;
    function isc_get_segment(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
      actual_seg_length: PUShort; seg_buffer_length: UShort;
      seg_buffer: PAnsiChar): ISC_STATUS;
    function fb_interpret(buffer: PAnsiChar; length:short; var status_vector: PISC_STATUS): ISC_STATUS;
    function isc_interprete(buffer: PAnsiChar; var status_vector: PISC_STATUS): ISC_STATUS; deprecated;
    function isc_open_blob2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE;
      blob_id: PISC_QUAD; bpb_length: Short; bpb_buffer: PAnsiChar): ISC_STATUS;
    function isc_prepare_transaction2(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
      msg_length: Short; msg: PAnsiChar): ISC_STATUS;
    function isc_put_segment(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
      seg_buffer_len: UShort; seg_buffer: PAnsiChar): ISC_STATUS;
    function isc_que_events(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
      event_id: PISC_LONG; length: Short; event_buffer: PAnsiChar;
      event_function: TISC_EVENT_CALLBACK; event_function_arg: PVoid): ISC_STATUS;
    function isc_rollback_retaining(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_rollback_transaction(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_start_multiple(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS;
    function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
    procedure isc_sql_interprete(sqlcode: Short; buffer: PAnsiChar; buffer_length: Short);
    function isc_vax_integer(buffer: PAnsiChar; length: Short): ISC_LONG;
    function isc_portable_integer(buffer: PAnsiChar; length: Short): ISC_INT64;
    // Security Functions
    function isc_add_user(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS;
    function isc_delete_user(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS;
    function isc_modify_user(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS;
    //
    procedure isc_version(db_handle: PISC_DB_HANDLE; CallbackProc: TISC_VERSION_CALLBACK; isc_arg3: Pointer);
    // Other OSRI functions
    function isc_prepare_transaction(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;

   // Service manager functions
    function isc_service_attach(status_vector: PISC_STATUS;
      isc_arg2: UShort;
      isc_arg3: PAnsiChar;
      service_handle: PISC_SVC_HANDLE;
      isc_arg5: UShort;
      isc_arg6: PAnsiChar): ISC_STATUS;
    function isc_service_detach(status_vector: PISC_STATUS;
      service_handle: PISC_SVC_HANDLE): ISC_STATUS;
    function isc_service_query(status_vector: PISC_STATUS;
      service_handle: PISC_SVC_HANDLE;
      recv_handle: PISC_SVC_HANDLE;
      isc_arg4: UShort;
      isc_arg5: PAnsiChar;
      isc_arg6: UShort;
      isc_arg7: PAnsiChar;
      isc_arg8: UShort;
      isc_arg9: PAnsiChar): ISC_STATUS;
    function isc_service_start(status_vector: PISC_STATUS;
      service_handle: PISC_SVC_HANDLE;
      recv_handle: PISC_SVC_HANDLE;
      isc_arg4: UShort;
      isc_arg5: PAnsiChar): ISC_STATUS;
    // Client information functions
    procedure isc_get_client_version(buffer: PAnsiChar);
    function isc_get_client_major_version: Integer;
    function isc_get_client_minor_version: Integer;
   // Other Blob functions
//   http://www.ibphoenix.com/main.nfs?a=ibphoenix&page=ibp_stream_blob
    function Bopen(blob_id: PISC_QUAD; db_handle: TISC_DB_HANDLE; trans_handle: TISC_TR_HANDLE; mode:PAnsiChar):PBSTREAM;
    function Bclose(Stream: PBSTREAM): PISC_STATUS;

    function BLOB_put(isc_arg1: char; isc_arg2: PBSTREAM): Int;
    function BLOB_get(isc_arg1: PBSTREAM): Int;
   //utils

    property Name:string read FName;
    property ClientName: string read GetClientName;
    property ClientVersion: Single read FClientVersion;
    property InstanceName: string read GetInstanceName;
    property InstancePath: string read FInstancePath;
    property IsEmbed: Boolean read GetIsEmbed;
  end;

  TFBClientClass = class of TCustomFBClient;

  TFBClient = class(TCustomFBClient)
  private
  protected
    function GetClientName: string; override;
    function GetInstanceName: string; override;
    function GetIsEmbed: Boolean; override;
  public
    constructor Create;
  end;

  { TFBEmbed }

  TFBEmbed = class(TCustomFBClient)
  private
    FClientName: string;
  protected
    function GetClientName: string; override;
    function GetInstanceName: string; override;
    function GetIsEmbed: Boolean; override;
  public
    constructor Create(AClientName: string);
  end;

procedure SetFBClient(NewFBClient: TCustomFBClient);
function IsFBClientSet: Boolean;
function FBClient: TCustomFBClient;

procedure InitClientMode;
procedure InitEmbedMode(ClientName: string = '');

{ Library Initialization }
//let the system kill the library

implementation

uses
  Registry;

var
  FFBClient: TCustomFBClient = nil;

procedure SetFBClient(NewFBClient: TCustomFBClient);
begin
  if Assigned(FFBClient) then
    raise EFBClientError.Create('FBClient class is already registerd');
  FFBClient := NewFBClient;
  FFBClient.CheckLoaded;
end;

function IsFBClientSet: Boolean;
begin
  Result := Assigned(FFBClient);
end;

procedure InitClientMode;
begin
  SetFBClient(TFBClient.Create);
end;

procedure InitEmbedMode(ClientName: string);
begin
  SetFBClient(TFBEmbed.Create(ClientName));
end;

function FBClient: TCustomFBClient;
begin
  if not Assigned(FFBClient) then
    InitClientMode;
  Result := FFBClient;
end;

procedure TCustomFBClient.LoadClientLibrary;
var
  CurLibrary: THandle;

  function TryGetProcAddr(ProcName: PAnsiChar): Pointer;
  begin
    Result := GetProcAddress(CurLibrary, ProcName);
  end;

  function GetProcAddr(ProcName: PAnsiChar): Pointer;
  begin
    Result := GetProcAddress(CurLibrary, ProcName);
    if not Assigned(Result) then
      RaiseLastOSError;
  end;
var
  OldDir: string;
begin
  if (FInstancePath = '') or not FileExists(FInstancePath + 'bin\' + GetClientName) then
    FLibrary := LoadLibrary(PAnsiChar(GetClientName))
  else
  begin
    OldDir := GetCurrentDir;
    SetCurrentDir(FInstancePath + 'bin\');

    try
      FLibrary := LoadLibrary(PAnsiChar(FInstancePath + 'bin\' + GetClientName));
    finally
      SetCurrentDir(OldDir);
    end;
  end;

  if (FLibrary <> 0) then
  begin
    CurLibrary := FLibrary;
    
    FBLOB_get := GetProcAddr('BLOB_get');
    FBLOB_put := GetProcAddr('BLOB_put');
    FBopen := GetProcAddr('Bopen');
    FBclose := GetProcAddr('BLOB_close');

    Fisc_sqlcode := GetProcAddr('isc_sqlcode');
    Fisc_sql_interprete := GetProcAddr('isc_sql_interprete');
    Ffb_interpret := GetProcAddr('fb_interpret');
    Fisc_interprete := GetProcAddr('isc_interprete');
    Fisc_vax_integer := GetProcAddr('isc_vax_integer');
    Fisc_portable_integer := GetProcAddr('isc_portable_integer');
    Fisc_blob_info := GetProcAddr('isc_blob_info');
    Fisc_open_blob2 := GetProcAddr('isc_open_blob2');
    Fisc_close_blob := GetProcAddr('isc_close_blob');
    Fisc_get_segment := GetProcAddr('isc_get_segment');
    Fisc_put_segment := GetProcAddr('isc_put_segment');
    Fisc_create_blob2 := GetProcAddr('isc_create_blob2');
    Fisc_cancel_blob := GetProcAddr('isc_cancel_blob');

    Fisc_version := GetProcAddr('isc_version');

    Fisc_array_gen_sdl := GetProcAddr('isc_array_gen_sdl');
    Fisc_array_get_slice := GetProcAddr('isc_array_get_slice');
    Fisc_array_lookup_bounds := GetProcAddr('isc_array_lookup_bounds');
    Fisc_array_lookup_desc := GetProcAddr('isc_array_lookup_desc');
    Fisc_array_set_desc := GetProcAddr('isc_array_set_desc');
    Fisc_array_put_slice := GetProcAddr('isc_array_put_slice');
    Fisc_blob_default_desc := GetProcAddr('isc_blob_default_desc');
    Fisc_blob_gen_bpb := GetProcAddr('isc_blob_gen_bpb');
    Fisc_blob_lookup_desc := GetProcAddr('isc_blob_lookup_desc');
    Fisc_blob_set_desc := GetProcAddr('isc_blob_set_desc');
    Fisc_decode_date := GetProcAddr('isc_decode_date');
    Fisc_encode_date := GetProcAddr('isc_encode_date');
    Fisc_dsql_free_statement := GetProcAddr('isc_dsql_free_statement');
    Fisc_dsql_execute2 := GetProcAddr('isc_dsql_execute2');
    Fisc_dsql_execute := GetProcAddr('isc_dsql_execute');
    Fisc_dsql_set_cursor_name := GetProcAddr('isc_dsql_set_cursor_name');
    Fisc_dsql_fetch := GetProcAddr('isc_dsql_fetch');
    Fisc_dsql_sql_info := GetProcAddr('isc_dsql_sql_info');
    Fisc_dsql_alloc_statement2 := GetProcAddr('isc_dsql_alloc_statement2');
    Fisc_dsql_prepare := GetProcAddr('isc_dsql_prepare');
    Fisc_dsql_describe_bind := GetProcAddr('isc_dsql_describe_bind');
    Fisc_dsql_describe := GetProcAddr('isc_dsql_describe');
    Fisc_dsql_execute_immediate := GetProcAddr('isc_dsql_execute_immediate');
    Fisc_drop_database := GetProcAddr('isc_drop_database');
    Fisc_detach_database := GetProcAddr('isc_detach_database');
    Fisc_attach_database := GetProcAddr('isc_attach_database');
    Fisc_database_info := GetProcAddr('isc_database_info');
    Fisc_start_multiple := GetProcAddr('isc_start_multiple');
    Fisc_commit_transaction := GetProcAddr('isc_commit_transaction');
    Fisc_commit_retaining := GetProcAddr('isc_commit_retaining');
    Fisc_rollback_transaction := GetProcAddr('isc_rollback_transaction');
    Fisc_cancel_events := GetProcAddr('isc_cancel_events');
    Fisc_que_events := GetProcAddr('isc_que_events');
    Fisc_event_counts := GetProcAddr('isc_event_counts');
    Fisc_event_block := GetProcAddr('isc_event_block');
    Fisc_free := GetProcAddr('isc_free');
    Fisc_add_user := GetProcAddr('isc_add_user');
    Fisc_delete_user := GetProcAddr('isc_delete_user');
    Fisc_modify_user := GetProcAddr('isc_modify_user');
    Fisc_prepare_transaction := GetProcAddr('isc_prepare_transaction');
    Fisc_prepare_transaction2 := GetProcAddr('isc_prepare_transaction2');

    Fisc_rollback_retaining := GetProcAddr('isc_rollback_retaining');
    Fisc_service_attach := GetProcAddr('isc_service_attach');
    Fisc_service_detach := GetProcAddr('isc_service_detach');
    Fisc_service_query := GetProcAddr('isc_service_query');
    Fisc_service_start := GetProcAddr('isc_service_start');
    Fisc_decode_sql_date := GetProcAddr('isc_decode_sql_date');
    Fisc_decode_sql_time := GetProcAddr('isc_decode_sql_time');
    Fisc_decode_timestamp := GetProcAddr('isc_decode_timestamp');
    Fisc_encode_sql_date := GetProcAddr('isc_encode_sql_date');
    Fisc_encode_sql_time := GetProcAddr('isc_encode_sql_time');
    Fisc_encode_timestamp := GetProcAddr('isc_encode_timestamp');
    Fisc_get_client_version := GetProcAddr('isc_get_client_version');
    Fisc_get_client_major_version := GetProcAddr('isc_get_client_major_version');
    Fisc_get_client_minor_version := GetProcAddr('isc_get_client_minor_version');
    FClientVersion := isc_get_client_major_version + (isc_get_client_minor_version / 10);
  end;
end;

procedure TCustomFBClient.FreeClientLibrary;
begin
  if FLibrary <> 0 then
  begin
    FreeLibrary(FLibrary);
    FLibrary := 0;
  end;
end;

function TCustomFBClient.LoadClient: Boolean;
begin
  if (FLibrary <> 0) then
    LoadClientLibrary;
  if (FLibrary <> 0) then
    Result := False
  else
    Result := True;
end;

procedure TCustomFBClient.CheckLoaded;
begin
  if not LoadClient then
    raise EFBClientError.Create('Firebird library fbclient.dll/fbembed not found in the path. Please install Firebird to use this functionality');
end;

{ TCustomFBClient }

function TCustomFBClient.BLOB_put(isc_arg1: char; isc_arg2: PBSTREAM): Int;
begin
  Result := FBLOB_put(isc_arg1, isc_arg2);
end;

function TCustomFBClient.isc_add_user(status_vector: PISC_STATUS;
  user_sec_data: PUserSecData): ISC_STATUS;
begin
  Result := Fisc_add_user(status_vector, user_sec_data);
end;

function TCustomFBClient.isc_array_get_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
  array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; dest_array: PVoid;
  slice_length: ISC_LONG): ISC_STATUS;
begin
  Result := Fisc_array_get_slice(status_vector, db_handle, trans_handle,
    array_id, descriptor, dest_array, slice_length);
end;

function TCustomFBClient.isc_array_lookup_bounds(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PAnsiChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := Fisc_array_lookup_bounds(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TCustomFBClient.isc_array_lookup_desc(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name,
  column_name: PAnsiChar; descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := Fisc_array_lookup_desc(status_vector, db_handle, trans_handle,
    table_name, column_name, descriptor);
end;

function TCustomFBClient.isc_array_put_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
  array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; source_array: PVoid;
  slice_length: PISC_LONG): ISC_STATUS;
begin
  Result := Fisc_array_put_slice(status_vector, db_handle, trans_handle,
    array_id, descriptor, source_array, slice_length);
end;

function TCustomFBClient.isc_array_set_desc(status_vector: PISC_STATUS;
  table_name, column_name: PAnsiChar; sql_dtype, sql_length,
  sql_dimensions: PShort; descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := Fisc_array_set_desc(status_vector, table_name, column_name,
    sql_dtype, sql_length, sql_dimensions, descriptor);
end;

function TCustomFBClient.isc_attach_database(status_vector: PISC_STATUS;
  db_name_length: Short; db_name: PAnsiChar; db_handle: PISC_DB_HANDLE;
  parm_buffer_length: Short; parm_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_attach_database(status_vector, db_name_length, db_name,
    db_handle, parm_buffer_length, parm_buffer);
end;

procedure TCustomFBClient.isc_blob_default_desc(descriptor: PISC_BLOB_DESC;
  table_name, column_name: PUChar);
begin
  Fisc_blob_default_desc(descriptor, table_name, column_name);
end;

function TCustomFBClient.isc_blob_gen_bpb(status_vector: PISC_STATUS;
  to_descriptor, from_descriptor: PISC_BLOB_DESC;
  bpb_buffer_length: UShort; bpb_buffer: PUChar;
  bpb_length: PUShort): ISC_STATUS;
begin
  Result := Fisc_blob_gen_bpb(status_vector, to_descriptor, from_descriptor,
    bpb_buffer_length, bpb_buffer, bpb_length);
end;

function TCustomFBClient.isc_blob_info(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PAnsiChar; result_buffer_length: Short;
  result_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_blob_info(status_vector, blob_handle, item_list_buffer_length,
    item_list_buffer, result_buffer_length, result_buffer);
end;

function TCustomFBClient.isc_blob_lookup_desc(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name,
  column_name: PAnsiChar; descriptor: PISC_BLOB_DESC;
  global: PUChar): ISC_STATUS;
begin
  Result := Fisc_blob_lookup_desc(status_vector, db_handle, trans_handle,
    table_name, column_name, descriptor, global);
end;

function TCustomFBClient.isc_blob_set_desc(status_vector: PISC_STATUS;
  table_name, column_name: PAnsiChar; subtype, charset, segment_size: Short;
  descriptor: PISC_BLOB_DESC): ISC_STATUS;
begin
  Result := Fisc_blob_set_desc(status_vector, table_name, column_name, subtype,
    charset, segment_size, descriptor);
end;

function TCustomFBClient.isc_cancel_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := Fisc_cancel_blob(status_vector, blob_handle);
end;

function TCustomFBClient.isc_cancel_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
begin
  Result := Fisc_cancel_events(status_vector, db_handle, event_id);
end;

function TCustomFBClient.isc_close_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := Fisc_close_blob(status_vector, blob_handle);
end;

function TCustomFBClient.isc_commit_retaining(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_commit_retaining(status_vector, tran_handle);
end;

function TCustomFBClient.isc_commit_transaction(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_commit_transaction(status_vector, tran_handle);
end;

function TCustomFBClient.isc_create_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_address: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_create_blob2(status_vector, db_handle, tran_handle,
    blob_handle, blob_id, bpb_length, bpb_address);
end;

function TCustomFBClient.isc_database_info(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PAnsiChar; result_buffer_length: Short;
  result_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_database_info(status_vector, db_handle, item_list_buffer_length,
    item_list_buffer, result_buffer_length, result_buffer);
end;

procedure TCustomFBClient.isc_decode_date(ib_date: PISC_QUAD;
  tm_date: PCTimeStructure);
begin
  Fisc_decode_date(ib_date, tm_date);
end;

procedure TCustomFBClient.isc_decode_sql_date(ib_date: PISC_DATE;
  tm_date: PCTimeStructure);
begin
  Fisc_decode_sql_date(ib_date, tm_date);
end;

procedure TCustomFBClient.isc_decode_sql_time(ib_time: PISC_TIME;
  tm_date: PCTimeStructure);
begin
  Fisc_decode_sql_time(ib_time, tm_date);
end;

procedure TCustomFBClient.isc_decode_timestamp(
  ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure);
begin
  Fisc_decode_timestamp(ib_timestamp, tm_date);
end;

function TCustomFBClient.isc_delete_user(status_vector: PISC_STATUS;
  user_sec_data: PUserSecData): ISC_STATUS;
begin
  Result := Fisc_delete_user(status_vector, user_sec_data);
end;

function TCustomFBClient.isc_detach_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := Fisc_detach_database(status_vector, db_handle);
end;

function TCustomFBClient.isc_drop_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := Fisc_drop_database(status_vector, db_handle);
end;

function TCustomFBClient.isc_dsql_alloc_statement2(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := Fisc_dsql_alloc_statement2(status_vector, db_handle, stmt_handle);
end;

function TCustomFBClient.isc_dsql_describe(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: UShort;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_describe(status_vector, stmt_handle, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_describe_bind(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: UShort;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_describe_bind(status_vector, stmt_handle, dialect,
    xsqlda);
end;

function TCustomFBClient.isc_dsql_execute(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
  dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_execute(status_vector, tran_handle, stmt_handle,
    dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_execute_immediate(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  tran_handle: PISC_TR_HANDLE; length: UShort; statement: PAnsiChar;
  dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_execute_immediate(status_vector, db_handle,
    tran_handle, length, statement, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_execute2(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
  dialect: UShort; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_execute2(status_vector, tran_handle, stmt_handle,
    dialect, in_xsqlda, out_xsqlda);
end;

function TCustomFBClient.isc_dsql_fetch(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: UShort;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_fetch(status_vector, stmt_handle, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_free_statement(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  options: UShort): ISC_STATUS;
begin
  Result := Fisc_dsql_free_statement(status_vector, stmt_handle, options);
end;

function TCustomFBClient.isc_dsql_prepare(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
  length: UShort; statement: PAnsiChar; dialect: UShort;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_prepare(status_vector, tran_handle, stmt_handle,
    length, statement, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_set_cursor_name(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  cursor_name: PAnsiChar; _type: UShort): ISC_STATUS;
begin
  Result := Fisc_dsql_set_cursor_name(status_vector, stmt_handle, cursor_name,
    _type);
end;

function TCustomFBClient.isc_dsql_sql_info(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PAnsiChar;
  buffer_length: Short; buffer: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_dsql_sql_info(status_vector, stmt_handle, item_length, items,
    buffer_length, buffer);
end;

procedure TCustomFBClient.isc_encode_date(tm_date: PCTimeStructure;
  ib_date: PISC_QUAD);
begin
  Fisc_encode_date(tm_date, ib_date);
end;

procedure TCustomFBClient.isc_encode_sql_date(tm_date: PCTimeStructure;
  ib_date: PISC_DATE);
begin
  Fisc_encode_sql_date(tm_date, ib_date);
end;

procedure TCustomFBClient.isc_encode_sql_time(tm_date: PCTimeStructure;
  ib_time: PISC_TIME);
begin
  Fisc_encode_sql_time(tm_date, ib_time);
end;

procedure TCustomFBClient.isc_encode_timestamp(tm_date: PCTimeStructure;
  ib_timestamp: PISC_TIMESTAMP);
begin
  Fisc_encode_timestamp(tm_date, ib_timestamp);
end;

type
  Tsib_event_block = function(EventBuffer, ResultBuffer: PPAnsiChar; IDCount: UShort;
    Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event9,
    Event10, Event11, Event12, Event13, Event14, Event15: PAnsiChar): ISC_LONG; cdecl;

function TCustomFBClient.isc_event_block(event_buffer,
  result_buffer: PPAnsiChar; id_count: UShort;
  event_list: array of PAnsiChar): ISC_LONG;
begin
  Result := Tsib_event_block(Fisc_event_block)(event_buffer, result_buffer,
    id_count, event_list[0], event_list[1], event_list[2], event_list[3],
    event_list[4], event_list[5], event_list[6], event_list[7], event_list[8],
    event_list[9], event_list[10], event_list[11], event_list[12],
    event_list[13], event_list[14]);
end;

procedure TCustomFBClient.isc_event_counts(status_vector: PISC_STATUS;
  buffer_length: Short; event_buffer, result_buffer: PAnsiChar);
begin
  Fisc_event_counts(status_vector, buffer_length, event_buffer, result_buffer);
end;

function TCustomFBClient.isc_free(isc_arg1: PAnsiChar): ISC_LONG;
begin
  Result := Fisc_free(isc_arg1);
end;

function TCustomFBClient.isc_get_client_major_version: Integer;
begin
  Result := Fisc_get_client_major_version
end;

function TCustomFBClient.isc_get_client_minor_version: Integer;
begin
  Result := Fisc_get_client_minor_version
end;

procedure TCustomFBClient.isc_get_client_version(buffer: PAnsiChar);
begin
  Fisc_get_client_version(buffer);
end;

function TCustomFBClient.isc_get_segment(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PUShort;
  seg_buffer_length: UShort; seg_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_get_segment(status_vector, blob_handle, actual_seg_length,
    seg_buffer_length, seg_buffer);
end;

function TCustomFBClient.isc_interprete(buffer: PAnsiChar;
  var status_vector: PISC_STATUS): ISC_STATUS;
begin
  Result := Fisc_interprete(buffer, @status_vector);
end;

function TCustomFBClient.fb_interpret(buffer: PAnsiChar; length:short; var status_vector: PISC_STATUS): ISC_STATUS;
begin
  Result := Ffb_interpret(buffer, length, @status_vector);
end;

function TCustomFBClient.isc_modify_user(status_vector: PISC_STATUS;
  user_sec_data: PUserSecData): ISC_STATUS;
begin
  Result := Fisc_modify_user(status_vector, user_sec_data);
end;

function TCustomFBClient.isc_open_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_open_blob2(status_vector, db_handle, tran_handle,
    blob_handle, blob_id, bpb_length, bpb_buffer);
end;

function TCustomFBClient.isc_prepare_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_prepare_transaction(status_vector, tran_handle);
end;

function TCustomFBClient.isc_prepare_transaction2(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  msg_length: Short; msg: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_prepare_transaction2(status_vector, tran_handle, msg_length, msg);
end;

function TCustomFBClient.isc_put_segment(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: UShort;
  seg_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_put_segment(status_vector, blob_handle, seg_buffer_len, seg_buffer);
end;

function TCustomFBClient.isc_que_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
  event_buffer: PAnsiChar; event_function: TISC_EVENT_CALLBACK;
  event_function_arg: PVoid): ISC_STATUS;
begin
  Result := Fisc_que_events(status_vector, db_handle, event_id, length, event_buffer, event_function, event_function_arg);
end;

function TCustomFBClient.isc_rollback_retaining(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_rollback_retaining(status_vector, tran_handle);
end;

function TCustomFBClient.isc_rollback_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_rollback_transaction(status_vector, tran_handle);
end;

function TCustomFBClient.isc_service_attach(status_vector: PISC_STATUS;
  isc_arg2: UShort; isc_arg3: PAnsiChar; service_handle: PISC_SVC_HANDLE;
  isc_arg5: UShort; isc_arg6: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_service_attach(status_vector, isc_arg2, isc_arg3,
    service_handle, isc_arg5, isc_arg6);
end;

function TCustomFBClient.isc_service_detach(status_vector: PISC_STATUS;
  service_handle: PISC_SVC_HANDLE): ISC_STATUS;
begin
  Result := Fisc_service_detach(status_vector, service_handle);
end;

function TCustomFBClient.isc_service_query(status_vector: PISC_STATUS;
  service_handle, recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort;
  isc_arg5: PAnsiChar; isc_arg6: UShort; isc_arg7: PAnsiChar; isc_arg8: UShort;
  isc_arg9: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_service_query(status_vector, service_handle, recv_handle,
    isc_arg4, isc_arg5, isc_arg6, isc_arg7, isc_arg8, isc_arg9);
end;

function TCustomFBClient.isc_service_start(status_vector: PISC_STATUS;
  service_handle, recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort;
  isc_arg5: PAnsiChar): ISC_STATUS;
begin
  Result := Fisc_service_start(status_vector, service_handle, recv_handle,
    isc_arg4, isc_arg5);
end;

procedure TCustomFBClient.isc_sql_interprete(sqlcode: Short; buffer: PAnsiChar;
  buffer_length: Short);
begin
  Fisc_sql_interprete(sqlcode, buffer, buffer_length);
end;

function TCustomFBClient.isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
begin
  Result := Fisc_sqlcode(status_vector);
end;

function TCustomFBClient.isc_start_multiple(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
  teb_vector_address: PISC_TEB): ISC_STATUS;
begin
  Result := Fisc_start_multiple(status_vector, tran_handle, db_handle_count,
    teb_vector_address);
end;

function TCustomFBClient.isc_vax_integer(buffer: PAnsiChar; length: Short): ISC_LONG;
begin
  Result := Fisc_vax_integer(buffer, length);
end;

function TCustomFBClient.BLOB_get(isc_arg1: PBSTREAM): Int;
begin
  Result := FBLOB_get(isc_arg1);
end;

destructor TCustomFBClient.Destroy;
begin
  FFBClient := nil;
  inherited;
end;

constructor TFBClient.Create;
{$ifdef WINDOWS}
var
  Reg: TRegistry;
{$endif}
begin
  inherited;
  {$ifdef WINDOWS}
  Reg := TRegistry.Create;
  try
    Reg.Access := KEY_READ;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(REGSTR_PATH_FB + '\Instances', False) and Reg.ValueExists(GetInstanceName) then
      FInstancePath := IncludeTrailingPathDelimiter(Reg.ReadString(GetInstanceName))
    else
      FInstancePath := '';
    finally
      Reg.Free;
    end;
  {$endif}
end;

function TFBClient.GetInstanceName: string;
begin
  Result := FB_DefaultInstance;
end;

function TFBClient.GetClientName: string;
begin
  Result := FBClient_LIB;
end;

constructor TCustomFBClient.Create;
begin
  inherited;
end;

function TCustomFBClient.Bopen(blob_id: PISC_QUAD; db_handle: TISC_DB_HANDLE; trans_handle: TISC_TR_HANDLE; mode: PAnsiChar): PBSTREAM;
begin
  Result := FBopen(blob_id, db_handle, trans_handle, mode);
end;

function TCustomFBClient.Bclose(Stream: PBSTREAM): PISC_STATUS;
begin
  Result := FBclose(Stream);
end;

procedure TCustomFBClient.isc_version(db_handle: PISC_DB_HANDLE;
  CallbackProc: TISC_VERSION_CALLBACK; isc_arg3: Pointer);
begin
  Fisc_version(db_handle, CallbackProc, isc_arg3);
end;

function TCustomFBClient.isc_portable_integer(buffer: PAnsiChar;
  length: Short): ISC_INT64;
begin
  Result := Fisc_portable_integer(buffer, length);
end;

{ TFBEmbed }

constructor TFBEmbed.Create(AClientName: string);
begin
  inherited Create;
  if AClientName = '' then
    FClientName := FBEmbed_LIB
  else
    FClientName := AClientName;
  FInstancePath := '';
end;

function TFBEmbed.GetClientName: string;
begin
  Result := FClientName;
end;

function TFBEmbed.GetInstanceName: string;
begin
  Result := FB_DefaultInstance;
end;

function TFBClient.GetIsEmbed: Boolean;
begin
  Result := False;
end;

function TFBEmbed.GetIsEmbed: Boolean;
begin
  Result := True;
end;

initialization
finalization
{$IFDEF FREEFBCLIENT}
  FreeAndNil(FFBClient);
{$ENDIF}
end.

