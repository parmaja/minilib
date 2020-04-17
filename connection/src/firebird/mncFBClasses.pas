unit mncFBClasses;
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
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @comment   for Firebird 2.5
 *}

interface

uses
  Windows, SysUtils, Classes, Variants,
  mncFBHeader;

const
  DefaultBlobSegmentSize = 16 * 1024;

type
  TBlobStreamMode = (bmRead, bmWrite, bmReadWrite);

  EFBError = class(Exception)
  private
    FSQLCode: Long;
    FErrorCode: Long;
  public
    constructor Create(ASQLCode: Long; Msg: string); overload;
    constructor Create(ASQLCode: Long; AErrorCode: Long; Msg: string); overload;
    property SQLCode: Long read FSQLCode;
    property ErrorCode: Long read FErrorCode;
  end;

  EFBExceptionError = class(EFBError)
  private
    FExceptionID: Integer;
    FExceptionMsg: string;
    FExceptionName: string;
  public
    constructor Create(ASQLCode: Long; AErrorCode: Long; AExceptionID: Integer; AExceptionName, AExceptionMsg: string; Msg: string); overload;
    property ExceptionID: Integer read FExceptionID;
    property ExceptionName: string read FExceptionName;
    property ExceptionMsg: string read FExceptionMsg;
  end;

  EFBRoleError = class(EFBError);
  EFBClientError = class(EFBError);
  EFBPlanError = class(EFBError);

  TFBDataBaseErrorMessage = (ShowSQLCode, ShowFBMessage, ShowSQLMessage);
  TFBDataBaseErrorMessages = set of TFBDataBaseErrorMessage;

  TFBError = (fbceUnknownError, fbceFirebirdInstallMissing, fbceNotSupported, fbceNotPermitted, fbceFileAccessError, fbceConnectionTimeout, fbceCannotSetDatabase, fbceCannotSetTransaction, fbceOperationCancelled, fbceDPBConstantNotSupported,
    fbceDPBConstantUnknown, fbceTPBConstantNotSupported, fbceTPBConstantUnknown, fbceDatabaseClosed, fbceDatabaseOpen, fbceDatabaseNameMissing, fbceNotInTransaction, fbceInTransaction, fbceTimeoutNegative, fbceUpdateWrongDB, fbceUpdateWrongTR,
    fbceDatabaseNotAssigned, fbceTransactionNotAssigned, fbceXSQLDAIndexOutOfRange, fbceXSQLDANameDoesNotExist, fbceEOF, fbceBOF, fbceInvalidStatementHandle, fbceSQLOpen, fbceSQLClosed, fbceDatasetOpen, fbceDatasetClosed, fbceUnknownSQLDataType,
    fbceInvalidColumnIndex, fbceInvalidParamColumnIndex, fbceInvalidDataConversion, fbceColumnIsNotNullable, fbceBlobCannotBeRead, fbceBlobCannotBeWritten, fbceEmptyQuery, fbceCannotOpenNonSQLSelect, fbceNoFieldAccess, fbceFieldReadOnly,
    fbceFieldNotFound, fbceNotEditing, fbceCannotInsert, fbceCannotPost, fbceCannotUpdate, fbceCannotDelete, fbceCannotRefresh, fbceBufferNotSet, fbceCircularReference, fbceSQLParseError, fbceUserAbort, fbceDataSetUniDirectional,
    fbceCannotCreateSharedResource, fbceWindowsAPIError, fbceColumnListsDontMatch, fbceColumnTypesDontMatch, fbceFieldUnsupportedType, fbceCircularDataLink, fbceEmptySQLStatement, fbceIsASelectStatement, fbceRequiredParamNotSet, fbceNoStoredProcName,
    fbceIsAExecuteProcedure, fbceUpdateFailed, fbceNotCachedUpdates, fbceNotLiveRequest, fbceNoProvider, fbceNoRecordsAffected, fbceNoTableName, fbceCannotCreatePrimaryIndex, fbceCannotDropSystemIndex, fbceTableNameMismatch, fbceIndexFieldMissing,
    fbceInvalidCancellation, fbceInvalidEvent, fbceMaximumEvents, fbceNoEventsRegistered, fbceInvalidQueueing, fbceInvalidRegistration, fbceInvalidBatchMove, fbceSQLDialectInvalid, fbceSPBConstantNotSupported, fbceSPBConstantUnknown, fbceServiceActive,
    fbceServiceInActive, fbceServerNameMissing, fbceQueryParamsError, fbceStartParamsError, fbceOutputParsingError, fbceUseSpecificProcedures, fbceSQLMonitorAlreadyPresent, fbceCantPrintValue, fbceEOFReached, fbceEOFInComment, fbceEOFInString,
    fbceParamNameExpected, fbceSuccess, fbceException, fbceNoOptionsSet, fbceNoDestinationDirectory, fbceNosourceDirectory, fbceNoUninstallFile, fbceOptionNeedsClient, fbceOptionNeedsServer, fbceInvalidOption, fbceInvalidOnErrorResult,
    fbceInvalidOnStatusResult, fbceDPBConstantUnknownEx, fbceTPBConstantUnknownEx, fbceUnknownPlan, fbceFieldSizeMismatch, fbceEventAlreadyRegistered, fbceStringTooLarge);

  TFBDSQLTypes = (SQLUnknown, SQLSelect, SQLInsert, SQLUpdate, SQLDelete, SQLDDL, SQLGetSegment, SQLPutSegment, SQLExecProcedure, SQLStartTransaction, SQLCommit, SQLRollback, SQLSelectForUpdate, SQLSetSequence, SQLSavePoint);

  TCustomFBClient = class(TObject)
  private
    FLibrary: THandle;
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

    Fisc_version: Tisc_version;

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

    function isc_attach_database(status_vector: PISC_STATUS; db_name_length: Short; db_name: PByte; db_handle: PISC_DB_HANDLE; parm_buffer_length: Short; parm_buffer: PByte): ISC_STATUS;
    function isc_array_get_slice(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    function isc_array_lookup_bounds(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_lookup_desc(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_set_desc(status_vector: PISC_STATUS; table_name: PByte; column_name: PByte; sql_dtype, sql_length, sql_dimensions: PShort; descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_put_slice(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    procedure isc_blob_default_desc(descriptor: PISC_BLOB_DESC; table_name: PUChar; column_name: PUChar);
    function isc_blob_gen_bpb(status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC; bpb_buffer_length: UShort; bpb_buffer: PUChar; bpb_length: PUShort): ISC_STATUS;
    function isc_blob_info(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short; item_list_buffer: PByte; result_buffer_length: Short; result_buffer: PByte): ISC_STATUS;
    function isc_blob_lookup_desc(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_BLOB_DESC; global: PUChar): ISC_STATUS;
    function isc_blob_set_desc(status_vector: PISC_STATUS; table_name, column_name: PByte; subtype, charset, segment_size: Short; descriptor: PISC_BLOB_DESC): ISC_STATUS;
    function isc_cancel_blob(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_cancel_events(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    function isc_close_blob(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_commit_retaining(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_transaction(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_create_blob2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short; bpb_address: PByte): ISC_STATUS;
    function isc_database_info(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short; item_list_buffer: PByte; result_buffer_length: Short; result_buffer: PByte): ISC_STATUS;
    procedure isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    procedure isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
    procedure isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
    procedure isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure);
    function isc_detach_database(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_drop_database(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_dsql_alloc_statement2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_describe(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_describe_bind(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute2(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute_immediate(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: UShort; statement: PByte; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_fetch(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_free_statement(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; options: UShort): ISC_STATUS;
    function isc_dsql_prepare(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; length: UShort; statement: PByte; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_set_cursor_name(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; cursor_name: PByte; _type: UShort): ISC_STATUS;
    function isc_dsql_sql_info(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PByte; buffer_length: Short; buffer: PByte): ISC_STATUS;
    procedure isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    procedure isc_encode_sql_date(tm_date: PCTimeStructure; ib_date: PISC_DATE);
    procedure isc_encode_sql_time(tm_date: PCTimeStructure; ib_time: PISC_TIME);
    procedure isc_encode_timestamp(tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP);
    function isc_event_block(event_buffer: PPByte; result_buffer: PPByte; id_count: UShort; event_list: array of PByte): ISC_LONG;
    procedure isc_event_counts(status_vector: PISC_STATUS; buffer_length: Short; event_buffer: PByte; result_buffer: PByte);
    function isc_free(isc_arg1: PByte): ISC_LONG;
    function isc_get_segment(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PUShort; seg_buffer_length: UShort; seg_buffer: PByte): ISC_STATUS;
    function fb_interpret(buffer: PByte; length: Short; var status_vector: PISC_STATUS): ISC_STATUS;
    function isc_interprete(buffer: PByte; var status_vector: PISC_STATUS): ISC_STATUS; deprecated;
    function isc_open_blob2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short; bpb_buffer: PByte): ISC_STATUS;
    function isc_prepare_transaction2(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; msg_length: Short; Msg: PByte): ISC_STATUS;
    function isc_put_segment(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: UShort; seg_buffer: PByte): ISC_STATUS;
    function isc_que_events(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short; event_buffer: PByte; event_function: TISC_EVENT_CALLBACK; event_function_arg: PVoid): ISC_STATUS;
    function isc_rollback_retaining(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_rollback_transaction(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_start_multiple(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS;
    function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
    procedure isc_sql_interprete(SQLCode: Short; buffer: PByte; buffer_length: Short);
    function isc_vax_integer(buffer: PByte; length: Short): ISC_LONG;
    function isc_portable_integer(buffer: PByte; length: Short): ISC_INT64;
    // Security Functions
    function isc_add_user(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS;
    function isc_delete_user(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS;
    function isc_modify_user(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS;
    //
    procedure isc_version(db_handle: PISC_DB_HANDLE; CallbackProc: TISC_VERSION_CALLBACK; isc_arg3: Pointer);
    // Other OSRI functions
    function isc_prepare_transaction(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;

    // Service manager functions
    function isc_service_attach(status_vector: PISC_STATUS; isc_arg2: UShort; isc_arg3: PByte; service_handle: PISC_SVC_HANDLE; isc_arg5: UShort; isc_arg6: PByte): ISC_STATUS;
    function isc_service_detach(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE): ISC_STATUS;
    function isc_service_query(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: PByte; isc_arg8: UShort; isc_arg9: PByte): ISC_STATUS;
    function isc_service_start(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort; isc_arg5: PByte): ISC_STATUS;
    // Client information functions
    procedure isc_get_client_version(buffer: PByte);
    function isc_get_client_major_version: Integer;
    function isc_get_client_minor_version: Integer;
    // Other Blob functions
    // http://www.ibphoenix.com/main.nfs?a=ibphoenix&page=ibp_stream_blob
    function Bopen(blob_id: PISC_QUAD; db_handle: TISC_DB_HANDLE; trans_handle: TISC_TR_HANDLE; mode: PByte): PBSTREAM;
    function Bclose(Stream: PBSTREAM): PISC_STATUS;

    function BLOB_put(isc_arg1: byte; isc_arg2: PBSTREAM): Int;
    function BLOB_get(isc_arg1: PBSTREAM): Int;
    // utils

    property Name: string read FName;
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

  TFBBlobStream = class(TStream)
  private
    // FBase: TFBBase;
    FBlobInitialized: Boolean;
    FBlobID: TISC_QUAD;
    FBlobMaxSegmentSize: Long;
    FBlobNumSegments: Long;
    FBlobSize: Long;
    FBlobType: Short; { 0 = segmented, 1 = streamed }
    FBuffer: PByte;
    FHandle: TISC_BLOB_HANDLE;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FPosition: Long;
  protected
    FDBHandle: PISC_DB_HANDLE;
    FTRHandle: PISC_TR_HANDLE;

    procedure CloseBlob;
    procedure CreateBlob;
    procedure EnsureBlobInitialized;
    procedure GetBlobInfo;
    procedure OpenBlob;
    procedure SetBlobID(Value: TISC_QUAD);
    procedure SetMode(Value: TBlobStreamMode);
  public
    constructor Create(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE);
    destructor Destroy; override;
    function Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
    procedure Cancel;
    procedure CheckReadable;
    procedure CheckWritable;
    procedure Finalize;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(Stream: TStream);
    function Read(var buffer; Count: Longint): Longint; override;
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(Stream: TStream);
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Long); override;
    procedure Truncate;
    function Write(const buffer; Count: Longint): Longint; override;
    property Handle: TISC_BLOB_HANDLE read FHandle;
    property BlobID: TISC_QUAD read FBlobID write SetBlobID;
    property BlobMaxSegmentSize: Long read FBlobMaxSegmentSize;
    property BlobNumSegments: Long read FBlobNumSegments;
    property BlobSize: Long read FBlobSize;
    property BlobType: Short read FBlobType;
    property mode: TBlobStreamMode read FMode write SetMode;
    property Modified: Boolean read FModified;
  end;

  { TmncSQLVAR }

  TmncSQLVAR = class(TObject)
  private
    FXSQLVAR: PXSQLVAR;
    FIgnored: Boolean;
    FModified: Boolean;
    FMaxLen: Short;
    function GetSqlDef: Short;
  protected
    function GetSQLVAR: PXSQLVAR;
    procedure SetSQLVAR(const AValue: PXSQLVAR);
    function GetAliasName: string;
    function GetOwnName: string;
    function GetRelName: string;
    function GetSqlData: PByte;
    function GetSqlInd: PShort;
    function GetSqlLen: Short;
    function GetSqlName: string;
    function GetSqlPrecision: Short;
    function GetSqlScale: Short;
    function GetSqlSubtype: Short;
    function GetSqlType: Short;
    procedure SetAliasName(const AValue: string);
    procedure SetOwnName(const AValue: string);
    procedure SetRelName(const AValue: string);
    procedure SetSqlName(const AValue: string);
    procedure SetSqlData(const AValue: PByte);
    procedure SetSqlInd(const AValue: PShort);
    procedure SetSqlLen(const AValue: Short);
    procedure SetSqlPrecision(const AValue: Short);
    procedure SetSqlScale(const AValue: Short);
    procedure SetSqlSubtype(const AValue: Short);
    procedure SetSqlType(const AValue: Short);
  public
    procedure UpdateData(OldSize, NewSize: Integer);
    procedure UpdateSQLInd;
    property XSQLVar: PXSQLVAR read GetSQLVAR write SetSQLVAR;

    property SqlType: Short read GetSqlType write SetSqlType;
    property SqlDef: Short read GetSqlDef;
    property SqlScale: Short read GetSqlScale write SetSqlScale;
    property SqlPrecision: Short read GetSqlPrecision write SetSqlPrecision;
    property SqlSubtype: Short read GetSqlSubtype write SetSqlSubtype;
    property SqlLen: Short read GetSqlLen write SetSqlLen;
    property SqlData: PByte read GetSqlData write SetSqlData;
    property SqlInd: PShort read GetSqlInd write SetSqlInd;

    property SqlName: string read GetSqlName write SetSqlName;
    property RelName: string read GetRelName write SetRelName;
    property OwnName: string read GetOwnName write SetOwnName;
    property AliasName: string read GetAliasName write SetAliasName;
    property Ignored: Boolean read FIgnored write FIgnored; // used to manual Ignored blob fields
  private
    function GetAsCurrency: Currency;
    function GetAsInt64: Int64;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Double;
    function GetAsLong: Long;
    function GetAsPointer: Pointer;
    function GetAsQuad: TISC_QUAD;
    function GetAsShort: Short;
    function GetAsString: string;
    function GetAsVariant: Variant;
    function GetIsNull: Boolean;
    function GetIsNullable: Boolean;
    function GetSize: Integer;
    procedure SetAsCurrency(AValue: Currency);
    procedure SetAsInt64(AValue: Int64);
    procedure SetAsDate(AValue: TDateTime);
    procedure SetAsLong(AValue: Integer);
    procedure SetAsTime(AValue: TDateTime);
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsDouble(AValue: Double);
    procedure SetAsFloat(AValue: Double);
    procedure SetAsPointer(AValue: Pointer);
    procedure SetAsQuad(AValue: TISC_QUAD);
    procedure SetAsShort(AValue: Short);
    procedure SetAsString(AValue: string);
    procedure SetAsVariant(AValue: Variant);
    procedure SetIsNull(AValue: Boolean);
    procedure SetIsNullable(AValue: Boolean);
    procedure SetAsStrip(const AValue: string);
    function GetAsStrip: string;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const AValue: Boolean);
    function GetAsGUID: TGUID;
    procedure SetAsGUID(const AValue: TGUID);
    procedure SetModified(const AValue: Boolean);
    function GetAsHex: string;
    procedure SetAsHex(const AValue: string);
    function GetAsText: string;
    procedure SetAsText(const AValue: string);
    procedure SetAsNullString(const AValue: string);
    function GetAsChar: char;
    procedure SetAsChar(const AValue: char);
  protected
    FDBHandle: PISC_DB_HANDLE;
    FTRHandle: PISC_TR_HANDLE;
    procedure CheckHandles;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Attach(vDBHandle: PISC_DB_HANDLE; vTRHandle: PISC_TR_HANDLE);
    procedure Detach;
    procedure Assign(Source: TmncSQLVAR);
    procedure Prepare;
    procedure Clear;
    procedure SetBuffer(buffer: Pointer; Size: Integer); // TODO check if used
    procedure CopySQLVAR(const AValue: TmncSQLVAR);

    function CreateReadBlobSteam: TFBBlobStream;
    function CreateWriteBlobSteam: TFBBlobStream;
    procedure LoadFromIStream(Stream: IStreamPersist);
    procedure SaveToIStream(Stream: IStreamPersist);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);

    property Modified: Boolean read FModified write SetModified;
    property Size: Integer read GetSize;
    property MaxLen: Short read FMaxLen write FMaxLen;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsID: Int64 read GetAsInt64 write SetAsInt64; // More flixable names
    property AsInteger: Integer read GetAsLong write SetAsLong;
    property AsLong: Long read GetAsLong write SetAsLong;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property AsShort: Short read GetAsShort write SetAsShort;
    property AsString: string read GetAsString write SetAsString;
    property AsChar: char read GetAsChar write SetAsChar;
    property AsNullString: string read GetAsString write SetAsNullString;
    property AsHex: string read GetAsHex write SetAsHex;
    property AsText: string read GetAsText write SetAsText; // binary blob not text will convert to hex
    property AsTrimString: string read GetAsStrip write SetAsStrip;
    property AsStrip: string read GetAsStrip write SetAsStrip;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AValue: Variant read GetAsVariant write SetAsVariant;
    property AsGUID: TGUID read GetAsGUID write SetAsGUID;

    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;

  end;

  TXSQLDAHelper = record helper for TXSQLVAR
    procedure SetAliasName(const Value: string);
    procedure SetOwnName(const Value: string);
    procedure SetRelName(const Value: string);
    procedure SetSqlName(const Value: string);

    function GetAliasName: string;
    function GetOwnName: string;
    function GetRelName: string;
    function GetSqlName: string;
  end;

procedure FreeSQLDA(var Data: PXSQLDA; Clean: Boolean = True);
function getb(p: PBSTREAM): Byte;
function putb(x: Byte; p: PBSTREAM): Int;
function putbx(x: Byte; p: PBSTREAM): Int;

procedure FBGetBlobInfo(hBlobHandle: PISC_BLOB_HANDLE; out NumSegments, MaxSegmentSize, TotalSize: Long; out BlobType: Short);
procedure FBReadBlob(hBlobHandle: PISC_BLOB_HANDLE; buffer: PByte; BlobSize: Long);
procedure FBWriteBlob(hBlobHandle: PISC_BLOB_HANDLE; buffer: PByte; BlobSize: Long);

function FBGetBlob(DBHandle: TISC_DB_HANDLE; TRHandle: TISC_TR_HANDLE; BlobID: PISC_QUAD): PByte;
procedure InitSQLDA(var Data: PXSQLDA; New: Integer; Clean: Boolean = True);

procedure SetFBClient(NewFBClient: TCustomFBClient);
function IsFBClientSet: Boolean;
function FBClient: TCustomFBClient;

procedure InitClientMode;
procedure InitEmbedMode(ClientName: string = '');

const
  FBErrorMessages: array [TFBError] of string = (SUnknownError, SFirebirdInstallMissing, SNotSupported, SNotPermitted, SFileAccessError, SConnectionTimeout, SCannotSetDatabase, SCannotSetTransaction, SOperationCancelled, SDPBConstantNotSupported,
    SDPBConstantUnknown, STPBConstantNotSupported, STPBConstantUnknown, SDatabaseClosed, SDatabaseOpen, SDatabaseNameMissing, SNotInTransaction, SInTransaction, STimeoutNegative, SUpdateWrongDB, SUpdateWrongTR, SDatabaseNotAssigned,
    STransactionNotAssigned, SXSQLDAIndexOutOfRange, SXSQLDANameDoesNotExist, SEOF, SBOF, SInvalidStatementHandle, SSQLOpen, SSQLClosed, SDatasetOpen, SDatasetClosed, SUnknownSQLDataType, SInvalidColumnIndex, SInvalidParamColumnIndex,
    SInvalidDataConversion, SColumnIsNotNullable, SBlobCannotBeRead, SBlobCannotBeWritten, SEmptyQuery, SCannotOpenNonSQLSelect, SNoFieldAccess, SFieldReadOnly, SFieldNotFound, SNotEditing, SCannotInsert, SCannotPost, SCannotUpdate, SCannotDelete,
    SCannotRefresh, SBufferNotSet, SCircularReference, SSQLParseError, SUserAbort, SDataSetUniDirectional, SCannotCreateSharedResource, SWindowsAPIError, SColumnListsDontMatch, SColumnTypesDontMatch, SFieldUnsupportedType, SCircularDataLink,
    SEmptySQLStatement, SIsASelectStatement, SRequiredParamNotSet, SNoStoredProcName, SIsAExecuteProcedure, SUpdateFailed, SNotCachedUpdates, SNotLiveRequest, SNoProvider, SNoRecordsAffected, SNoTableName, SCannotCreatePrimaryIndex,
    SCannotDropSystemIndex, STableNameMismatch, SIndexFieldMissing, SInvalidCancellation, SInvalidEvent, SMaximumEvents, SNoEventsRegistered, SInvalidQueueing, SInvalidRegistration, SInvalidBatchMove, SSQLDialectInvalid, SSPBConstantNotSupported,
    SSPBConstantUnknown, SServiceActive, SServiceInActive, SServerNameMissing, SQueryParamsError, SStartParamsError, SOutputParsingError, SUseSpecificProcedures, SSQLMonitorAlreadyPresent, SCantPrintValue, SEOFReached, SEOFInComment, SEOFInString,
    SParamNameExpected, SSuccess, SException, SNoOptionsSet, SNoDestinationDirectory, SNosourceDirectory, SNoUninstallFile, SOptionNeedsClient, SOptionNeedsServer, SInvalidOption, SInvalidOnErrorResult, SInvalidOnStatusResult, SDPBConstantUnknownEx,
    STPBConstantUnknownEx, SUnknownPlan, SFieldSizeMismatch, SEventAlreadyRegistered, SStringTooLarge);

implementation

uses
  {$ifdef WINDOWS} Registry, {$endif}
  mncFBUtils;

procedure FBGetBlobInfo(hBlobHandle: PISC_BLOB_HANDLE; out NumSegments, MaxSegmentSize, TotalSize: Long; out BlobType: Short);
var
  items: array [0 .. 3] of byte;
  results: array [0 .. 99] of byte;
  i, item_length: Integer;
  item: Integer;
  StatusVector: TStatusVector;
begin
  items[0] := isc_info_blob_num_segments;
  items[1] := isc_info_blob_max_segment;
  items[2] := isc_info_blob_total_length;
  items[3] := isc_info_blob_type;

  if FBClient.isc_blob_info(@StatusVector, hBlobHandle, 4, @items[0], SizeOf(results), @results[0]) > 0 then
    FBRaiseError(StatusVector);

  i := 0;
  while (i < SizeOf(results)) and (results[i] <> isc_info_end) do
  begin
    item := Integer(results[i]);
    Inc(i);
    item_length := FBClient.isc_vax_integer(@results[i], 2);
    Inc(i, 2);
    case item of
      isc_info_blob_num_segments:
        NumSegments := FBClient.isc_vax_integer(@results[i], item_length);
      isc_info_blob_max_segment:
        MaxSegmentSize := FBClient.isc_vax_integer(@results[i], item_length);
      isc_info_blob_total_length:
        TotalSize := FBClient.isc_vax_integer(@results[i], item_length);
      isc_info_blob_type:
        BlobType := FBClient.isc_vax_integer(@results[i], item_length);
    end;
    Inc(i, item_length);
  end;
end;

procedure FBReadBlob(hBlobHandle: PISC_BLOB_HANDLE; buffer: PByte; BlobSize: Long);
var
  CurPos: Long;
  BytesRead, SegLen: UShort;
  LocalBuffer: PByte;
  StatusVector: TStatusVector;
begin
  CurPos := 0;
  LocalBuffer := buffer;
  SegLen := UShort(DefaultBlobSegmentSize * 2);
  while (CurPos < BlobSize) do
  begin
    if (CurPos + SegLen > BlobSize) then
      SegLen := BlobSize - CurPos;
    if not((FBClient.isc_get_segment(@StatusVector, hBlobHandle, @BytesRead, SegLen, LocalBuffer) = 0) or (StatusVector[1] = isc_segment)) then
      FBRaiseError(StatusVector);
    Inc(LocalBuffer, BytesRead);
    Inc(CurPos, BytesRead);
    BytesRead := 0;
  end;
end;

procedure FBWriteBlob(hBlobHandle: PISC_BLOB_HANDLE; buffer: PByte; BlobSize: Long);
var
  StatusVector: TStatusVector;
  CurPos, SegLen: Long;
begin
  CurPos := 0;
  SegLen := DefaultBlobSegmentSize;
  while (CurPos < BlobSize) do
  begin
    if (CurPos + SegLen > BlobSize) then
      SegLen := BlobSize - CurPos;
    if FBClient.isc_put_segment(@StatusVector, hBlobHandle, SegLen, PByte(@buffer[CurPos])) > 0 then
      FBRaiseError(StatusVector);
    Inc(CurPos, SegLen);
  end;
end;

function FBGetBlob(DBHandle: TISC_DB_HANDLE; TRHandle: TISC_TR_HANDLE; BlobID: PISC_QUAD): PByte;
const
  cDefaultSize = 1024;
var
  bStream: PBSTREAM;
  aPos, aSize: Integer;
  p: PByte;
  s: string;
begin
  Result := nil;
  aPos := 0;
  aSize := 0;
  with FBClient do
  begin
    s := 'R';
    bStream := Bopen(BlobID, DBHandle, TRHandle, @s[1]);
    try
      p := nil;
      while bStream^.bstr_cnt > 0 do
      begin
        if aPos >= aSize then
        begin
          aSize := aSize + cDefaultSize;
          ReallocMem(Result, aSize);
          p := Result;
          Inc(p, aPos);
        end;
        p^ := getb(bStream);
        Inc(p);
        Inc(aPos);
      end;
      ReallocMem(Result, aPos);
    finally
      Bclose(bStream);
    end;
  end;
end;

function getb(p: PBSTREAM): Byte;
(* The C-macro reads like this:
  getb(p)	(--(p)->bstr_cnt >= 0 ? *(p)->bstr_ptr++ & 0377: BLOB_get (p)) *)
begin
  Dec(p^.bstr_cnt);
  if (p^.bstr_cnt >= 0) then
  begin
    Result := (Int(p^.bstr_ptr^) and Int(0377));
    Inc(p^.bstr_ptr);
  end
  else
    Result := FBClient.BLOB_get(p);
end;

function putb(x: byte; p: PBSTREAM): Int;
{ The C-macro reads like this:
  putb(x,p) ((x == '\n' || (!(--(p)->bstr_cnt))) ?      // then
  BLOB_put (x,p) :                                    // else
  ((int) (*(p)->bstr_ptr++ = (unsigned) (x)))) }
begin
  Dec(p^.bstr_cnt);
  if (x = (Int('n') - Int('a'))) or (p^.bstr_cnt = 0) then
    Result := FBClient.BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := x;
    Result := UInt(x);
    Inc(p^.bstr_ptr^);
  end;
end;

function putbx(x: Byte; p: PBSTREAM): Int;
{ The C-macro reads like this:
  putbx(x,p) ((!(--(p)->bstr_cnt)) ?    // then
  BLOB_put (x,p) :                    // else
  ((int) (*(p)->bstr_ptr++ = (unsigned) (x)))) }
begin
  Dec(p^.bstr_cnt);
  if (p^.bstr_cnt = 0) then
    Result := FBClient.BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := ord(x);
    Inc(p^.bstr_ptr^);
    Result := UInt(x);
  end;
end;

procedure InitSQLDA(var Data: PXSQLDA; New: Integer; Clean: Boolean = True);
var
  old: Integer;
var
  p: PXSQLVAR;
  i: Integer;
begin
  if Data = nil then
    old := 0
  else
    old := Data^.sqln;

  if Clean and (New < old) then
  begin
    p := @Data^.sqlvar[New];
    for i := New to old - 1 do
    begin
      FBFree(p^.SqlData);
      FBFree(p^.SqlInd);
      p := Pointer(PByte(p) + XSQLVar_Size);
    end;
  end;

  FBAlloc(Data, XSQLDA_LENGTH(old), XSQLDA_LENGTH(New));
  Data^.version := SQLDA_VERSION1;
  Data^.sqln := New;
end;

procedure FreeSQLDA(var Data: PXSQLDA; Clean: Boolean = True);
var
  p: PXSQLVAR;
  i: Integer;
begin
  if Data <> nil then
  begin
    if Clean then
    begin
      p := @Data^.sqlvar[0];
      for i := 0 to Data.sqln - 1 do
      begin
        FBFree(p^.SqlData);
        FBFree(p^.SqlInd);
        p := Pointer(PByte(p) + XSQLVar_Size);
      end;
    end;
    FBFree(Data);
  end;
end;

{ TmncSQLVAR }

function TmncSQLVAR.GetAliasName: string;
begin
  Result := FXSQLVAR^.GetAliasName;
end;

function TmncSQLVAR.GetOwnName: string;
begin
  Result := FXSQLVAR^.GetOwnName;
end;

function TmncSQLVAR.GetRelName: string;
begin
  Result := FXSQLVAR^.GetRelName;
end;

function TmncSQLVAR.GetSqlData: PByte;
begin
  Result := FXSQLVAR^.SqlData;
end;

function TmncSQLVAR.GetSqlInd: PShort;
begin
  Result := FXSQLVAR^.SqlInd;
end;

function TmncSQLVAR.GetSqlLen: Short;
begin
  Result := FXSQLVAR^.SqlLen;
end;

function TmncSQLVAR.GetSqlName: string;
begin
  Result := FXSQLVAR^.GetSqlName;
end;

function TmncSQLVAR.GetSqlPrecision: Short;
begin
  case SqlType and not 1 of
    SQL_SHORT:
      Result := 4;
    SQL_LONG:
      Result := 9;
    SQL_INT64:
      Result := 18;
  else
    Result := 0;
  end;
end;

function TmncSQLVAR.GetSqlScale: Short;
begin
  Result := FXSQLVAR^.SqlScale;
end;

function TmncSQLVAR.GetSqlSubtype: Short;
begin
  Result := FXSQLVAR^.SqlSubtype;
end;

function TmncSQLVAR.GetSqlType: Short;
begin
  Result := FXSQLVAR^.SqlType;
end;

function TmncSQLVAR.GetSqlDef: Short;
begin
  Result := SqlType and (not 1);
end;

function TmncSQLVAR.GetSQLVAR: PXSQLVAR;
begin
  Result := FXSQLVAR;
end;

procedure TmncSQLVAR.SetAliasName(const AValue: string);
begin
  FXSQLVAR^.SetAliasName(AValue);
end;

procedure TmncSQLVAR.UpdateData(OldSize, NewSize: Integer);
begin
  if NewSize = 0 then
  begin
    if (FXSQLVAR <> nil) and (FXSQLVAR^.SqlData <> nil) then
      FBFree(FXSQLVAR^.SqlData)
  end
  else
    FBAlloc(FXSQLVAR^.SqlData, OldSize, NewSize);
end;

procedure TmncSQLVAR.UpdateSQLInd;
begin
  if IsNullable then
  begin
    if not Assigned(FXSQLVAR^.SqlInd) then
      FBAlloc(FXSQLVAR^.SqlInd, 0, SizeOf(Short))
  end
  else if Assigned(FXSQLVAR^.SqlInd) then
    FBFree(FXSQLVAR^.SqlInd);
end;

procedure TmncSQLVAR.SetOwnName(const AValue: string);
begin
  FXSQLVAR^.SetOwnName(AValue);
end;

procedure TmncSQLVAR.SetRelName(const AValue: string);
begin
  FXSQLVAR^.SetRelName(AValue);
end;

procedure TmncSQLVAR.SetSqlData(const AValue: PByte);
begin
  FXSQLVAR^.SqlData := AValue;
end;

procedure TmncSQLVAR.SetSqlInd(const AValue: PShort);
begin
  FXSQLVAR^.SqlInd := AValue
end;

procedure TmncSQLVAR.SetSqlLen(const AValue: Short);
begin
  FXSQLVAR^.SqlLen := AValue
end;

procedure TmncSQLVAR.SetSqlName(const AValue: string);
begin
  FXSQLVAR^.SetSqlName(AValue);
end;

procedure TmncSQLVAR.SetSqlPrecision(const AValue: Short);
begin
  FBRaiseError(fbceNotSupported, []);
end;

procedure TmncSQLVAR.SetSqlScale(const AValue: Short);
begin
  FXSQLVAR^.SqlScale := AValue
end;

procedure TmncSQLVAR.SetSqlSubtype(const AValue: Short);
begin
  FXSQLVAR^.SqlSubtype := AValue
end;

procedure TmncSQLVAR.SetSqlType(const AValue: Short);
begin
  FXSQLVAR^.SqlType := AValue
end;

procedure TmncSQLVAR.SetSQLVAR(const AValue: PXSQLVAR);
begin
  FXSQLVAR := AValue;
  // TODO Prepare
end;

{ TmncSQLVAR }

function TmncSQLVAR.CreateReadBlobSteam: TFBBlobStream;
begin
  Result := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    Result.mode := bmRead;
    Result.BlobID := AsQuad;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TmncSQLVAR.CreateWriteBlobSteam: TFBBlobStream;
begin
  Result := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    Result.mode := bmWrite;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TmncSQLVAR.LoadFromStream(Stream: TStream);
var
  bs: TFBBlobStream;
begin
  CheckHandles;
  bs := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    bs.mode := bmWrite;
    // Stream.Seek(0, soFromBeginning);//not all stream support seek
    bs.LoadFromStream(Stream);
    bs.Finalize;
    AsQuad := bs.BlobID;
  finally
    bs.Free;
  end;
end;

procedure TmncSQLVAR.SaveToFile(const Filename: string);
var
  fs: TFileStream;
begin
  CheckHandles;
  fs := TFileStream.Create(Filename, fmCreate or fmShareExclusive);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TmncSQLVAR.SaveToStream(Stream: TStream);
var
  bs: TFBBlobStream;
begin
  CheckHandles;
  bs := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    bs.mode := bmRead;
    bs.BlobID := AsQuad;
    bs.SaveToStream(Stream);
  finally
    bs.Free;
  end;
end;

procedure TmncSQLVAR.SaveToIStream(Stream: IStreamPersist);
var
  bs: TFBBlobStream;
begin
  CheckHandles;
  bs := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    bs.mode := bmRead;
    bs.BlobID := AsQuad;
    Stream.LoadFromStream(bs);
  finally
    bs.Free;
  end;
end;

procedure TmncSQLVAR.LoadFromIStream(Stream: IStreamPersist);
var
  bs: TFBBlobStream;
begin
  CheckHandles;
  bs := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    bs.mode := bmWrite;
    Stream.SaveToStream(bs);
    bs.Finalize;
    AsQuad := bs.BlobID;
  finally
    bs.Free;
  end;
end;

procedure TmncSQLVAR.LoadFromFile(const Filename: string);
var
  fs: TFileStream;
begin
  CheckHandles;
  fs := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TmncSQLVAR.Assign(Source: TmncSQLVAR);
var
  szBuff: PByte;
  s_bhandle, d_bhandle: TISC_BLOB_HANDLE;
  bSourceBlob, bDestBlob: Boolean;
  iSegs, iMaxSeg, iSize, OldSize: Long;
  iBlobType: Short;
  StatusVector: TStatusVector;
begin
  szBuff := nil;
  bSourceBlob := True;
  bDestBlob := True;
  s_bhandle := nil;
  d_bhandle := nil;
  iSize := 0;
  try
    if (Source.IsNull) then
    begin
      IsNull := True;
    end
    else if (SqlDef = SQL_ARRAY) or (Source.SqlDef = SQL_ARRAY) then
      { arrays not supported }
    else if (SqlDef <> SQL_BLOB) and (Source.SqlDef <> SQL_BLOB) then
    begin
      AValue := Source.AValue;
    end
    else
    begin
      if (Source.SqlDef <> SQL_BLOB) then
      begin
        szBuff := nil;
        FBAlloc(szBuff, 0, Source.SqlLen);
        if (Source.SqlDef = SQL_TEXT) or (Source.SqlDef = SQL_VARYING) then
        begin
          iSize := FBClient.isc_vax_integer(Source.SqlData, 2);
          Move(Source.SqlData[2], szBuff[0], iSize)
        end
        else
        begin
          iSize := Source.SqlLen;
          Move(Source.SqlData[0], szBuff[0], iSize);
        end;
        bSourceBlob := False;
      end
      else if (SqlDef <> SQL_BLOB) then
        bDestBlob := False;

      if bSourceBlob then
      begin
        { read the blob }
        FBCall(FBClient.isc_open_blob2(@StatusVector, @FDBHandle, @FTRHandle, @s_bhandle, PISC_QUAD(Source.SqlData), 0, nil), StatusVector, True);
        try
          FBGetBlobInfo(@s_bhandle, iSegs, iMaxSeg, iSize, iBlobType);
          szBuff := nil;
          FBAlloc(szBuff, 0, iSize);
          FBReadBlob(@s_bhandle, szBuff, iSize);
        finally
          FBCall(FBClient.isc_close_blob(@StatusVector, @s_bhandle), StatusVector, True);
        end;
      end;

      if bDestBlob then
      begin
        { write the blob }
        FBCall(FBClient.isc_create_blob2(@StatusVector, @FDBHandle, @FTRHandle, @d_bhandle, PISC_QUAD(SqlData), 0, nil), StatusVector, True);
        try
          FBWriteBlob(@d_bhandle, szBuff, iSize);
          IsNull := False;
        finally
          FBCall(FBClient.isc_close_blob(@StatusVector, @d_bhandle), StatusVector, True);
        end;
      end
      else
      begin
        { just copy the buffer }
        SqlType := SQL_TEXT;
        OldSize := SqlLen;
        if iSize > FMaxLen then
          SqlLen := FMaxLen
        else
          SqlLen := iSize;
        UpdateData(OldSize, SqlLen + 1);
        Move(szBuff[0], SqlData[0], SqlLen);
      end;
    end;
  finally
    FreeMem(szBuff);
  end;
end;

procedure TmncSQLVAR.Prepare;
begin
  { if Items[i].Name = '' then
    begin
    if AliasName = '' then
    AliasName := 'F_' + IntToStr(i);
    Items[i].Name := FBDequoteName(aliasname);
    end; }

  if (SqlDef = SQL_VARYING) or (SqlDef = SQL_TEXT) then
    FMaxLen := SqlLen
  else
    FMaxLen := 0;

  if FXSQLVAR^.SqlData = nil then
    case SqlDef of
      SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP, SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT, SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT, SQL_BOOLEAN:
        begin
          if (SqlLen = 0) then
            { Make sure you get a valid pointer anyway
              select '' from foo }
            UpdateData(0, 1)
          else
            UpdateData(0, SqlLen)
        end;
      SQL_VARYING:
        begin
          UpdateData(0, SqlLen + 2);
        end;
    else
      FBRaiseError(fbceUnknownSQLDataType, [SqlDef])
    end;
  UpdateSQLInd;
end;

function TmncSQLVAR.GetAsChar: char;
var
  s: string;
begin
  s := AsString;
  if length(s) > 0 then
    Result := s[1]
  else
    Result := #0;
end;

function TmncSQLVAR.GetAsCurrency: Currency;
begin
  Result := 0;
  if not IsNull then
    case SqlDef of
      SQL_TEXT, SQL_VARYING:
        begin
          try
            Result := StrToCurr(AsString);
          except
            on E: Exception do
              FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      SQL_SHORT:
        Result := FBScaleCurrency(Int64(PShort(SqlData)^), SqlScale);
      SQL_LONG:
        Result := FBScaleCurrency(Int64(PLong(SqlData)^), SqlScale);
      SQL_INT64:
        Result := FBScaleCurrency(PInt64(SqlData)^, SqlScale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := GetAsDouble;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsInt64: Int64;
begin
  Result := 0;
  if not IsNull then
    case SqlDef of
      SQL_TEXT, SQL_VARYING:
      begin
        try
          Result := StrToInt64(AsString);
        except
          on E: Exception do
            FBRaiseError(fbceInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT: Result := FBScaleInt64(Int64(PShort(SqlData)^), SqlScale);
      SQL_LONG: Result := FBScaleInt64(Int64(PLong(SqlData)^), SqlScale);
      SQL_INT64: Result := FBScaleInt64(PInt64(SqlData)^, SqlScale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PShort(SqlData)^ of
          ISC_TRUE:
            Result := 1;
          ISC_FALSE:
            Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsDateTime: TDateTime;
var
  tm_date: TCTimeStructure;
begin
  Result := 0;
  if not IsNull then
    case SqlDef of
      SQL_TEXT, SQL_VARYING:
      begin
        try
          Result := StrToDate(AsString);
        except
          on E: EConvertError do
            FBRaiseError(fbceInvalidDataConversion, [nil]);
        end;
      end;
      SQL_TYPE_DATE:
      begin
        FBClient.isc_decode_sql_date(PISC_DATE(SqlData), @tm_date);
        try
          Result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1), Word(tm_date.tm_mday));
        except
          on E: EConvertError do
          begin
            FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      end;
      SQL_TYPE_TIME:
      begin
        FBClient.isc_decode_sql_time(PISC_TIME(SqlData), @tm_date);
        try
          Result := EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min), Word(tm_date.tm_sec), 0)
        except
          on E: EConvertError do
          begin
            FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      end;
      SQL_TIMESTAMP:
      begin
        FBClient.isc_decode_date(PISC_QUAD(SqlData), @tm_date);
        try
          Result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1), Word(tm_date.tm_mday));
          if Result >= 0 then
            Result := Result + EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min), Word(tm_date.tm_sec), 0)
          else
            Result := Result - EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min), Word(tm_date.tm_sec), 0)
        except
          on E: EConvertError do
          begin
            FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      end;
      else
        FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsDouble: Double;
begin
  Result := 0;
  if not IsNull then
  begin
    case SqlDef of
      SQL_TEXT, SQL_VARYING:
        begin
          try
            Result := StrToFloat(AsString);
          except
            on E: Exception do
              FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      SQL_SHORT:
        Result := FBScaleDouble(Int64(PShort(SqlData)^), SqlScale);
      SQL_LONG:
        Result := FBScaleDouble(Int64(PLong(SqlData)^), SqlScale);
      SQL_INT64:
        Result := FBScaleDouble(PInt64(SqlData)^, SqlScale);
      SQL_FLOAT:
        Result := PFloat(SqlData)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        Result := PDouble(SqlData)^;
      SQL_BOOLEAN:
        case PShort(SqlData)^ of
          ISC_TRUE:
            Result := 1;
          ISC_FALSE:
            Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
  end;
end;

function TmncSQLVAR.GetAsFloat: Double;
begin
  Result := 0;
  try
    Result := AsDouble;
  except
    on E: SysUtils.EOverflow do
      FBRaiseError(fbceInvalidDataConversion, [nil])
    else
      raise;
  end;
end;

function TmncSQLVAR.GetAsLong: Long;
begin
  Result := 0;
  if not IsNull then
    case SqlDef of
      SQL_TEXT, SQL_VARYING:
        begin
          try
            Result := StrToInt(AsString);
          except
            on E: Exception do
              FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      SQL_SHORT:
        Result := Trunc(FBScaleDouble(Int64(PShort(SqlData)^), SqlScale));
      SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP, SQL_LONG:
        Result := Trunc(FBScaleDouble(Int64(PLong(SqlData)^), SqlScale));
      SQL_INT64:
        Result := Trunc(FBScaleDouble(PInt64(SqlData)^, SqlScale));
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PShort(SqlData)^ of
          ISC_TRUE:
            Result := 1;
          ISC_FALSE:
            Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsPointer: Pointer;
begin
  if not IsNull then
    Result := SqlData
  else
    Result := nil;
end;

function TmncSQLVAR.GetAsQuad: TISC_QUAD;
begin
  Result.gds_quad_high := 0;
  Result.gds_quad_low := 0;
  if not IsNull then
    case SqlDef of
      SQL_BLOB, SQL_ARRAY, SQL_QUAD:
        Result := PISC_QUAD(SqlData)^;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsShort: Short;
begin
  Result := 0;
  try
    Result := AsLong;
  except
    on E: Exception do
      FBRaiseError(fbceInvalidDataConversion, [nil]);
  end;
end;

function TmncSQLVAR.GetAsString: string;
var
  sz: PByte;
  str_len: Integer;
  ss: TStringStream;
  s: UTF8String;
begin
  Result := '';
  { Check null, if so return a default string }
  if not IsNull then
    case SqlDef of
      SQL_ARRAY: Result := '[Array]';
      SQL_BLOB:
      begin
        ss := TStringStream.Create('', TEncoding.UTF8);
        try
          SaveToStream(ss); // TODO not work witout handles
          Result := ss.DataString;
        finally
          ss.Free;
        end;
      end;
      SQL_TEXT, SQL_VARYING:
      begin
        sz := SqlData;
        if (SqlDef = SQL_TEXT) then
          str_len := SqlLen
        else
        begin
          str_len := FBClient.isc_vax_integer(SqlData, 2);
          Inc(sz, 2);
        end;
        SetLength(s, str_len);
        Move(sz^, PByte(s)^, str_len);
        Result := s;
      end;
      SQL_TYPE_DATE: Result := FormatDateTime('yyyy-mm-dd', AsDateTime);
      SQL_TYPE_TIME: Result := TimeToStr(AsDateTime);
      SQL_TIMESTAMP: Result := FormatDateTime('yyyy-mm-dd hh:nn:ss am/pm', AsDateTime);
      SQL_SHORT, SQL_LONG:
      begin
        if SqlScale = 0 then
          Result := IntToStr(AsLong)
        else if SqlScale >= (-4) then
          Result := CurrToStr(AsCurrency)
        else
          Result := FloatToStr(AsDouble);
      end;
      SQL_INT64:
      begin
        if SqlScale = 0 then
          Result := IntToStr(AsInt64)
        else if SqlScale >= (-4) then
          Result := CurrToStr(AsCurrency)
        else
          Result := FloatToStr(AsDouble);
      end;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: Result := FloatToStr(AsDouble);
      SQL_BOOLEAN:
      begin
        if AsBoolean then
          Result := 'True'
        else
          Result := 'False';
      end
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsVariant: Variant;
begin
  if IsNull then
    Result := NULL
    { Check null, if so return a default string }
  else
    case SqlDef of
      SQL_ARRAY:
        Result := '[Array]';
      SQL_BLOB:
        begin
          if SqlSubtype = 1 then
            Result := AsString
          else
            Result := '[Blob]';
        end;
      SQL_TEXT, SQL_VARYING:
        Result := AsString;
      SQL_TIMESTAMP, SQL_TYPE_DATE, SQL_TYPE_TIME:
        Result := AsDateTime;
      SQL_SHORT, SQL_LONG:
        if SqlScale = 0 then
          Result := AsLong
        else if SqlScale >= (-4) then
          Result := AsCurrency
        else
          Result := AsDouble;
      SQL_INT64:
        if SqlScale = 0 then
          Result := AsInt64
        else if SqlScale >= (-4) then
          Result := AsCurrency
        else
          Result := AsDouble;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := AsDouble;
      SQL_BOOLEAN:
        Result := AsBoolean;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetIsNull: Boolean;
begin
  Result := IsNullable and Assigned(SqlInd) and (SqlInd^ = -1);
end;

function TmncSQLVAR.GetIsNullable: Boolean;
begin
  Result := (SqlType and 1 = 1);
end;

function TmncSQLVAR.GetSize: Integer;
begin
  Result := SqlLen;
end;

procedure TmncSQLVAR.SetAsChar(const AValue: char);
begin
  AsString := AValue;
end;

constructor TmncSQLVAR.Create;
begin
  inherited;
end;

procedure TmncSQLVAR.CheckHandles;
begin
  if (FDBHandle = nil) or (FTRHandle = nil) then
    raise EFBClientError.Create('Handles not opened');
end;

procedure TmncSQLVAR.Detach;
begin
  FDBHandle := nil;
  FTRHandle := nil;
end;

procedure TmncSQLVAR.Attach(vDBHandle: PISC_DB_HANDLE; vTRHandle: PISC_TR_HANDLE);
begin
  FDBHandle := vDBHandle;
  FTRHandle := vTRHandle;
end;

procedure TmncSQLVAR.SetAsCurrency(AValue: Currency);
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_INT64 or (SqlType and 1);
  SqlScale := -4;
  SqlLen := SizeOf(Int64);
  UpdateData(0, SqlLen);
  PCurrency(SqlData)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsInt64(AValue: Int64);
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_INT64 or (SqlType and 1);
  SqlScale := 0;
  SqlLen := SizeOf(Int64);
  UpdateData(0, SqlLen);
  PInt64(SqlData)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsDate(AValue: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy: Word;
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_TYPE_DATE or (SqlType and 1);
  DecodeDate(AValue, Yr, Mn, Dy);
  with tm_date do
  begin
    tm_sec := 0;
    tm_min := 0;
    tm_hour := 0;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  SqlLen := SizeOf(ISC_DATE);
  UpdateData(0, SqlLen);
  FBClient.isc_encode_sql_date(@tm_date, PISC_DATE(SqlData));
  Modified := True;
end;

procedure TmncSQLVAR.SetAsLong(AValue: Integer);
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_LONG or (SqlType and 1);
  SqlLen := SizeOf(Long);
  SqlScale := 0;
  UpdateData(0, SqlLen);
  PLong(SqlData)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsTime(AValue: TDateTime);
var
  tm_date: TCTimeStructure;
  Hr, Mt, s, Ms: Word;
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_TYPE_TIME or (SqlType and 1);
  DecodeTime(AValue, Hr, Mt, s, Ms);
  with tm_date do
  begin
    tm_sec := s;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := 0;
    tm_mon := 0;
    tm_year := 0;
  end;
  SqlLen := SizeOf(ISC_TIME);
  UpdateData(0, SqlLen);
  FBClient.isc_encode_sql_time(@tm_date, PISC_TIME(SqlData));
  Modified := True;
end;

procedure TmncSQLVAR.SetAsDateTime(AValue: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy, Hr, Mt, s, Ms: Word;
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_TIMESTAMP or (SqlType and 1);
  DecodeDate(AValue, Yr, Mn, Dy);
  DecodeTime(AValue, Hr, Mt, s, Ms);
  with tm_date do
  begin
    tm_sec := s;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  SqlLen := SizeOf(TISC_QUAD);
  UpdateData(0, SqlLen);
  FBClient.isc_encode_date(@tm_date, PISC_QUAD(SqlData));
  Modified := True;
end;

procedure TmncSQLVAR.SetAsDouble(AValue: Double);
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_DOUBLE or (SqlType and 1);
  SqlLen := SizeOf(Double);
  SqlScale := 0;
  UpdateData(0, SqlLen);
  PDouble(SqlData)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsFloat(AValue: Double);
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_FLOAT or (SqlType and 1);
  SqlLen := SizeOf(Float);
  SqlScale := 0;
  UpdateData(0, SqlLen);
  PSingle(SqlData)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsNullString(const AValue: string);
begin
  if AValue = '' then
    Clear
  else
    AsString := AValue;
end;

procedure TmncSQLVAR.SetAsPointer(AValue: Pointer);
begin
  if IsNullable and (AValue = nil) then
    IsNull := True
  else
  begin
    IsNull := False;
    SqlType := SQL_TEXT or (SqlType and 1);
    Move(AValue^, SqlData^, SqlLen);
    Modified := True;
  end;
end;

procedure TmncSQLVAR.SetAsQuad(AValue: TISC_QUAD);
begin
  if IsNullable then
    IsNull := False;
  if (SqlDef <> SQL_BLOB) and (SqlDef <> SQL_ARRAY) then
    FBRaiseError(fbceInvalidDataConversion, [nil]);
  SqlLen := SizeOf(TISC_QUAD);
  UpdateData(0, SqlLen);
  PISC_QUAD(SqlData)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsShort(AValue: Short);
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_SHORT or (SqlType and 1);
  SqlLen := SizeOf(Short);
  SqlScale := 0;
  UpdateData(0, SqlLen);
  PShort(SqlData)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsString(AValue: string);
var
  stype: Integer;
  ss: TStringStream;

  procedure SetStringValue;
  var
    l: Integer;
    s: UTF8String;
  begin
    s := AValue;
    l := Length(s);

    if (SqlName = 'DB_KEY') or (SqlName = 'RDB$DB_KEY') then
      Move(s[1], SqlData^, SqlLen)
    else
    begin
      SqlType := SQL_TEXT or (SqlType and 1);
      if (FMaxLen > 0) and (l > FMaxLen) then
        s := Copy(s, 1, FMaxLen);
      SqlLen := l;
      UpdateData(0, SqlLen + 1);
      if (l > 0) then
        Move(s[1], SqlData^, SqlLen);
    end;
    Modified := True;
  end;

begin
  if IsNullable then
    IsNull := False;
  stype := SqlDef;
  if (stype = SQL_TEXT) or (stype = SQL_VARYING) then
    SetStringValue
  else
  begin
    if (stype = SQL_BLOB) then
    begin
      if AValue = '' then
        IsNull := True
      else
      begin
        ss := TStringStream.Create(AValue, TEncoding.UTF8);
        try
          LoadFromStream(ss); // TODO not work without handles
        finally
          ss.Free;
        end;
      end;
    end
    else if AValue = '' then
      IsNull := True
    else if (stype = SQL_TIMESTAMP) or (stype = SQL_TYPE_DATE) or (stype = SQL_TYPE_TIME) then
      SetAsDateTime(StrToDateTime(AValue))
    else
      SetStringValue;
  end;
end;

procedure TmncSQLVAR.SetAsVariant(AValue: Variant);
begin
  if VarIsNull(AValue) then
    IsNull := True
  else
    case VarType(AValue) of
      varEmpty, varNull:
        IsNull := True;
      varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord:
        AsLong := AValue;
      varSingle, varDouble:
        AsDouble := AValue;
      varCurrency:
        AsCurrency := AValue;
      varBoolean:
        if AValue then
          AsBoolean := True
        else
          AsBoolean := False;
      varDate:
        AsDateTime := AValue;
      varOleStr, varString, varUString:
        AsString := AValue;
      varArray:
        FBRaiseError(fbceNotSupported, [nil]);
      varByRef, varDispatch, varError, varUnknown, varVariant:
        FBRaiseError(fbceNotPermitted, [nil]);
      varInt64:
        AsInt64 := AValue;
    else
      FBRaiseError(fbceNotSupported, [nil]);
    end;
end;

procedure TmncSQLVAR.SetIsNull(AValue: Boolean);
begin
  if AValue then
  begin
    if not IsNullable then
      IsNullable := True;
    if Assigned(SqlInd) then
      SqlInd^ := -1;
    Modified := True;
  end
  else if ((not AValue) and IsNullable) then
  begin
    if Assigned(SqlInd) then
      SqlInd^ := 0;
    Modified := True;
  end;
end;

procedure TmncSQLVAR.SetIsNullable(AValue: Boolean);
begin
  if (AValue <> IsNullable) then
  begin
    if AValue then
    begin
      SqlType := SqlType or 1;
      UpdateSQLInd;
    end
    else
    begin
      SqlType := SqlDef;
      UpdateSQLInd;
    end;
  end;
end;

procedure TmncSQLVAR.Clear;
begin
  IsNull := True;
end;

procedure TmncSQLVAR.SetAsStrip(const AValue: string);
begin
  if AValue = '' then
    Clear
  else
    SetAsString(TrimRight(AValue));
end;

function TmncSQLVAR.GetAsStrip: string;
begin
  Result := TrimRight(GetAsString);
end;

function TmncSQLVAR.GetAsBoolean: Boolean;
begin
  Result := False;
  if not IsNull then
    case SqlDef of
      SQL_INT64:
        Result := PInt64(SqlData)^ <> ISC_FALSE;
      SQL_LONG:
        Result := PLong(SqlData)^ <> ISC_FALSE;
      SQL_SHORT, SQL_BOOLEAN:
        Result := PShort(SqlData)^ <> ISC_FALSE
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

procedure TmncSQLVAR.SetAsBoolean(const AValue: Boolean);
begin
  if IsNullable then
    IsNull := False;
  if AValue then
    PShort(SqlData)^ := ISC_TRUE
  else
    PShort(SqlData)^ := ISC_FALSE;
end;

procedure TmncSQLVAR.CopySQLVAR(const AValue: TmncSQLVAR);
var
  local_sqlind: PShort;
  local_sqldata: PByte;
  local_sqllen: Integer;
begin
  local_sqlind := SqlInd;
  local_sqldata := SqlData;
  Move(AValue.FXSQLVAR^, FXSQLVAR^, SizeOf(TXSQLVAR));
  // Now make new value
  SqlInd := local_sqlind;
  SqlData := local_sqldata;
  if (AValue.SqlType and 1 = 1) then
  begin
    if (SqlInd = nil) then
      FBAlloc(FXSQLVAR.SqlInd, 0, SizeOf(Short));
    SqlInd^ := AValue.SqlInd^;
  end
  else if (SqlInd <> nil) then
    FBFree(FXSQLVAR.SqlInd);
  if ((SqlDef) = SQL_VARYING) then
    local_sqllen := SqlLen + 2
  else
    local_sqllen := SqlLen;
  SqlScale := AValue.SqlScale;
  UpdateData(0, local_sqllen);
  Move(AValue.SqlData[0], SqlData[0], local_sqllen);
  Modified := True;
end;

destructor TmncSQLVAR.Destroy;
begin
  UpdateData(0, 0);
  inherited;
end;

procedure TmncSQLVAR.SetModified(const AValue: Boolean);
begin
  FModified := AValue;
end;

function TmncSQLVAR.GetAsHex: string;
var
  s: string;
begin
  s := GetAsString;
  SetLength(Result, length(s) * 2);
  BinToHex(PAnsiChar(s), PAnsiChar(Result), length(s));
end;

procedure TmncSQLVAR.SetAsHex(const AValue: string);
var
  s: string;
begin
  SetLength(s, length(AValue) div 2);
  HexToBin(PChar(AValue), @s[1], length(s));
  AsString := s;
end;

function TmncSQLVAR.GetAsText: string;
begin
  if (SqlDef = SQL_BLOB) and (SqlSubtype <> 1) then
    Result := AsHex
  else
    Result := AsString;
end;

procedure TmncSQLVAR.SetAsText(const AValue: string);
begin
  if (SqlDef = SQL_BLOB) and (SqlSubtype <> 1) then
    AsHex := AValue
  else
    AsString := AValue;
end;

procedure TmncSQLVAR.SetBuffer(buffer: Pointer; Size: Integer);
var
  sz: PByte;
  len: Integer;
begin
  sz := SqlData;
  if (SqlDef = SQL_TEXT) then
    len := SqlLen
  else
  begin
    len := FBClient.isc_vax_integer(SqlData, 2);
    Inc(sz, 2);
  end;
  if (Size <> 0) and (len > Size) then
    len := Size;
  Move(sz^, buffer^, len);
end;

function TmncSQLVAR.GetAsGUID: TGUID;
begin
end;

procedure TmncSQLVAR.SetAsGUID(const AValue: TGUID);
begin
end;

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

  function TryGetProcAddr(ProcName: string): Pointer;
  begin
    Result := GetProcAddress(CurLibrary, PChar(ProcName));
  end;

  function GetProcAddr(ProcName: string): Pointer;
  begin
    Result := GetProcAddress(CurLibrary, PChar(ProcName));
    if not Assigned(Result) then
      RaiseLastOSError;
  end;

var
  OldDir: string;
begin
  if (FInstancePath = '') or not FileExists(FInstancePath + 'bin\' + GetClientName) then
    FLibrary := LoadLibrary(PChar(GetClientName))
  else
  begin
    OldDir := GetCurrentDir;
    SetCurrentDir(FInstancePath + 'bin\');

    try
      FLibrary := LoadLibrary(PChar(FInstancePath + 'bin\' + GetClientName));
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
  if (FLibrary = 0) then
    LoadClientLibrary;
  if (FLibrary = 0) then
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

function TCustomFBClient.BLOB_put(isc_arg1: byte; isc_arg2: PBSTREAM): Int;
begin
  Result := FBLOB_put(isc_arg1, isc_arg2);
end;

function TCustomFBClient.isc_add_user(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS;
begin
  Result := Fisc_add_user(status_vector, user_sec_data);
end;

function TCustomFBClient.isc_array_get_slice(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
begin
  Result := Fisc_array_get_slice(status_vector, db_handle, trans_handle, array_id, descriptor, dest_array, slice_length);
end;

function TCustomFBClient.isc_array_lookup_bounds(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := Fisc_array_lookup_bounds(status_vector, db_handle, trans_handle, table_name, column_name, descriptor);
end;

function TCustomFBClient.isc_array_lookup_desc(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := Fisc_array_lookup_desc(status_vector, db_handle, trans_handle, table_name, column_name, descriptor);
end;

function TCustomFBClient.isc_array_put_slice(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
begin
  Result := Fisc_array_put_slice(status_vector, db_handle, trans_handle, array_id, descriptor, source_array, slice_length);
end;

function TCustomFBClient.isc_array_set_desc(status_vector: PISC_STATUS; table_name, column_name: PByte; sql_dtype, sql_length, sql_dimensions: PShort; descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := Fisc_array_set_desc(status_vector, table_name, column_name, sql_dtype, sql_length, sql_dimensions, descriptor);
end;

function TCustomFBClient.isc_attach_database(status_vector: PISC_STATUS; db_name_length: Short; db_name: PByte; db_handle: PISC_DB_HANDLE; parm_buffer_length: Short; parm_buffer: PByte): ISC_STATUS;
begin
  Result := Fisc_attach_database(status_vector, db_name_length, db_name, db_handle, parm_buffer_length, parm_buffer);
end;

procedure TCustomFBClient.isc_blob_default_desc(descriptor: PISC_BLOB_DESC; table_name, column_name: PUChar);
begin
  Fisc_blob_default_desc(descriptor, table_name, column_name);
end;

function TCustomFBClient.isc_blob_gen_bpb(status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC; bpb_buffer_length: UShort; bpb_buffer: PUChar; bpb_length: PUShort): ISC_STATUS;
begin
  Result := Fisc_blob_gen_bpb(status_vector, to_descriptor, from_descriptor, bpb_buffer_length, bpb_buffer, bpb_length);
end;

function TCustomFBClient.isc_blob_info(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short; item_list_buffer: PByte; result_buffer_length: Short; result_buffer: PByte): ISC_STATUS;
begin
  Result := Fisc_blob_info(status_vector, blob_handle, item_list_buffer_length, item_list_buffer, result_buffer_length, result_buffer);
end;

function TCustomFBClient.isc_blob_lookup_desc(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_BLOB_DESC; global: PUChar): ISC_STATUS;
begin
  Result := Fisc_blob_lookup_desc(status_vector, db_handle, trans_handle, table_name, column_name, descriptor, global);
end;

function TCustomFBClient.isc_blob_set_desc(status_vector: PISC_STATUS; table_name, column_name: PByte; subtype, charset, segment_size: Short; descriptor: PISC_BLOB_DESC): ISC_STATUS;
begin
  Result := Fisc_blob_set_desc(status_vector, table_name, column_name, subtype, charset, segment_size, descriptor);
end;

function TCustomFBClient.isc_cancel_blob(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := Fisc_cancel_blob(status_vector, blob_handle);
end;

function TCustomFBClient.isc_cancel_events(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
begin
  Result := Fisc_cancel_events(status_vector, db_handle, event_id);
end;

function TCustomFBClient.isc_close_blob(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := Fisc_close_blob(status_vector, blob_handle);
end;

function TCustomFBClient.isc_commit_retaining(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_commit_retaining(status_vector, tran_handle);
end;

function TCustomFBClient.isc_commit_transaction(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_commit_transaction(status_vector, tran_handle);
end;

function TCustomFBClient.isc_create_blob2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short; bpb_address: PByte): ISC_STATUS;
begin
  Result := Fisc_create_blob2(status_vector, db_handle, tran_handle, blob_handle, blob_id, bpb_length, bpb_address);
end;

function TCustomFBClient.isc_database_info(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short; item_list_buffer: PByte; result_buffer_length: Short; result_buffer: PByte): ISC_STATUS;
begin
  Result := Fisc_database_info(status_vector, db_handle, item_list_buffer_length, item_list_buffer, result_buffer_length, result_buffer);
end;

procedure TCustomFBClient.isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
begin
  Fisc_decode_date(ib_date, tm_date);
end;

procedure TCustomFBClient.isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
begin
  Fisc_decode_sql_date(ib_date, tm_date);
end;

procedure TCustomFBClient.isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
begin
  Fisc_decode_sql_time(ib_time, tm_date);
end;

procedure TCustomFBClient.isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure);
begin
  Fisc_decode_timestamp(ib_timestamp, tm_date);
end;

function TCustomFBClient.isc_delete_user(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS;
begin
  Result := Fisc_delete_user(status_vector, user_sec_data);
end;

function TCustomFBClient.isc_detach_database(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := Fisc_detach_database(status_vector, db_handle);
end;

function TCustomFBClient.isc_drop_database(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := Fisc_drop_database(status_vector, db_handle);
end;

function TCustomFBClient.isc_dsql_alloc_statement2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := Fisc_dsql_alloc_statement2(status_vector, db_handle, stmt_handle);
end;

function TCustomFBClient.isc_dsql_describe(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_describe(status_vector, stmt_handle, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_describe_bind(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_describe_bind(status_vector, stmt_handle, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_execute(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_execute(status_vector, tran_handle, stmt_handle, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_execute_immediate(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: UShort; statement: PByte; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_execute_immediate(status_vector, db_handle, tran_handle, length, statement, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_execute2(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_execute2(status_vector, tran_handle, stmt_handle, dialect, in_xsqlda, out_xsqlda);
end;

function TCustomFBClient.isc_dsql_fetch(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_fetch(status_vector, stmt_handle, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_free_statement(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; options: UShort): ISC_STATUS;
begin
  Result := Fisc_dsql_free_statement(status_vector, stmt_handle, options);
end;

function TCustomFBClient.isc_dsql_prepare(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; length: UShort; statement: PByte; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := Fisc_dsql_prepare(status_vector, tran_handle, stmt_handle, length, statement, dialect, xsqlda);
end;

function TCustomFBClient.isc_dsql_set_cursor_name(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; cursor_name: PByte; _type: UShort): ISC_STATUS;
begin
  Result := Fisc_dsql_set_cursor_name(status_vector, stmt_handle, cursor_name, _type);
end;

function TCustomFBClient.isc_dsql_sql_info(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PByte; buffer_length: Short; buffer: PByte): ISC_STATUS;
begin
  Result := Fisc_dsql_sql_info(status_vector, stmt_handle, item_length, items, buffer_length, buffer);
end;

procedure TCustomFBClient.isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
begin
  Fisc_encode_date(tm_date, ib_date);
end;

procedure TCustomFBClient.isc_encode_sql_date(tm_date: PCTimeStructure; ib_date: PISC_DATE);
begin
  Fisc_encode_sql_date(tm_date, ib_date);
end;

procedure TCustomFBClient.isc_encode_sql_time(tm_date: PCTimeStructure; ib_time: PISC_TIME);
begin
  Fisc_encode_sql_time(tm_date, ib_time);
end;

procedure TCustomFBClient.isc_encode_timestamp(tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP);
begin
  Fisc_encode_timestamp(tm_date, ib_timestamp);
end;

type
  Tsib_event_block = function(EventBuffer, ResultBuffer: PPByte; IDCount: UShort; Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event9, Event10, Event11, Event12, Event13, Event14, Event15: PByte): ISC_LONG; cdecl;

function TCustomFBClient.isc_event_block(event_buffer, result_buffer: PPByte; id_count: UShort; event_list: array of PByte): ISC_LONG;
begin
  Result := Tsib_event_block(Fisc_event_block)(event_buffer, result_buffer, id_count, event_list[0], event_list[1], event_list[2], event_list[3], event_list[4], event_list[5], event_list[6], event_list[7], event_list[8], event_list[9], event_list[10],
    event_list[11], event_list[12], event_list[13], event_list[14]);
end;

procedure TCustomFBClient.isc_event_counts(status_vector: PISC_STATUS; buffer_length: Short; event_buffer, result_buffer: PByte);
begin
  Fisc_event_counts(status_vector, buffer_length, event_buffer, result_buffer);
end;

function TCustomFBClient.isc_free(isc_arg1: PByte): ISC_LONG;
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

procedure TCustomFBClient.isc_get_client_version(buffer: PByte);
begin
  Fisc_get_client_version(buffer);
end;

function TCustomFBClient.isc_get_segment(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PUShort; seg_buffer_length: UShort; seg_buffer: PByte): ISC_STATUS;
begin
  Result := Fisc_get_segment(status_vector, blob_handle, actual_seg_length, seg_buffer_length, seg_buffer);
end;

function TCustomFBClient.isc_interprete(buffer: PByte; var status_vector: PISC_STATUS): ISC_STATUS;
begin
  Result := Fisc_interprete(buffer, @status_vector);
end;

function TCustomFBClient.fb_interpret(buffer: PByte; length: Short; var status_vector: PISC_STATUS): ISC_STATUS;
begin
  Result := Ffb_interpret(buffer, length, @status_vector);
end;

function TCustomFBClient.isc_modify_user(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS;
begin
  Result := Fisc_modify_user(status_vector, user_sec_data);
end;

function TCustomFBClient.isc_open_blob2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short; bpb_buffer: PByte): ISC_STATUS;
begin
  Result := Fisc_open_blob2(status_vector, db_handle, tran_handle, blob_handle, blob_id, bpb_length, bpb_buffer);
end;

function TCustomFBClient.isc_prepare_transaction(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_prepare_transaction(status_vector, tran_handle);
end;

function TCustomFBClient.isc_prepare_transaction2(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; msg_length: Short; Msg: PByte): ISC_STATUS;
begin
  Result := Fisc_prepare_transaction2(status_vector, tran_handle, msg_length, Msg);
end;

function TCustomFBClient.isc_put_segment(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: UShort; seg_buffer: PByte): ISC_STATUS;
begin
  Result := Fisc_put_segment(status_vector, blob_handle, seg_buffer_len, seg_buffer);
end;

function TCustomFBClient.isc_que_events(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short; event_buffer: PByte; event_function: TISC_EVENT_CALLBACK; event_function_arg: PVoid): ISC_STATUS;
begin
  Result := Fisc_que_events(status_vector, db_handle, event_id, length, event_buffer, event_function, event_function_arg);
end;

function TCustomFBClient.isc_rollback_retaining(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_rollback_retaining(status_vector, tran_handle);
end;

function TCustomFBClient.isc_rollback_transaction(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := Fisc_rollback_transaction(status_vector, tran_handle);
end;

function TCustomFBClient.isc_service_attach(status_vector: PISC_STATUS; isc_arg2: UShort; isc_arg3: PByte; service_handle: PISC_SVC_HANDLE; isc_arg5: UShort; isc_arg6: PByte): ISC_STATUS;
begin
  Result := Fisc_service_attach(status_vector, isc_arg2, isc_arg3, service_handle, isc_arg5, isc_arg6);
end;

function TCustomFBClient.isc_service_detach(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE): ISC_STATUS;
begin
  Result := Fisc_service_detach(status_vector, service_handle);
end;

function TCustomFBClient.isc_service_query(status_vector: PISC_STATUS; service_handle, recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: PByte; isc_arg8: UShort; isc_arg9: PByte): ISC_STATUS;
begin
  Result := Fisc_service_query(status_vector, service_handle, recv_handle, isc_arg4, isc_arg5, isc_arg6, isc_arg7, isc_arg8, isc_arg9);
end;

function TCustomFBClient.isc_service_start(status_vector: PISC_STATUS; service_handle, recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort; isc_arg5: PByte): ISC_STATUS;
begin
  Result := Fisc_service_start(status_vector, service_handle, recv_handle, isc_arg4, isc_arg5);
end;

procedure TCustomFBClient.isc_sql_interprete(SQLCode: Short; buffer: PByte; buffer_length: Short);
begin
  Fisc_sql_interprete(SQLCode, buffer, buffer_length);
end;

function TCustomFBClient.isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
begin
  Result := Fisc_sqlcode(status_vector);
end;

function TCustomFBClient.isc_start_multiple(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS;
begin
  Result := Fisc_start_multiple(status_vector, tran_handle, db_handle_count, teb_vector_address);
end;

function TCustomFBClient.isc_vax_integer(buffer: PByte; length: Short): ISC_LONG;
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
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
begin
  inherited;
{$IFDEF WINDOWS}
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
{$ENDIF}
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

function TCustomFBClient.Bopen(blob_id: PISC_QUAD; db_handle: TISC_DB_HANDLE; trans_handle: TISC_TR_HANDLE; mode: PByte): PBSTREAM;
begin
  Result := FBopen(blob_id, db_handle, trans_handle, mode);
end;

function TCustomFBClient.Bclose(Stream: PBSTREAM): PISC_STATUS;
begin
  Result := FBclose(Stream);
end;

procedure TCustomFBClient.isc_version(db_handle: PISC_DB_HANDLE; CallbackProc: TISC_VERSION_CALLBACK; isc_arg3: Pointer);
begin
  Fisc_version(db_handle, CallbackProc, isc_arg3);
end;

function TCustomFBClient.isc_portable_integer(buffer: PByte; length: Short): ISC_INT64;
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

{ EFBError }

constructor EFBError.Create(ASQLCode: Long; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
end;

constructor EFBError.Create(ASQLCode: Long; AErrorCode: Long; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
  FErrorCode := AErrorCode;
end;

{ EFBExceptionError }

constructor EFBExceptionError.Create(ASQLCode: Long; AErrorCode: Long; AExceptionID: Integer; AExceptionName, AExceptionMsg: string; Msg: string);
begin
  inherited Create(ASQLCode, AErrorCode, Msg);
  FExceptionID := AExceptionID;
  FExceptionName := AExceptionName;
  FExceptionMsg := AExceptionMsg;
end;

{ TFBBlobStream }

constructor TFBBlobStream.Create(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE);
begin
  inherited Create;
  FDBHandle := DBHandle;
  FTRHandle := TRHandle;
  FBuffer := nil;
  FBlobSize := 0;
end;

destructor TFBBlobStream.Destroy;
var
  StatusVector: TStatusVector;
begin
  if (FHandle <> nil) and (Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, False) > 0) then
    FBRaiseError(StatusVector);
  SetSize(0);
  inherited;
end;

function TFBBlobStream.Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := FBCall(ErrCode, StatusVector, RaiseError);
end;

procedure TFBBlobStream.CheckReadable;
begin
  if FMode = bmWrite then
    FBRaiseError(fbceBlobCannotBeRead, [nil]);
end;

procedure TFBBlobStream.CheckWritable;
begin
  if FMode = bmRead then
    FBRaiseError(fbceBlobCannotBeWritten, [nil]);
end;

procedure TFBBlobStream.CloseBlob;
var
  StatusVector: TStatusVector;
begin
  Finalize;
  if (FHandle <> nil) and (Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, False) > 0) then
    FBRaiseError(StatusVector);
end;

procedure TFBBlobStream.CreateBlob;
begin
  CheckWritable;
  FBlobID.gds_quad_high := 0;
  FBlobID.gds_quad_low := 0;
  Truncate;
end;

procedure TFBBlobStream.EnsureBlobInitialized;
begin
  if not FBlobInitialized then
  begin
    case FMode of
      bmWrite:
        CreateBlob;
      bmReadWrite:
        begin
          if (FBlobID.gds_quad_high = 0) and (FBlobID.gds_quad_low = 0) then
            CreateBlob
          else
            OpenBlob;
        end;
    else
      OpenBlob;
    end;
    FBlobInitialized := True;
  end;
end;

procedure TFBBlobStream.Finalize;
var
  StatusVector: TStatusVector;
begin
  if FBlobInitialized and (FMode <> bmRead) then
  begin
    { need to start writing to a blob, create one }
    Call(FBClient.isc_create_blob2(@StatusVector, FDBHandle, FTRHandle, @FHandle, @FBlobID, 0, nil), StatusVector, True);
    FBWriteBlob(@FHandle, FBuffer, FBlobSize);
    Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, True);
    FModified := False;
  end;
end;

procedure TFBBlobStream.GetBlobInfo;
var
  iBlobSize: Long;
begin
  FBGetBlobInfo(@FHandle, FBlobNumSegments, FBlobMaxSegmentSize, iBlobSize, FBlobType);
  SetSize(iBlobSize);
end;

procedure TFBBlobStream.LoadFromFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TFBBlobStream.LoadFromStream(Stream: TStream);
begin
  CheckWritable;
  EnsureBlobInitialized;
  Stream.Position := 0;
  SetSize(Stream.Size);
  if FBlobSize <> 0 then
    Stream.ReadBuffer(FBuffer^, FBlobSize);
  FModified := True;
end;

procedure TFBBlobStream.OpenBlob;
var
  StatusVector: TStatusVector;
begin
  CheckReadable;
  Call(FBClient.isc_open_blob2(@StatusVector, FDBHandle, FTRHandle, @FHandle, @FBlobID, 0, nil), StatusVector, True);
  try
    GetBlobInfo;
    SetSize(FBlobSize);
    FBReadBlob(@FHandle, FBuffer, FBlobSize);
  except
    Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, False);
    raise;
  end;
  Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, True);
end;

function TFBBlobStream.Read(var buffer; Count: Longint): Longint;
begin
  CheckReadable;
  EnsureBlobInitialized;
  if (Count <= 0) then
  begin
    Result := 0;
    exit;
  end;
  if (FPosition + Count > FBlobSize) then
    Result := FBlobSize - FPosition
  else
    Result := Count;
  Move(FBuffer[FPosition], buffer, Result);
  Inc(FPosition, Result);
end;

procedure TFBBlobStream.SaveToFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TFBBlobStream.SaveToStream(Stream: TStream);
begin
  CheckReadable;
  EnsureBlobInitialized;
  if FBlobSize <> 0 then
  begin
    Seek(0, soFromBeginning);
    Stream.WriteBuffer(FBuffer^, FBlobSize);
  end;
end;

function TFBBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  EnsureBlobInitialized;
  case Origin of
    soFromBeginning:
      FPosition := Offset;
    soFromCurrent:
      Inc(FPosition, Offset);
    soFromEnd:
      FPosition := FBlobSize + Offset; // ask: error must be FBlobSize - Offset
  end;
  Result := FPosition;
end;

procedure TFBBlobStream.SetBlobID(Value: TISC_QUAD);
begin
  FBlobID := Value;
  FBlobInitialized := False;
end;

procedure TFBBlobStream.SetMode(Value: TBlobStreamMode);
begin
  FMode := Value;
  FBlobInitialized := False;
end;

procedure TFBBlobStream.SetSize(NewSize: Long);
begin
  if (NewSize <> FBlobSize) then
  begin
    if NewSize = 0 then
    begin
      FreeMem(FBuffer);
      FBuffer := nil;
    end
    else if Assigned(FBuffer) then
      ReallocMem(FBuffer, NewSize)
    else
      GetMem(FBuffer, NewSize);
    FBlobSize := NewSize;
  end;
end;

{
  procedure TFBBlobStream.SetTransaction(Value: TFBTransaction);
  begin
  FBase.Transaction := Value;
  FBlobInitialized := False;
  end;
}
procedure TFBBlobStream.Truncate;
begin
  SetSize(0);
end;

function TFBBlobStream.Write(const buffer; Count: Longint): Longint;
begin
  CheckWritable;
  EnsureBlobInitialized;
  Result := Count;
  if Count <= 0 then
    exit;
  if (FPosition + Count > FBlobSize) then
    SetSize(FPosition + Count);
  Move(buffer, FBuffer[FPosition], Count);
  Inc(FPosition, Count);
  FModified := True;
end;

procedure TFBBlobStream.Cancel;
begin
  if (not FBlobInitialized) or (FMode = bmRead) then
    exit;
  if FModified then
    OpenBlob;
  FModified := False;
end;

{ TXSQLDAHelper }

function TXSQLDAHelper.GetAliasName: string;
begin
  Result := UTF8ToString(AliasName);
  SetLength(Result, aliasname_length);
end;

function TXSQLDAHelper.GetOwnName: string;
begin
  Result := UTF8ToString(OwnName);
  SetLength(Result, ownname_length);
end;

function TXSQLDAHelper.GetRelName: string;
begin
  Result := UTF8ToString(RelName);
  SetLength(Result, relname_length);
end;

function TXSQLDAHelper.GetSqlName: string;
begin
  Result := UTF8ToString(SqlName);
  SetLength(Result, Sqlname_length);
end;

procedure TXSQLDAHelper.SetAliasName(const Value: string);
var
  s: RawByteString;
begin
  s := UTF8Encode(Value);
  Move(PByte(s)^, PByte(AliasName[0])^, length(s));
  aliasname_length := length(s);
end;

procedure TXSQLDAHelper.SetOwnName(const Value: string);
var
  s: RawByteString;
begin
  s := UTF8Encode(Value);
  Move(PByte(s)^, PByte(OwnName[0])^, length(s));
  ownname_length := length(s);
end;

procedure TXSQLDAHelper.SetRelName(const Value: string);
var
  s: RawByteString;
begin
  s := UTF8Encode(Value);
  Move(PByte(s)^, PByte(RelName[0])^, length(s));
  relname_length := length(s);
end;

procedure TXSQLDAHelper.SetSqlName(const Value: string);
var
  s: RawByteString;
begin
  s := UTF8Encode(Value);
  Move(PByte(s)^, PByte(SqlName[0])^, length(s));
  Sqlname_length := length(s);
end;

initialization

finalization

FreeAndNil(FFBClient);

end.
