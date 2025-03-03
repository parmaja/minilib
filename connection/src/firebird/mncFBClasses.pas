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
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @comment   for Firebird 2.5
 *}

interface

uses
  SysUtils, Classes, Variants,
  StrUtils, mnUtils,
  mncFBHeader;

const
  DefaultBlobSegmentSize = 16 * 1024;

  sCRLF = #13#10;
  sCR   = #$0D;
  sLF   = #$0A;
  sTAB  = #9;
  sNULL_TERMINATOR = #0;
  {$ifdef WINDOWS}
  sLineFeed = sCRLF;
  {$else}
  sLineFeed = sLF;
  {$endif}

resourcestring
{ generic strings used in code }
  SFBDatabaseEditor = 'Da&tabase Editor...';
  SFBTransactionEditor = '&Transaction Editor...';
  SDatabaseFilter = 'Database Files (*.fdb)|*.fdb|All files (*.*)|*.*';
  SDisconnectDatabase = 'Database is currently connected. Disconnect and continue?';
  SCommitTransaction = 'Transaction is currently Active. Rollback and continue?';
  SExecute = 'E&xecute';
  SNoDataSet = 'No dataset association';
  SSQLGenSelect = 'Must select at least one key field and one update field';
  SSQLNotGenerated = 'Update SQL statements not generated, exit anyway?';
  SFBUpdateSQLEditor = '&UpdateSQL Editor...';
  SFBDataSetEditor = '&Dataset Editor...';
  SSQLDataSetOpen = 'Unable to determine field names for %s';
  STransaction = '%s, Default';

{ strings used in error messages}
  SUnknownError = 'Unknown error';
  SFirebirdInstallMissing = 'Firebird Install DLL ibinstall.dll not found in the path. Please install Firebird 6 to use this functionality';
  SFBCfeature = '%s is an Firebird 2.0 function. Please upgrade to Firebird 6 to use this functonality';
  SNotSupported = 'Unsupported feature';
  SNotPermitted = 'Not permitted';
  SFileAccessError = 'Temporary file access error';
  SConnectionTimeout = 'Database connection timed out';
  SCannotSetDatabase = 'Cannot set database';
  SCannotSetTransaction = 'Cannot set transaction';
  SOperationCancelled = 'Operation cancelled at user''s request';
  SDPBConstantNotSupported = 'DPB Constant (isc_dpb_%s) is unsupported';
  SDPBConstantUnknown = 'DPB Constant (%d) is unknown';
  STPBConstantNotSupported = 'TPB Constant (isc_tpb_%s) is unsupported';
  STPBConstantUnknown = 'TPB Constant (%d) is unknown';
  SDatabaseClosed = 'Cannot perform operation -- DB is not open';
  SDatabaseOpen = 'Cannot perform operation -- DB is currently open';
  SDatabaseNameMissing = 'Database name is missing';
  SNotInTransaction = 'Transaction is not active';
  SInTransaction = 'Transaction is active';
  STimeoutNegative = 'Timeout values cannot be negative';
  SUpdateWrongDB = 'Updating wrong database';
  SUpdateWrongTR = 'Updating wrong transaction. Unique transaction expected in set';
  SDatabaseNotAssigned = 'Database not assigned';
  STransactionNotAssigned = 'Transaction not assigned';
  SXSQLDAIndexOutOfRange = 'XSQLDA index out of range';
  SXSQLDANameDoesNotExist = 'XSQLDA name does not exist (%s)';
  SEOF = 'End of file';
  SBOF = 'Beginning of file';
  SInvalidStatementHandle = 'Invalid statement handle';
  SSQLOpen = 'FBDSQL Open';
  SSQLClosed = 'FBDSQL Closed';
  SDatasetOpen = 'Dataset open';
  SDatasetClosed = 'Dataset closed';
  SUnknownSQLDataType = 'Unknown SQL Data type (%d)';
  SInvalidColumnIndex = 'Invalid column index (index exceeds permitted range)';
  SInvalidParamColumnIndex = 'Invalid parameter index (index exceeds permitted range)';
  SInvalidDataConversion = 'Invalid data conversion';
  SColumnIsNotNullable = 'Column cannot be set to null (%s)';
  SBlobCannotBeRead = 'Blob stream cannot be read';
  SBlobCannotBeWritten = 'Blob stream cannot be written';
  SEmptyQuery = 'Empty query';
  SCannotOpenNonSQLSelect = 'Cannot "open" a non-select statement. Use ExecQuery';
  SNoFieldAccess = 'No access to field "%s"';
  SFieldReadOnly = 'Field "%s" is read-only';
  SFieldNotFound = 'Field "%s" not found';
  SNotEditing = 'Not in edit mode';
  SCannotInsert = 'Cannot insert into dataset. (No insert query)';
  SCannotPost = 'Cannot post. (No update/insert query)';
  SCannotUpdate = 'Cannot update. (No update query)';
  SCannotDelete = 'Cannot delete from dataset. (No delete query)';
  SCannotRefresh = 'Cannot refresh row. (No refresh query)';
  SBufferNotSet = 'Buffer not set';
  SCircularReference = 'Circular references not permitted';
  SSQLParseError = 'SQL Parse Error:' + sLineFeed + sLineFeed + '%s';
  SUserAbort = 'User abort';
  SDataSetUniDirectional = 'Data set is uni-directional';
  SCannotCreateSharedResource = 'Cannot create shared resource. (Windows error %d)';
  SWindowsAPIError = 'Windows API error. (Windows error %d [$%.8x])';
  SColumnListsDontMatch = 'Column lists do not match';
  SColumnTypesDontMatch = 'Column types don''t match. (From index: %d; To index: %d)';
  SFieldUnsupportedType = 'Unsupported Field Type';
  SCircularDataLink = 'Circular DataLink Reference';
  SEmptySQLStatement = 'Empty SQL Statement';
  SIsASelectStatement = 'use Open for a Select Statement';
  SRequiredParamNotSet = 'Required Param value not set';
  SNoStoredProcName = 'No Stored Procedure Name assigned';
  SIsAExecuteProcedure = 'use ExecProc for Procedure; use TQuery for Select procedures';
  SUpdateFailed = 'Update Failed';
  SNotCachedUpdates = 'CachedUpdates not enabled';
  SNotLiveRequest = 'Request is not live - cannot modify';
  SNoProvider = 'No Provider';
  SNoRecordsAffected = 'No Records Affected';
  SNoTableName = 'No Table Name assigned';
  SCannotCreatePrimaryIndex = 'Cannot Create Primary Index; are created automatically';
  SCannotDropSystemIndex = 'Cannot Drop System Index';
  STableNameMismatch = 'Table Name Mismatch';
  SIndexFieldMissing = 'Index Field Missing';
  SInvalidCancellation = 'Cannot Cancel events while processing';
  SInvalidEvent = 'Invalid Event';
  SMaximumEvents = 'Exceded Maximum Event limits';
  SNoEventsRegistered = 'No Events Registered';
  SInvalidQueueing = 'Invalid Queueing';
  SInvalidRegistration = 'Invalid Registration';
  SInvalidBatchMove = 'Invalid Batch Move';
  SSQLDialectInvalid = 'SQL Dialect Invalid';
  SSPBConstantNotSupported = 'SPB Constant Not supported';
  SSPBConstantUnknown = 'SPB Constant Unknown';
  SServiceActive = 'Cannot perform operation -- service is not attached';
  SServiceInActive = 'Cannot perform operation -- service is attached';
  SServerNameMissing = 'Server Name Missing';
  SQueryParamsError = 'Query Parameters missing or incorrect';
  SStartParamsError = 'start Parameters missing or incorrect';
  SOutputParsingError = 'Unexpected Output buffer value';
  SUseSpecificProcedures = 'Generic ServiceStart not applicable: Use Specific Procedures to set configuration params';
  SSQLMonitorAlreadyPresent = 'SQL Monitor Instance is already present';
  SCantPrintValue = 'Cannot print value';
  SEOFReached = 'SEOFReached';
  SEOFInComment = 'EOF in comment detected';
  SEOFInString = 'EOF in string detected';
  SParamNameExpected = 'Parameter name expected';
  SSuccess = 'Successful execution';
  SException = 'Exception %s';
  SNoOptionsSet = 'No Install Options selected';
  SNoDestinationDirectory = 'DestinationDirectory is not set';
  SNosourceDirectory = 'SourceDirectory is not set';
  SNoUninstallFile = 'Uninstall File Name is not set';
  SOptionNeedsClient = '%s component requires Client to function properly';
  SOptionNeedsServer = '%s component requires Server to function properly';
  SInvalidOption = 'Invalid option specified';
  SInvalidOnErrorResult = 'Unexpected onError return value';
  SInvalidOnStatusResult = 'Unexpected onStatus return value';

  SFirebirdExpressVersion = 'FirebirdExpress 1.1';
  SEditSQL = 'Edit SQL';
  SDPBConstantUnknownEx = 'DPB Constant (%s) is unknown';
  STPBConstantUnknownEx = 'TPB Constant (%s) is unknown';
  SFirebirdExpressVersionEx = 'FirebirdExpress %g';
  SUnknownPlan = 'Unknown Error - Can''t retrieve plan';
  SFieldSizeMismatch = 'Size Mismatch - Field %s size is too small for data';
  SEventAlreadyRegistered   = 'Events already registered';
  SStringTooLarge = 'Trying to store a string of length %d into a field that can only contain %d';
  SFBServiceEditor = '&Service Editor ...';
  SFBSuccessConnect = 'Successful Connection';
  SFBInvalidStatement = 'Invalid statement';
  SFBInvalidComment = 'Invalid Comment';

const
  DPBPrefix = 'isc_dpb_';
  DPBConstantNames: array[1..isc_dpb_last_dpb_constant] of string = (
    'cdd_pathname',
    'allocation',
    'journal',
    'page_size',
    'num_buffers',
    'buffer_length',
    'debug',
    'garbage_collect',
    'verify',
    'sweep',
    'enable_journal',
    'disable_journal',
    'dbkey_scope',
    'number_of_users',
    'trace',
    'no_garbage_collect',
    'damaged',
    'license',
    'sys_user_name',
    'encrypt_key',
    'activate_shadow',
    'sweep_interval',
    'delete_shadow',
    'force_write',
    'begin_log',
    'quit_log',
    'no_reserve',
    'user_name',
    'password',
    'password_enc',
    'sys_user_name_enc',
    'interp',
    'online_dump',
    'old_file_size',
    'old_num_files',
    'old_file',
    'old_start_page',
    'old_start_seqno',
    'old_start_file',
    'drop_walfile',
    'old_dump_id',
    'wal_backup_dir',
    'wal_chkptlen',
    'wal_numbufs',
    'wal_bufsize',
    'wal_grp_cmt_wait',
    'lc_messages',
    'lc_ctype',
    'cache_manager',
    'shutdown',
    'online',
    'shutdown_delay',
    'reserved',
    'overwrite',
    'sec_attach',
    'disable_wal',
    'connect_timeout',
    'dummy_packet_interval',
    'gbak_attach',
    'sql_role_name',
    'set_page_buffers',
    'working_directory',
    'sql_dialect',
    'set_db_readonly',
    'set_db_sql_dialect',
    'gfix_attach',
    'gstat_attach',
    'set_db_charset',
    'gsec_attach',
    'address_pat',
    'process_id ',
    'no_db_trigg',
    'trusted_auth',
    'process_name',
    'trusted_role ',
    'org_filename',
    'utf8_filename',
    'ext_call_depth',
    'dpb_auth_block',
    'dpb_client_version',
    'dpb_remote_protocol',
    'dpb_host_name',
    'dpb_os_user',
    'dpb_specific_auth_data',
    'dpb_auth_plugin_list',
    'dpb_auth_plugin_name',
    'dpb_config',
    'dpb_nolinger',
    'dpb_reset_icu',
    'dpb_map_attach',
    'dpb_session_time_zone',
    'dpb_set_db_replica',
    'dpb_set_bind',
    'dpb_decfloat_round',
    'dpb_decfloat_traps',
    'dpb_clear_map'
  );

  TPBPrefix = 'isc_tpb_';
  TPBConstantNames: array[1..isc_tpb_last_tpb_constant] of string = (
    'consistency',
    'concurrency',
    'shared',
    'protected',
    'exclusive',
    'wait',
    'nowait',
    'read',
    'write',
    'lock_read',
    'lock_write',
    'verb_time',
    'commit_time',
    'ignore_limbo',
    'read_committed',
    'autocommit',
    'rec_version',
    'no_rec_version',
    'restart_requests',
    'no_auto_undo',
    'lock_timeout', //fb2
    'tpb_read_consistency',
    'tpb_at_snapshot_number'
  );

  QUOTE = '''';
  DBL_QUOTE = '"';

  XSQLVAR_SIZE                             = sizeof(TXSQLVAR);

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

  TFBBlobStream = class(TStream)
  private
    // FBase: TFBBase;
    FBlobInitialized: Boolean;
    FBlobID: TISC_QUAD;
    FBlobMaxSegmentSize: Long;
    FBlobNumSegments: Long;
    FBlobSize: Long;
    FBlobType: SmallInt; { 0 = segmented, 1 = streamed }
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
    procedure SetSize(const NewSize: Int64); override;
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

    procedure Truncate;
    function Write(const buffer; Count: Longint): Longint; override;
    property Handle: TISC_BLOB_HANDLE read FHandle;
    property BlobID: TISC_QUAD read FBlobID write SetBlobID;
    property BlobMaxSegmentSize: Long read FBlobMaxSegmentSize;
    property BlobNumSegments: Long read FBlobNumSegments;
    property BlobSize: Long read FBlobSize;
    property BlobType: SmallInt read FBlobType;
    property mode: TBlobStreamMode read FMode write SetMode;
    property Modified: Boolean read FModified;
  end;

  { TmncSQLVAR }

  TmncSQLVAR = class(TObject)
  private
    FXSQLVAR: PXSQLVAR;
    FIgnored: Boolean;
    FModified: Boolean;
    FMaxLen: SmallInt;
    function GetSqlDef: SmallInt;
  protected
    function GetSQLVAR: PXSQLVAR;
    procedure SetSQLVAR(const AValue: PXSQLVAR);
    function GetAliasName: string;
    function GetOwnName: string;
    function GetRelName: string;
    function GetSqlData: PByte;
    function GetSqlInd: PSmallInt;
    function GetSqlLen: SmallInt;
    function GetSqlName: string;
    function GetSqlPrecision: SmallInt;
    function GetSqlScale: SmallInt;
    function GetSqlSubtype: SmallInt;
    function GetSqlType: SmallInt;
    procedure SetAliasName(const AValue: string);
    procedure SetOwnName(const AValue: string);
    procedure SetRelName(const AValue: string);
    procedure SetSqlName(const AValue: string);
    procedure SetSqlData(const AValue: PByte);
    procedure SetSqlInd(const AValue: PSmallInt);
    procedure SetSqlLen(const AValue: SmallInt);
    procedure SetSqlPrecision(const AValue: SmallInt);
    procedure SetSqlScale(const AValue: SmallInt);
    procedure SetSqlSubtype(const AValue: SmallInt);
    procedure SetSqlType(const AValue: SmallInt);
  public
    procedure UpdateData(OldSize, NewSize: Integer);
    procedure UpdateSQLInd;
    property XSQLVar: PXSQLVAR read GetSQLVAR write SetSQLVAR;

    property SqlType: SmallInt read GetSqlType write SetSqlType;
    property SqlDef: SmallInt read GetSqlDef;
    property SqlScale: SmallInt read GetSqlScale write SetSqlScale;
    property SqlPrecision: SmallInt read GetSqlPrecision write SetSqlPrecision;
    property SqlSubtype: SmallInt read GetSqlSubtype write SetSqlSubtype;
    property SqlLen: SmallInt read GetSqlLen write SetSqlLen;
    property SqlData: PByte read GetSqlData write SetSqlData;
    property SqlInd: PSmallInt read GetSqlInd write SetSqlInd;

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
    function GetAsShort: SmallInt;
    function GetAsString: string;
    function GetAsVariant: Variant;
    function GetIsNull: Boolean;
    function GetIsNullable: Boolean;
    function GetSize: Integer;
    procedure SetAsCurrency(AValue: Currency);
    procedure SetAsInt64(AValue: Int64);
    procedure SetAsDate(AValue: TDateTime);
    procedure SetAsLong(AValue: Long);
    procedure SetAsTime(AValue: TDateTime);
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsDouble(AValue: Double);
    procedure SetAsFloat(AValue: Double);
    procedure SetAsPointer(AValue: Pointer);
    procedure SetAsQuad(AValue: TISC_QUAD);
    procedure SetAsShort(AValue: SmallInt);
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
    function GetBytes: TBytes;
    procedure SetBytes(const Value: TBytes);
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
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
    property MaxLen: SmallInt read FMaxLen write FMaxLen;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsID: Int64 read GetAsInt64 write SetAsInt64; // More flixable names
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsLong: Long read GetAsLong write SetAsLong;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property AsShort: SmallInt read GetAsShort write SetAsShort;
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
    property AsBytes: TBytes read GetBytes write SetBytes;

    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;

  end;

  { TXSQLDAHelper }

  TXSQLVARHelper = record helper for TXSQLVAR
    procedure SetAliasName(const Value: string);
    procedure SetOwnName(const Value: string);
    procedure SetRelName(const Value: string);
    procedure SetSqlName(const Value: string);

    function GetAliasName: string;
    function GetOwnName: string;
    function GetRelName: string;
    function GetSqlName: string;
    procedure Clean;
  protected
  end;

function getb(p: PBSTREAM): Byte;
function putb(x: Byte; p: PBSTREAM): Integer;
function putbx(x: Byte; p: PBSTREAM): Integer;
function FBSqlDef(X: SmallInt): SmallInt;

procedure FBGetBlobInfo(hBlobHandle: PISC_BLOB_HANDLE; out NumSegments, MaxSegmentSize, TotalSize: Long; out BlobType: SmallInt);
procedure FBReadBlob(hBlobHandle: PISC_BLOB_HANDLE; buffer: PByte; BlobSize: Long);
procedure FBWriteBlob(hBlobHandle: PISC_BLOB_HANDLE; buffer: PByte; BlobSize: Long);
function FBGetBlob(DBHandle: TISC_DB_HANDLE; TRHandle: TISC_TR_HANDLE; BlobID: PISC_QUAD): PByte;

procedure InitSQLDA(var Data: PXSQLDA; New: Integer; Clean: Boolean = True);
procedure FreeSQLDA(var Data: PXSQLDA; Clean: Boolean = True);

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
  mncFBUtils;

procedure FBGetBlobInfo(hBlobHandle: PISC_BLOB_HANDLE; out NumSegments, MaxSegmentSize, TotalSize: Long; out BlobType: SmallInt);
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

  if FBLib.isc_blob_info(@StatusVector, hBlobHandle, 4, @items[0], SizeOf(results), @results[0]) > 0 then
    FBRaiseError(StatusVector);

  i := 0;
  while (i < SizeOf(results)) and (results[i] <> isc_info_end) do
  begin
    item := Integer(results[i]);
    Inc(i);
    item_length := FBLib.isc_vax_integer(@results[i], 2);
    Inc(i, 2);
    case item of
      isc_info_blob_num_segments:
        NumSegments := FBLib.isc_vax_integer(@results[i], item_length);
      isc_info_blob_max_segment:
        MaxSegmentSize := FBLib.isc_vax_integer(@results[i], item_length);
      isc_info_blob_total_length:
        TotalSize := FBLib.isc_vax_integer(@results[i], item_length);
      isc_info_blob_type:
        BlobType := FBLib.isc_vax_integer(@results[i], item_length);
    end;
    Inc(i, item_length);
  end;
end;

procedure FBReadBlob(hBlobHandle: PISC_BLOB_HANDLE; buffer: PByte; BlobSize: Long);
var
  CurPos: Long;
  BytesRead, SegLen: Word;
  LocalBuffer: PByte;
  StatusVector: TStatusVector;
begin
  CurPos := 0;
  LocalBuffer := buffer;
  SegLen := Word(DefaultBlobSegmentSize * 2);
  while (CurPos < BlobSize) do
  begin
    if (CurPos + SegLen > BlobSize) then
      SegLen := BlobSize - CurPos;
    if not((FBLib.isc_get_segment(@StatusVector, hBlobHandle, @BytesRead, SegLen, LocalBuffer) = 0) or (StatusVector[1] = isc_segment)) then
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
    if FBLib.isc_put_segment(@StatusVector, hBlobHandle, SegLen, PByte(@buffer[CurPos])) > 0 then
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
  with FBLib do
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
      BLOB_close(bStream);
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
    Result := (Integer(p^.bstr_ptr^) and Integer(0377));
    Inc(p^.bstr_ptr);
  end
  else
    Result := FBLib.BLOB_get(p);
end;

function putb(x: byte; p: PBSTREAM): Integer;
{ The C-macro reads like this:
  putb(x,p) ((x == '\n' || (!(--(p)->bstr_cnt))) ?      // then
  BLOB_put (x,p) :                                    // else
  ((Integer) (*(p)->bstr_ptr++ = (unsigned) (x)))) }
begin
  Dec(p^.bstr_cnt);
  if (x = (Integer('n') - Integer('a'))) or (p^.bstr_cnt = 0) then
    Result := FBLib.BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := x;
    Result := x;
    Inc(p^.bstr_ptr^);
  end;
end;

function putbx(x: Byte; p: PBSTREAM): Integer;
{ The C-macro reads like this:
  putbx(x,p) ((!(--(p)->bstr_cnt)) ?    // then
  BLOB_put (x,p) :                    // else
  ((Integer) (*(p)->bstr_ptr++ = (unsigned) (x)))) }
begin
  Dec(p^.bstr_cnt);
  if (p^.bstr_cnt = 0) then
    Result := FBLib.BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := ord(x);
    Inc(p^.bstr_ptr^);
    Result := x;
  end;
end;

function FBSqlDef(X: SmallInt): SmallInt;
begin
  Result := x and (not 1);
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
      p^.Clean;
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
        p^.Clean;
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

function TmncSQLVAR.GetSqlInd: PSmallInt;
begin
  Result := FXSQLVAR^.SqlInd;
end;

function TmncSQLVAR.GetSqlLen: SmallInt;
begin
  Result := FXSQLVAR^.SqlLen;
end;

function TmncSQLVAR.GetSqlName: string;
begin
  Result := FXSQLVAR^.GetSqlName;
end;

function TmncSQLVAR.GetSqlPrecision: SmallInt;
begin
  case SqlDef of
    SQL_SHORT:
      Result := 4;
    SQL_LONG:
      Result := 9;
    SQL_INT64, SQL_INT128:
      Result := 18;
  else
    Result := 0;
  end;
end;

function TmncSQLVAR.GetSqlScale: SmallInt;
begin
  Result := FXSQLVAR^.SqlScale;
end;

function TmncSQLVAR.GetSqlSubtype: SmallInt;
begin
  Result := FXSQLVAR^.SqlSubtype;
end;

function TmncSQLVAR.GetSqlType: SmallInt;
begin
  Result := FXSQLVAR^.SqlType;
end;

function TmncSQLVAR.GetSqlDef: SmallInt;
begin
  Result := FBSqlDef(SqlType);
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
      FBAlloc(FXSQLVAR^.SqlInd, 0, SizeOf(SmallInt))
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

procedure TmncSQLVAR.SetSqlInd(const AValue: PSmallInt);
begin
  FXSQLVAR^.SqlInd := AValue
end;

procedure TmncSQLVAR.SetSqlLen(const AValue: SmallInt);
begin
  FXSQLVAR^.SqlLen := AValue
end;

procedure TmncSQLVAR.SetSqlName(const AValue: string);
begin
  FXSQLVAR^.SetSqlName(AValue);
end;

procedure TmncSQLVAR.SetSqlPrecision(const AValue: SmallInt);
begin
  FBRaiseError(fbceNotSupported, []);
end;

procedure TmncSQLVAR.SetSqlScale(const AValue: SmallInt);
begin
  FXSQLVAR^.SqlScale := AValue
end;

procedure TmncSQLVAR.SetSqlSubtype(const AValue: SmallInt);
begin
  FXSQLVAR^.SqlSubtype := AValue
end;

procedure TmncSQLVAR.SetSqlType(const AValue: SmallInt);
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
  iBlobType: SmallInt;
  StatusVector: TStatusVector;
begin
  szBuff := nil;
  bSourceBlob := True;
  bDestBlob := True;
  s_bhandle := 0;
  d_bhandle := 0;
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
          iSize := FBLib.isc_vax_integer(Source.SqlData, 2);
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
        FBCall(FBLib.isc_open_blob2(@StatusVector, @FDBHandle, @FTRHandle, @s_bhandle, PISC_QUAD(Source.SqlData), 0, nil), StatusVector, True);
        try
          FBGetBlobInfo(@s_bhandle, iSegs, iMaxSeg, iSize, iBlobType);
          szBuff := nil;
          FBAlloc(szBuff, 0, iSize);
          FBReadBlob(@s_bhandle, szBuff, iSize);
        finally
          FBCall(FBLib.isc_close_blob(@StatusVector, @s_bhandle), StatusVector, True);
        end;
      end;

      if bDestBlob then
      begin
        { write the blob }
        FBCall(FBLib.isc_create_blob2(@StatusVector, @FDBHandle, @FTRHandle, @d_bhandle, PISC_QUAD(SqlData), 0, nil), StatusVector, True);
        try
          FBWriteBlob(@d_bhandle, szBuff, iSize);
          IsNull := False;
        finally
          FBCall(FBLib.isc_close_blob(@StatusVector, @d_bhandle), StatusVector, True);
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
      SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP, SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT, SQL_LONG, SQL_INT64, SQL_INT128, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT, SQL_BOOLEAN:
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
        Result := FBScaleCurrency(Int64(PSmallInt(SqlData)^), SqlScale);
      SQL_LONG:
        Result := FBScaleCurrency(Int64(PLong(SqlData)^), SqlScale);
      SQL_INT64, SQL_INT128:
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
      SQL_SHORT: Result := FBScaleInt64(Int64(PSmallInt(SqlData)^), SqlScale);
      SQL_LONG: Result := FBScaleInt64(Int64(PLong(SqlData)^), SqlScale);
      SQL_INT64, SQL_INT128: Result := FBScaleInt64(PInt64(SqlData)^, SqlScale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PSmallInt(SqlData)^ of
          ISC_TRUE:
            Result := 1;
          ISC_FALSE:
            Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsInteger: Integer;
begin
  Result := Integer(GetAsLong);
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
        FBLib.isc_decode_sql_date(PISC_DATE(SqlData), @tm_date);
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
        FBLib.isc_decode_sql_time(PISC_TIME(SqlData), @tm_date);
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
        FBLib.isc_decode_date(PISC_QUAD(SqlData), @tm_date);
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
        Result := FBScaleDouble(Int64(PSmallInt(SqlData)^), SqlScale);
      SQL_LONG:
        Result := FBScaleDouble(Int64(PLong(SqlData)^), SqlScale);
      SQL_INT64, SQL_INT128:
        Result := FBScaleDouble(PInt64(SqlData)^, SqlScale);
      SQL_FLOAT:
        Result := PSingle(SqlData)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        Result := PDouble(SqlData)^;
      SQL_BOOLEAN:
        case PSmallInt(SqlData)^ of
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
        Result := Trunc(FBScaleDouble(Int64(PSmallInt(SqlData)^), SqlScale));
      SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP, SQL_LONG:
        Result := Trunc(FBScaleDouble(Int64(PLong(SqlData)^), SqlScale));
      SQL_INT64, SQL_INT128:
        Result := Trunc(FBScaleDouble(PInt64(SqlData)^, SqlScale));
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PSmallInt(SqlData)^ of
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

function TmncSQLVAR.GetAsShort: SmallInt;
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

begin
  Result := '';
  { Check null, if so return a default string }
  if not IsNull then
    case SqlDef of
      SQL_ARRAY: Result := '[Array]';
      SQL_BLOB:
      begin
        {$ifdef FPC}
        ss := TStringStream.Create('');
        {$else}
        ss := TStringStream.Create('', TEncoding.UTF8);
        {$endif}
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
          str_len := FBLib.isc_vax_integer(SqlData, 2);
          Inc(sz, 2);
        end;

        {SetLength(s, str_len);
        Move(sz^, PByte(s)^, str_len);
        Result := s;}
        if str_len<>0 then
        begin
          if SqlSubtype = fb_text_subtype_binary then
          begin
            SetLength(Result, str_len * 2);
            BinToHex(sz, PChar(Result), str_len);
          end
          else
            Result := TEncoding.UTF8.GetString(sz, str_len);
        end
        else
          Result := '';
      end;
      SQL_TYPE_DATE: Result := FormatDateTime('yyyy-mm-dd', AsDateTime);
      SQL_TYPE_TIME: Result := TimeToStr(AsDateTime);
      SQL_TIMESTAMP: Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', AsDateTime);//use ios format
      SQL_SHORT, SQL_LONG:
      begin
        if SqlScale = 0 then
          Result := IntToStr(AsLong)
        else if SqlScale >= (-4) then
          Result := CurrToStr(AsCurrency)
        else
          Result := FloatToStr(AsDouble);
      end;
      SQL_INT64, SQL_INT128:
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
        if SqlSubtype = isc_blob_text then
          Result := AsString
        else
          Result := '[Blob]';
      end;
      SQL_TEXT, SQL_VARYING:
      begin
        if SqlSubtype = fb_text_subtype_binary then
          Result := AsBytes
        else
          Result := AsString;
      end;
      SQL_TIMESTAMP, SQL_TYPE_DATE, SQL_TYPE_TIME:
        Result := AsDateTime;
      SQL_SHORT, SQL_LONG:
        if SqlScale = 0 then
          Result := AsLong
        else if SqlScale >= (-4) then
          Result := AsCurrency
        else
          Result := AsDouble;
      SQL_INT64, SQL_INT128:
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

function TmncSQLVAR.GetBytes: TBytes;
var
  sz: PByte;
  str_len: Integer;
  ss: TStringStream;
  s: UTF8String;
begin
  Result := nil;
  { Check null, if so return a default string }
  if not IsNull then
    case SqlDef of
      SQL_TEXT, SQL_VARYING:
      begin
        sz := SqlData;
        if (SqlDef = SQL_TEXT) then
          str_len := SqlLen
        else
        begin
          str_len := FBLib.isc_vax_integer(SqlData, 2);
          Inc(sz, 2);
        end;
        SetLength(Result, str_len);
        Move(sz^, PByte(Result)^, str_len);
      end;
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

procedure TmncSQLVAR.SetAsInteger(const Value: Integer);
begin
  SetAsLong(Value);

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
  FBLib.isc_encode_sql_date(@tm_date, PISC_DATE(SqlData));
  Modified := True;
end;

procedure TmncSQLVAR.SetAsLong(AValue: Long);
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
  FBLib.isc_encode_sql_time(@tm_date, PISC_TIME(SqlData));
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
  FBLib.isc_encode_date(@tm_date, PISC_QUAD(SqlData));
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
  SqlLen := SizeOf(Single);
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

procedure TmncSQLVAR.SetAsShort(AValue: SmallInt);
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_SHORT or (SqlType and 1);
  SqlLen := SizeOf(SmallInt);
  SqlScale := 0;
  UpdateData(0, SqlLen);
  PSmallInt(SqlData)^ := AValue;
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
        {$ifdef FPC}
        ss := TStringStream.Create(AValue);
        {$else}
        ss := TStringStream.Create(AValue, TEncoding.UTF8);
        {$endif}
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
      varArray .. varArray + varRecord: //http://www.verycomputer.com/205_228a9bec472bd3dd_1.htm  = VarType 8209 means a byte array. ((varArray + varByte) 8192 + 17)
        AsBytes := AValue;
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
      SQL_INT64,
      SQL_INT128:  Result := PInt64(SqlData)^ <> ISC_FALSE;
      SQL_LONG:    Result := PLong(SqlData)^ <> ISC_FALSE;
      SQL_SHORT:   Result := PSmallInt(SqlData)^ <> ISC_FALSE;
      SQL_BOOLEAN: Result := PSmallInt(SqlData)^ > ISC_FALSE; //sometimes -1
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

procedure TmncSQLVAR.SetAsBoolean(const AValue: Boolean);
begin
  if IsNullable then
    IsNull := False;
  if AValue then
    PSmallInt(SqlData)^ := ISC_TRUE
  else
    PSmallInt(SqlData)^ := ISC_FALSE;
end;

procedure TmncSQLVAR.CopySQLVAR(const AValue: TmncSQLVAR);
var
  local_sqlind: PSmallInt;
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
      FBAlloc(FXSQLVAR.SqlInd, 0, SizeOf(SmallInt));
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
  if (SqlDef = SQL_BLOB) and (SqlSubtype <> isc_blob_text) then
    Result := AsHex
  else
    Result := AsString;
end;

procedure TmncSQLVAR.SetAsText(const AValue: string);
begin
  if (SqlDef = SQL_BLOB) and (SqlSubtype <> isc_blob_text) then
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
    len := FBLib.isc_vax_integer(SqlData, 2);
    Inc(sz, 2);
  end;
  if (Size <> 0) and (len > Size) then
    len := Size;
  Move(sz^, buffer^, len);
end;

procedure TmncSQLVAR.SetBytes(const Value: TBytes);
begin
  if IsNullable then
    IsNull := False;
  SqlType := SQL_TEXT or (SqlType and 1);
  SqlScale := 0;
  SqlLen := Length(Value);
  //SqlSubtype := fb_text_subtype_binary;{ TODO : support sub type }
  UpdateData(0, SqlLen);
  if (SqlLen > 0) then
    Move(Value[0], SqlData^, SqlLen);
  Modified := True;
end;

function TmncSQLVAR.GetAsGUID: TGUID;
begin
end;

procedure TmncSQLVAR.SetAsGUID(const AValue: TGUID);
begin
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
  if (FHandle <> 0) and (Call(FBLib.isc_close_blob(@StatusVector, @FHandle), StatusVector, False) > 0) then
    FBRaiseError(StatusVector);
  Size := 0;
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
  if (FHandle <> 0) and (Call(FBLib.isc_close_blob(@StatusVector, @FHandle), StatusVector, False) > 0) then
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
    Call(FBLib.isc_create_blob2(@StatusVector, FDBHandle, FTRHandle, @FHandle, @FBlobID, 0, nil), StatusVector, True);
    FBWriteBlob(@FHandle, FBuffer, FBlobSize);
    Call(FBLib.isc_close_blob(@StatusVector, @FHandle), StatusVector, True);
    FModified := False;
  end;
end;

procedure TFBBlobStream.GetBlobInfo;
var
  iBlobSize: Long;
begin
  FBGetBlobInfo(@FHandle, FBlobNumSegments, FBlobMaxSegmentSize, iBlobSize, FBlobType);
  Size := iBlobSize;
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
  Size := Stream.Size;
  if FBlobSize <> 0 then
    Stream.ReadBuffer(FBuffer^, FBlobSize);
  FModified := True;
end;

procedure TFBBlobStream.OpenBlob;
var
  StatusVector: TStatusVector;
begin
  CheckReadable;
  Call(FBLib.isc_open_blob2(@StatusVector, FDBHandle, FTRHandle, @FHandle, @FBlobID, 0, nil), StatusVector, True);
  try
    GetBlobInfo;
    //Size := FBlobSize; set in GetBlobInfo
    FBReadBlob(@FHandle, FBuffer, FBlobSize);
  except
    Call(FBLib.isc_close_blob(@StatusVector, @FHandle), StatusVector, False);
    raise;
  end;
  Call(FBLib.isc_close_blob(@StatusVector, @FHandle), StatusVector, True);
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

procedure TFBBlobStream.SetSize(const NewSize: Int64);
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
  Size := 0;
end;

function TFBBlobStream.Write(const buffer; Count: Longint): Longint;
begin
  CheckWritable;
  EnsureBlobInitialized;
  Result := Count;
  if Count <= 0 then
    exit;
  if (FPosition + Count > FBlobSize) then
    Size := FPosition + Count;
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

{ TXSQLVARHelper }

procedure TXSQLVARHelper.Clean;
begin
  FBFree(sqldata);
  FBFree(SqlInd);
end;

function TXSQLVARHelper.GetAliasName: string;
begin
  Result := PUTF8Char(@AliasName);
  SetLength(Result, aliasname_length);
end;

function TXSQLVARHelper.GetOwnName: string;
begin
  Result := PUTF8Char(@OwnName);
  SetLength(Result, ownname_length);
end;

function TXSQLVARHelper.GetRelName: string;
begin
  Result := PUTF8Char(@RelName);
  SetLength(Result, relname_length);
end;

function TXSQLVARHelper.GetSqlName: string;
begin
  Result := PUTF8Char(@SqlName);
  SetLength(Result, Sqlname_length);
end;

procedure TXSQLVARHelper.SetAliasName(const Value: string);
var
  s: RawByteString;
begin
  s := UTF8Encode(Value);
  Move(PByte(s)^, AliasName, length(s));
  aliasname_length := length(s);
end;

procedure TXSQLVARHelper.SetOwnName(const Value: string);
var
  s: RawByteString;
begin
  s := UTF8Encode(Value);
  Move(PByte(s)^, OwnName, length(s));
  ownname_length := length(s);
end;

procedure TXSQLVARHelper.SetRelName(const Value: string);
var
  s: RawByteString;
begin
  s := UTF8Encode(Value);
  Move(PByte(s)^, RelName, length(s));
  relname_length := length(s);
end;

procedure TXSQLVARHelper.SetSqlName(const Value: string);
var
  s: RawByteString;
begin
  s := UTF8Encode(Value);
  Move(PByte(s)^, SqlName, length(s));
  Sqlname_length := length(s);
end;

end.
