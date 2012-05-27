unit mncFBStrings;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}{$H+}
{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
interface

uses
  mncFBHeader;

const
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
    'ext_call_depth'
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
    'lock_timeout'//fb2
    );

  QUOTE = '''';
  DBL_QUOTE = '"';

implementation

end.
 
