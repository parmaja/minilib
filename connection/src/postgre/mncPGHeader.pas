unit mncPGHeader;
{ postgresql 18.x }
{$IFDEF FPC}
{$MODE delphi}
{$PACKRECORDS C}
{$ENDIF}
{.$define pg18}
{$M+}{$H+}

//{$MINENUMSIZE 4} //same as {$Z4} All enum must be sized as Integer
{$Z4}{$A8}

{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belalhamed at gmail dot com>  
 *
 * src/interfaces/libpq/libpq-fe.h
 *}

{*-------------------------------------------------------------------------
 *
 * libpq-fe.h
 *	  This file contains definitions for structures and
 *	  externs for functions used by frontend postgres applications.
 *
 * Portions Copyright (c) 1996-2025, PostgreSQL Global Development Group
 * Portions Copyright (c) 1994, Regents of the University of California
 *
 * src/interfaces/libpq/libpq-fe.h
 *
 *-------------------------------------------------------------------------
 *}

interface

uses
  Types,
  mnLibraries,
  SysUtils;

const

  OIDNAMELEN = 36;

  INV_WRITE = $00020000;
  INV_READ = $00040000;

  BLOB_SEEK_SET = 0;
  BLOB_SEEK_CUR = 1;
  BLOB_SEEK_END = 2;

  OID_BOOL     = 16;
  OID_BYTEA    = 17;
  OID_TEXT     = 25;
  OID_OID      = 26;
  OID_NAME     = 19;
  OID_INT8     = 20;
  OID_INT2     = 21;
  OID_INT4     = 23;
  OID_FLOAT4   = 700;
  OID_MONEY    = 790;
  OID_FLOAT8   = 701;
  OID_UNKNOWN  = 705;
  OID_BPCHAR   = 1042;
  OID_VARCHAR  = 1043;
  OID_TIMESTAMP = 1114;
  OID_DATE      = 1082;
  OID_TIME      = 1083;
  OID_NUMERIC   = 1700;

  CMDSTATUS_LEN = 64;

  {*
    * These symbols may be used in compile-time #ifdef tests for the availability
    * of newer libpq features.
    */

    /* Indicates presence of PQenterPipelineMode and friends */
    #define LIBPQ_HAS_PIPELINING 1
    /* Indicates presence of PQsetTraceFlags; also new PQtrace output format */
    #define LIBPQ_HAS_TRACE_FLAGS 1
  }

  {*
   * These symbols may be used in compile-time #ifdef tests for the availability
   * of v14-and-newer libpq features.
   *}
  { Features added in PostgreSQL v14: }
  { Indicates presence of PQenterPipelineMode and friends }
  LIBPQ_HAS_PIPELINING = 1;
  { Indicates presence of PQsetTraceFlags; also new PQtrace output format }
  LIBPQ_HAS_TRACE_FLAGS = 1;

  { Features added in PostgreSQL v15: }
  { Indicates that PQsslAttribute(NULL, "library") is useful }
  LIBPQ_HAS_SSL_LIBRARY_DETECTION = 1;

  { Features added in PostgreSQL v17: }
  { Indicates presence of PGcancelConn typedef and associated routines }
  LIBPQ_HAS_ASYNC_CANCEL = 1;
  { Indicates presence of PQchangePassword }
  LIBPQ_HAS_CHANGE_PASSWORD = 1;
  { Indicates presence of PQsetChunkedRowsMode, PGRES_TUPLES_CHUNK }
  LIBPQ_HAS_CHUNK_MODE = 1;
  { Indicates presence of PQclosePrepared, PQclosePortal, etc }
  LIBPQ_HAS_CLOSE_PREPARED = 1;
  { Indicates presence of PQsendPipelineSync }
  LIBPQ_HAS_SEND_PIPELINE_SYNC = 1;
  { Indicates presence of PQsocketPoll, PQgetCurrentTimeUSec }
  LIBPQ_HAS_SOCKET_POLL = 1;

  {*
    * Option flags for PQcopyResult
  }
  PG_COPYRES_ATTRS		     = $01;
  PG_COPYRES_TUPLES		     = $02;	{ Implies PG_COPYRES_ATTRS }
  PG_COPYRES_EVENTS		     = $04;
  PG_COPYRES_NOTICEHOOKS	 = $08;


{ ****************** Plain API Types definition ***************** }

type
  {$ifndef FPC}
    {$ifdef CPU64}
    ULONG_PTR = QWord;
    {$else}
    ULONG_PTR = DWord;
    {$endif}
  SIZE_T = ULONG_PTR;
  {$endif}

  PText = PByte;
  OID = Integer;
  POID = ^OID;

{ Application-visible enum types }

  TConnStatusType = (
    CONNECTION_OK,
    CONNECTION_BAD,

  	{ Non-blocking mode only below here }

  	{
  	 * The existence of these should never be relied upon - they should only
  	 * be used for user feedback or similar purposes.
  	 }
  	CONNECTION_STARTED,			{ Waiting for connection to be made.  }
  	CONNECTION_MADE,			{ Connection OK; waiting to send.     }
  	CONNECTION_AWAITING_RESPONSE,	{ Waiting for a response from the
  									 * postmaster.        }
  	CONNECTION_AUTH_OK,			{ Received authentication; waiting for
  								 * backend startup. }
  	CONNECTION_SETENV,			{ This state is no longer used. }
  	CONNECTION_SSL_STARTUP,		{ Performing SSL handshake. }
  	CONNECTION_NEEDED,			{ Internal state: connect() needed. }
  	CONNECTION_CHECK_WRITABLE,	{ Checking if session is read-write. }
  	CONNECTION_CONSUME,			{ Consuming any extra messages. }
  	CONNECTION_GSS_STARTUP,		{ Negotiating GSSAPI. }
  	CONNECTION_CHECK_TARGET,		{ * Internal state: checking target server properties. }
    CONNECTION_CHECK_STANDBY,	{ Checking if server is in standby mode. }
  	CONNECTION_ALLOCATED,		{ Waiting for connection attempt to be started. }
    CONNECTION_AUTHENTICATING 	{ Authentication is in progress with some
    								 * external system. }
    );

  TPostgresPollingStatusType = (
  	PGRES_POLLING_FAILED = 0,
  	PGRES_POLLING_READING,		{ These two indicate that one may	  }
  	PGRES_POLLING_WRITING,		{ use select before polling again.   }
  	PGRES_POLLING_OK,
  	PGRES_POLLING_ACTIVE		{ unused; keep for backwards compatibility }
  );

  TExecStatusType = (
    PGRES_EMPTY_QUERY = 0,	{ empty query string was executed }
    PGRES_COMMAND_OK,			  { a query command that doesn't return
                                 * anything was executed properly by the
                                 * backend }
    PGRES_TUPLES_OK,			  { a query command that returns tuples was
                                 * executed properly by the backend, PGresult
                                 * contains the result tuples }
    PGRES_COPY_OUT,				  { Copy Out data transfer in progress }
    PGRES_COPY_IN,				  { Copy In data transfer in progress }
    PGRES_BAD_RESPONSE,			{ an unexpected response was recv'd from the
                                 * backend }
    PGRES_NONFATAL_ERROR,		{ notice or warning message }
    PGRES_FATAL_ERROR,			{ query failed }
    PGRES_COPY_BOTH,			  { Copy In/Out data transfer in progress }
  	PGRES_SINGLE_TUPLE,			{ single tuple from larger resultset }
  	PGRES_PIPELINE_SYNC,		{ pipeline synchronization point }
  	PGRES_PIPELINE_ABORTED,	{ Command didn't run because of an abort earlier in a pipeline }
    PGRES_TUPLES_CHUNK			{ chunk of tuples from larger resultset }
  );

  TPGTransactionStatusType = (
      PQTRANS_IDLE,				{ connection idle }
      PQTRANS_ACTIVE,				{ command in progress }
      PQTRANS_INTRANS,			{ idle, within transaction block }
      PQTRANS_INERROR,			{ idle, within failed transaction }
      PQTRANS_UNKNOWN				{ cannot determine status }
  );

  TPGVerbosity = (
      PQERRORS_TERSE,				{ single-line error messages }
      PQERRORS_DEFAULT,			{ recommended style }
      PQERRORS_VERBOSE,			{ all the facts, ma'am }
      PQERRORS_SQLSTATE			{ only error severity and SQLSTATE code }
  );

  TPGContextVisibility = (
      PQSHOW_CONTEXT_NEVER,		{ never show CONTEXT field }
      PQSHOW_CONTEXT_ERRORS,		{ show CONTEXT for errors only (default) }
      PQSHOW_CONTEXT_ALWAYS		{ always show CONTEXT field }
  );

  {
   * PGPing - The ordering of this enum should not be altered because the
   * values are exposed externally via pg_isready.
  }

  TPGPing = (
      PQPING_OK,					{ server is accepting connections }
      PQPING_REJECT,				{ server is alive but rejecting connections }
      PQPING_NO_RESPONSE,			{ could not establish connection }
      PQPING_NO_ATTEMPT			{ connection not attempted (bad params) }
  );

  {
    PGpipelineStatus - Current status of pipeline mode
  }
  TPGpipelineStatusMode = (
  	PQ_PIPELINE_OFF,
  	PQ_PIPELINE_ON,
  	PQ_PIPELINE_ABORTED
  );

  TPGauthData = (
  	PQAUTHDATA_PROMPT_OAUTH_DEVICE, { user must visit a device-authorization
  									 * URL }
  	PQAUTHDATA_OAUTH_BEARER_TOKEN	{ server requests an OAuth Bearer token }
  );

{
  PGconn encapsulates a connection to the backend.
  The contents of this struct are not supposed to be known to applications.
}
  TPGconn = type Pointer;
  PPGconn = ^TPGconn;

  {*
    PGcancelConn encapsulates a cancel connection to the backend.
    The contents of this struct are not supposed to be known to applications.
  *}

  TPGCancelConn = type Pointer;
  PPGCancelConn = ^TPGCancelConn;

  { moved from bottom }
  TPQnoticeReceiver = procedure(arg: Pointer; res: Pointer); cdecl;
  TPQnoticeProcessor = procedure(arg: Pointer; message: PByte); cdecl;

  { Fields needed for notice handling }

  PPGNoticeHooks = ^TPGNoticeHooks;
  TPGNoticeHooks = record
    noticeRec: TPQnoticeReceiver; { notice message receiver }
    noticeRecArg: Pointer;
    noticeProc: TPQnoticeProcessor;   { notice message processor }
    noticeProcArg: Pointer;
  end;

  {
    PGresult and the subsidiary types PGresAttDesc, PGresAttValue
    represent the result of a query (or more precisely, of a single SQL
    command --- a query string given to PQexec can contain multiple commands).
    Note we assume that a single command can return at most one tuple group,
    hence there is no need for multiple descriptor sets.
  }

  {
    Subsidiary-storage management structure for PGresult.
    See space management routines in fe-exec.c for details.
    Note that space[k] refers to the k'th byte starting from the physical
    head of the block --- it's a union, not a struct!
  }

  PPGresult_data = ^TPGresult_data;
  TPGresult_data = record
    Next: PPGresult_data;
    Space: array[0..0] of Byte;
  end;

{
   PGresult encapsulates the result of a query (or more precisely, of a single
   SQL command --- a query string given to PQsendQuery can contain multiple
   commands and thus return multiple PGresult objects).
   The contents of this struct are not supposed to be known to applications.
}
  TPGresult = record //Do not use Packed!!!
     ntups: Integer;
     numAttributes: Integer;
     attDescs: Pointer; //PPGresAttDesc;
     tuples: Pointer; //PGresAttValue;     { each PGresult tuple is an array of PGresAttValue's }
     tupArrSize: Integer;     { allocated size of tuples array }
     numParameters: Integer;
     paramDescs: Pointer; //PGresParamDesc;
     resultStatus: TExecStatusType;
     cmdStatus: array [0..CMDSTATUS_LEN - 1] of Byte;   { cmd status from the query }
     binary: Integer;         { binary tuple values if binary == 1, otherwise text }

     {
      * These fields are copied from the originating PGconn, so that operations
      * on the PGresult don't have to reference the PGconn.
      }
     noticeHooks: TPGNoticeHooks;
     events: Pointer; //*PGEvent;
     nEvents: Integer;
     client_encoding: Integer;    { encoding id }

     {
      * Error information (all NULL if not an error result).  errMsg is the
      * "overall" error message returned by PQresultErrorMessage.  If we have
      * per-field info then it is stored in a linked list.
      }
     errMsg: PByte;         { error message, or NULL if no error }
     errFields: Pointer; //PGMessageField;  { message broken into fields }
     errQuery: PByte;       { text of triggering query, if available }

     { All NULL attributes in the query result point to this null string }
     null_field: array[0..0] of Byte;

     {
      * Space management information.  Note that attDescs and error stuff, if
      * not null, point into allocated blocks.  But tuples points to a
      * separately malloc'd block, so that we can realloc it.
      }
     curBlock: PPGresult_data;    { most recently allocated block }
     curOffset: Integer;      { start offset of free space in block }
     spaceLeft: Integer;      { number of free bytes remaining in block }

     memorySize: size_t;     { total space allocated for this PGresult }
  end;
  PPGresult = ^TPGresult;

{
  PGcancel encapsulates the information needed to cancel a running
  query on an existing connection.
  The contents of this struct are not supposed to be known to applications.
}
  TPGcancel = type Pointer;
  PPGcancel= ^TPGcancel;

{
  PGnotify represents the occurrence of a NOTIFY message.
  Ideally this would be an opaque typedef, but it's so simple that it's
  unlikely to change.
  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  whereas in earlier versions it was always your own backend's PID.
}
  PPGnotify = ^TPGnotify;
  TPGnotify = record
    relname: PByte; { notification condition name }
    be_pid: Integer;  { process ID of notifying server process }
    extra: PByte; { notification parameter }
    { Fields below here are private to libpq; apps should not use 'em }
    next: PPGnotify;		{ list link }
  end;

  { pg_usec_time_t is like time_t, but with microsecond resolution }

  TPG_USecTime = Int64;

  { Function types for notice-handling callbacks }

  { moved to top
  TPQnoticeReceiver = procedure(arg: Pointer; var res: TPGresult); cdecl;
  TPQnoticeProcessor = procedure(arg: Pointer; message: PByte); cdecl;
  }

{ Print options for PQprint() }

  TPQBool = type byte;

  //PPChar = array[00..$FF] of PByte;

  TPQprintOpt = record
    header: Byte; { print output field headings and row count }
    align: Byte; { fill align the fields }
    standard: Byte; { old brain dead format }
    html3: Byte; { output html tables }
    expanded: Byte; { expand tables }
    pager: Byte; { use pager for output if needed }
    fieldSep: PByte; { field separator }
    tableOpt: PByte; { insert to HTML <table ...> }
    caption: PByte; { HTML <caption> }
    fieldName: array[00..$FF] of PByte; { null terminated array of repalcement field names }
  end;

  PPQprintOpt = ^TPQprintOpt;

  { ----------------
   * Structure for the conninfo parameter definitions returned by PQconndefaults
   * or PQconninfoParse.
   *
   * All fields except "val" point at static strings which must not be altered.
   * "val" is either NULL or a malloc'd current-value string.  PQconninfoFree()
   * will release both the val strings and the PQconninfoOption array itself.
   * ----------------
  }

  TPQconninfoOption = record
    Keyword: PByte; { The keyword of the option }
    EnvVar: PByte; { Fallback environment variable name }
    Compiled: PByte; { Fallback compiled in default value  }
    Val: PByte; { Options value	}
    Lab: PByte; { Label for field in connect dialog }
    Dispchar: PByte; { Character to display for this field
                           in a connect dialog. Values are:
                           ""	Display entered value as is
                           "*"	Password field - hide value
                           "D"	Debug options - don't
                           create a field by default }
    Dispsize: Integer; { Field size in characters for dialog }
  end;

  PPQConninfoOption = ^TPQconninfoOption;

  {
   ----------------
   * PQArgBlock -- structure for PQfn() arguments
   * ----------------
  }

  TPQArgBlock = record
    len: Integer;
    isint: Integer;
    case u: Boolean of
      True: (ptr: PInteger); { can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PPQArgBlock = ^TPQArgBlock;

  {
   ----------------
    PGresAttDesc -- Data about a single attribute (column) of a query result
   ----------------
  }

  PPGresAttDesc = ^TPGresAttDesc;
  TPGresAttDesc = record
      name: PByte;			{ column name }
      TableId: Oid;     		{ source table, if known }
      ColumnId: Integer;		{ source column, if known }
      Format: Integer;			{ format code for value (text/binary) }
      TypId: Oid;			      { type id }
      Typlen: Integer;			{ type size }
      Atttypmod: Integer;		{ type-specific modifier info }
  end;

  {
    ----------------
    Exported functions of libpq
    ----------------
  }

  { ===	in fe-connect.c === }

  { make a new client connection to the backend }
  { Asynchronous (non-blocking) }

  TPQconnectStart = function(ConnInfo: PByte): PPGconn; cdecl;
  TPQconnectStartParams = function(Keywords: Pointer; Values: Pointer; expand_dbname: Integer): PPGconn; cdecl;
  TPQconnectPoll = function(conn: PPGconn): TPostgresPollingStatusType; cdecl;

  TPQconnectdb = function(ConnInfo: PText): PPGconn; cdecl;
  TPQconnectdbParams = function(Keywords: Pointer; Values: Pointer; expand_dbname: Integer): PPGconn; cdecl;
  TPQsetdbLogin = function(Host, Port, Options, Tty, Db, User, Passwd: PText): PPGconn; cdecl;

  { close the current connection and free the PGconn data structure }
  TPQfinish = procedure(conn: PPGconn); cdecl;

  { get info about connection options known to PQconnectdb }
  TPQconndefaults = function: PPQconninfoOption; cdecl;

  { parse connection options in same way as PQconnectdb }
  TPQconninfoParse = function(conninfo: PByte; errmsg: PPAnsiChar): PPQconninfoOption; cdecl;

  { return the connection options used by a live connection }
  TPQconninfo = function(conn: PPGconn): PPQconninfoOption; cdecl;

  { free the data structure returned by PQconndefaults() or PQconninfoParse() }
  TPQconninfoFree = procedure(connOptions: PPQconninfoOption); cdecl;

  {
   * close the current connection and reestablish a new one with the same
   * parameters
  }
  { Asynchronous (non-blocking) }
  TPQresetStart = function(conn: PPGconn): Integer; cdecl;
  TPQresetPoll = function(conn: PPGconn): TPostgresPollingStatusType; cdecl;

  { Synchronous (blocking) }
  TPQreset = procedure(conn: PPGconn); cdecl;

  { Create a PGcancelConn that's used to cancel a query on the given PGconn }
  TPQcancelCreate = function(conn: PPGconn): PPGcancelConn; cdecl;

  { issue a cancel request in a non-blocking manner }
  TPQcancelStart = function(cancelConn: PPGcancelConn): Integer; cdecl;

  { issue a blocking cancel request }
  TPQcancelBlocking = function(cancelConn: PPGcancelConn): Integer; cdecl;

  { poll a non-blocking cancel request }
  TPQcancelPoll = function(cancelConn: PPGcancelConn): TPostgresPollingStatusType; cdecl;
  TPQcancelStatus = function(const cancelConn: PPGcancelConn): TConnStatusType; cdecl;
  TPQcancelSocket = function(const cancelConn: PPGcancelConn): Integer; cdecl;
  TPQcancelErrorMessage = function(const cancelConn: PPGcancelConn): PUTF8Char; cdecl;
  TPQcancelReset = procedure(cancelConn: PPGcancelConn); cdecl;
  TPQcancelFinish = procedure(cancelConn: PPGcancelConn); cdecl;

  { request a cancel structure }
  TPQgetCancel = function(conn: PPGconn): PPGcancel; cdecl;

  { free a cancel structure }
  TPQfreeCancel = procedure(cancel: PPGcancel); cdecl;

  { deprecated version of PQcancelBlocking, but one which is signal-safe }
  //PQcancel = function(cancel: PPGcancel; errbuf: PPAnsiChar; errbufsize: Integer): Integer; cdecl; deprecated;

  { deprecated version of PQcancel; not thread-safe }
  //TPQrequestCancel = function(conn: PPGconn): Integer; cdecl; //deprecated;

  { Accessor functions for PGconn objects }
  TPQdb = function(conn: PPGconn): PByte; cdecl;
  TPQuser = function(conn: PPGconn): PByte; cdecl;
  TPQpass = function(conn: PPGconn): PByte; cdecl;
  TPQhost = function(conn: PPGconn): PByte; cdecl;
  TPQhostaddr = function(conn: PPGconn): PByte; cdecl;
  TPQport = function(conn: PPGconn): PByte; cdecl;
  TPQtty = function(conn: PPGconn): PByte; cdecl;
  TPQoptions = function(conn: PPGconn): PByte; cdecl;
  TPQstatus = function(conn: PPGconn): TConnStatusType; cdecl;
  TPQtransactionStatus = function(conn: PPGconn): TPGTransactionStatusType; cdecl;
  TPQparameterStatus = function(conn: PPGconn; paramName: PByte): PByte; cdecl; //or maybe PPAnsiChar
  TPQprotocolVersion = function(conn: PPGconn): Integer; cdecl;
  TPQfullProtocolVersion = function(conn: PPGconn): Integer; cdecl;
  TPQserverVersion = function(conn: PPGconn): Integer; cdecl;
  TPQerrorMessage = function(conn: PPGconn): PByte; cdecl;
  TPQsocket = function(conn: PPGconn): Integer; cdecl;
  TPQbackendPID = function(conn: PPGconn): Integer; cdecl;
  TPGpipelineStatus = procedure(conn: PPGconn); cdecl;
  TPQconnectionNeedsPassword = function(conn: PPGconn): Integer; cdecl;
  TPQconnectionUsedPassword = function(conn: PPGconn): Integer; cdecl;
  TPQconnectionUsedGSSAPI = function(conn: PPGconn): Integer; cdecl;
  TPQclientEncoding = function(conn: PPGconn): Integer; cdecl;
  TPQsetClientEncoding = function(conn: PPGconn; encoding: PByte): Integer; cdecl;

  { SSL information functions }

  TPQsslInUse = function(conn: PPGconn): Integer; cdecl;
  TPQsslStruct = procedure(conn: PPGconn; struct_name: PByte); cdecl;
  TPQsslAttribute = function(conn: PPGconn; attribute_name: PByte): PByte; cdecl;
  TPQsslAttributeNames = function(conn: PPGconn): PPAnsiChar; cdecl;

  { Get the OpenSSL structure associated with a connection. Returns NULL for
   * unencrypted connections or if any other TLS library is in use. }
  TPQgetssl = procedure(conn: PPGconn); cdecl;

  { Tell libpq whether it needs to initialize OpenSSL }
  TPQinitSSL = procedure(do_init: Integer); cdecl;

  { More detailed way to tell libpq whether it needs to initialize OpenSSL }
  TPQinitOpenSSL = procedure(do_ssl: Integer; do_crypto: Integer); cdecl;

  { Return true if GSSAPI encryption is in use }
  TPQgssEncInUse = function(conn: PPGconn): Integer; cdecl;

  { Returns GSSAPI context if GSSAPI is in use }
  TPQgetgssctx = procedure(conn: PPGconn); cdecl;

  { Set verbosity for PQerrorMessage and PQresultErrorMessage }
  TPQsetErrorVerbosity = function(conn: PPGconn; verbosity: TPGVerbosity): TPGVerbosity; cdecl; //TODO check return

  { Set CONTEXT visibility for PQerrorMessage and PQresultErrorMessage }
  TPQsetErrorContextVisibility = function(conn: PPGconn; show_context: TPGContextVisibility): TPGContextVisibility; cdecl;

  { Override default notice handling routines }
  TPQsetNoticeReceiver = function(conn: PPGconn; proc: TPQnoticeReceiver; Arg: Pointer): TPQnoticeReceiver; cdecl;
  TPQsetNoticeProcessor = function(conn: PPGconn; proc: TPQnoticeProcessor; Arg: Pointer): TPQnoticeProcessor; cdecl;

  {
   *	   Used to set callback that prevents concurrent access to
   *	   non-thread safe functions that libpq needs.
   *	   The default implementation uses a libpq internal mutex.
   *	   Only required for multithreaded apps that use kerberos
   *	   both within their app and for postgresql connections.
  }
  TPGthreadlock = procedure(acquire: Integer); cdecl; //callback

  TPQregisterThreadLock = function(newhandler: TPGthreadlock): TPGthreadlock; cdecl;

  TPQtrace = procedure(conn: PPGconn; DebugPort: Pointer); cdecl;
  TPQuntrace = procedure(conn: PPGconn); cdecl;

  { flags controlling trace output: }
  { omit timestamps from each line }
  const PQTRACE_SUPPRESS_TIMESTAMPS		= 1;
  { redact portions of some messages, for testing frameworks }
  const PQTRACE_REGRESS_MODE			    = 2;

type
  TPQsetTraceFlags = procedure(conn: PPGconn; flags: integer); cdecl;

  { Simple synchronous query }
  TPQexec = function(conn: PPGconn; query: PByte): PPGresult; cdecl;
  TPQexecParams = function(conn: PPGconn; command: PByte; nParams: Integer; paramTypes: POID; paramValues: PPAnsiChar; paramLengths, paramFormats: Pointer; resultFormat: Integer): PPGresult; cdecl; //todo check arg

  TPQPrepare = function(conn: PPGconn; stmtName, query: PByte; nParams: Integer; paramTypes: POid): PPGresult; cdecl;
  TPQExecPrepared = function(conn: PPGconn; stmtName: PByte; nParams: Integer; paramValues: PPAnsiChar; paramLengths, paramFormats: PInteger; resultFormat: Integer): PPGresult; cdecl;

  const PQ_QUERY_PARAM_MAX_LIMIT  = 65535;

type
  TPQsendQuery = function(conn: PPGconn; query: PByte): Integer; cdecl;
  TPQsendQueryParams = function(conn: PPGconn; command: PByte; nParams: Integer; paramTypes: POID; paramValues: PPAnsiChar; paramLengths, paramFormats: PInteger; resultFormat: Integer): Integer; cdecl;

  TPQsendPrepare = function(conn: PPGconn; stmtName, query: PByte; nParams: Integer; paramTypes: POid): Integer; cdecl;
  TPQsendQueryPrepared = function(conn: PPGconn; stmtName: PByte; nParams: Integer; paramValues: PPAnsiChar; paramLengths, paramFormats: PInteger; resultFormat: Integer): Integer; cdecl;

  TPQsetSingleRowMode = function(conn: PPGconn): Integer; cdecl;
  TPQsetChunkedRowsMode = function(conn: PPGconn; chunkSize: Integer): Integer; cdecl;

  TPQgetResult = function(conn: PPGconn): PPGresult; cdecl;

  { Routines for managing an asynchronous query }

  TPQisBusy = function(conn: PPGconn): Integer; cdecl;
  TPQconsumeInput = function(conn: PPGconn): Integer; cdecl;

  { Routines for pipeline mode management }

  TPQenterPipelineMode = function(conn: PPGconn): integer;
  TPQexitPipelineMode = function(conn: PPGconn): Integer; cdecl;
  TPQpipelineSync = function(conn: PPGconn): Integer; cdecl;
  TPQsendFlushRequest = function(conn: PPGconn): Integer; cdecl;
  TPQsendPipelineSync = function(conn: PPGconn): Integer; cdecl;

  { LISTEN/NOTIFY support }

  TPQnotifies = function(conn: PPGconn): PPGnotify; cdecl;

  { Routines for copy in/out }
  TPQputCopyData = function(conn: PPGconn; buffer: PByte; nbytes: Integer): Integer; cdecl;
  TPQputCopyEnd = function(conn: PPGconn; errormsg: PPAnsiChar): Integer; cdecl;
  TPQgetCopyData = function(conn: PPGconn; buffer: PPAnsiChar; async: Integer): Integer; cdecl;

  { Exists for backward compatibility.  bjm 2003-03-24 }
  //TPQfreeNotify = procedure(Handle: PPGnotify); deprecated; cdecl;
  //TPQgetline = function(conn: PPGconn; Buffer: PByte; length: Integer): Integer; deprecated; cdecl;
  //TPQputline = function(conn: PPGconn; Str: PByte): Integer; deprecated; cdecl;
  //TPQgetlineAsync = function(conn: PPGconn; Buffer: PByte; BufSize: Integer): Integer; deprecated; cdecl;
  //TPQputnbytes = function(conn: PPGconn; Buffer: PByte; NBytes: Integer): Integer; deprecated; cdecl;
  //TPQendcopy = function(conn: PPGconn): Integer; deprecated; cdecl;

  //* Set blocking/nonblocking connection to the backend }
  TPQsetnonblocking = function(conn: PPGconn; arg: Integer): Integer; cdecl;
  TPQisnonblocking = function(conn: PPGconn): Integer; cdecl;
  TPQisthreadsafe = function(): Integer; cdecl;
  TPQping = function(ConnInfo: PByte): TPGPing; cdecl;
  TPQpingParams = function(Keywords: Pointer; Values: Pointer; expand_dbname: Integer): TPGPing; cdecl;

  { Force the write buffer to be written (or at least try) }
  TPQflush = function(conn: PPGconn): Integer;

  {
   * "Fast path" interface --- not really recommended for application
   * use
  }
  TPQfn = function(conn: PPGconn; fnid: Integer; result_buf, result_len: PInteger; result_is_int: Integer; args: PPQArgBlock; nargs: Integer): PPGresult; cdecl;

  { Accessor functions for PGresult objects }
  TPQresultStatus = function(result: PPGresult): TExecStatusType; cdecl;
  TPQresStatus = function(status: TExecStatusType): PByte; cdecl;
  TPQresultErrorMessage = function(Result: PPGresult): PByte; cdecl;
  TPQresultVerboseErrorMessage = function (Result: PPGresult; verbosity: TPGVerbosity; show_context: TPGContextVisibility): PByte; cdecl;
  TPQresultErrorField = function(result: PPGResult; fieldcode: integer): PByte; cdecl;

  TPQntuples = function(Result: PPGresult): Integer; cdecl;
  TPQnfields = function(Result: PPGresult): Integer; cdecl;
  TPQbinaryTuples = function(Result: PPGresult): Integer; cdecl; { deprecated; }
  TPQfname = function(Result: PPGresult; field_num: Integer): PByte; cdecl;
  TPQfnumber = function(Result: PPGresult; field_name: PByte): Integer; cdecl;
  TPQftable = function(Result: PPGresult; field_num: Integer): OID; cdecl;
  TPQftablecol = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQfformat = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQftype = function(Result: PPGresult; field_num: Integer): OID; cdecl;
  TPQfsize = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQfmod = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQcmdStatus = function(Result: PPGresult): PByte; cdecl;
  TPQoidStatus = function(Result: PPGresult): PByte; cdecl; { deprecated 'use PQoidValue'; }
  TPQoidValue = function(Result: PPGresult): OID; cdecl;
  TPQcmdTuples = function(Result: PPGresult): PByte; cdecl;
  TPQgetvalue = function(Result: PPGresult; tup_num, field_num: Integer): PByte; cdecl;
  TPQgetlength = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQgetisnull = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQnparams = function(Result: PPGresult): Integer; cdecl;
  TPQparamtype = function(Result: PPGresult; field_num: Integer): OID; cdecl;

  { Describe prepared statements and portals }
  TPQdescribePrepared = function(conn: PPGconn; stmt: PAnsiString): PPGresult; cdecl;
  TPQdescribePortal = function(conn: PPGconn; portal: PAnsiString): PPGresult; cdecl;
  TPQsendDescribePrepared = function(conn: PPGconn; stmt: PAnsiString): Integer; cdecl;
  TPQsendDescribePortal = function(conn: PPGconn; portal: PAnsiString): Integer; cdecl;

  { Close prepared statements and portals }
  TPQclosePrepared  = function(conn: PPGconn; stmt: PAnsiString): PPGresult; cdecl;
  TPQclosePortal = function(conn: PPGconn; portal: PAnsiString): PPGresult; cdecl;
  TPQsendClosePrepared = function(conn: PPGconn; stmt: PAnsiString): Integer; cdecl;
  TPQsendClosePortal = function(conn: PPGconn; portal: PAnsiString): Integer; cdecl;

  { Delete a PGresult }
  TPQclear = procedure(Result: PPGresult); cdecl;

  { For freeing other alloc'd results, such as PGnotify structs }
  TPQFreemem = procedure(ptr: Pointer); cdecl;

  { Create and manipulate PGresults }
  TPQmakeEmptyPGresult = function(conn: PPGconn; status: TExecStatusType): PPGresult; cdecl;
  TPQcopyResult = function(src: PPGresult; flags: integer): PPGresult; cdecl;
  TPQsetResultAttrs = function(res: PPGresult; numAttributes: Integer; attDescs: PPGresAttDesc): Integer; cdecl;
  TPQresultAlloc = procedure(res: PPGresult; nBytes: size_t); cdecl;
  TPQresultMemorySize = function(res: PPGresult): size_t; cdecl;
  TPQsetvalue = function(res: PPGresult; tup_num: Integer;  field_num: Integer; value: PByte; len: Integer): Integer; cdecl;

  { Quoting strings before inclusion in queries. }
  TPQescapeStringConn = function(conn: PPGconn; toStr: PByte; fromStr: PByte; length: size_t; var error: integer): size_t; cdecl; //TODO Check toStr
  TPQescapeLiteral = function(conn: PPGconn; str: PByte; len: size_t): PByte; cdecl;
  TPQescapeIdentifier = function(conn: PPGconn; str: PByte; len: size_t): PByte; cdecl;
  TPQescapeByteaConn = function(conn: PPGconn; const from: PByte; from_length: longword; to_lenght: PLongword): PByte; cdecl;
  TPQunescapeBytea = function(const from: PByte; to_lenght: PLongword): PByte; cdecl;

  //TPQescapeBytea = function(const from: PByte; from_length: longword; to_lenght: PLongword): PByte; cdecl; deprecated;

(*
  /* === in fe-print.c === */

  extern void PQprint(FILE *fout, /* output stream */
  					const PGresult *res,
  					const PQprintOpt *po);	/* option structure */

  /*
   * really old printing routines
   */
  extern void PQdisplayTuples(const PGresult *res,
  							FILE *fp,	/* where to send the output */
  							int fillAlign,	/* pad the fields with spaces */
  							const char *fieldSep,	/* field separator */
  							int printHeader,	/* display headers? */
  							int quiet);

  extern void PQprintTuples(const PGresult *res,
  						  FILE *fout,	/* output stream */
  						  int PrintAttNames,	/* print attribute names */
  						  int TerseOutput,	/* delimiter bars */
  						  int colWidth);	/* width of column, if 0, use
  											 * variable width */

*)
  { Large-object access routines }
  Tlo_open = function(conn: PPGconn; lobjId: OID; mode: Integer): Integer; cdecl;
  Tlo_close = function(conn: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_read = function(conn: PPGconn; fd: Integer; buf: PByte; len: Integer): Integer; cdecl;
  Tlo_write = function(conn: PPGconn; fd: Integer; buf: PByte; len: Integer): Integer; cdecl;
  Tlo_lseek = function(conn: PPGconn; fd, offset, whence: Integer): Integer; cdecl;
  Tlo_lseek64 = function(conn: PPGconn; fd: Integer; offset: Int64; whence: Integer): Int64; cdecl;
  Tlo_creat = function(conn: PPGconn; mode: Integer): OID; cdecl;
  Tlo_create = function(conn: PPGconn; lobjId:Oid): Oid; cdecl;
  Tlo_tell = function(conn: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_tell64 = function(conn: PPGconn; fd: Integer): Int64; cdecl;
  Tlo_truncate = function(conn: PPGconn; fd, len: Integer): Integer; cdecl;
  Tlo_truncate64 = function(conn: PPGconn; fd: Integer; len: Int64): Int64; cdecl;
  Tlo_unlink = function(conn: PPGconn; lobjId: OID): Integer; cdecl;
  Tlo_import = function(conn: PPGconn; filename: PUTF8Char): OID; cdecl;
  Tlo_import_with_oid = function(conn: PPGconn; const filename: PByte; lobjId: Oid): Oid; cdecl;
  Tlo_export = function(conn: PPGconn; lobjId: OID; filename: PByte): Integer; cdecl;

  { Get the version of the libpq library in use }
  TPQlibVersion = function(): Integer; cdecl;

  { Poll a socket for reading and/or writing with an optional timeout }
  TPQsocketPoll= function(sock: Integer; forRead: Integer; forWrite: Integer; end_time: TPG_USecTime): Integer; cdecl;

  { Get current time in the form PQsocketPoll wants }
  TPQgetCurrentTimeUSec = function(): TPG_USecTime; cdecl;

  { Determine length of multibyte encoded char at *s }
  TPQmblen = function(s: PByte; encoding: integer): integer; cdecl;

  { Same, but not more than the distance to the end of string s }
  TPQmblenBounded = function(s: PByte; encoding: integer): integer; cdecl;

  { Determine display length of multibyte encoded char at *s }
  TPQdsplen = function(s: PByte; encoding: integer): Integer; cdecl;

  { Get encoding id from environment variable PGCLIENTENCODING }
  TPQenv2encoding = function(): Integer; cdecl;

  TPGpromptOAuthDevice = record
  	verification_uri: PText;	{ verification URI to visit }
  	user_code: PText;		{ user code to enter }
  	verification_uri_complete: PText;	{ optional combination of URI and
  											 * code, or NULL }
  	expires_in: integer;	{ seconds until user code expires }
  end;

  SOCKTYPE = Integer;               { Platform-specific socket type - adjust as needed }
  PSOCKTYPE = ^SOCKTYPE;

  { for PGoauthBearerRequest.async() }

  PPGoauthBearerRequest = ^TPGoauthBearerRequest;
  TPGoauthBearerRequest = record
  type
      TAsyncCallback = function(conn: TPGconn; request: PPGoauthBearerRequest; altsock: PSOCKTYPE): TPostgresPollingStatusType; cdecl;
      TCleanupCallback = procedure(conn: TPGconn; request: PPGoauthBearerRequest); cdecl;
  public
 	  { Hook inputs (constant across all calls) }
  	openid_configuration: PText;	{ OIDC discovery URI }
  	scope: PText;			{ required scope(s), or NULL }

  	{ Hook outputs }

  	{*---------
  	 * Callback implementing a custom asynchronous OAuth flow.
  	 *
  	 * The callback may return
  	 * - PGRES_POLLING_READING/WRITING, to indicate that a socket descriptor
  	 *   has been stored in *altsock and libpq should wait until it is
  	 *   readable or writable before calling back;
  	 * - PGRES_POLLING_OK, to indicate that the flow is complete and
  	 *   request->token has been set; or
  	 * - PGRES_POLLING_FAILED, to indicate that token retrieval has failed.
  	 *
  	 * This callback is optional. If the token can be obtained without
  	 * blocking during the original call to the PQAUTHDATA_OAUTH_BEARER_TOKEN
  	 * hook, it may be returned directly, but one of request->async or
  	 * request->token must be set by the hook.
  	 *}
    async: TAsyncCallback;

  	{*
  	 * Callback to clean up custom allocations. A hook implementation may use
  	 * this to free request->token and any resources in request->user.
  	 *
  	 * This is technically optional, but highly recommended, because there is
  	 * no other indication as to when it is safe to free the token.
  	 *}
  	cleanup: TCleanupCallback;

  	{*
  	 * The hook should set this to the Bearer token contents for the
  	 * connection, once the flow is completed.  The token contents must remain
  	 * available to libpq until the hook's cleanup callback is called.
  	 *}
  	token: PAnsiChar;

  	{*
  	 * Hook-defined data. libpq will not modify this pointer across calls to
  	 * the async callback, so it can be used to keep track of
  	 * application-specific state. Resources allocated here should be freed by
  	 * the cleanup callback.
  	 *}
  	user: Pointer;
  end;

  TPQencryptPassword = function(passwd: PByte; user: PByte): PByte; cdecl;
  TPQencryptPasswordConn = function(conn: PPGconn; passwd: PByte; user: PByte; algorithm: PByte): PByte; cdecl;
  TPQchangePassword = function(conn: PPGconn; user: PAnsiChar; passwd: PAnsiChar): PPGresult; cdecl;

  TPQauthDataHook_type = function(auth_type: TPGauthData; conn: TPGconn; data: Pointer): Integer; cdecl;

  TPQsetAuthDataHook = procedure(hook: TPQauthDataHook_type); cdecl;
  TPQgetAuthDataHook = function: TPQauthDataHook_type; cdecl;
  TPQdefaultAuthDataHook = function (auth_type: TPGauthData; conn: TPGconn; data: Pointer): Integer; cdecl;

  Tpg_char_to_encoding = function(name: PByte): Integer; cdecl;
  Tpg_encoding_to_char = function(encoding: Integer): PByte; cdecl;
  Tpg_valid_server_encoding_id = function(encoding: Integer): Integer; cdecl;

  { Support for overriding sslpassword handling with a callback }
  {this callback}
  TPQsslKeyPassHook_OpenSSL_type = function(buf: PByte; size: Integer; conn: PPGconn): Integer; cdecl;

  TPQgetSSLKeyPassHook_OpenSSL = function(): TPQsslKeyPassHook_OpenSSL_type; cdecl;
  TPQsetSSLKeyPassHook_OpenSSL = procedure (hook: TPQsslKeyPassHook_OpenSSL_type); cdecl;
  TPQdefaultSSLKeyPassHook_OpenSSL = function(buf: PByte; size: Integer; conn: PPGconn): Integer; cdecl;

var

  PQconnectStart: TPQconnectStart;
  PQconnectStartParams: TPQconnectStartParams;
  PQconnectPoll: TPQconnectPoll;

  PQconnectdb: TPQconnectdb;
  PQsetdbLogin: TPQsetdbLogin;
  PQfinish: TPQfinish;
  PQconndefaults: TPQconndefaults;
  PQconninfoParse: TPQconninfoParse;
  PQconninfo: TPQconninfo;
  PQconninfoFree: TPQconninfoFree;

  PQresetStart: TPQresetStart;
  PQresetPoll: TPQresetPoll;

  PQreset: TPQreset;

  PQcancelCreate: TPQcancelCreate;
  PQcancelStart: TPQcancelStart;
  PQcancelBlocking: TPQcancelBlocking;
  PQcancelPoll: TPQcancelPoll;
  PQcancelStatus: TPQcancelStatus;
  PQcancelSocket: TPQcancelSocket;
  PQcancelErrorMessage: TPQcancelErrorMessage;
  PQcancelReset: TPQcancelReset;
  PQcancelFinish: TPQcancelFinish;

  PQgetCancel: TPQgetCancel;
  PQfreeCancel: TPQfreeCancel;
  //PQrequestCancel: TPQrequestCancel; deprecated;

  PQdb: TPQdb;
  PQuser: TPQuser;
  PQpass: TPQpass;
  PQhost: TPQhost;
  PQhostaddr: TPQhostaddr;
  PQport: TPQport;
  PQtty: TPQtty;
  PQoptions: TPQoptions;
  PQstatus: TPQstatus;
  PQtransactionStatus: TPQtransactionStatus;
  PQparameterStatus: TPQparameterStatus;
  PQprotocolVersion: TPQprotocolVersion;
  {$ifdef pg18}
  PQfullProtocolVersion: TPQfullProtocolVersion;
  {$endif}
  PQserverVersion: TPQserverVersion;
  PQerrorMessage: TPQerrorMessage;
  PQsocket: TPQsocket;
  PQbackendPID: TPQbackendPID;
  PGpipelineStatus: TPGpipelineStatus;
  PQconnectionNeedsPassword: TPQconnectionNeedsPassword;
  PQconnectionUsedPassword: TPQconnectionUsedPassword;
  PQconnectionUsedGSSAPI: TPQconnectionUsedGSSAPI;
  PQclientEncoding: TPQclientEncoding;
  PQsetClientEncoding: TPQsetClientEncoding;

  PQsslInUse: TPQsslInUse;
  PQsslStruct: TPQsslStruct;
  PQsslAttribute: TPQsslAttribute;
  PQsslAttributeNames: TPQsslAttributeNames;

  PQgetssl: TPQgetssl;
  PQinitSSL: TPQinitSSL;
  PQinitOpenSSL: TPQinitOpenSSL;
  PQgssEncInUse: TPQgssEncInUse;
  PQgetgssctx: TPQgetgssctx;
  PQsetErrorVerbosity: TPQsetErrorVerbosity;
  PQsetErrorContextVisibility: TPQsetErrorContextVisibility;

  PQsetNoticeReceiver: TPQsetNoticeReceiver;
  PQsetNoticeProcessor: TPQsetNoticeProcessor;

  PQregisterThreadLock: TPQregisterThreadLock;

  PQtrace: TPQtrace;
  PQuntrace: TPQuntrace;
  PQsetTraceFlags: TPQsetTraceFlags;

  PQexec: TPQexec;
  PQexecParams: TPQexecParams;
  PQPrepare: TPQPrepare;
  PQExecPrepared: TPQExecPrepared;
  PQsendQuery: TPQsendQuery;
  PQsendQueryParams: TPQsendQueryParams;
  PQsendPrepare: TPQsendPrepare;
  PQsendQueryPrepared: TPQsendQueryPrepared;
  PQsetSingleRowMode: TPQsetSingleRowMode;
  PQsetChunkedRowsMode: TPQsetChunkedRowsMode;

  PQgetResult: TPQgetResult;

  PQisBusy: TPQisBusy;
  PQconsumeInput: TPQconsumeInput;

  PQenterPipelineMode: TPQenterPipelineMode;
  PQexitPipelineMode: TPQexitPipelineMode;
  PQpipelineSync: TPQpipelineSync;
  PQsendFlushRequest: TPQsendFlushRequest;
  PQsendPipelineSync: TPQsendPipelineSync;

  PQnotifies: TPQnotifies;

  PQputCopyData: TPQputCopyData;
  PQputCopyEnd: TPQputCopyEnd;
  PQgetCopyData: TPQgetCopyData;

  { Deprecated routines for copy in/out }

  //PQfreeNotify: TPQfreeNotify;
  //PQgetline: TPQgetline;
  //PQputline: TPQputline;
  //PQgetlineAsync: TPQgetlineAsync;
  //PQputnbytes: TPQputnbytes;
  //PQendcopy: TPQendcopy;

  PQsetnonblocking: TPQsetnonblocking;
  PQisnonblocking: TPQisnonblocking;
  PQisthreadsafe: TPQisthreadsafe;
  PQping: TPQping;
  PQpingParams: TPQpingParams;

  PQflush: TPQflush;

  PQfn: TPQfn;
  PQresultStatus: TPQresultStatus;
  PQresStatus: TPQresStatus;
  PQresultErrorMessage: TPQresultErrorMessage;
  PQresultVerboseErrorMessage: TPQresultVerboseErrorMessage;
  PQresultErrorField: TPQresultErrorField;

  PQntuples: TPQntuples;
  PQnfields: TPQnfields;
  PQbinaryTuples: TPQbinaryTuples;
  PQfname: TPQfname;
  PQfnumber: TPQfnumber;
  PQftable: TPQftable;
  PQftablecol: TPQftablecol;
  PQfformat: TPQfformat;
  PQftype: TPQftype;
  PQfsize: TPQfsize;
  PQfmod: TPQfmod;
  PQcmdStatus: TPQcmdStatus;
  PQoidValue: TPQoidValue;
  PQoidStatus: TPQoidStatus;
  PQcmdTuples: TPQcmdTuples;
  PQgetvalue: TPQgetvalue;
  PQgetlength: TPQgetlength;
  PQgetisnull: TPQgetisnull;
  PQnparams: TPQnparams;
  PQparamtype: TPQparamtype;

  PQdescribePrepared: TPQdescribePrepared;
  PQdescribePortal: TPQdescribePortal;
  PQsendDescribePrepared: TPQsendDescribePrepared;
  PQsendDescribePortal: TPQsendDescribePortal;

  PQclosePrepared: TPQclosePrepared;
  PQclosePortal: TPQclosePortal;
  PQsendClosePrepared: TPQsendClosePrepared;
  PQsendClosePortal: TPQsendClosePortal;

  PQclear: TPQclear;
  PQFreemem: TPQFreemem;

  PQmakeEmptyPGresult: TPQmakeEmptyPGresult;
  PQcopyResult: TPQcopyResult;
  PQsetResultAttrs: TPQsetResultAttrs;
  PQresultAlloc: TPQresultAlloc;
  PQresultMemorySize: TPQresultMemorySize;
  PQsetvalue: TPQsetvalue;

  PQescapeStringConn: TPQescapeStringConn;
  PQescapeLiteral: TPQescapeLiteral;
  PQescapeIdentifier: TPQescapeIdentifier;
  PQescapeByteaConn: TPQescapeByteaConn;
  PQunescapeBytea: TPQunescapeBytea;

  //PQescapeBytea: TPQescapeBytea;

{ === in fe-lobj.c === }
  lo_open: Tlo_open;
  lo_close: Tlo_close;
  lo_read: Tlo_read;
  lo_write: Tlo_write;
  lo_lseek: Tlo_lseek;
  lo_lseek64: Tlo_lseek64;
  lo_creat: Tlo_creat;
  lo_create: Tlo_create;
  lo_tell: Tlo_tell;
  lo_tell64: Tlo_tell64;
  lo_truncate: Tlo_truncate;
  lo_truncate64: Tlo_truncate64;
  lo_unlink: Tlo_unlink;
  lo_import: Tlo_import;
  lo_import_with_oid: Tlo_import_with_oid;
  lo_export: Tlo_export;

  PQlibVersion: TPQlibVersion;

  PQsocketPoll: TPQsocketPoll;
  PQgetCurrentTimeUSec: TPQgetCurrentTimeUSec;

  PQmblen: TPQmblen;
  PQmblenBounded: TPQmblenBounded;
  PQdsplen: TPQdsplen;
  PQenv2encoding: TPQenv2encoding;
  PQencryptPassword: TPQencryptPassword;
  PQencryptPasswordConn: TPQencryptPasswordConn;
  PQchangePassword: TPQchangePassword;

  PQsetAuthDataHook: TPQsetAuthDataHook;
  PQgetAuthDataHook: TPQgetAuthDataHook;
  PQdefaultAuthDataHook: TPQdefaultAuthDataHook;

  pg_char_to_encoding: Tpg_char_to_encoding;
  pg_encoding_to_char: Tpg_encoding_to_char;
  pg_valid_server_encoding_id: Tpg_valid_server_encoding_id;

  PQgetSSLKeyPassHook_OpenSSL: TPQgetSSLKeyPassHook_OpenSSL;
  PQsetSSLKeyPassHook_OpenSSL: TPQsetSSLKeyPassHook_OpenSSL;
  PQdefaultSSLKeyPassHook_OpenSSL: TPQdefaultSSLKeyPassHook_OpenSSL;

type

  { TmncPGLib }

  TmncPGLib = class(TmnLibrary)
  protected
    procedure Link; override;
  end;

var
  PGLib: TmncPGLib = nil;

function PQsetdb(Host, Port, Options, Tty, Db: PByte): PPGconn;

implementation

function PQsetdb(Host, Port, Options, Tty, Db: PByte): PPGconn;
begin
  Result := PQsetdbLogin(Host, Port, Options, Tty, Db, nil, nil);
end;

procedure TmncPGLib.Link;
begin
  RaiseError := False;
  PQconnectStart := GetAddress('PQconnectStart');
  PQconnectStartParams := GetAddress('PQconnectStartParams');
  PQconnectPoll := GetAddress('PQconnectPoll');

  PQconnectdb := GetAddress('PQconnectdb');
  PQsetdbLogin := GetAddress('PQsetdbLogin');
  PQfinish := GetAddress('PQfinish');
  PQconndefaults := GetAddress('PQconndefaults');
  PQconninfoParse := GetAddress('PQconninfoParse');
  PQconninfo := GetAddress('PQconninfo');
  PQconninfoFree := GetAddress('PQconninfoFree');

  PQresetStart := GetAddress('PQresetStart');
  PQresetPoll := GetAddress('PQresetPoll');

  PQreset := GetAddress('PQreset');

  PQcancelCreate := GetAddress('PQcancelCreate');
  PQcancelStart := GetAddress('PQcancelStart');
  PQcancelBlocking := GetAddress('PQcancelBlocking');
  PQcancelPoll := GetAddress('PQcancelPoll');
  PQcancelStatus := GetAddress('PQcancelStatus');
  PQcancelSocket := GetAddress('PQcancelSocket');
  PQcancelErrorMessage := GetAddress('PQcancelErrorMessage');
  PQcancelReset := GetAddress('PQcancelReset');
  PQcancelFinish := GetAddress('PQcancelFinish');

  PQgetCancel := GetAddress('PQgetCancel');
  PQfreeCancel := GetAddress('PQfreeCancel');

  //PQrequestCancel := GetAddress('PQrequestCancel'); deprecated
  PQdb := GetAddress('PQdb');
  PQuser := GetAddress('PQuser');
  PQpass := GetAddress('PQpass');
  PQhost := GetAddress('PQhost');
  PQhostaddr := GetAddress('PQhostaddr');
  PQport := GetAddress('PQport');
  PQtty := GetAddress('PQtty');
  PQoptions := GetAddress('PQoptions');
  PQstatus := GetAddress('PQstatus');
  PQtransactionStatus := GetAddress('PQtransactionStatus');
  PQparameterStatus := GetAddress('PQparameterStatus');
  PQprotocolVersion := GetAddress('PQprotocolVersion');
  {$ifdef pg18}
  PQfullProtocolVersion := GetAddress('PQfullProtocolVersion');
  {$endif}
  PQserverVersion := GetAddress('PQserverVersion');

  PQerrorMessage := GetAddress('PQerrorMessage');
  PQsocket := GetAddress('PQsocket');
  PQbackendPID := GetAddress('PQbackendPID');
  PGpipelineStatus := GetAddress('PGpipelineStatus');
  PQconnectionNeedsPassword := GetAddress('PQconnectionNeedsPassword');
  PQconnectionUsedPassword := GetAddress('PQconnectionUsedPassword');
  PQconnectionUsedGSSAPI := GetAddress('PQconnectionUsedGSSAPI');
  PQclientEncoding := GetAddress('PQclientEncoding');
  PQsetClientEncoding := GetAddress('PQsetClientEncoding');

  PQsslInUse := GetAddress('PQsslInUse');
  PQsslStruct := GetAddress('PQsslStruct');
  PQsslAttribute := GetAddress('PQsslAttribute');
  PQsslAttributeNames := GetAddress('PQsslAttributeNames');

  PQgetssl := GetAddress('PQgetssl');
  PQinitSSL := GetAddress('PQinitSSL');
  PQinitOpenSSL := GetAddress('PQinitOpenSSL');
  PQgssEncInUse := GetAddress('PQgssEncInUse');
  PQgetgssctx := GetAddress('PQgetgssctx');
  PQsetErrorVerbosity := GetAddress('PQsetErrorVerbosity');
  PQsetErrorContextVisibility := GetAddress('PQsetErrorContextVisibility');

  PQsetNoticeReceiver := GetAddress('PQsetNoticeReceiver');
  PQsetNoticeProcessor := GetAddress('PQsetNoticeProcessor');

  PQregisterThreadLock := GetAddress('PQregisterThreadLock');

  PQtrace := GetAddress('PQtrace');
  PQuntrace := GetAddress('PQuntrace');
  PQsetTraceFlags := GetAddress('PQsetTraceFlags');

  PQexec := GetAddress('PQexec');
  PQexecParams := GetAddress('PQexecParams');

  PQsendQuery := GetAddress('PQsendQuery');
  PQsendQueryParams := GetAddress('PQsendQueryParams');

  PQPrepare := GetAddress('PQprepare');
  PQExecPrepared := GetAddress('PQexecPrepared');

  PQsendPrepare := GetAddress('PQsendPrepare');
  PQsendQueryPrepared := GetAddress('PQsendQueryPrepared');

  PQsetSingleRowMode := GetAddress('PQsetSingleRowMode');
  PQsetChunkedRowsMode := GetAddress('PQsetChunkedRowsMode');
  PQgetResult := GetAddress('PQgetResult');

  PQisBusy := GetAddress('PQisBusy');
  PQconsumeInput := GetAddress('PQconsumeInput');

  PQenterPipelineMode := GetAddress('PQenterPipelineMode');
  PQexitPipelineMode := GetAddress('PQexitPipelineMode');
  PQpipelineSync := GetAddress('PQpipelineSync');
  PQsendFlushRequest := GetAddress('PQsendFlushRequest');
  PQsendPipelineSync := GetAddress('PQsendPipelineSync');

  PQnotifies := GetAddress('PQnotifies');

  PQputCopyData:= GetAddress('PQputCopyData');
  PQputCopyEnd := GetAddress('PQputCopyEnd');
  PQgetCopyData := GetAddress('PQgetCopyData');

  //  PQgetline := GetAddress('PQgetline');
  //  PQputline := GetAddress('PQputline');
  //  PQgetlineAsync := GetAddress('PQgetlineAsync');
  //  PQputnbytes := GetAddress('PQputnbytes');
  //  PQendcopy := GetAddress('PQendcopy');

  PQsetnonblocking := GetAddress('PQsetnonblocking');
  PQisnonblocking := GetAddress('PQisnonblocking');
  PQisthreadsafe := GetAddress('PQisthreadsafe');
  PQping := GetAddress('PQping');
  PQpingParams := GetAddress('PQpingParams');
  PQflush := GetAddress('PQflush');

  PQfn := GetAddress('PQfn');
  PQresultStatus := GetAddress('PQresultStatus');
  PQresStatus := GetAddress('PQresStatus');
  PQresultErrorMessage := GetAddress('PQresultErrorMessage');
  PQresultVerboseErrorMessage := GetAddress('PQresultVerboseErrorMessage');
  PQresultErrorField := GetAddress('PQresultErrorField');

  PQntuples := GetAddress('PQntuples');
  PQnfields := GetAddress('PQnfields');
  PQbinaryTuples := GetAddress('PQbinaryTuples');
  PQfname := GetAddress('PQfname');
  PQfnumber := GetAddress('PQfnumber');
  PQftable:= GetAddress('PQftable');
  PQftablecol:= GetAddress('PQftablecol');
  PQfformat:= GetAddress('PQfformat');
  PQftype := GetAddress('PQftype');
  PQfsize := GetAddress('PQfsize');
  PQfmod := GetAddress('PQfmod');
  PQcmdStatus := GetAddress('PQcmdStatus');
  PQoidValue := GetAddress('PQoidValue');
  PQoidStatus := GetAddress('PQoidStatus');
  PQcmdTuples := GetAddress('PQcmdTuples');
  PQgetvalue := GetAddress('PQgetvalue');
  PQgetlength := GetAddress('PQgetlength');
  PQgetisnull := GetAddress('PQgetisnull');
  PQnparams := GetAddress('PQnparams');
  PQparamtype := GetAddress('PQparamtype');

  PQdescribePrepared := GetAddress('PQdescribePrepared');
  PQdescribePortal := GetAddress('PQdescribePortal');
  PQsendDescribePrepared := GetAddress('PQsendDescribePrepared');
  PQsendDescribePortal := GetAddress('PQsendDescribePortal');

  PQclosePrepared := GetAddress('PQclosePrepared');
  PQclosePortal := GetAddress('PQclosePortal');
  PQsendClosePrepared := GetAddress('PQsendClosePrepared');
  PQsendClosePortal := GetAddress('PQsendClosePortal');

  PQclear := GetAddress('PQclear');
  PQfreemem := GetAddress('PQfreemem');

  PQmakeEmptyPGresult := GetAddress('PQmakeEmptyPGresult');
  PQcopyResult:= GetAddress('PQcopyResult');
  PQsetResultAttrs:= GetAddress('PQsetResultAttrs');
  PQresultAlloc:= GetAddress('PQresultAlloc');
  PQresultMemorySize:= GetAddress('PQresultMemorySize');
  PQsetvalue:= GetAddress('PQsetvalue');

  PQescapeStringConn := GetAddress('PQescapeStringConn');
  PQescapeLiteral := GetAddress('PQescapeLiteral');
  PQescapeIdentifier := GetAddress('PQescapeIdentifier');
  PQescapeByteaConn := GetAddress('PQescapeByteaConn');
  PQunescapeBytea := GetAddress('PQunescapeBytea');

  //PQescapeBytea := GetAddress('PQescapeBytea');

  lo_open := GetAddress('lo_open');
  lo_close := GetAddress('lo_close');
  lo_read := GetAddress('lo_read');
  lo_write := GetAddress('lo_write');
  lo_lseek := GetAddress('lo_lseek');
  lo_lseek64 := GetAddress('lo_lseek64');
  lo_creat := GetAddress('lo_creat');
  lo_create := GetAddress('lo_create');
  lo_tell := GetAddress('lo_tell');
  lo_tell64 := GetAddress('lo_tell64');
  lo_truncate := GetAddress('lo_truncate');
  lo_truncate64 := GetAddress('lo_truncate64');
  lo_unlink := GetAddress('lo_unlink');
  lo_import := GetAddress('lo_import');
  lo_import_with_oid := GetAddress('lo_import_with_oid');
  lo_export := GetAddress('lo_export');

  PQlibVersion := GetAddress('PQlibVersion');

  PQsocketPoll := GetAddress('PQsocketPoll');
  PQgetCurrentTimeUSec := GetAddress('PQgetCurrentTimeUSec');

  PQmblen := GetAddress('PQmblen');
  PQmblenBounded := GetAddress('PQmblenBounded');
  PQdsplen := GetAddress('PQdsplen');
  PQenv2encoding := GetAddress('PQenv2encoding');
  PQencryptPassword := GetAddress('PQencryptPassword');
  PQencryptPasswordConn := GetAddress('PQencryptPasswordConn');
  PQchangePassword := GetAddress('PQchangePassword');

  {$ifdef pg18}
  PQsetAuthDataHook := GetAddress('PQsetAuthDataHook');
  PQgetAuthDataHook := GetAddress('PQgetAuthDataHook');
  PQdefaultAuthDataHook := GetAddress('PQdefaultAuthDataHook');
  {$endif}

  pg_char_to_encoding := GetAddress('pg_char_to_encoding');
  pg_encoding_to_char := GetAddress('pg_encoding_to_char');
  pg_valid_server_encoding_id := GetAddress('pg_valid_server_encoding_id');

  PQgetSSLKeyPassHook_OpenSSL := GetAddress('PQgetSSLKeyPassHook_OpenSSL');
  PQsetSSLKeyPassHook_OpenSSL := GetAddress('PQsetSSLKeyPassHook_OpenSSL');
  PQdefaultSSLKeyPassHook_OpenSSL := GetAddress('PQdefaultSSLKeyPassHook_OpenSSL');

end;

initialization
  PGLib := TmncPGLib.Create('libpq');
finalization
  FreeAndNil(PGLib);
end.

