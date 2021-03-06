unit mncPGHeader;
{$ifdef FPC}{$mode Delphi}{$H+}{$endif}
{}
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belalhamed at gmail dot com>  
 *
 *}
 {
   This file sqlite3.inc ported from Lazarus just to be compatiple in both Delphi and FPC
 }

interface

uses
  mnLibraries,
  SysUtils;


const

  NAMEDATALEN = 32;
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

{ ****************** Plain API Types definition ***************** }

type
  {$ifdef FPC}
  {$PACKRECORDS C}
  {$else DELPHI}
  TLibHandle = System.THandle;
  {$endif}

  OID = Integer;

{ Application-visible enum types }
  ConnStatusType = (
    CONNECTION_OK,
    CONNECTION_BAD
    );

  TExecStatusType = (
    PGRES_EMPTY_QUERY,
    PGRES_COMMAND_OK, { a query command that doesn't return anything
      was executed properly by the backend }
    PGRES_TUPLES_OK, { a query command that returns tuples
      was executed properly by the backend,
      PGresult contains the result tuples }
    PGRES_COPY_OUT, { Copy Out data transfer in progress }
    PGRES_COPY_IN, { Copy In data transfer in progress }
    PGRES_BAD_RESPONSE, { an unexpected response was recv'd from
      the backend }
    PGRES_NONFATAL_ERROR,
    PGRES_FATAL_ERROR,
    PGRES_COPY_BOTH,
    PGRES_SINGLE_TUPLE
    );

{ String descriptions of the TExecStatusTypes }
  pgresStatus = array[$00..$FF] of PAnsiChar;

{ PGconn encapsulates a connection to the backend.
  The contents of this struct are not supposed to be known to applications.
}
  PGconn = Pointer;
  PPGconn = Pointer;

{ PGresult encapsulates the result of a query (or more precisely, of a single
  SQL command --- a query string given to PQsendQuery can contain multiple
  commands and thus return multiple PGresult objects).
  The contents of this struct are not supposed to be known to applications.
}
  PGresult = Pointer;
  PPGresult = Pointer;

{ PGnotify represents the occurrence of a NOTIFY message.
  Ideally this would be an opaque typedef, but it's so simple that it's
  unlikely to change.
  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  whereas in earlier versions it was always your own backend's PID.
}
  PGnotify = packed record
    relname: PAnsiChar; { name of relation containing data }
    be_pid: Integer; { process id of backend }
    extra: PAnsiChar;
  end;

  PPGnotify = ^PGnotify;

{ PQnoticeProcessor is the function type for the notice-message callback. }

  PQnoticeProcessor = procedure(arg: Pointer; message: PAnsiChar); cdecl;

{ Print options for PQprint() }

{
  We can't use the conventional "bool", because we are designed to be
  included in a user's program, and user may already have that type
  defined.  Pqbool, on the other hand, is unlikely to be used.
}

  PPChar = array[00..$FF] of PAnsiChar;

  PQprintOpt = packed record
    header: Byte; { print output field headings and row count }
    align: Byte; { fill align the fields }
    standard: Byte; { old brain dead format }
    html3: Byte; { output html tables }
    expanded: Byte; { expand tables }
    pager: Byte; { use pager for output if needed }
    fieldSep: PAnsiChar; { field separator }
    tableOpt: PAnsiChar; { insert to HTML <table ...> }
    caption: PAnsiChar; { HTML <caption> }
    fieldName: PPChar; { null terminated array of repalcement field names }
  end;

  PPQprintOpt = ^PQprintOpt;

{ ----------------
  Structure for the conninfo parameter definitions returned by PQconndefaults
  ----------------
}
  PQconninfoOption = packed record
    keyword: PAnsiChar; { The keyword of the option }
    envvar: PAnsiChar; { Fallback environment variable name }
    compiled: PAnsiChar; { Fallback compiled in default value  }
    val: PAnsiChar; { Options value	}
    lab: PAnsiChar; { Label for field in connect dialog }
    dispchar: PAnsiChar; { Character to display for this field
     in a connect dialog. Values are:
     ""	Display entered value as is
     "*"	Password field - hide value
     "D"	Debug options - don't
     create a field by default }
    dispsize: Integer; { Field size in characters for dialog }
  end;

  PPQConninfoOption = ^PQconninfoOption;

{ ----------------
  PQArgBlock -- structure for PQfn() arguments
  ----------------
}
  PQArgBlock = packed record
    len: Integer;
    isint: Integer;
    case u: Boolean of
      True: (ptr: PInteger); { can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PPQArgBlock = ^PQArgBlock;


{ ************** Plain API Function types definition ************* }

{ ===	in fe-connect.c === }
  TPQconnectdb = function(ConnInfo: PAnsiChar): PPGconn; cdecl; // FirmOS 8.1 OK
  TPQsetdbLogin = function(Host, Port, Options, Tty, Db, User, Passwd: PAnsiChar): PPGconn; cdecl; // FirmOS 8.1 OK
//15022006 FirmOS: omitting   PQconnectStart
//15022006 FirmOS: omitting  PQconnectPoll
  TPQconndefaults = function: PPQconninfoOption; cdecl;
  TPQfinish = procedure(Handle: PPGconn); cdecl;
  TPQreset = procedure(Handle: PPGconn); cdecl;

//15022006 FirmOS: omitting PQresetStart
//15022006 FirmOS: omitting PQresetPoll

  TPQrequestCancel = function(Handle: PPGconn): Integer; cdecl;

  TPQdb = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQuser = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQpass = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQhost = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQport = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQtty = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQoptions = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQstatus = function(Handle: PPGconn): ConnStatusType; cdecl;

//TBD  PGTransactionStatusType PQtransactionStatus(const PGconn *conn);

//15022006 FirmOS: omitting const char *PQparameterStatus(const PGconn *conn, const char *paramName);

//15022006 FirmOS: omitting  PQprotocolVersion
//15022006 FirmOS: omitting  PQserverVersion

  TPQerrorMessage = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQsocket = function(Handle: PPGconn): Integer; cdecl;
  TPQbackendPID = function(Handle: PPGconn): Integer; cdecl;

//15022006 FirmOS: omitting  SSL *PQgetssl(const PGconn *conn);


  TPQtrace = procedure(Handle: PPGconn; DebugPort: Pointer); cdecl;
  TPQuntrace = procedure(Handle: PPGconn); cdecl;
  TPQsetNoticeProcessor = procedure(Handle: PPGconn; Proc: PQnoticeProcessor; Arg: Pointer); cdecl;

{ === in fe-exec.c === }
  TPQexec = function(Handle: PPGconn; Query: PAnsiChar): PPGresult; cdecl;
  TPQnotifies = function(Handle: PPGconn): PPGnotify; cdecl;
  TPQfreeNotify = procedure(Handle: PPGnotify); cdecl;
  TPQsendQuery = function(Handle: PPGconn; Query: PAnsiChar): Integer; cdecl;
  TPQgetResult = function(Handle: PPGconn): PPGresult; cdecl;
  TPQisBusy = function(Handle: PPGconn): Integer; cdecl;
  TPQconsumeInput = function(Handle: PPGconn): Integer; cdecl;
  TPQgetline = function(Handle: PPGconn; Str: PAnsiChar; length: Integer): Integer; cdecl;
  TPQputline = function(Handle: PPGconn; Str: PAnsiChar): Integer; cdecl;
  TPQgetlineAsync = function(Handle: PPGconn; Buffer: PAnsiChar; BufSize: Integer): Integer; cdecl;
  TPQputnbytes = function(Handle: PPGconn; Buffer: PAnsiChar; NBytes: Integer): Integer; cdecl;
  TPQendcopy = function(Handle: PPGconn): Integer; cdecl;
  TPQfn = function(Handle: PPGconn; fnid: Integer; result_buf, result_len: PInteger; result_is_int: Integer; args: PPQArgBlock; nargs: Integer): PPGresult; cdecl;
  TPQresultStatus = function(Result: PPGresult): TExecStatusType; cdecl;
  TPQresultErrorMessage = function(Result: PPGresult): PAnsiChar; cdecl;

  //PAnsiChar = PUtf8Char
  TPQPrepare = function(Handle: PPGconn; Name, Query: PAnsiChar; nParams: Integer; pTypes: Pointer): PPGresult; cdecl;
  TPQExecPrepared = function(Handle: PPGconn; Name: PAnsiChar; nParams: Integer; pValues, pLength, pFormats: Pointer; rFormat: Integer): PPGresult; cdecl;
  TPQdescribePrepared = function(Handle: PPGconn; Name: PAnsiChar): PPGresult; cdecl;
  TPQnparams  = function(Result: PPGresult): Integer; cdecl;
  TPQparamtype = function(Result: PPGresult; param_num: Integer): Integer; cdecl;
  TPQsendQueryPrepared = function(Handle: PPGconn; Name: PAnsiChar; nParams: Integer; pValues, pLength, pFormats: Pointer; rFormat: Integer): Integer; cdecl;
  TPQsetSingleRowMode = function(Handle: PPGconn): Integer; cdecl;

  //p = params
  //r = result

//new  char *PQresultErrorField(const PGresult *res, int fieldcode);
  TPQresultErrorField = function(result: PPGResult; fieldcode: integer): PAnsiChar; cdecl;

  TPQntuples = function(Result: PPGresult): Integer; cdecl;
  TPQnfields = function(Result: PPGresult): Integer; cdecl;
  TPQbinaryTuples = function(Result: PPGresult): Integer; cdecl;
  TPQfname = function(Result: PPGresult; field_num: Integer): PAnsiChar; cdecl;
  TPQfnumber = function(Result: PPGresult; field_name: PAnsiChar): Integer; cdecl;
  TPQftype = function(Result: PPGresult; field_num: Integer): OID; cdecl;
  TPQfsize = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQfmod = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQcmdStatus = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQoidValue = function(Result: PPGresult): OID; cdecl;
  TPQoidStatus = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQcmdTuples = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQgetvalue = function(Result: PPGresult; tup_num, field_num: Integer): PAnsiChar; cdecl;
  TPQgetlength = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQgetisnull = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQclear = procedure(Result: PPGresult); cdecl;
  TPQmakeEmptyPGresult = function(Handle: PPGconn; status: TExecStatusType): PPGresult; cdecl;

//FirmOS: New defines

  TPQescapeByteaConn = function(Handle: PPGconn; const from: PAnsiChar; from_length: longword; to_lenght: PLongword): PAnsiChar; cdecl;
  TPQescapeBytea = function(const from: PByte; from_length: longword; to_lenght: PLongword): PByte; cdecl;

//TODO  TPQescapeString    =function(const from:PAnsiChar;from_length:longword;to_lenght:PLongword):PAnsiChar;cdecl;

//unsigned char *PQescapeByteaConn(PGconn *conn,
//                                 const unsigned char *from,
//                                 size_t from_length,
//                                 size_t *to_length);
  TPQunescapeBytea = function(const from: PByte; to_lenght: PLongword): PByte; cdecl;
//unsigned char *PQunescapeBytea(const unsigned char *from, size_t *to_length);

  TPQFreemem = procedure(ptr: Pointer); cdecl;
// void PQfreemem(void *ptr);

{ === in fe-lobj.c === }
  Tlo_open = function(Handle: PPGconn; lobjId: OID; mode: Integer): Integer; cdecl;
  Tlo_close = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_read = function(Handle: PPGconn; fd: Integer; buf: PAnsiChar; len: Integer): Integer; cdecl;
  Tlo_write = function(Handle: PPGconn; fd: Integer; buf: PAnsiChar; len: Integer): Integer; cdecl;
  Tlo_lseek = function(Handle: PPGconn; fd, offset, whence: Integer): Integer; cdecl;
  Tlo_creat = function(Handle: PPGconn; mode: Integer): OID; cdecl;
  Tlo_tell = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_unlink = function(Handle: PPGconn; lobjId: OID): Integer; cdecl;
  Tlo_import = function(Handle: PPGconn; filename: PAnsiChar): OID; cdecl;
  Tlo_export = function(Handle: PPGconn; lobjId: OID; filename: PAnsiChar): Integer; cdecl;
  Tlo_truncate = function(Handle: PPGconn; fd, len: Integer): Integer; cdecl;


{ ************* Plain API Function variables definition ************ }

var
{ ===	in fe-connect.c === }
  PQconnectdb: TPQconnectdb;
  PQsetdbLogin: TPQsetdbLogin;
  PQconndefaults: TPQconndefaults;
  PQfinish: TPQfinish;
  PQreset: TPQreset;
  PQrequestCancel: TPQrequestCancel;
  PQdb: TPQdb;
  PQuser: TPQuser;
  PQpass: TPQpass;
  PQhost: TPQhost;
  PQport: TPQport;
  PQtty: TPQtty;
  PQoptions: TPQoptions;
  PQstatus: TPQstatus;
  PQerrorMessage: TPQerrorMessage;
  PQsocket: TPQsocket;
  PQbackendPID: TPQbackendPID;
  PQtrace: TPQtrace;
  PQuntrace: TPQuntrace;
  PQsetNoticeProcessor: TPQsetNoticeProcessor;

{ === in fe-exec.c === }
  PQexec: TPQexec;
  PQnotifies: TPQnotifies;
  PQfreeNotify: TPQfreeNotify;
  PQsendQuery: TPQsendQuery;
  PQgetResult: TPQgetResult;
  PQisBusy: TPQisBusy;
  PQconsumeInput: TPQconsumeInput;
  PQgetline: TPQgetline;
  PQputline: TPQputline;
  PQgetlineAsync: TPQgetlineAsync;
  PQputnbytes: TPQputnbytes;
  PQendcopy: TPQendcopy;
  PQfn: TPQfn;
  PQresultStatus: TPQresultStatus;
  PQresultErrorMessage: TPQresultErrorMessage;
  PQresultErrorField: TPQresultErrorField; //Firmos
  PQntuples: TPQntuples;
  PQnfields: TPQnfields;
  PQbinaryTuples: TPQbinaryTuples;
  PQfname: TPQfname;
  PQfnumber: TPQfnumber;
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
  PQclear: TPQclear;
  PQmakeEmptyPGresult: TPQmakeEmptyPGresult;
  //belal
  PQPrepare: TPQPrepare;
  PQExecPrepared: TPQExecPrepared;
  PQdescribePrepared: TPQdescribePrepared;
  PQnparams: TPQnparams;
  PQparamtype: TPQparamtype;
  PQsendQueryPrepared: TPQsendQueryPrepared;
  PQsetSingleRowMode: TPQsetSingleRowMode;

//FirmOS: New defines
  PQescapeByteaConn: TPQescapeByteaConn;
  PQescapeBytea: TPQescapeBytea;
  PQunescapeBytea: TPQunescapeBytea;
  PQFreemem: TPQFreemem;

{ === in fe-lobj.c === }
  lo_open: Tlo_open;
  lo_close: Tlo_close;
  lo_read: Tlo_read;
  lo_write: Tlo_write;
  lo_lseek: Tlo_lseek;
  lo_creat: Tlo_creat;
  lo_tell: Tlo_tell;
  lo_unlink: Tlo_unlink;
  lo_import: Tlo_import;
  lo_export: Tlo_export;
  lo_truncate: Tlo_truncate;

type

  { TmncPGLib }

  TmncPGLib = class(TmnLibrary)
  protected
    procedure Link; override;
  end;

var
  PGLib: TmncPGLib = nil;

implementation

procedure TmncPGLib.Link;
begin
{ ===	in fe-connect.c === }
  PQfreemem := GetAddress('PQfreemem');
  PQescapeByteaConn := GetAddress('PQescapeByteaConn');
  PQescapeBytea := GetAddress('PQescapeBytea');
  PQunescapeBytea := GetAddress('PQunescapeBytea');

  PQconnectdb := GetAddress('PQconnectdb');
  PQsetdbLogin := GetAddress('PQsetdbLogin');
  PQconndefaults := GetAddress('PQconndefaults');
  PQfinish := GetAddress('PQfinish');
  PQreset := GetAddress('PQreset');
  PQrequestCancel := GetAddress('PQrequestCancel');
  PQdb := GetAddress('PQdb');
  PQuser := GetAddress('PQuser');
  PQpass := GetAddress('PQpass');
  PQhost := GetAddress('PQhost');
  PQport := GetAddress('PQport');
  PQtty := GetAddress('PQtty');
  PQoptions := GetAddress('PQoptions');
  PQstatus := GetAddress('PQstatus');
  PQerrorMessage := GetAddress('PQerrorMessage');
  PQsocket := GetAddress('PQsocket');
  PQbackendPID := GetAddress('PQbackendPID');
  PQtrace := GetAddress('PQtrace');
  PQuntrace := GetAddress('PQuntrace');
  PQsetNoticeProcessor := GetAddress('PQsetNoticeProcessor');

{ === in fe-exec.c === }
  PQexec := GetAddress('PQexec');
  PQnotifies := GetAddress('PQnotifies');
  PQfreeNotify := GetAddress('PQfreeNotify');
  PQsendQuery := GetAddress('PQsendQuery');
  PQgetResult := GetAddress('PQgetResult');
  PQisBusy := GetAddress('PQisBusy');
  PQconsumeInput := GetAddress('PQconsumeInput');
  PQgetline := GetAddress('PQgetline');
  PQputline := GetAddress('PQputline');
  PQgetlineAsync := GetAddress('PQgetlineAsync');
  PQputnbytes := GetAddress('PQputnbytes');
  PQendcopy := GetAddress('PQendcopy');
  PQfn := GetAddress('PQfn');
  PQresultStatus := GetAddress('PQresultStatus');
  PQresultErrorMessage := GetAddress('PQresultErrorMessage');
  PQresultErrorField := GetAddress('PQresultErrorField');
  PQntuples := GetAddress('PQntuples');
  PQnfields := GetAddress('PQnfields');
  PQbinaryTuples := GetAddress('PQbinaryTuples');
  PQfname := GetAddress('PQfname');
  PQfnumber := GetAddress('PQfnumber');
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
  PQclear := GetAddress('PQclear');
  PQmakeEmptyPGresult := GetAddress('PQmakeEmptyPGresult');
  PQPrepare := GetAddress('PQprepare');
  PQExecPrepared := GetAddress('PQexecPrepared');
  PQdescribePrepared := GetAddress('PQdescribePrepared');
  PQnparams := GetAddress('PQnparams');
  PQparamtype := GetAddress('PQparamtype');
  PQsendQueryPrepared := GetAddress('PQsendQueryPrepared');
  PQsetSingleRowMode := GetAddress('PQsetSingleRowMode');

{ === in fe-lobj.c === }
  lo_open := GetAddress('lo_open');
  lo_close := GetAddress('lo_close');
  lo_read := GetAddress('lo_read');
  lo_write := GetAddress('lo_write');
  lo_lseek := GetAddress('lo_lseek');
  lo_creat := GetAddress('lo_creat');
  lo_tell := GetAddress('lo_tell');
  lo_unlink := GetAddress('lo_unlink');
  lo_import := GetAddress('lo_import');
  lo_export := GetAddress('lo_export');
  lo_truncate := GetAddress('lo_truncate');
end;

initialization
  PGLib := TmncPGLib.Create('libpq');
finalization
  FreeAndNil(PGLib);
end.

