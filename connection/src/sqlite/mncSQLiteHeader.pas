unit mncSQLiteHeader;
{}
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 *
 * @author FPC Team
 *         modified by Zaher Dirkey <zaher, zaherdirkey>
 *
 *  This file ported from FPC sqlite3.inc, just to be compatiple in both Delphi and FPC
 *}
{
  SQqlite Version x.x.x //i forget it :P

  TODO:
  * Improve the header to reach last sqlite version.
}

{$IFDEF fpc}
{$MODE DELPHI}
{$ELSE}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF BSD}
{$LINKLIB c}
{$LINKLIB pthread}
{$ENDIF}

interface

uses
  Classes,
  mnLibraries,
  SysUtils;

{$ifdef FPC}
{$PACKRECORDS C}
{$endif}

const
  SQLITE_INTEGER = 1;
  SQLITE_FLOAT = 2;
{ #define SQLITE_TEXT  3  // See below  }
  SQLITE_BLOB = 4;
  SQLITE_NULL = 5;
  SQLITE_TEXT = 3;
  SQLITE3_TEXT = 3;
  SQLITE_UTF8 = 1;
  SQLITE_UTF16LE = 2;
  SQLITE_UTF16BE = 3;
{ Use native byte order  }
  SQLITE_UTF16 = 4;
{ sqlite3_create_function only  }
  SQLITE_ANY = 5;
  SQLITE_UTF16_ALIGNED = 8;

   //sqlite_exec return values
  SQLITE_OK = 0;
  SQLITE_ERROR = 1; { SQL error or missing database  }
  SQLITE_INTERNAL = 2; { An internal logic error in SQLite  }
  SQLITE_PERM = 3; { Access permission denied  }
  SQLITE_ABORT = 4; { Callback routine requested an abort  }
  SQLITE_BUSY = 5; { The database file is locked  }
  SQLITE_LOCKED = 6; { A table in the database is locked  }
  SQLITE_NOMEM = 7; { A malloc() failed  }
  SQLITE_READONLY = 8; { Attempt to write a readonly database  }
  SQLITE_INTERRUPT = 9; { Operation terminated by sqlite3_interrupt() }
  SQLITE_IOERR = 10; { Some kind of disk I/O error occurred  }
  SQLITE_CORRUPT = 11; { The database disk image is malformed  }
  SQLITE_NOTFOUND = 12; { (Internal Only) Table or record not found  }
  SQLITE_FULL = 13; { Insertion failed because database is full  }
  SQLITE_CANTOPEN = 14; { Unable to open the database file  }
  SQLITE_PROTOCOL = 15; { Database lock protocol error  }
  SQLITE_EMPTY = 16; { Database is empty  }
  SQLITE_META = 17; { The database meta changed  }
  SQLITE_TOOBIG = 18; { Too much data for one row of a table  }
  SQLITE_CONSTRAINT = 19; { Abort due to contraint violation  }
  SQLITE_MISMATCH = 20; { Data type mismatch  }
  SQLITE_MISUSE = 21; { Library used incorrectly  }
  SQLITE_NOLFS = 22; { Uses OS features not supported on host  }
  SQLITE_AUTH = 23; { Authorization denied  }
  SQLITE_FORMAT = 24; { Auxiliary database format error  }
  SQLITE_RANGE = 25; { 2nd parameter to sqlite3_bind out of range  }
  SQLITE_NOTADB = 26; { File opened that is not a database file  }
  SQLITE_ROW = 100; { sqlite3_step() has another row ready  }
  SQLITE_DONE = 101; { sqlite3_step() has finished executing  }

  SQLITE_COPY = 0;
  SQLITE_CREATE_INDEX = 1;
  SQLITE_CREATE_TABLE = 2;
  SQLITE_CREATE_TEMP_INDEX = 3;
  SQLITE_CREATE_TEMP_TABLE = 4;
  SQLITE_CREATE_TEMP_TRIGGER = 5;
  SQLITE_CREATE_TEMP_VIEW = 6;
  SQLITE_CREATE_TRIGGER = 7;
  SQLITE_CREATE_VIEW = 8;
  SQLITE_DELETE = 9;
  SQLITE_DROP_INDEX = 10;
  SQLITE_DROP_TABLE = 11;
  SQLITE_DROP_TEMP_INDEX = 12;
  SQLITE_DROP_TEMP_TABLE = 13;
  SQLITE_DROP_TEMP_TRIGGER = 14;
  SQLITE_DROP_TEMP_VIEW = 15;
  SQLITE_DROP_TRIGGER = 16;
  SQLITE_DROP_VIEW = 17;
  SQLITE_INSERT = 18;
  SQLITE_PRAGMA = 19;
  SQLITE_READ = 20;
  SQLITE_SELECT = 21;
  SQLITE_TRANSACTION = 22;
  SQLITE_UPDATE = 23;
  SQLITE_ATTACH = 24;
  SQLITE_DETACH = 25;
  SQLITE_ALTER_TABLE = 26;
  SQLITE_REINDEX = 27;
  SQLITE_ANALYZE = 28;
  SQLITE_CREATE_VTABLE = 29;
  SQLITE_DROP_VTABLE = 30;
  SQLITE_FUNCTION = 31;
  SQLITE_SAVEPOINT = 32;
  SQLITE_RECURSIVE = 33;


  SQLITE_DENY = 1;
  SQLITE_IGNORE = 2;

  SQLITE_OPEN_READONLY = $00000001;
  SQLITE_OPEN_READWRITE = $00000002;
  SQLITE_OPEN_CREATE = $00000004;
  SQLITE_OPEN_DELETEONCLOSE = $00000008;
  SQLITE_OPEN_EXCLUSIVE = $00000010;
  SQLITE_OPEN_MAIN_DB = $00000100;
  SQLITE_OPEN_TEMP_DB = $00000200;
  SQLITE_OPEN_TRANSIENT_DB = $00000400;
  SQLITE_OPEN_MAIN_JOURNAL = $00000800;
  SQLITE_OPEN_TEMP_JOURNAL = $00001000;
  SQLITE_OPEN_SUBJOURNAL = $00002000;
  SQLITE_OPEN_MASTER_JOURNAL = $00004000;
  SQLITE_OPEN_NOMUTEX = $00008000;
  SQLITE_OPEN_FULLMUTEX = $00010000;
  SQLITE_OPEN_SHAREDCACHE = $00020000;
  SQLITE_OPEN_PRIVATECACHE = $00040000;
  SQLITE_OPEN_WAL = $00080000;
  SQLITE_OPEN_NOFOLLOW = $01000000;

  SQLITE_IOCAP_ATOMIC                  = $00000001;
  SQLITE_IOCAP_ATOMIC512               = $00000002;
  SQLITE_IOCAP_ATOMIC1K                = $00000004;
  SQLITE_IOCAP_ATOMIC2K                = $00000008;
  SQLITE_IOCAP_ATOMIC4K                = $00000010;
  SQLITE_IOCAP_ATOMIC8K                = $00000020;
  SQLITE_IOCAP_ATOMIC16K               = $00000040;
  SQLITE_IOCAP_ATOMIC32K               = $00000080;
  SQLITE_IOCAP_ATOMIC64K               = $00000100;
  SQLITE_IOCAP_SAFE_APPEND             = $00000200;
  SQLITE_IOCAP_SEQUENTIAL              = $00000400;
  SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN   = $00000800;
  SQLITE_IOCAP_POWERSAFE_OVERWRITE     = $00001000;
  SQLITE_IOCAP_IMMUTABLE               = $00002000;
  SQLITE_IOCAP_BATCH_ATOMIC            = $00004000;

  SQLITE_LOCK_NONE         = 0;
  SQLITE_LOCK_SHARED       = 1;
  SQLITE_LOCK_RESERVED     = 2;
  SQLITE_LOCK_PENDING      = 3;
  SQLITE_LOCK_EXCLUSIVE    = 4;

  SQLITE_SYNC_NORMAL        = $00002;
  SQLITE_SYNC_FULL          = $00003;
  SQLITE_SYNC_DATAONLY      = $00010;

  SQLITE_FCNTL_LOCKSTATE             = 1;
  SQLITE_FCNTL_GET_LOCKPROXYFILE     = 2;
  SQLITE_FCNTL_SET_LOCKPROXYFILE     = 3;
  SQLITE_FCNTL_LAST_ERRNO            = 4;
  SQLITE_FCNTL_SIZE_HINT             = 5;
  SQLITE_FCNTL_CHUNK_SIZE            = 6;
  SQLITE_FCNTL_FILE_POINTER          = 7;
  SQLITE_FCNTL_SYNC_OMITTED          = 8;
  SQLITE_FCNTL_WIN32_AV_RETRY        = 9;
  SQLITE_FCNTL_PERSIST_WAL          = 10;
  SQLITE_FCNTL_OVERWRITE            = 11;
  SQLITE_FCNTL_VFSNAME              = 12;
  SQLITE_FCNTL_POWERSAFE_OVERWRITE  = 13;
  SQLITE_FCNTL_PRAGMA               = 14;
  SQLITE_FCNTL_BUSYHANDLER          = 15;
  SQLITE_FCNTL_TEMPFILENAME         = 16;
  SQLITE_FCNTL_MMAP_SIZE            = 18;
  SQLITE_FCNTL_TRACE                = 19;
  SQLITE_FCNTL_HAS_MOVED            = 20;
  SQLITE_FCNTL_SYNC                 = 21;
  SQLITE_FCNTL_COMMIT_PHASETWO      = 22;
  SQLITE_FCNTL_WIN32_SET_HANDLE     = 23;
  SQLITE_FCNTL_WAL_BLOCK            = 24;
  SQLITE_FCNTL_ZIPVFS               = 25;
  SQLITE_FCNTL_RBU                  = 26;
  SQLITE_FCNTL_VFS_POINTER          = 27;
  SQLITE_FCNTL_JOURNAL_POINTER      = 28;
  SQLITE_FCNTL_WIN32_GET_HANDLE     = 29;
  SQLITE_FCNTL_PDB                  = 30;
  SQLITE_FCNTL_BEGIN_ATOMIC_WRITE   = 31;
  SQLITE_FCNTL_COMMIT_ATOMIC_WRITE  = 32;
  SQLITE_FCNTL_ROLLBACK_ATOMIC_WRITE = 33;
  SQLITE_FCNTL_LOCK_TIMEOUT         = 34;
  SQLITE_FCNTL_DATA_VERSION         = 35;
  SQLITE_FCNTL_SIZE_LIMIT           = 36;
  SQLITE_FCNTL_CKPT_DONE            = 37;


// Original from sqlite3.h:
//#define SQLITE_STATIC      ((void(*)(void *))0)
//#define SQLITE_TRANSIENT   ((void(*)(void *))-1)
const
  SQLITE_STATIC = 0;
  SQLITE_TRANSIENT = -1;

type
  sqlite_int64 = int64;
  sqlite_uint64 = int64;
  PPPChar = ^PPAnsiChar;
  Psqlite3 = Pointer;
  PPSqlite3 = ^PSqlite3;
  Psqlite3_context = Pointer;
  Psqlite3_stmt = Pointer;
  PPsqlite3_stmt = ^Psqlite3_stmt;
  Psqlite3_value = Pointer;
  PPsqlite3_value = ^Psqlite3_value;

//Callback function types
//Notice that most functions were named using as prefix the function name that uses them,
//rather than describing their functions

  sqlite3_callback = function(_para1: pointer; _para2: longint; _para3: PPAnsiChar; _para4: PPAnsiChar): longint; cdecl;
  busy_handler_func = function(_para1: pointer; _para2: longint): longint; cdecl;
  sqlite3_set_authorizer_func = function(_para1: pointer; _para2: longint; _para3: PAnsiChar; _para4: PAnsiChar; _para5: PAnsiChar; _para6: PAnsiChar): longint; cdecl;
  sqlite3_trace_func = procedure(_para1: pointer; _para2: PAnsiChar); cdecl;
  sqlite3_progress_handler_func = function(_para1: pointer): longint; cdecl;
  sqlite3_commit_hook_func = function(_para1: pointer): longint; cdecl;
  bind_destructor_func = procedure(_para1: pointer); cdecl;
  create_function_step_func = procedure(_para1: Psqlite3_context; _para2: longint; _para3: PPsqlite3_value); cdecl;
  create_function_func_func = procedure(_para1: Psqlite3_context; _para2: longint; _para3: PPsqlite3_value); cdecl;
  create_function_final_func = procedure(_para1: Psqlite3_context); cdecl;
  sqlite3_set_auxdata_func = procedure(_para1: pointer); cdecl;
  sqlite3_result_func = procedure(_para1: pointer); cdecl;
  sqlite3_create_collation_func = function(_para1: pointer; _para2: longint; _para3: pointer; _para4: longint; _para5: pointer): longint; cdecl;
  sqlite3_collation_needed_func = procedure(_para1: pointer; _para2: Psqlite3; eTextRep: longint; _para4: PAnsiChar); cdecl;

{$IFNDEF WINDOWS}
{var
  //This is not working under windows. Any clues?
  sqlite3_temp_directory: PAnsiChar; cvar; external;}
{$ENDIF}

var
  sqlite3_close: function(_para1: Psqlite3): longint; cdecl;
  sqlite3_exec: function(_para1: Psqlite3; sql: PAnsiChar; _para3: sqlite3_callback; _para4: pointer; errmsg: PPAnsiChar): longint; cdecl;
  sqlite3_last_insert_rowid: function(_para1: Psqlite3): sqlite_int64; cdecl;
  sqlite3_changes: function(_para1: Psqlite3): longint; cdecl;
  sqlite3_total_changes: function(_para1: Psqlite3): longint; cdecl;
  sqlite3_interrupt: procedure(_para1: Psqlite3); cdecl;
  sqlite3_complete: function(sql: PAnsiChar): longint; cdecl;
  sqlite3_complete16: function(sql: pointer): longint; cdecl;
  sqlite3_busy_handler: function(_para1: Psqlite3; _para2: busy_handler_func; _para3: pointer): longint; cdecl;
  sqlite3_busy_timeout: function(_para1: Psqlite3; ms: longint): longint; cdecl;
  sqlite3_get_table: function(_para1: Psqlite3; sql: PAnsiChar; resultp: PPPChar; nrow: Plongint; ncolumn: Plongint; errmsg: PPAnsiChar): longint; cdecl;
  sqlite3_free_table: procedure(result: PPAnsiChar); cdecl;
// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
//   sqlite3_mprintf : function (_para1:PAnsiChar; args:array of const):PAnsiChar;cdecl;
  sqlite3_mprintf: function(_para1: PAnsiChar): PAnsiChar; cdecl;
//  sqlite3_vmprintf : function (_para1:PAnsiChar; _para2:va_list):PAnsiChar;cdecl;
  sqlite3_free: procedure(z: PAnsiChar); cdecl;
//  sqlite3_snprintf : function (_para1:longint; _para2:PAnsiChar; _para3:PAnsiChar; args:array of const):PAnsiChar;cdecl;
  sqlite3_snprintf: function(_para1: longint; _para2: PAnsiChar; _para3: PAnsiChar): PAnsiChar; cdecl;
  sqlite3_set_authorizer: function(_para1: Psqlite3; xAuth: sqlite3_set_authorizer_func; pUserData: pointer): longint; cdecl;
  sqlite3_trace: function(_para1: Psqlite3; xTrace: sqlite3_trace_func; _para3: pointer): pointer; cdecl;
  sqlite3_progress_handler: procedure(_para1: Psqlite3; _para2: longint; _para3: sqlite3_progress_handler_func; _para4: pointer); cdecl;
  sqlite3_commit_hook: function(_para1: Psqlite3; _para2: sqlite3_commit_hook_func; _para3: pointer): pointer; cdecl;
  sqlite3_open: function(filename: PAnsiChar; ppDb: PPsqlite3): longint; cdecl;
  sqlite3_open_v2: function(filename: PAnsiChar; ppDb: PPsqlite3; flags: longint; zVfs: PAnsiChar): longint; cdecl;
  sqlite3_open16: function(filename: pointer; ppDb: PPsqlite3): longint; cdecl;
  sqlite3_errcode: function(db: Psqlite3): longint; cdecl;
  sqlite3_errmsg: function(_para1: Psqlite3): PAnsiChar; cdecl;
  sqlite3_errmsg16: function(_para1: Psqlite3): pointer; cdecl;
  sqlite3_prepare: function(db: Psqlite3; zSql: PAnsiChar; nBytes: longint; ppStmt: PPsqlite3_stmt; pzTail: PPAnsiChar): longint; cdecl;
  sqlite3_prepare16: function(db: Psqlite3; zSql: pointer; nBytes: longint; ppStmt: PPsqlite3_stmt; pzTail: Ppointer): longint; cdecl;
  sqlite3_bind_blob: function(_para1: Psqlite3_stmt; _para2: longint; _para3: pointer; n: longint; _para5: bind_destructor_func): longint; cdecl;
  sqlite3_bind_double: function(_para1: Psqlite3_stmt; _para2: longint; _para3: double): longint; cdecl;
  sqlite3_bind_int: function(_para1: Psqlite3_stmt; _para2: longint; _para3: longint): longint; cdecl;
  sqlite3_bind_int64: function(_para1: Psqlite3_stmt; _para2: longint; _para3: sqlite_int64): longint; cdecl;
  sqlite3_bind_null: function(_para1: Psqlite3_stmt; _para2: longint): longint; cdecl;
  sqlite3_bind_text: function(_para1: Psqlite3_stmt; _para2: longint; _para3: PAnsiChar; n: longint; _para5: bind_destructor_func): longint; cdecl;
  sqlite3_bind_text16: function(_para1: Psqlite3_stmt; _para2: longint; _para3: pointer; _para4: longint; _para5: bind_destructor_func): longint; cdecl;
//  sqlite3_bind_value : function (_para1:Psqlite3_stmt; _para2:longint; _para3:Psqlite3_value):longint;cdecl;
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
  sqlite3_bind_blob1: function(_para1: Psqlite3_stmt; _para2: longint; _para3: pointer; n: longint; _para5: longint): longint; cdecl;
  sqlite3_bind_text1: function(_para1: Psqlite3_stmt; _para2: longint; _para3: PAnsiChar; n: longint; _para5: longint): longint; cdecl;
  sqlite3_bind_text161: function(_para1: Psqlite3_stmt; _para2: longint; _para3: pointer; _para4: longint; _para5: longint): longint; cdecl;

  sqlite3_bind_parameter_count: function(_para1: Psqlite3_stmt): longint; cdecl;
  sqlite3_bind_parameter_name: function(_para1: Psqlite3_stmt; _para2: longint): PAnsiChar; cdecl;
  sqlite3_bind_parameter_index: function(_para1: Psqlite3_stmt; zName: PAnsiChar): longint; cdecl;
//  sqlite3_clear_bindings : function (_para1:Psqlite3_stmt):longint;cdecl;
  sqlite3_column_count: function(pStmt: Psqlite3_stmt): longint; cdecl;
  sqlite3_column_name: function(_para1: Psqlite3_stmt; _para2: longint): PAnsiChar; cdecl;
  sqlite3_column_name16: function(_para1: Psqlite3_stmt; _para2: longint): pointer; cdecl;
  sqlite3_column_decltype: function(_para1: Psqlite3_stmt; i: longint): PAnsiChar; cdecl;
  sqlite3_column_decltype16: function(_para1: Psqlite3_stmt; _para2: longint): pointer; cdecl;
  sqlite3_step: function(_para1: Psqlite3_stmt): longint; cdecl;
  sqlite3_data_count: function(pStmt: Psqlite3_stmt): longint; cdecl;
  sqlite3_column_blob: function(_para1: Psqlite3_stmt; iCol: longint): pointer; cdecl;
  sqlite3_column_bytes: function(_para1: Psqlite3_stmt; iCol: longint): longint; cdecl;
  sqlite3_column_bytes16: function(_para1: Psqlite3_stmt; iCol: longint): longint; cdecl;
  sqlite3_column_double: function(_para1: Psqlite3_stmt; iCol: longint): double; cdecl;
  sqlite3_column_int: function(_para1: Psqlite3_stmt; iCol: longint): longint; cdecl;
  sqlite3_column_int64: function(_para1: Psqlite3_stmt; iCol: longint): sqlite_int64; cdecl;
  sqlite3_column_text: function(_para1: Psqlite3_stmt; iCol: longint): PAnsiChar; cdecl;
  sqlite3_column_text16: function(_para1: Psqlite3_stmt; iCol: longint): pointer; cdecl;
  sqlite3_column_type: function(_para1: Psqlite3_stmt; iCol: longint): longint; cdecl;
  sqlite3_finalize: function(pStmt: Psqlite3_stmt): longint; cdecl;
  sqlite3_reset: function(pStmt: Psqlite3_stmt): longint; cdecl;
  sqlite3_create_function: function(_para1: Psqlite3; zfunctionName: PAnsiChar; nArg: longint; eTextRep: longint; _para5: pointer; xFunc: create_function_func_func; xStep: create_function_step_func; xFinal: create_function_final_func): longint; cdecl;
  sqlite3_create_function16: function(_para1: Psqlite3; zfunctionName: pointer; nArg: longint; eTextRep: longint; _para5: pointer; xFunc: create_function_func_func; xStep: create_function_step_func; xFinal: create_function_final_func): longint; cdecl;
  sqlite3_aggregate_count: function(_para1: Psqlite3_context): longint; cdecl;
  sqlite3_value_blob: function(_para1: Psqlite3_value): pointer; cdecl;
  sqlite3_value_bytes: function(_para1: Psqlite3_value): longint; cdecl;
  sqlite3_value_bytes16: function(_para1: Psqlite3_value): longint; cdecl;
  sqlite3_value_double: function(_para1: Psqlite3_value): double; cdecl;
  sqlite3_value_int: function(_para1: Psqlite3_value): longint; cdecl;
  sqlite3_value_int64: function(_para1: Psqlite3_value): sqlite_int64; cdecl;
  sqlite3_value_text: function(_para1: Psqlite3_value): PAnsiChar; cdecl;
  sqlite3_value_text16: function(_para1: Psqlite3_value): pointer; cdecl;
  sqlite3_value_text16le: function(_para1: Psqlite3_value): pointer; cdecl;
  sqlite3_value_text16be: function(_para1: Psqlite3_value): pointer; cdecl;
  sqlite3_value_type: function(_para1: Psqlite3_value): longint; cdecl;
  sqlite3_aggregate_context: function(_para1: Psqlite3_context; nBytes: longint): pointer; cdecl;
  sqlite3_user_data: function(_para1: Psqlite3_context): pointer; cdecl;
  sqlite3_get_auxdata: function(_para1: Psqlite3_context; _para2: longint): pointer; cdecl;
  sqlite3_set_auxdata: procedure(_para1: Psqlite3_context; _para2: longint; _para3: pointer; _para4: sqlite3_set_auxdata_func); cdecl;
  sqlite3_result_blob: procedure(_para1: Psqlite3_context; _para2: pointer; _para3: longint; _para4: sqlite3_result_func); cdecl;
  sqlite3_result_double: procedure(_para1: Psqlite3_context; _para2: double); cdecl;
  sqlite3_result_error: procedure(_para1: Psqlite3_context; _para2: PAnsiChar; _para3: longint); cdecl;
  sqlite3_result_error16: procedure(_para1: Psqlite3_context; _para2: pointer; _para3: longint); cdecl;
  sqlite3_result_int: procedure(_para1: Psqlite3_context; _para2: longint); cdecl;
  sqlite3_result_int64: procedure(_para1: Psqlite3_context; _para2: sqlite_int64); cdecl;
  sqlite3_result_null: procedure(_para1: Psqlite3_context); cdecl;
  sqlite3_result_text: procedure(_para1: Psqlite3_context; _para2: PAnsiChar; _para3: longint; _para4: sqlite3_result_func); cdecl;
  sqlite3_result_text16: procedure(_para1: Psqlite3_context; _para2: pointer; _para3: longint; _para4: sqlite3_result_func); cdecl;
  sqlite3_result_text16le: procedure(_para1: Psqlite3_context; _para2: pointer; _para3: longint; _para4: sqlite3_result_func); cdecl;
  sqlite3_result_text16be: procedure(_para1: Psqlite3_context; _para2: pointer; _para3: longint; _para4: sqlite3_result_func); cdecl;
  sqlite3_result_value: procedure(_para1: Psqlite3_context; _para2: Psqlite3_value); cdecl;
  sqlite3_create_collation: function(_para1: Psqlite3; zName: PAnsiChar; eTextRep: longint; _para4: pointer; xCompare: sqlite3_create_collation_func): longint; cdecl;
  sqlite3_create_collation16: function(_para1: Psqlite3; zName: PAnsiChar; eTextRep: longint; _para4: pointer; xCompare: sqlite3_create_collation_func): longint; cdecl;
  sqlite3_collation_needed: function(_para1: Psqlite3; _para2: pointer; _para3: sqlite3_collation_needed_func): longint; cdecl;
  sqlite3_collation_needed16: function(_para1: Psqlite3; _para2: pointer; _para3: sqlite3_collation_needed_func): longint; cdecl;
  sqlite3_libversion: function: PAnsiChar; cdecl;
//Alias for allowing better code portability (win32 is not working with external variables)
  sqlite3_version: function: PAnsiChar; cdecl;

// Not published functions
  sqlite3_libversion_number: function: longint; cdecl;
//  sqlite3_key : function (db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;
//  sqlite3_rekey : function (db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;
//  sqlite3_sleep : function (_para1:longint):longint;cdecl;
//  sqlite3_expired : function (_para1:Psqlite3_stmt):longint;cdecl;
//function sqlite3_global_recover:longint;cdecl;

type

  { TmncSQLiteLib }

  TmncSQLiteLib = class(TmnLibrary)
  protected
    procedure Link; override;
  end;

var
  SQLiteLib: TmncSQLiteLib = nil;

implementation

{ TmncSQLiteLib }

procedure TmncSQLiteLib.Link;
begin
  @sqlite3_close := GetAddress('sqlite3_close');
  @sqlite3_exec := GetAddress('sqlite3_exec');
  @sqlite3_last_insert_rowid := GetAddress('sqlite3_last_insert_rowid');
  @sqlite3_changes := GetAddress('sqlite3_changes');
  @sqlite3_total_changes := GetAddress('sqlite3_total_changes');
  @sqlite3_interrupt := GetAddress('sqlite3_interrupt');
  @sqlite3_complete := GetAddress('sqlite3_complete');
  @sqlite3_complete16 := GetAddress('sqlite3_complete16');
  @sqlite3_busy_handler := GetAddress('sqlite3_busy_handler');
  @sqlite3_busy_timeout := GetAddress('sqlite3_busy_timeout');
  @sqlite3_get_table := GetAddress('sqlite3_get_table');
  @sqlite3_free_table := GetAddress('sqlite3_free_table');
// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
//   @sqlite3_mprintf := GetAddress(Handle,'sqlite3_mprintf');
  @sqlite3_mprintf := GetAddress('sqlite3_mprintf');
//  @(sqlite3_vmprintf := GetAddress(Handle,'sqlite3_vmprintf');
  @sqlite3_free := GetAddress('sqlite3_free');
//  @(sqlite3_snprintf := GetAddress(Handle,'sqlite3_snprintf');
  @sqlite3_snprintf := GetAddress('sqlite3_snprintf');
  @sqlite3_set_authorizer := GetAddress('sqlite3_set_authorizer');
  @sqlite3_trace := GetAddress('sqlite3_trace');
  @sqlite3_progress_handler := GetAddress('sqlite3_progress_handler');
  @sqlite3_commit_hook := GetAddress('sqlite3_commit_hook');
  @sqlite3_open := GetAddress('sqlite3_open');
  @sqlite3_open_v2 := GetAddress('sqlite3_open_v2');
  @sqlite3_open16 := GetAddress('sqlite3_open16');
  @sqlite3_errcode := GetAddress('sqlite3_errcode');
  @sqlite3_errmsg := GetAddress('sqlite3_errmsg');
  @sqlite3_errmsg16 := GetAddress('sqlite3_errmsg16');
  @sqlite3_prepare := GetAddress('sqlite3_prepare');
  @sqlite3_prepare16 := GetAddress('sqlite3_prepare16');
  @sqlite3_bind_blob := GetAddress('sqlite3_bind_blob');
  @sqlite3_bind_double := GetAddress('sqlite3_bind_double');
  @sqlite3_bind_int := GetAddress('sqlite3_bind_int');
  @sqlite3_bind_int64 := GetAddress('sqlite3_bind_int64');
  @sqlite3_bind_null := GetAddress('sqlite3_bind_null');
  @sqlite3_bind_text := GetAddress('sqlite3_bind_text');
  @sqlite3_bind_text16 := GetAddress('sqlite3_bind_text16');
//  pointer(sqlite3_bind_value) := GetAddress(Handle,'sqlite3_bind_value');
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
  @sqlite3_bind_blob1 := GetAddress('sqlite3_bind_blob');
  @sqlite3_bind_text1 := GetAddress('sqlite3_bind_text');
  @sqlite3_bind_text161 := GetAddress('sqlite3_bind_text16');

  @sqlite3_bind_parameter_count := GetAddress('sqlite3_bind_parameter_count');
  @sqlite3_bind_parameter_name := GetAddress('sqlite3_bind_parameter_name');
  @sqlite3_bind_parameter_index := GetAddress('sqlite3_bind_parameter_index');
//  @(sqlite3_clear_bindings) := GetAddress(Handle,'sqlite3_clear_bindings');
  @sqlite3_column_count := GetAddress('sqlite3_column_count');
  @sqlite3_column_name := GetAddress('sqlite3_column_name');
  @sqlite3_column_name16 := GetAddress('sqlite3_column_name16');
  @sqlite3_column_decltype := GetAddress('sqlite3_column_decltype');
  @sqlite3_column_decltype16 := GetAddress('sqlite3_column_decltype16');
  @sqlite3_step := GetAddress('sqlite3_step');
  @sqlite3_data_count := GetAddress('sqlite3_data_count');
  @sqlite3_column_blob := GetAddress('sqlite3_column_blob');
  @sqlite3_column_bytes := GetAddress('sqlite3_column_bytes');
  @sqlite3_column_bytes16 := GetAddress('sqlite3_column_bytes16');
  @sqlite3_column_double := GetAddress('sqlite3_column_double');
  @sqlite3_column_int := GetAddress('sqlite3_column_int');
  @sqlite3_column_int64 := GetAddress('sqlite3_column_int64');
  @sqlite3_column_text := GetAddress('sqlite3_column_text');
  @sqlite3_column_text16 := GetAddress('sqlite3_column_text16');
  @sqlite3_column_type := GetAddress('sqlite3_column_type');
  @sqlite3_finalize := GetAddress('sqlite3_finalize');
  @sqlite3_reset := GetAddress('sqlite3_reset');
  @sqlite3_create_function := GetAddress('sqlite3_create_function');
  @sqlite3_create_function16 := GetAddress('sqlite3_create_function16');
  @sqlite3_aggregate_count := GetAddress('sqlite3_aggregate_count');
  @sqlite3_value_blob := GetAddress('sqlite3_value_blob');
  @sqlite3_value_bytes := GetAddress('sqlite3_value_bytes');
  @sqlite3_value_bytes16 := GetAddress('sqlite3_value_bytes16');
  @sqlite3_value_double := GetAddress('sqlite3_value_double');
  @sqlite3_value_int := GetAddress('sqlite3_value_int');
  @sqlite3_value_int64 := GetAddress('sqlite3_value_int64');
  @sqlite3_value_text := GetAddress('sqlite3_value_text');
  @sqlite3_value_text16 := GetAddress('sqlite3_value_text16');
  @sqlite3_value_text16le := GetAddress('sqlite3_value_text16le');
  @sqlite3_value_text16be := GetAddress('sqlite3_value_text16be');
  @sqlite3_value_type := GetAddress('sqlite3_value_type');
  @sqlite3_aggregate_context := GetAddress('sqlite3_aggregate_context');
  @sqlite3_user_data := GetAddress('sqlite3_user_data');
  @sqlite3_get_auxdata := GetAddress('sqlite3_get_auxdata');
  @sqlite3_set_auxdata := GetAddress('sqlite3_set_auxdata');
  @sqlite3_result_blob := GetAddress('sqlite3_result_blob');
  @sqlite3_result_double := GetAddress('sqlite3_result_double');
  @sqlite3_result_error := GetAddress('sqlite3_result_error');
  @sqlite3_result_error16 := GetAddress('sqlite3_result_error16');
  @sqlite3_result_int := GetAddress('sqlite3_result_int');
  @sqlite3_result_int64 := GetAddress('sqlite3_result_int64');
  @sqlite3_result_null := GetAddress('sqlite3_result_null');
  @sqlite3_result_text := GetAddress('sqlite3_result_text');
  @sqlite3_result_text16 := GetAddress('sqlite3_result_text16');
  @sqlite3_result_text16le := GetAddress('sqlite3_result_text16le');
  @sqlite3_result_text16be := GetAddress('sqlite3_result_text16be');
  @sqlite3_result_value := GetAddress('sqlite3_result_value');
  @sqlite3_create_collation := GetAddress('sqlite3_create_collation');
  @sqlite3_create_collation16 := GetAddress('sqlite3_create_collation16');
  @sqlite3_collation_needed := GetAddress('sqlite3_collation_needed');
  @sqlite3_collation_needed16 := GetAddress('sqlite3_collation_needed16');
  @sqlite3_libversion := GetAddress('sqlite3_libversion');
//Alias for allowing better code portability (win32 is not working with external variables)
  @sqlite3_version := GetAddress('sqlite3_libversion');

// Not published functions
  @sqlite3_libversion_number := GetAddress('sqlite3_libversion_number');
//  @(sqlite3_key) := GetAddress(Handle,'sqlite3_key');
//  @(sqlite3_rekey) := GetAddress(Handle,'sqlite3_rekey');
//  @(sqlite3_sleep) := GetAddress(Handle,'sqlite3_sleep');
//  @(sqlite3_expired) := GetAddress(Handle,'sqlite3_expired');
//  function sqlite3_global_recover:longint;cdecl;
end;

initialization
  SQLiteLib := TmncSQLiteLib.Create('sqlite3');
finalization
  FreeAndNil(SQLiteLib);
end.
