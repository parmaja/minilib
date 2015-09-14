unit mncSQLiteHeader;
{}
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 *
 * @author FPC Team
 *         modified by Zaher Dirkey <zaher at parmaja dot com>
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
{$ifdef FPC}
  dynlibs,
{$else }
  Windows,
{$endif}
  SysUtils;

{$ifdef FPC}
{$PACKRECORDS C}
{$else DELPHI}
type
  TLibHandle = System.THandle;
{$endif}

const
{$IFDEF WINDOWS}
  Sqlite3Lib = 'sqlite3.dll';
{$ELSE}
  Sqlite3Lib = 'libsqlite3.'+sharedsuffix;
{$ENDIF}

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
  SQLITE_Meta = 17; { The database Meta changed  }
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

// Original from sqlite3.h:
//#define SQLITE_STATIC      ((void(*)(void *))0)
//#define SQLITE_TRANSIENT   ((void(*)(void *))-1)
const
  SQLITE_STATIC = 0;
  SQLITE_TRANSIENT = -1;

type
  sqlite_int64 = int64;
  sqlite_uint64 = int64;
  PPPChar = ^PPChar;
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

  sqlite3_callback = function(_para1: pointer; _para2: longint; _para3: PPchar; _para4: PPchar): longint; cdecl;
  busy_handler_func = function(_para1: pointer; _para2: longint): longint; cdecl;
  sqlite3_set_authorizer_func = function(_para1: pointer; _para2: longint; _para3: Pchar; _para4: Pchar; _para5: Pchar; _para6: Pchar): longint; cdecl;
  sqlite3_trace_func = procedure(_para1: pointer; _para2: Pchar); cdecl;
  sqlite3_progress_handler_func = function(_para1: pointer): longint; cdecl;
  sqlite3_commit_hook_func = function(_para1: pointer): longint; cdecl;
  bind_destructor_func = procedure(_para1: pointer); cdecl;
  create_function_step_func = procedure(_para1: Psqlite3_context; _para2: longint; _para3: PPsqlite3_value); cdecl;
  create_function_func_func = procedure(_para1: Psqlite3_context; _para2: longint; _para3: PPsqlite3_value); cdecl;
  create_function_final_func = procedure(_para1: Psqlite3_context); cdecl;
  sqlite3_set_auxdata_func = procedure(_para1: pointer); cdecl;
  sqlite3_result_func = procedure(_para1: pointer); cdecl;
  sqlite3_create_collation_func = function(_para1: pointer; _para2: longint; _para3: pointer; _para4: longint; _para5: pointer): longint; cdecl;
  sqlite3_collation_needed_func = procedure(_para1: pointer; _para2: Psqlite3; eTextRep: longint; _para4: Pchar); cdecl;

{$IFNDEF WINDOWS}
var
  //This is not working under windows. Any clues?
  sqlite3_temp_directory: Pchar; cvar; external;
{$ENDIF}

var
  sqlite3_close: function(_para1: Psqlite3): longint; cdecl;
  sqlite3_exec: function(_para1: Psqlite3; sql: Pchar; _para3: sqlite3_callback; _para4: pointer; errmsg: PPchar): longint; cdecl;
  sqlite3_last_insert_rowid: function(_para1: Psqlite3): sqlite_int64; cdecl;
  sqlite3_changes: function(_para1: Psqlite3): longint; cdecl;
  sqlite3_total_changes: function(_para1: Psqlite3): longint; cdecl;
  sqlite3_interrupt: procedure(_para1: Psqlite3); cdecl;
  sqlite3_complete: function(sql: Pchar): longint; cdecl;
  sqlite3_complete16: function(sql: pointer): longint; cdecl;
  sqlite3_busy_handler: function(_para1: Psqlite3; _para2: busy_handler_func; _para3: pointer): longint; cdecl;
  sqlite3_busy_timeout: function(_para1: Psqlite3; ms: longint): longint; cdecl;
  sqlite3_get_table: function(_para1: Psqlite3; sql: Pchar; resultp: PPPchar; nrow: Plongint; ncolumn: Plongint; errmsg: PPchar): longint; cdecl;
  sqlite3_free_table: procedure(result: PPchar); cdecl;
// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
//   sqlite3_mprintf : function (_para1:Pchar; args:array of const):Pchar;cdecl;
  sqlite3_mprintf: function(_para1: Pchar): Pchar; cdecl;
//  sqlite3_vmprintf : function (_para1:Pchar; _para2:va_list):Pchar;cdecl;
  sqlite3_free: procedure(z: Pchar); cdecl;
//  sqlite3_snprintf : function (_para1:longint; _para2:Pchar; _para3:Pchar; args:array of const):Pchar;cdecl;
  sqlite3_snprintf: function(_para1: longint; _para2: Pchar; _para3: Pchar): Pchar; cdecl;
  sqlite3_set_authorizer: function(_para1: Psqlite3; xAuth: sqlite3_set_authorizer_func; pUserData: pointer): longint; cdecl;
  sqlite3_trace: function(_para1: Psqlite3; xTrace: sqlite3_trace_func; _para3: pointer): pointer; cdecl;
  sqlite3_progress_handler: procedure(_para1: Psqlite3; _para2: longint; _para3: sqlite3_progress_handler_func; _para4: pointer); cdecl;
  sqlite3_commit_hook: function(_para1: Psqlite3; _para2: sqlite3_commit_hook_func; _para3: pointer): pointer; cdecl;
  sqlite3_open: function(filename: Pchar; ppDb: PPsqlite3): longint; cdecl;
  sqlite3_open_v2: function(filename: Pchar; ppDb: PPsqlite3; flags: longint; zVfs: pchar): longint; cdecl;
  sqlite3_open16: function(filename: pointer; ppDb: PPsqlite3): longint; cdecl;
  sqlite3_errcode: function(db: Psqlite3): longint; cdecl;
  sqlite3_errmsg: function(_para1: Psqlite3): Pchar; cdecl;
  sqlite3_errmsg16: function(_para1: Psqlite3): pointer; cdecl;
  sqlite3_prepare: function(db: Psqlite3; zSql: Pchar; nBytes: longint; ppStmt: PPsqlite3_stmt; pzTail: PPchar): longint; cdecl;
  sqlite3_prepare16: function(db: Psqlite3; zSql: pointer; nBytes: longint; ppStmt: PPsqlite3_stmt; pzTail: Ppointer): longint; cdecl;
  sqlite3_bind_blob: function(_para1: Psqlite3_stmt; _para2: longint; _para3: pointer; n: longint; _para5: bind_destructor_func): longint; cdecl;
  sqlite3_bind_double: function(_para1: Psqlite3_stmt; _para2: longint; _para3: double): longint; cdecl;
  sqlite3_bind_int: function(_para1: Psqlite3_stmt; _para2: longint; _para3: longint): longint; cdecl;
  sqlite3_bind_int64: function(_para1: Psqlite3_stmt; _para2: longint; _para3: sqlite_int64): longint; cdecl;
  sqlite3_bind_null: function(_para1: Psqlite3_stmt; _para2: longint): longint; cdecl;
  sqlite3_bind_text: function(_para1: Psqlite3_stmt; _para2: longint; _para3: Pchar; n: longint; _para5: bind_destructor_func): longint; cdecl;
  sqlite3_bind_text16: function(_para1: Psqlite3_stmt; _para2: longint; _para3: pointer; _para4: longint; _para5: bind_destructor_func): longint; cdecl;
//  sqlite3_bind_value : function (_para1:Psqlite3_stmt; _para2:longint; _para3:Psqlite3_value):longint;cdecl;
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
  sqlite3_bind_blob1: function(_para1: Psqlite3_stmt; _para2: longint; _para3: pointer; n: longint; _para5: longint): longint; cdecl;
  sqlite3_bind_text1: function(_para1: Psqlite3_stmt; _para2: longint; _para3: Pchar; n: longint; _para5: longint): longint; cdecl;
  sqlite3_bind_text161: function(_para1: Psqlite3_stmt; _para2: longint; _para3: pointer; _para4: longint; _para5: longint): longint; cdecl;

  sqlite3_bind_parameter_count: function(_para1: Psqlite3_stmt): longint; cdecl;
  sqlite3_bind_parameter_name: function(_para1: Psqlite3_stmt; _para2: longint): Pchar; cdecl;
  sqlite3_bind_parameter_index: function(_para1: Psqlite3_stmt; zName: Pchar): longint; cdecl;
//  sqlite3_clear_bindings : function (_para1:Psqlite3_stmt):longint;cdecl;
  sqlite3_column_count: function(pStmt: Psqlite3_stmt): longint; cdecl;
  sqlite3_column_name: function(_para1: Psqlite3_stmt; _para2: longint): Pchar; cdecl;
  sqlite3_column_name16: function(_para1: Psqlite3_stmt; _para2: longint): pointer; cdecl;
  sqlite3_column_decltype: function(_para1: Psqlite3_stmt; i: longint): Pchar; cdecl;
  sqlite3_column_decltype16: function(_para1: Psqlite3_stmt; _para2: longint): pointer; cdecl;
  sqlite3_step: function(_para1: Psqlite3_stmt): longint; cdecl;
  sqlite3_data_count: function(pStmt: Psqlite3_stmt): longint; cdecl;
  sqlite3_column_blob: function(_para1: Psqlite3_stmt; iCol: longint): pointer; cdecl;
  sqlite3_column_bytes: function(_para1: Psqlite3_stmt; iCol: longint): longint; cdecl;
  sqlite3_column_bytes16: function(_para1: Psqlite3_stmt; iCol: longint): longint; cdecl;
  sqlite3_column_double: function(_para1: Psqlite3_stmt; iCol: longint): double; cdecl;
  sqlite3_column_int: function(_para1: Psqlite3_stmt; iCol: longint): longint; cdecl;
  sqlite3_column_int64: function(_para1: Psqlite3_stmt; iCol: longint): sqlite_int64; cdecl;
  sqlite3_column_text: function(_para1: Psqlite3_stmt; iCol: longint): PChar; cdecl;
  sqlite3_column_text16: function(_para1: Psqlite3_stmt; iCol: longint): pointer; cdecl;
  sqlite3_column_type: function(_para1: Psqlite3_stmt; iCol: longint): longint; cdecl;
  sqlite3_finalize: function(pStmt: Psqlite3_stmt): longint; cdecl;
  sqlite3_reset: function(pStmt: Psqlite3_stmt): longint; cdecl;
  sqlite3_create_function: function(_para1: Psqlite3; zfunctionName: Pchar; nArg: longint; eTextRep: longint; _para5: pointer; xFunc: create_function_func_func; xStep: create_function_step_func; xFinal: create_function_final_func): longint; cdecl;
  sqlite3_create_function16: function(_para1: Psqlite3; zfunctionName: pointer; nArg: longint; eTextRep: longint; _para5: pointer; xFunc: create_function_func_func; xStep: create_function_step_func; xFinal: create_function_final_func): longint; cdecl;
  sqlite3_aggregate_count: function(_para1: Psqlite3_context): longint; cdecl;
  sqlite3_value_blob: function(_para1: Psqlite3_value): pointer; cdecl;
  sqlite3_value_bytes: function(_para1: Psqlite3_value): longint; cdecl;
  sqlite3_value_bytes16: function(_para1: Psqlite3_value): longint; cdecl;
  sqlite3_value_double: function(_para1: Psqlite3_value): double; cdecl;
  sqlite3_value_int: function(_para1: Psqlite3_value): longint; cdecl;
  sqlite3_value_int64: function(_para1: Psqlite3_value): sqlite_int64; cdecl;
  sqlite3_value_text: function(_para1: Psqlite3_value): PChar; cdecl;
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
  sqlite3_result_error: procedure(_para1: Psqlite3_context; _para2: Pchar; _para3: longint); cdecl;
  sqlite3_result_error16: procedure(_para1: Psqlite3_context; _para2: pointer; _para3: longint); cdecl;
  sqlite3_result_int: procedure(_para1: Psqlite3_context; _para2: longint); cdecl;
  sqlite3_result_int64: procedure(_para1: Psqlite3_context; _para2: sqlite_int64); cdecl;
  sqlite3_result_null: procedure(_para1: Psqlite3_context); cdecl;
  sqlite3_result_text: procedure(_para1: Psqlite3_context; _para2: Pchar; _para3: longint; _para4: sqlite3_result_func); cdecl;
  sqlite3_result_text16: procedure(_para1: Psqlite3_context; _para2: pointer; _para3: longint; _para4: sqlite3_result_func); cdecl;
  sqlite3_result_text16le: procedure(_para1: Psqlite3_context; _para2: pointer; _para3: longint; _para4: sqlite3_result_func); cdecl;
  sqlite3_result_text16be: procedure(_para1: Psqlite3_context; _para2: pointer; _para3: longint; _para4: sqlite3_result_func); cdecl;
  sqlite3_result_value: procedure(_para1: Psqlite3_context; _para2: Psqlite3_value); cdecl;
  sqlite3_create_collation: function(_para1: Psqlite3; zName: Pchar; eTextRep: longint; _para4: pointer; xCompare: sqlite3_create_collation_func): longint; cdecl;
  sqlite3_create_collation16: function(_para1: Psqlite3; zName: Pchar; eTextRep: longint; _para4: pointer; xCompare: sqlite3_create_collation_func): longint; cdecl;
  sqlite3_collation_needed: function(_para1: Psqlite3; _para2: pointer; _para3: sqlite3_collation_needed_func): longint; cdecl;
  sqlite3_collation_needed16: function(_para1: Psqlite3; _para2: pointer; _para3: sqlite3_collation_needed_func): longint; cdecl;
  sqlite3_libversion: function: PChar; cdecl;
//Alias for allowing better code portability (win32 is not working with external variables)
  sqlite3_version: function: PChar; cdecl;

// Not published functions
  sqlite3_libversion_number: function: longint; cdecl;
//  sqlite3_key : function (db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;
//  sqlite3_rekey : function (db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;
//  sqlite3_sleep : function (_para1:longint):longint;cdecl;
//  sqlite3_expired : function (_para1:Psqlite3_stmt):longint;cdecl;
//function sqlite3_global_recover:longint;cdecl;

function TryInitializeSqlite(const LibraryName: string): Integer;
function InitializeSqlite(LibraryName: string = ''): Integer;
function IsInitializeSqlite: Boolean;
procedure ReleaseSQLite;

var
  SQLiteLibraryHandle: TLibHandle;
  DefaultLibrary: string = Sqlite3Lib;

implementation

resourcestring
  SErrLoadFailed = 'Can not load SQLite client library "%s". Check your installation.';
  SErrAlreadyLoaded = 'SQLIte interface already initialized from library %s.';

var
  RefCount: integer = 0;
  LoadedLibrary: string;

{$ifdef FPC}
{$else}
function LoadLibrary(LibraryName: string):TLibHandle;
begin
  Result := Windows.LoadLibrary(PChar(LibraryName));
end;
{$endif}
procedure LoadAddresses(LibHandle: TLibHandle);
begin
  @sqlite3_close := GetProcAddress(LibHandle, 'sqlite3_close');
  @sqlite3_exec := GetProcAddress(LibHandle, 'sqlite3_exec');
  @sqlite3_last_insert_rowid := GetProcAddress(LibHandle, 'sqlite3_last_insert_rowid');
  @sqlite3_changes := GetProcAddress(LibHandle, 'sqlite3_changes');
  @sqlite3_total_changes := GetProcAddress(LibHandle, 'sqlite3_total_changes');
  @sqlite3_interrupt := GetProcAddress(LibHandle, 'sqlite3_interrupt');
  @sqlite3_complete := GetProcAddress(LibHandle, 'sqlite3_complete');
  @sqlite3_complete16 := GetProcAddress(LibHandle, 'sqlite3_complete16');
  @sqlite3_busy_handler := GetProcAddress(LibHandle, 'sqlite3_busy_handler');
  @sqlite3_busy_timeout := GetProcAddress(LibHandle, 'sqlite3_busy_timeout');
  @sqlite3_get_table := GetProcAddress(LibHandle, 'sqlite3_get_table');
  @sqlite3_free_table := GetProcAddress(LibHandle, 'sqlite3_free_table');
// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
//   @sqlite3_mprintf := GetProcAddress(LibHandle,'sqlite3_mprintf');
  @sqlite3_mprintf := GetProcAddress(LibHandle, 'sqlite3_mprintf');
//  @(sqlite3_vmprintf := GetProcAddress(LibHandle,'sqlite3_vmprintf');
  @sqlite3_free := GetProcAddress(LibHandle, 'sqlite3_free');
//  @(sqlite3_snprintf := GetProcAddress(LibHandle,'sqlite3_snprintf');
  @sqlite3_snprintf := GetProcAddress(LibHandle, 'sqlite3_snprintf');
  @sqlite3_set_authorizer := GetProcAddress(LibHandle, 'sqlite3_set_authorizer');
  @sqlite3_trace := GetProcAddress(LibHandle, 'sqlite3_trace');
  @sqlite3_progress_handler := GetProcAddress(LibHandle, 'sqlite3_progress_handler');
  @sqlite3_commit_hook := GetProcAddress(LibHandle, 'sqlite3_commit_hook');
  @sqlite3_open := GetProcAddress(LibHandle, 'sqlite3_open');
  @sqlite3_open_v2 := GetProcAddress(LibHandle,'sqlite3_open_v2');
  @sqlite3_open16 := GetProcAddress(LibHandle, 'sqlite3_open16');
  @sqlite3_errcode := GetProcAddress(LibHandle, 'sqlite3_errcode');
  @sqlite3_errmsg := GetProcAddress(LibHandle, 'sqlite3_errmsg');
  @sqlite3_errmsg16 := GetProcAddress(LibHandle, 'sqlite3_errmsg16');
  @sqlite3_prepare := GetProcAddress(LibHandle, 'sqlite3_prepare');
  @sqlite3_prepare16 := GetProcAddress(LibHandle, 'sqlite3_prepare16');
  @sqlite3_bind_blob := GetProcAddress(LibHandle, 'sqlite3_bind_blob');
  @sqlite3_bind_double := GetProcAddress(LibHandle, 'sqlite3_bind_double');
  @sqlite3_bind_int := GetProcAddress(LibHandle, 'sqlite3_bind_int');
  @sqlite3_bind_int64 := GetProcAddress(LibHandle, 'sqlite3_bind_int64');
  @sqlite3_bind_null := GetProcAddress(LibHandle, 'sqlite3_bind_null');
  @sqlite3_bind_text := GetProcAddress(LibHandle, 'sqlite3_bind_text');
  @sqlite3_bind_text16 := GetProcAddress(LibHandle, 'sqlite3_bind_text16');
//  pointer(sqlite3_bind_value) := GetProcAddress(LibHandle,'sqlite3_bind_value');
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
  @sqlite3_bind_blob1 := GetProcAddress(LibHandle, 'sqlite3_bind_blob');
  @sqlite3_bind_text1 := GetProcAddress(LibHandle, 'sqlite3_bind_text');
  @sqlite3_bind_text161 := GetProcAddress(LibHandle, 'sqlite3_bind_text16');

  @sqlite3_bind_parameter_count := GetProcAddress(LibHandle, 'sqlite3_bind_parameter_count');
  @sqlite3_bind_parameter_name := GetProcAddress(LibHandle, 'sqlite3_bind_parameter_name');
  @sqlite3_bind_parameter_index := GetProcAddress(LibHandle, 'sqlite3_bind_parameter_index');
//  @(sqlite3_clear_bindings) := GetProcAddress(LibHandle,'sqlite3_clear_bindings');
  @sqlite3_column_count := GetProcAddress(LibHandle, 'sqlite3_column_count');
  @sqlite3_column_name := GetProcAddress(LibHandle, 'sqlite3_column_name');
  @sqlite3_column_name16 := GetProcAddress(LibHandle, 'sqlite3_column_name16');
  @sqlite3_column_decltype := GetProcAddress(LibHandle, 'sqlite3_column_decltype');
  @sqlite3_column_decltype16 := GetProcAddress(LibHandle, 'sqlite3_column_decltype16');
  @sqlite3_step := GetProcAddress(LibHandle, 'sqlite3_step');
  @sqlite3_data_count := GetProcAddress(LibHandle, 'sqlite3_data_count');
  @sqlite3_column_blob := GetProcAddress(LibHandle, 'sqlite3_column_blob');
  @sqlite3_column_bytes := GetProcAddress(LibHandle, 'sqlite3_column_bytes');
  @sqlite3_column_bytes16 := GetProcAddress(LibHandle, 'sqlite3_column_bytes16');
  @sqlite3_column_double := GetProcAddress(LibHandle, 'sqlite3_column_double');
  @sqlite3_column_int := GetProcAddress(LibHandle, 'sqlite3_column_int');
  @sqlite3_column_int64 := GetProcAddress(LibHandle, 'sqlite3_column_int64');
  @sqlite3_column_text := GetProcAddress(LibHandle, 'sqlite3_column_text');
  @sqlite3_column_text16 := GetProcAddress(LibHandle, 'sqlite3_column_text16');
  @sqlite3_column_type := GetProcAddress(LibHandle, 'sqlite3_column_type');
  @sqlite3_finalize := GetProcAddress(LibHandle, 'sqlite3_finalize');
  @sqlite3_reset := GetProcAddress(LibHandle, 'sqlite3_reset');
  @sqlite3_create_function := GetProcAddress(LibHandle, 'sqlite3_create_function');
  @sqlite3_create_function16 := GetProcAddress(LibHandle, 'sqlite3_create_function16');
  @sqlite3_aggregate_count := GetProcAddress(LibHandle, 'sqlite3_aggregate_count');
  @sqlite3_value_blob := GetProcAddress(LibHandle, 'sqlite3_value_blob');
  @sqlite3_value_bytes := GetProcAddress(LibHandle, 'sqlite3_value_bytes');
  @sqlite3_value_bytes16 := GetProcAddress(LibHandle, 'sqlite3_value_bytes16');
  @sqlite3_value_double := GetProcAddress(LibHandle, 'sqlite3_value_double');
  @sqlite3_value_int := GetProcAddress(LibHandle, 'sqlite3_value_int');
  @sqlite3_value_int64 := GetProcAddress(LibHandle, 'sqlite3_value_int64');
  @sqlite3_value_text := GetProcAddress(LibHandle, 'sqlite3_value_text');
  @sqlite3_value_text16 := GetProcAddress(LibHandle, 'sqlite3_value_text16');
  @sqlite3_value_text16le := GetProcAddress(LibHandle, 'sqlite3_value_text16le');
  @sqlite3_value_text16be := GetProcAddress(LibHandle, 'sqlite3_value_text16be');
  @sqlite3_value_type := GetProcAddress(LibHandle, 'sqlite3_value_type');
  @sqlite3_aggregate_context := GetProcAddress(LibHandle, 'sqlite3_aggregate_context');
  @sqlite3_user_data := GetProcAddress(LibHandle, 'sqlite3_user_data');
  @sqlite3_get_auxdata := GetProcAddress(LibHandle, 'sqlite3_get_auxdata');
  @sqlite3_set_auxdata := GetProcAddress(LibHandle, 'sqlite3_set_auxdata');
  @sqlite3_result_blob := GetProcAddress(LibHandle, 'sqlite3_result_blob');
  @sqlite3_result_double := GetProcAddress(LibHandle, 'sqlite3_result_double');
  @sqlite3_result_error := GetProcAddress(LibHandle, 'sqlite3_result_error');
  @sqlite3_result_error16 := GetProcAddress(LibHandle, 'sqlite3_result_error16');
  @sqlite3_result_int := GetProcAddress(LibHandle, 'sqlite3_result_int');
  @sqlite3_result_int64 := GetProcAddress(LibHandle, 'sqlite3_result_int64');
  @sqlite3_result_null := GetProcAddress(LibHandle, 'sqlite3_result_null');
  @sqlite3_result_text := GetProcAddress(LibHandle, 'sqlite3_result_text');
  @sqlite3_result_text16 := GetProcAddress(LibHandle, 'sqlite3_result_text16');
  @sqlite3_result_text16le := GetProcAddress(LibHandle, 'sqlite3_result_text16le');
  @sqlite3_result_text16be := GetProcAddress(LibHandle, 'sqlite3_result_text16be');
  @sqlite3_result_value := GetProcAddress(LibHandle, 'sqlite3_result_value');
  @sqlite3_create_collation := GetProcAddress(LibHandle, 'sqlite3_create_collation');
  @sqlite3_create_collation16 := GetProcAddress(LibHandle, 'sqlite3_create_collation16');
  @sqlite3_collation_needed := GetProcAddress(LibHandle, 'sqlite3_collation_needed');
  @sqlite3_collation_needed16 := GetProcAddress(LibHandle, 'sqlite3_collation_needed16');
  @sqlite3_libversion := GetProcAddress(LibHandle, 'sqlite3_libversion');
//Alias for allowing better code portability (win32 is not working with external variables)
  @sqlite3_version := GetProcAddress(LibHandle, 'sqlite3_libversion');

// Not published functions
  @sqlite3_libversion_number := GetProcAddress(LibHandle, 'sqlite3_libversion_number');
//  @(sqlite3_key) := GetProcAddress(LibHandle,'sqlite3_key');
//  @(sqlite3_rekey) := GetProcAddress(LibHandle,'sqlite3_rekey');
//  @(sqlite3_sleep) := GetProcAddress(LibHandle,'sqlite3_sleep');
//  @(sqlite3_expired) := GetProcAddress(LibHandle,'sqlite3_expired');
//  function sqlite3_global_recover:longint;cdecl;
end;

function TryInitializeSqlite(const LibraryName: string): Integer;
begin
  Result := InterlockedIncrement(RefCount);
  if Result  = 1 then
  begin
    SQLiteLibraryHandle := LoadLibrary(LibraryName);
    if (SQLiteLibraryHandle = 0) then
    begin
      RefCount := 0;
      Result := -1;
      exit;
    end;
    LoadedLibrary := LibraryName;
    LoadAddresses(SQLiteLibraryHandle);
  end;
end;

function IsInitializeSqlite: Boolean;
begin
  Result := SQLiteLibraryHandle <> 0;
end;

function InitializeSQLite(LibraryName: string) :integer;
begin
  if LibraryName = '' then
    LibraryName := Sqlite3Lib;

  if (LoadedLibrary <> '') and (LoadedLibrary <> LibraryName) then
    raise EInoutError.CreateFmt(SErrAlreadyLoaded,[LoadedLibrary]);

  Result := TryInitializeSQLite(LibraryName);
  if Result = -1 then
    raise EInOutError.CreateFmt(SErrLoadFailed,[LibraryName]);
end;

procedure ReleaseSQLite;
begin
  if InterlockedDecrement(RefCount) <= 0 then
  begin
    if SQLiteLibraryHandle <> 0 then
      FreeLibrary(SQLiteLibraryHandle);
    SQLiteLibraryHandle := 0;
    LoadedLibrary := '';
    RefCount := 0;
  end;
end;

end.

