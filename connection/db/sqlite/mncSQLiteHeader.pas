unit mncSQLiteHeader;
{}
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 *
 * @author FPC Team
 *         Zaher Dirkey <zaher at parmaja dot com>
 *
 *  This file ported from FPC sqlite3.inc, just to be compatiple in both Delphi and FPC
 *}
{
  TODO:
  * Improve the header to catch last sqlite version.
}

{$IFDEF fpc}
{$MODE objfpc}
{$ELSE}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF BSD}
{$LINKLIB c}
{$LINKLIB pthread}
{$ENDIF}

interface

uses
  SysUtils, DynLibs;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
{$IFDEF WINDOWS}
  Sqlite3Lib = 'sqlite3.dll';
{$ELSE}
  Sqlite3Lib = 'libsqlite3.so';
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
  SQLITE_SCHEMA = 17; { The database schema changed  }
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

{$IFNDEF win32}
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

procedure LoadSQLiteLibrary; overload;
procedure LoadSQLiteLibrary(LibraryName: string); overload;
procedure ReleaseSQLiteLibrary;

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

procedure LoadAddresses(LibHandle: TLibHandle);
begin
  pointer(sqlite3_close) := GetprocedureAddress(LibHandle, 'sqlite3_close');
  pointer(sqlite3_exec) := GetprocedureAddress(LibHandle, 'sqlite3_exec');
  pointer(sqlite3_last_insert_rowid) := GetprocedureAddress(LibHandle, 'sqlite3_last_insert_rowid');
  pointer(sqlite3_changes) := GetprocedureAddress(LibHandle, 'sqlite3_changes');
  pointer(sqlite3_total_changes) := GetprocedureAddress(LibHandle, 'sqlite3_total_changes');
  pointer(sqlite3_interrupt) := GetprocedureAddress(LibHandle, 'sqlite3_interrupt');
  pointer(sqlite3_complete) := GetprocedureAddress(LibHandle, 'sqlite3_complete');
  pointer(sqlite3_complete16) := GetprocedureAddress(LibHandle, 'sqlite3_complete16');
  pointer(sqlite3_busy_handler) := GetprocedureAddress(LibHandle, 'sqlite3_busy_handler');
  pointer(sqlite3_busy_timeout) := GetprocedureAddress(LibHandle, 'sqlite3_busy_timeout');
  pointer(sqlite3_get_table) := GetprocedureAddress(LibHandle, 'sqlite3_get_table');
  pointer(sqlite3_free_table) := GetprocedureAddress(LibHandle, 'sqlite3_free_table');
// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
//   pointer(sqlite3_mprintf) := GetprocedureAddress(LibHandle,'sqlite3_mprintf');
  pointer(sqlite3_mprintf) := GetprocedureAddress(LibHandle, 'sqlite3_mprintf');
//  pointer(sqlite3_vmprintf) := GetprocedureAddress(LibHandle,'sqlite3_vmprintf');
  pointer(sqlite3_free) := GetprocedureAddress(LibHandle, 'sqlite3_free');
//  pointer(sqlite3_snprintf) := GetprocedureAddress(LibHandle,'sqlite3_snprintf');
  pointer(sqlite3_snprintf) := GetprocedureAddress(LibHandle, 'sqlite3_snprintf');
  pointer(sqlite3_set_authorizer) := GetprocedureAddress(LibHandle, 'sqlite3_set_authorizer');
  pointer(sqlite3_trace) := GetprocedureAddress(LibHandle, 'sqlite3_trace');
  pointer(sqlite3_progress_handler) := GetprocedureAddress(LibHandle, 'sqlite3_progress_handler');
  pointer(sqlite3_commit_hook) := GetprocedureAddress(LibHandle, 'sqlite3_commit_hook');
  pointer(sqlite3_open) := GetprocedureAddress(LibHandle, 'sqlite3_open');
  pointer(sqlite3_open16) := GetprocedureAddress(LibHandle, 'sqlite3_open16');
  pointer(sqlite3_errcode) := GetprocedureAddress(LibHandle, 'sqlite3_errcode');
  pointer(sqlite3_errmsg) := GetprocedureAddress(LibHandle, 'sqlite3_errmsg');
  pointer(sqlite3_errmsg16) := GetprocedureAddress(LibHandle, 'sqlite3_errmsg16');
  pointer(sqlite3_prepare) := GetprocedureAddress(LibHandle, 'sqlite3_prepare');
  pointer(sqlite3_prepare16) := GetprocedureAddress(LibHandle, 'sqlite3_prepare16');
  pointer(sqlite3_bind_blob) := GetprocedureAddress(LibHandle, 'sqlite3_bind_blob');
  pointer(sqlite3_bind_double) := GetprocedureAddress(LibHandle, 'sqlite3_bind_double');
  pointer(sqlite3_bind_int) := GetprocedureAddress(LibHandle, 'sqlite3_bind_int');
  pointer(sqlite3_bind_int64) := GetprocedureAddress(LibHandle, 'sqlite3_bind_int64');
  pointer(sqlite3_bind_null) := GetprocedureAddress(LibHandle, 'sqlite3_bind_null');
  pointer(sqlite3_bind_text) := GetprocedureAddress(LibHandle, 'sqlite3_bind_text');
  pointer(sqlite3_bind_text16) := GetprocedureAddress(LibHandle, 'sqlite3_bind_text16');
//  pointer(sqlite3_bind_value) := GetprocedureAddress(LibHandle,'sqlite3_bind_value');
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
  pointer(sqlite3_bind_blob1) := GetprocedureAddress(LibHandle, 'sqlite3_bind_blob');
  pointer(sqlite3_bind_text1) := GetprocedureAddress(LibHandle, 'sqlite3_bind_text');
  pointer(sqlite3_bind_text161) := GetprocedureAddress(LibHandle, 'sqlite3_bind_text16');

  pointer(sqlite3_bind_parameter_count) := GetprocedureAddress(LibHandle, 'sqlite3_bind_parameter_count');
  pointer(sqlite3_bind_parameter_name) := GetprocedureAddress(LibHandle, 'sqlite3_bind_parameter_name');
  pointer(sqlite3_bind_parameter_index) := GetprocedureAddress(LibHandle, 'sqlite3_bind_parameter_index');
//  pointer(sqlite3_clear_bindings) := GetprocedureAddress(LibHandle,'sqlite3_clear_bindings');
  pointer(sqlite3_column_count) := GetprocedureAddress(LibHandle, 'sqlite3_column_count');
  pointer(sqlite3_column_name) := GetprocedureAddress(LibHandle, 'sqlite3_column_name');
  pointer(sqlite3_column_name16) := GetprocedureAddress(LibHandle, 'sqlite3_column_name16');
  pointer(sqlite3_column_decltype) := GetprocedureAddress(LibHandle, 'sqlite3_column_decltype');
  pointer(sqlite3_column_decltype16) := GetprocedureAddress(LibHandle, 'sqlite3_column_decltype16');
  pointer(sqlite3_step) := GetprocedureAddress(LibHandle, 'sqlite3_step');
  pointer(sqlite3_data_count) := GetprocedureAddress(LibHandle, 'sqlite3_data_count');
  pointer(sqlite3_column_blob) := GetprocedureAddress(LibHandle, 'sqlite3_column_blob');
  pointer(sqlite3_column_bytes) := GetprocedureAddress(LibHandle, 'sqlite3_column_bytes');
  pointer(sqlite3_column_bytes16) := GetprocedureAddress(LibHandle, 'sqlite3_column_bytes16');
  pointer(sqlite3_column_double) := GetprocedureAddress(LibHandle, 'sqlite3_column_double');
  pointer(sqlite3_column_int) := GetprocedureAddress(LibHandle, 'sqlite3_column_int');
  pointer(sqlite3_column_int64) := GetprocedureAddress(LibHandle, 'sqlite3_column_int64');
  pointer(sqlite3_column_text) := GetprocedureAddress(LibHandle, 'sqlite3_column_text');
  pointer(sqlite3_column_text16) := GetprocedureAddress(LibHandle, 'sqlite3_column_text16');
  pointer(sqlite3_column_type) := GetprocedureAddress(LibHandle, 'sqlite3_column_type');
  pointer(sqlite3_finalize) := GetprocedureAddress(LibHandle, 'sqlite3_finalize');
  pointer(sqlite3_reset) := GetprocedureAddress(LibHandle, 'sqlite3_reset');
  pointer(sqlite3_create_function) := GetprocedureAddress(LibHandle, 'sqlite3_create_function');
  pointer(sqlite3_create_function16) := GetprocedureAddress(LibHandle, 'sqlite3_create_function16');
  pointer(sqlite3_aggregate_count) := GetprocedureAddress(LibHandle, 'sqlite3_aggregate_count');
  pointer(sqlite3_value_blob) := GetprocedureAddress(LibHandle, 'sqlite3_value_blob');
  pointer(sqlite3_value_bytes) := GetprocedureAddress(LibHandle, 'sqlite3_value_bytes');
  pointer(sqlite3_value_bytes16) := GetprocedureAddress(LibHandle, 'sqlite3_value_bytes16');
  pointer(sqlite3_value_double) := GetprocedureAddress(LibHandle, 'sqlite3_value_double');
  pointer(sqlite3_value_int) := GetprocedureAddress(LibHandle, 'sqlite3_value_int');
  pointer(sqlite3_value_int64) := GetprocedureAddress(LibHandle, 'sqlite3_value_int64');
  pointer(sqlite3_value_text) := GetprocedureAddress(LibHandle, 'sqlite3_value_text');
  pointer(sqlite3_value_text16) := GetprocedureAddress(LibHandle, 'sqlite3_value_text16');
  pointer(sqlite3_value_text16le) := GetprocedureAddress(LibHandle, 'sqlite3_value_text16le');
  pointer(sqlite3_value_text16be) := GetprocedureAddress(LibHandle, 'sqlite3_value_text16be');
  pointer(sqlite3_value_type) := GetprocedureAddress(LibHandle, 'sqlite3_value_type');
  pointer(sqlite3_aggregate_context) := GetprocedureAddress(LibHandle, 'sqlite3_aggregate_context');
  pointer(sqlite3_user_data) := GetprocedureAddress(LibHandle, 'sqlite3_user_data');
  pointer(sqlite3_get_auxdata) := GetprocedureAddress(LibHandle, 'sqlite3_get_auxdata');
  pointer(sqlite3_set_auxdata) := GetprocedureAddress(LibHandle, 'sqlite3_set_auxdata');
  pointer(sqlite3_result_blob) := GetprocedureAddress(LibHandle, 'sqlite3_result_blob');
  pointer(sqlite3_result_double) := GetprocedureAddress(LibHandle, 'sqlite3_result_double');
  pointer(sqlite3_result_error) := GetprocedureAddress(LibHandle, 'sqlite3_result_error');
  pointer(sqlite3_result_error16) := GetprocedureAddress(LibHandle, 'sqlite3_result_error16');
  pointer(sqlite3_result_int) := GetprocedureAddress(LibHandle, 'sqlite3_result_int');
  pointer(sqlite3_result_int64) := GetprocedureAddress(LibHandle, 'sqlite3_result_int64');
  pointer(sqlite3_result_null) := GetprocedureAddress(LibHandle, 'sqlite3_result_null');
  pointer(sqlite3_result_text) := GetprocedureAddress(LibHandle, 'sqlite3_result_text');
  pointer(sqlite3_result_text16) := GetprocedureAddress(LibHandle, 'sqlite3_result_text16');
  pointer(sqlite3_result_text16le) := GetprocedureAddress(LibHandle, 'sqlite3_result_text16le');
  pointer(sqlite3_result_text16be) := GetprocedureAddress(LibHandle, 'sqlite3_result_text16be');
  pointer(sqlite3_result_value) := GetprocedureAddress(LibHandle, 'sqlite3_result_value');
  pointer(sqlite3_create_collation) := GetprocedureAddress(LibHandle, 'sqlite3_create_collation');
  pointer(sqlite3_create_collation16) := GetprocedureAddress(LibHandle, 'sqlite3_create_collation16');
  pointer(sqlite3_collation_needed) := GetprocedureAddress(LibHandle, 'sqlite3_collation_needed');
  pointer(sqlite3_collation_needed16) := GetprocedureAddress(LibHandle, 'sqlite3_collation_needed16');
  pointer(sqlite3_libversion) := GetprocedureAddress(LibHandle, 'sqlite3_libversion');
//Alias for allowing better code portability (win32 is not working with external variables)
  pointer(sqlite3_version) := GetprocedureAddress(LibHandle, 'sqlite3_libversion');

// Not published functions
  pointer(sqlite3_libversion_number) := GetprocedureAddress(LibHandle, 'sqlite3_libversion_number');
//  pointer(sqlite3_key) := GetprocedureAddress(LibHandle,'sqlite3_key');
//  pointer(sqlite3_rekey) := GetprocedureAddress(LibHandle,'sqlite3_rekey');
//  pointer(sqlite3_sleep) := GetprocedureAddress(LibHandle,'sqlite3_sleep');
//  pointer(sqlite3_expired) := GetprocedureAddress(LibHandle,'sqlite3_expired');
// function sqlite3_global_recover:longint;cdecl;
end;

function TryInitialiseSqlite(const LibraryName: string): Boolean;
begin
  Result := False;
  if (RefCount = 0) then
  begin
    SQLiteLibraryHandle := LoadLibrary(LibraryName);
    Result := (SQLiteLibraryHandle <> nilhandle);
    if not Result then
      Exit;
    inc(RefCount);
    LoadedLibrary := LibraryName;
    LoadAddresses(SQLiteLibraryHandle);
  end
  else
  begin
    if (LoadedLibrary <> LibraryName) then
      raise EInoutError.CreateFmt(SErrAlreadyLoaded, [LoadedLibrary]);
    inc(RefCount);
    Result := True;
  end;
end;

procedure LoadSQLiteLibrary;
begin
  LoadSQLiteLibrary(DefaultLibrary);
end;

procedure LoadSQLiteLibrary(LibraryName: string);
begin
  if not TryInitialiseSQLIte(LibraryName) then
    raise EInOutError.CreateFmt(SErrLoadFailed, [LibraryName]);
end;

procedure ReleaseSQLiteLibrary;
begin
  if RefCount > 1 then
    Dec(RefCount)
  else if UnloadLibrary(SQLITELibraryHandle) then
  begin
    Dec(RefCount);
    SQLITELibraryHandle := NilHandle;
    LoadedLibrary := '';
  end;
end;

end.

