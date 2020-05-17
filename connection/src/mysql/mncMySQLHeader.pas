unit mncMySQLHeader;
{$IFDEF FPC}
{$MODE delphi}
{$MACRO on}
{$ENDIF}
{$M+}{$H+}

{**
 *  This file is part of the "Mini Connections"
 *
 * @license   MIT
 * @author    ported by Zaher Dirkey zaherdirkey
 *}

{$DEFINE MYSQL57}

interface

uses
   SysUtils, ctypes, mnLibraries;

const
{$IFDEF Unix}
  {$DEFINE extdecl:=cdecl}
    cMySQLLibName = 'libmysqlclient';
  {$IF DEFINED(mysql57)}
    mysqlvlib = cMySQLLibName+'.20';
  {$ELSEIF DEFINED(mysql55) or DEFINED(mysql56)}
    mysqlvlib = cMySQLLibName+'.18';
  {$ELSEIF DEFINED(mysql51)}
    mysqlvlib = cMySQLLibName+'.16';
  {$ELSEIF DEFINED(mysql50)}
    mysqlvlib = cMySQLLibName+'.15';
  {$ELSEIF DEFINED(mysql41)}
    mysqlvlib = cMySQLLibName+'.14';
  {$ELSE}
    mysqlvlib = cMySQLLibName+'.12';
  {$ENDIF}
{$ENDIF}
{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
    cMySQLLibName = 'libmysql';
{$ENDIF}


{$IFDEF mysql57}
  {$DEFINE mysql56}
{$ENDIF mysql57}

{$IFDEF mysql56}
  {$DEFINE mysql55}
{$ENDIF mysql56}

{$IFDEF mysql55}
  {$DEFINE mysql51}
{$ENDIF mysql55}

{$IFDEF mysql51}
  {$DEFINE mysql50}
{$ENDIF mysql51}

{$IFDEF mysql50}
  {$DEFINE mysql41}
{$ENDIF mysql50}

{$PACKRECORDS C}

  { Copyright (C) 2000-2003 MySQL AB

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2 of the License, or
     (at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  }

    type
       my_bool = cchar;
       Pmy_bool  = ^my_bool;
       ppcchar = ^pcchar;
       psize_t = pointer;

       PVIO = Pointer;

       Pgptr = ^gptr;
       gptr = ^cchar;

       Pmy_socket = ^my_socket;
       my_socket = cint;

{  ------------ Start of declaration in "mysql_com.h"   ---------------------  }

  {
  ** Common definition between mysql server & client
   }

  { Field/table name length  }

  const
     HOSTNAME_LENGTH = 60;

     SYSTEM_CHARSET_MBMAXLEN = 3;
     FILENAME_CHARSET_MBMAXLEN = 5;
     NAME_CHAR_LEN = 64;              // Field/table name length
     USERNAME_CHAR_LENGTH = 16;
     NAME_LEN = (NAME_CHAR_LEN*SYSTEM_CHARSET_MBMAXLEN);
     USERNAME_LENGTH = (USERNAME_CHAR_LENGTH*SYSTEM_CHARSET_MBMAXLEN);

     MYSQL_AUTODETECT_CHARSET_NAME = 'auto';
     SERVER_VERSION_LENGTH = 60;
     SQLSTATE_LENGTH = 5;
     LOCAL_HOST = 'localhost';
     LOCAL_HOST_NAMEDPIPE = '.';

  { Maximum length of comments }
  const
     TABLE_COMMENT_INLINE_MAXLEN = 180;
     TABLE_COMMENT_MAXLEN = 2048;
     COLUMN_COMMENT_MAXLEN = 1024;
     INDEX_COMMENT_MAXLEN = 1024;
     TABLE_PARTITION_COMMENT_MAXLEN = 1024;

  { Maximum length of protocol packet. }
  { OK packet length limit also restricted to this value as any length greater
    than this value will have first byte of OK packet to be 254 thus does not
    provide a means to identify if this is OK or EOF packet. }
     MAX_PACKET_LENGTH = (256*256*256-1);

  const
     MYSQL_NAMEDPIPE = 'MySQL';
     MYSQL_SERVICENAME = 'MySQL';

  type
     enum_server_command = (COM_SLEEP,COM_QUIT,COM_INIT_DB,COM_QUERY,
       COM_FIELD_LIST,COM_CREATE_DB,COM_DROP_DB,
       COM_REFRESH,COM_SHUTDOWN,COM_STATISTICS,
       COM_PROCESS_INFO,COM_CONNECT,COM_PROCESS_KILL,
       COM_DEBUG,COM_PING,COM_TIME,COM_DELAYED_INSERT,
       COM_CHANGE_USER,COM_BINLOG_DUMP,COM_TABLE_DUMP,
       COM_CONNECT_OUT,COM_REGISTER_SLAVE,
       COM_STMT_PREPARE, COM_STMT_EXECUTE, COM_STMT_SEND_LONG_DATA, COM_STMT_CLOSE,
       COM_STMT_RESET, COM_SET_OPTION, COM_STMT_FETCH,
       COM_DAEMON,
       COM_BINLOG_DUMP_GTID,
       COM_RESET_CONNECTION,
       { Must be last }
       COM_END
       );

  {
    Length of random string sent by server on handshake; this is also length of
    obfuscated password, recieved from client
   }

  const
     SCRAMBLE_LENGTH = 20;
     SCRAMBLE_LENGTH_323 = 8;

  { length of password stored in the db: new passwords are preceeded with '*'  }

     SCRAMBLED_PASSWORD_CHAR_LENGTH = SCRAMBLE_LENGTH*2+1;
     SCRAMBLED_PASSWORD_CHAR_LENGTH_323 = SCRAMBLE_LENGTH_323*2;


       NOT_NULL_FLAG = 1;       //  Field can't be NULL
       PRI_KEY_FLAG = 2;        //  Field is part of a primary key
       UNIQUE_KEY_FLAG = 4;     //  Field is part of a unique key
       MULTIPLE_KEY_FLAG = 8;   //  Field is part of a key
       BLOB_FLAG = 16;          //  Field is a blob
       UNSIGNED_FLAG = 32;      //  Field is unsigned
       ZEROFILL_FLAG = 64;      //  Field is zerofill
       BINARY_FLAG = 128;       //  Field is binary

    { The following are only sent to new clients  }

       ENUM_FLAG = 256;            // field is an enum
       AUTO_INCREMENT_FLAG = 512;  // field is a autoincrement field
       TIMESTAMP_FLAG = 1024;      // Field is a timestamp
       SET_FLAG = 2048;            // field is a set
       NO_DEFAULT_VALUE_FLAG=4096; // Field doesn't have default value
       ON_UPDATE_NOW_FLAG=8192;    // Field is set to NOW on UPDATE
       NUM_FLAG = 32768;           // Field is num (for clients)
       PART_KEY_FLAG = 16384;      // Intern; Part of some key
       GROUP_FLAG = 32768;         // Intern: Group field
       UNIQUE_FLAG = 65536;        // Intern: Used by sql_yacc
       BINCMP_FLAG = 131072;       // Intern: Used by sql_yacc
       GET_FIXED_FIELDS_FLAG = (1 shl 18);    // Used to get fields in item tree
       FIELD_IN_PART_FUNC_FLAG = (1 shl 19);  // Field part of partition func
       FIELD_IN_ADD_INDEX = (1 shl 20);       // Intern: Field in TABLE object for new version of altered table,
                                              //         which participates in a newly added index.
       FIELD_IS_RENAMED = (1 shl 21);         // Intern: Field is being renamed
       FIELD_FLAGS_STORAGE_MEDIA = 22;        // Field storage media, bit 22-23
       FIELD_FLAGS_STORAGE_MEDIA_MASK = (3 shl FIELD_FLAGS_STORAGE_MEDIA);
       FIELD_FLAGS_COLUMN_FORMAT = 24;        // Field column format, bit 24-25
       FIELD_FLAGS_COLUMN_FORMAT_MASK = (3 shl FIELD_FLAGS_COLUMN_FORMAT);
       FIELD_IS_DROPPED = (1 shl 26);         // Intern: Field is being dropped
       EXPLICIT_NULL_FLAG = (1 shl 27);       // Field is explicitly specified as NULL by the user

       REFRESH_GRANT = 1;          // Refresh grant tables
       REFRESH_LOG = 2;            // Start on new log file
       REFRESH_TABLES = 4;         // close all tables
       REFRESH_HOSTS = 8;          // Flush host cache
       REFRESH_STATUS = 16;        // Flush status variables
       REFRESH_THREADS = 32;       // Flush thread cache
       REFRESH_SLAVE = 64;         // Reset master info and restart slave thread
       REFRESH_MASTER = 128;       // Remove all bin logs in the index and truncate the index
       REFRESH_ERROR_LOG = 256;    // Rotate only the erorr log
       REFRESH_ENGINE_LOG = 512;   // Flush all storage engine logs
       REFRESH_BINARY_LOG = 1024;  // Flush the binary log
       REFRESH_RELAY_LOG = 2048;   // Flush the relay log
       REFRESH_GENERAL_LOG = 4096; // Flush the general log
       REFRESH_SLOW_LOG = 8192;    // Flush the slow query log

    { The following can't be set with mysql_refresh()  }
       REFRESH_READ_LOCK = 16384;          // Lock tables for read
       REFRESH_FAST = 32768;               // Intern flag
       REFRESH_QUERY_CACHE = 65536;        // RESET (remove all queries) from query cache
       REFRESH_QUERY_CACHE_FREE = $20000;  // pack query cache

       REFRESH_DES_KEY_FILE = $40000;
       REFRESH_USER_RESOURCES = $80000;
       REFRESH_FOR_EXPORT = $100000;       // FLUSH TABLES ... FOR EXPORT
       REFRESH_OPTIMIZER_COSTS = $200000;  // FLUSH OPTIMIZER_COSTS

       CLIENT_LONG_PASSWORD = 1;           // new more secure passwords
       CLIENT_FOUND_ROWS = 2;              // Found instead of affected rows
       CLIENT_LONG_FLAG = 4;               // Get all column flags
       CLIENT_CONNECT_WITH_DB = 8;         // One can specify db on connect
       CLIENT_NO_SCHEMA = 16;              // Don't allow database.table.column
       CLIENT_COMPRESS = 32;               // Can use compression protocol
       CLIENT_ODBC = 64;                   // Odbc client
       CLIENT_LOCAL_FILES = 128;           // Can use LOAD DATA LOCAL
       CLIENT_IGNORE_SPACE = 256;          // Ignore spaces before '('
       CLIENT_PROTOCOL_41 = 512;           // New 4.1 protocol
       CLIENT_INTERACTIVE = 1024;          // This is an interactive client
       CLIENT_SSL = 2048;                  // Switch to SSL after handshake
       CLIENT_IGNORE_SIGPIPE = 4096;       // IGNORE sigpipes
       CLIENT_TRANSACTIONS = 8192;         // Client knows about transactions
       CLIENT_RESERVED = 16384;            // Old flag for 4.1 protocol
       CLIENT_SECURE_CONNECTION = 32768;   // Old flag for 4.1 authentication
       CLIENT_MULTI_STATEMENTS = 65536;    // Enable/disable multi-stmt support
       CLIENT_MULTI_RESULTS = 131072;      // Enable/disable multi-results
       CLIENT_PS_MULTI_RESULTS : cardinal = 1 shl 18; // Multi-results in PS-protocol
       CLIENT_PLUGIN_AUTH : cardinal = 1 shl 19;      // Client supports plugin authentication
       CLIENT_CONNECT_ATTRS : cardinal = (1 shl 20);  // Client supports connection attributes
       CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA : cardinal = (1 shl 21);  // Enable authentication response packet to be larger than 255 bytes.
       CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS : cardinal = (1 shl 22);    // Don't close the connection for a connection with expired password.
       CLIENT_SESSION_TRACK : cardinal = (1 shl 23);  // Capable of handling server state change information. Its a hint to the server to include the state change information in Ok packet.
       CLIENT_DEPRECATE_EOF : cardinal = (1 shl 24);  // Client no longer needs EOF packet
       CLIENT_SSL_VERIFY_SERVER_CERT : cardinal = 1 shl 30;
       CLIENT_REMEMBER_OPTIONS : cardinal = 1 shl 31;


       SERVER_STATUS_IN_TRANS = 1;         // Is raised when a multi-statement transaction
                                           //  has been started, either explicitly, by means
                                           //  of BEGIN or COMMIT AND CHAIN, or implicitly, by the first transactional
                                           //  statement, when autocommit=off
       SERVER_STATUS_AUTOCOMMIT = 2;       // Server in auto_commit mode
       SERVER_STATUS_MORE_RESULTS = 4;     // More results on server
       SERVER_MORE_RESULTS_EXISTS = 8;     // Multi query - next query exists
       SERVER_QUERY_NO_GOOD_INDEX_USED = 16;
       SERVER_QUERY_NO_INDEX_USED = 32;
    { The server was able to fulfill the clients request and opened a
      read-only non-scrollable cursor for a query. This flag comes
      in reply to COM_STMT_EXECUTE and COM_STMT_FETCH commands. }
       SERVER_STATUS_CURSOR_EXISTS = 64;
    { This flag is sent when a read-only cursor is exhausted, in reply to
      COM_STMT_FETCH command. }
       SERVER_STATUS_LAST_ROW_SENT = 128;
       SERVER_STATUS_DB_DROPPED = 256;     // A database was dropped
       SERVER_STATUS_NO_BACKSLASH_ESCAPES = 512;
    {
      Sent to the client if after a prepared statement reprepare
      we discovered that the new statement returns a different
      number of result set columns.
    }
       SERVER_STATUS_METADATA_CHANGED = 1024;
       SERVER_QUERY_WAS_SLOW = 2048;
       SERVER_PS_OUT_PARAMS = 4096; // To mark ResultSet containing output parameter values.
       SERVER_STATUS_IN_TRANS_READONLY = 8192;
       SERVER_SESSION_STATE_CHANGED = (1 shl 14); // This status flag, when on, implies that one of the state information has changed on the server because of the execution of the last statement.

       MYSQL_ERRMSG_SIZE = 512;
       NET_READ_TIMEOUT = 30;              // Timeout on read
       NET_WRITE_TIMEOUT = 60;             // Timeout on write
       NET_WAIT_TIMEOUT	= 8*60*60;         // Wait for new query
       ONLY_KILL_QUERY = 1;

    const
       MAX_TINYINT_WIDTH = 3;           // Max width for a TINY w.o. sign
       MAX_SMALLINT_WIDTH = 5;          // Max width for a SHORT w.o. sign
       MAX_MEDIUMINT_WIDTH = 8;         // Max width for a INT24 w.o. sign
       MAX_INT_WIDTH = 10;              // Max width for a LONG w.o. sign
       MAX_BIGINT_WIDTH = 20;           // Max width for a LONGLONG
       MAX_CHAR_WIDTH = 255;            // Max length for a CHAR colum
       MAX_BLOB_WIDTH = 16777216;       // Default width for blob

    type
       Pst_net = ^st_net;
       st_net = record
            vio : PVio;
            buff : pcuchar;
            buff_end : pcuchar;
            write_pos : pcuchar;
            read_pos : pcuchar;
            fd : my_socket;     // For Perl DBI/dbd
            { The following variable is set if we are doing several queries in one
              command ( as in LOAD TABLE ... FROM MASTER ),
              and do not want to confuse the client with OK at the wrong time }
            remain_in_buf,length, buf_length, where_b: culong;
            max_packet,max_packet_size: culong;
            pkt_nr,compress_pkt_nr: cuint;
            write_timeout, read_timeout, retry_count: cuint;
            fcntl: cint;
            return_status: pcuint;
            reading_or_writing: cuchar;
            save_char: cchar;
            unused1: my_bool;  // Please remove with the next incompatible ABI change
            unused2: my_bool;  // Please remove with the next incompatible ABI change
            compress: my_bool;
            unused3: my_bool;  // Please remove with the next incompatible ABI change
            { Pointer to query object in query cache, do not equal NULL (0) for
              queries in cache that have not stored its results yet }
            unused: pcuchar;
            last_errno: cuint;
            error: cuchar;
            unused4: my_bool;  // Please remove with the next incompatible ABI change
            unused5: my_bool;  // Please remove with the next incompatible ABI change
            { Client library error message buffer. Actually belongs to struct MYSQL. }
            last_error: array[0..MYSQL_ERRMSG_SIZE-1] of cchar;
            { Client library sqlstate buffer. Set along with the error message. }
            sqlstate: array[0..SQLSTATE_LENGTH] of cchar;
            { Extension pointer, for the caller private use.
              Any program linking with the networking library can use this pointer,
              which is handy when private connection specific data needs to be
              maintained.
              The mysqld server process uses this pointer internally,
              to maintain the server internal instrumentation for the connection. }
            extension: Pointer;
         end;
       NET = st_net;
       PNET = ^NET;

    const
       packet_error : culong = culong(not(0));

    type
       enum_field_types = (MYSQL_TYPE_DECIMAL,MYSQL_TYPE_TINY,
         MYSQL_TYPE_SHORT,MYSQL_TYPE_LONG,MYSQL_TYPE_FLOAT,
         MYSQL_TYPE_DOUBLE,MYSQL_TYPE_NULL,
         MYSQL_TYPE_TIMESTAMP,MYSQL_TYPE_LONGLONG,
         MYSQL_TYPE_INT24,MYSQL_TYPE_DATE,MYSQL_TYPE_TIME,
         MYSQL_TYPE_DATETIME,MYSQL_TYPE_YEAR,
         MYSQL_TYPE_NEWDATE,
         MYSQL_TYPE_VARCHAR, MYSQL_TYPE_BIT,
         MYSQL_TYPE_TIMESTAMP2, MYSQL_TYPE_DATETIME2, MYSQL_TYPE_TIME2,
         MYSQL_TYPE_NEWDECIMAL = 246,
         MYSQL_TYPE_ENUM = 247,
         MYSQL_TYPE_SET = 248,MYSQL_TYPE_TINY_BLOB = 249,
         MYSQL_TYPE_MEDIUM_BLOB = 250,MYSQL_TYPE_LONG_BLOB = 251,
         MYSQL_TYPE_BLOB = 252,MYSQL_TYPE_VAR_STRING = 253,
         MYSQL_TYPE_STRING = 254,MYSQL_TYPE_GEOMETRY = 255
         );

    { For backward compatibility  }

    const
       CLIENT_MULTI_QUERIES = CLIENT_MULTI_STATEMENTS;
       FIELD_TYPE_DECIMAL = MYSQL_TYPE_DECIMAL;
       FIELD_TYPE_NEWDECIMAL = MYSQL_TYPE_NEWDECIMAL;
       FIELD_TYPE_TINY = MYSQL_TYPE_TINY;
       FIELD_TYPE_SHORT = MYSQL_TYPE_SHORT;
       FIELD_TYPE_LONG = MYSQL_TYPE_LONG;
       FIELD_TYPE_FLOAT = MYSQL_TYPE_FLOAT;
       FIELD_TYPE_DOUBLE = MYSQL_TYPE_DOUBLE;
       FIELD_TYPE_NULL = MYSQL_TYPE_NULL;
       FIELD_TYPE_TIMESTAMP = MYSQL_TYPE_TIMESTAMP;
       FIELD_TYPE_LONGLONG = MYSQL_TYPE_LONGLONG;
       FIELD_TYPE_INT24 = MYSQL_TYPE_INT24;
       FIELD_TYPE_DATE = MYSQL_TYPE_DATE;
       FIELD_TYPE_TIME = MYSQL_TYPE_TIME;
       FIELD_TYPE_DATETIME = MYSQL_TYPE_DATETIME;
       FIELD_TYPE_YEAR = MYSQL_TYPE_YEAR;
       FIELD_TYPE_NEWDATE = MYSQL_TYPE_NEWDATE;
       FIELD_TYPE_ENUM = MYSQL_TYPE_ENUM;
       FIELD_TYPE_SET = MYSQL_TYPE_SET;
       FIELD_TYPE_TINY_BLOB = MYSQL_TYPE_TINY_BLOB;
       FIELD_TYPE_MEDIUM_BLOB = MYSQL_TYPE_MEDIUM_BLOB;
       FIELD_TYPE_LONG_BLOB = MYSQL_TYPE_LONG_BLOB;
       FIELD_TYPE_BLOB = MYSQL_TYPE_BLOB;
       FIELD_TYPE_VAR_STRING = MYSQL_TYPE_VAR_STRING;
       FIELD_TYPE_STRING = MYSQL_TYPE_STRING;
       FIELD_TYPE_CHAR = MYSQL_TYPE_TINY;
       FIELD_TYPE_INTERVAL = MYSQL_TYPE_ENUM;
       FIELD_TYPE_GEOMETRY = MYSQL_TYPE_GEOMETRY;
       FIELD_TYPE_BIT = MYSQL_TYPE_BIT;
    { Shutdown/kill enums and constants  }
    { Bits for THD::killable.  }
       MYSQL_SHUTDOWN_KILLABLE_CONNECT    : cuchar = 1 shl 0;
       MYSQL_SHUTDOWN_KILLABLE_TRANS      : cuchar = 1 shl 1;
       MYSQL_SHUTDOWN_KILLABLE_LOCK_TABLE : cuchar = 1 shl 2;
       MYSQL_SHUTDOWN_KILLABLE_UPDATE     : cuchar = 1 shl 3;


    {   We want levels to be in growing order of hardness (because we use number
        comparisons). Note that DEFAULT does not respect the growing property, but
        it's ok.  }
    type
       mysql_enum_shutdown_level = (SHUTDOWN_DEFAULT = 0,
         SHUTDOWN_WAIT_CONNECTIONS = 1, //MYSQL_SHUTDOWN_KILLABLE_CONNECT,     // wait for existing connections to finish
         SHUTDOWN_WAIT_TRANSACTIONS = 2, //MYSQL_SHUTDOWN_KILLABLE_TRANS,      // wait for existing trans to finish
         SHUTDOWN_WAIT_UPDATES = 8, //MYSQL_SHUTDOWN_KILLABLE_UPDATE,          // wait for existing updates to finish (=> no partial MyISAM update)
         SHUTDOWN_WAIT_ALL_BUFFERS = 16, //MYSQL_SHUTDOWN_KILLABLE_UPDATE shl 1,// flush InnoDB buffers and other storage engines' buffers
         SHUTDOWN_WAIT_CRITICAL_BUFFERS = 17, //(MYSQL_SHUTDOWN_KILLABLE_UPDATE shl 1)+1, // don't flush InnoDB buffers, flush other storage engines' buffers
    { Now the 2 levels of the KILL command  }
{ $if MYSQL_VERSION_ID >= 50000}
         KILL_QUERY = 254,
{ $endif}
         KILL_CONNECTION = 255
         );

       enum_cursor_type = (CURSOR_TYPE_NO_CURSOR = 0,CURSOR_TYPE_READ_ONLY = 1, CURSOR_TYPE_FOR_UPDATE = 2,CURSOR_TYPE_SCROLLABLE = 4);

    { options for mysql_set_option  }
       enum_mysql_set_option = (MYSQL_OPTION_MULTI_STATEMENTS_ON, MYSQL_OPTION_MULTI_STATEMENTS_OFF);

    { Type of state change information that the server can include in the Ok
      packet.
      Note : 1) session_state_type shouldn't go past 255 (i.e. 1-byte boundary).
             2) Modify the definition of SESSION_TRACK_END when a new member is added.
    }
       enum_session_state_type = (
         SESSION_TRACK_SYSTEM_VARIABLES, // Session system variables
         SESSION_TRACK_SCHEMA,           // Current schema
         SESSION_TRACK_STATE_CHANGE,     // track session state changes
         SESSION_TRACK_GTIDS
       );

    const
       SESSION_TRACK_BEGIN = ord(SESSION_TRACK_SYSTEM_VARIABLES);
       SESSION_TRACK_END = ord(SESSION_TRACK_GTIDS);

    function net_new_transaction(net : st_net) : st_net;

    { The following function is not meant for normal usage
      Currently it's used internally by manager.c  }

    type
       Psockaddr = ^sockaddr;
       sockaddr = record
           // undefined structure
         end;

    type
       Prand_struct = ^rand_struct;
       rand_struct = record
            seed1 : culong;
            seed2 : culong;
            max_value : culong;
            max_value_dbl : cdouble;
         end;

    { The following is for user defined functions  }
       Item_result = (STRING_RESULT,REAL_RESULT,INT_RESULT, ROW_RESULT);
       PItem_result = ^Item_result;

       Pst_udf_args = ^st_udf_args;
       st_udf_args = record
            arg_count : cuint;           // Number of arguments
            arg_type : PItem_result;     // Pointer to item_results
            args : PPChar;               // Pointer to item_results
            lengths : pculong;            // Length of string arguments
            maybe_null : Pchar;          // Length of string arguments
            attributes : PPChar;         // Pointer to attribute name
            attribute_lengths : pculong;  // Length of attribute arguments
            extension: pointer;
         end;
       UDF_ARGS = st_udf_args;
       PUDF_ARGS = ^UDF_ARGS;

    { This holds information about the result  }

       Pst_udf_init = ^st_udf_init;
       st_udf_init = record
            maybe_null : my_bool;        // 1 if function can return NULL
            decimals : cuint;            // for real functions
            max_length : culong;          // For string functions
            ptr : Pchar;                 // free pointer for function data
            const_item : my_bool;        // free pointer for function data
            extension: pointer;
         end;
       UDF_INIT = st_udf_init;
       PUDF_INIT = ^UDF_INIT;

    { Constants when using compression  }
    const
       NET_HEADER_SIZE = 4;              // standard header size
       COMP_HEADER_SIZE = 3;             // compression header extra size

    { Prototypes to password functions  }

    { These functions are used for authentication by client and server and
      implemented in sql/password.c     }
    var
      my_init : function :my_bool;cdecl;
      my_thread_init : function :my_bool;cdecl;
      my_thread_end : procedure ;cdecl;

{$ifdef _global_h}
{    function net_field_length(packet:PPuchar):culong;extdecl;external cMySQLLibName name 'net_field_length_ll';
    function net_field_length_ll(packet:PPuchar):my_ulonglong;cdecl;external cMySQLLibName name 'net_field_length_ll';
    function net_store_length(pkg:Pchar; length:ulonglong):Pchar;cdecl;external cMySQLLibName name 'net_store_length';}
{$endif}

    const
       NULL_LENGTH : culong = culong(not(0)); // For net_store_length

    const
       MYSQL_STMT_HEADER      = 4;
       MYSQL_LONG_DATA_HEADER = 6;
       NOT_FIXED_DEC          = 31;

{  ------------ Stop of declaration in "mysql_com.h"   -----------------------  }

{ $include "mysql_time.h"}
    type
        mysql_timestamp_type = (
          MYSQL_TIMESTAMP_NONE = -2,
          MYSQL_TIMESTAMP_ERROR = -1,
          MYSQL_TIMESTAMP_DATE = 0,
          MYSQL_TIMESTAMP_DATETIME = 1,
          MYSQL_TIMESTAMP_TIME = 2
        );

        Pst_mysql_time = ^st_mysql_time;
        st_mysql_time = record
          year:        cuint;
          month:       cuint;
          day:         cuint;
          hour:        cuint;
          minute:      cuint;
          second:      cuint;
          second_part: culong;
          neg:         my_bool;
          time_type:   mysql_timestamp_type;
        end;

        PMYSQL_TIME = ^MYSQL_TIME;
        MYSQL_TIME = st_mysql_time;

{ $include "mysql_version.h"}
{ $include "typelib.h"}
{ $include "my_list.h" /* for LISTs used in 'MYSQL' and 'MYSQL_STMT' */}

    const
       CLIENT_NET_READ_TIMEOUT = 365*24*3600;     // Timeout on read
       CLIENT_NET_WRITE_TIMEOUT = 365*24*3600;    // Timeout on write

    type
       Pst_mysql_field = ^st_mysql_field;
       st_mysql_field = record
            name : Pchar;             // Name of column
            org_name : Pchar;         // Original column name, if an alias
            table : Pchar;            // Table of column if column was a field
            org_table : Pchar;        // Org table name, if table was an alias
            db : Pchar;               // Database for table
            catalog : Pchar;          // Catalog for table
            def : Pchar;              // Default value (set by mysql_list_fields)
            length : culong;          // Width of column (create length)
            max_length : culong;      // Max width for selected set
            name_length : cuint;
            org_name_length : cuint;
            table_length : cuint;
            org_table_length : cuint;
            db_length : cuint;
            catalog_length : cuint;
            def_length : cuint;
            flags : cuint;            // Div flags
            decimals : cuint;         // Number of decimals in field
            charsetnr : cuint;        // Character set
            ftype : enum_field_types; // Type of field. See mysql_com.h for types
            extension: pointer;
         end;
       MYSQL_FIELD = st_mysql_field;
       PMYSQL_FIELD = ^MYSQL_FIELD;

       PMYSQL_ROW = ^MYSQL_ROW;       // return data as array of strings
       MYSQL_ROW = ppchar;

       PMYSQL_FIELD_OFFSET = ^MYSQL_FIELD_OFFSET;     // offset to current field
       MYSQL_FIELD_OFFSET = cuint;

    function IS_PRI_KEY(n : longint) : boolean;
    function IS_NOT_NULL(n : longint) : boolean;
    function IS_BLOB(n : longint) : boolean;
    function IS_NUM(t : enum_field_types) : boolean;
    function INTERNAL_NUM_FIELD(f : Pst_mysql_field) : boolean;
    function IS_NUM_FIELD(f : Pst_mysql_field) : boolean;

    type
       my_ulonglong = cuint64;
       Pmy_ulonglong = ^my_ulonglong;

    const
       MYSQL_COUNT_ERROR = not (my_ulonglong(0));

    type
       Pst_mysql_rows = ^st_mysql_rows;
       st_mysql_rows = record
            next : Pst_mysql_rows;                    // list of rows
            data : MYSQL_ROW;
            length : culong;
         end;
       MYSQL_ROWS = st_mysql_rows;
       PMYSQL_ROWS = ^MYSQL_ROWS;

       PMYSQL_ROW_OFFSET = ^MYSQL_ROW_OFFSET;         // offset to current row
       MYSQL_ROW_OFFSET = MYSQL_ROWS;

{  ------------ Start of declaration in "my_alloc.h"     --------------------  }
{ $include "my_alloc.h"}

  const
     ALLOC_MAX_BLOCK_TO_DROP = 4096;
     ALLOC_MAX_BLOCK_USAGE_BEFORE_DROP = 10;

 { struct for once_alloc (block)  }
  type
     Pst_used_mem = ^st_used_mem;
     st_used_mem = record
          next : Pst_used_mem;   // Next block in use
          left : cuint;          // memory left in block
          size : cuint;          // size of block
       end;
     USED_MEM = st_used_mem;
     PUSED_MEM = ^USED_MEM;


     Pst_mem_root = ^st_mem_root;
     st_mem_root = record
          free : PUSED_MEM;      // blocks with free memory in it
          used : PUSED_MEM;      // blocks almost without free memory
          pre_alloc : PUSED_MEM; // preallocated block
          min_malloc : cuint;    // if block have less memory it will be put in 'used' list
          block_size : cuint;    // initial block size
          block_num : cuint;     // allocated blocks counter
  {    first free block in queue test counter (if it exceed
       MAX_BLOCK_USAGE_BEFORE_DROP block will be dropped in 'used' list)     }
          first_block_usage : cuint;
          error_handler : procedure ;cdecl;
       end;
     MEM_ROOT = st_mem_root;
     PMEM_ROOT = ^MEM_ROOT;

{  ------------ Stop of declaration in "my_alloc.h"    ----------------------  }

    type
       embedded_query_result = record end;

       Pst_mysql_data = ^st_mysql_data;
       st_mysql_data = record
            data: PMYSQL_ROWS;
            embedded_info: ^embedded_query_result;
            alloc: MEM_ROOT;
            rows: my_ulonglong;
            fields: cuint;
            // extra info for embedded library
            extension: pointer;
         end;
       MYSQL_DATA = st_mysql_data;
       PMYSQL_DATA = ^MYSQL_DATA;

       mysql_option = (MYSQL_OPT_CONNECT_TIMEOUT,MYSQL_OPT_COMPRESS,
         MYSQL_OPT_NAMED_PIPE,MYSQL_INIT_COMMAND,
         MYSQL_READ_DEFAULT_FILE,MYSQL_READ_DEFAULT_GROUP,
         MYSQL_SET_CHARSET_DIR,MYSQL_SET_CHARSET_NAME,
         MYSQL_OPT_LOCAL_INFILE,MYSQL_OPT_PROTOCOL,
         MYSQL_SHARED_MEMORY_BASE_NAME,MYSQL_OPT_READ_TIMEOUT,
         MYSQL_OPT_WRITE_TIMEOUT,MYSQL_OPT_USE_RESULT,
         MYSQL_OPT_USE_REMOTE_CONNECTION,MYSQL_OPT_USE_EMBEDDED_CONNECTION,
         MYSQL_OPT_GUESS_CONNECTION,MYSQL_SET_CLIENT_IP,
         MYSQL_SECURE_AUTH
         ,MYSQL_REPORT_DATA_TRUNCATION, MYSQL_OPT_RECONNECT
         ,MYSQL_OPT_SSL_VERIFY_SERVER_CERT
         ,MYSQL_PLUGIN_DIR, MYSQL_DEFAULT_AUTH
         ,MYSQL_OPT_BIND
         ,MYSQL_OPT_SSL_KEY, MYSQL_OPT_SSL_CERT, MYSQL_OPT_SSL_CA, MYSQL_OPT_SSL_CAPATH, MYSQL_OPT_SSL_CIPHER, MYSQL_OPT_SSL_CRL, MYSQL_OPT_SSL_CRLPATH
         ,MYSQL_OPT_CONNECT_ATTR_RESET, MYSQL_OPT_CONNECT_ATTR_ADD, MYSQL_OPT_CONNECT_ATTR_DELETE
         ,MYSQL_SERVER_PUBLIC_KEY
         ,MYSQL_ENABLE_CLEARTEXT_PLUGIN
         ,MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS
         ,MYSQL_OPT_SSL_ENFORCE
       );

    const
       MAX_MYSQL_MANAGER_ERR = 256;
       MAX_MYSQL_MANAGER_MSG = 256;
       MANAGER_OK = 200;
       MANAGER_INFO = 250;
       MANAGER_ACCESS = 401;
       MANAGER_CLIENT_ERR = 450;
       MANAGER_INTERNAL_ERR = 500;

    type
       st_dynamic_array = record
            buffer : ^char;
            elements : cuint;
            max_element : cuint;
            alloc_increment : cuint;
            size_of_element : cuint;
         end;
       DYNAMIC_ARRAY = st_dynamic_array;
       Pst_dynamic_array = ^st_dynamic_array;

       st_mysql_options_extention = record end;

       Pst_mysql_options = ^st_mysql_options;
       st_mysql_options = record
            connect_timeout : cuint;
            read_timeout : cuint;
            write_timeout : cuint;
            port : cuint;
            protocol : cuint;
            client_flag : culong;
            host : Pchar;
            user : Pchar;
            password : Pchar;
            unix_socket : Pchar;
            db : Pchar;
            init_commands : Pst_dynamic_array;
            my_cnf_file : Pchar;
            my_cnf_group : Pchar;
            charset_dir : Pchar;
            charset_name : Pchar;
            ssl_key : Pchar;                 // PEM key file
            ssl_cert : Pchar;                // PEM cert file
            ssl_ca : Pchar;                  // PEM CA file
            ssl_capath : Pchar;              // PEM directory of CA-s?
            ssl_cipher : Pchar;              // cipher to use
            shared_memory_base_name : Pchar;
            max_allowed_packet : culong;
            use_ssl : my_bool;               // if to use SSL or not
            compress : my_bool;
            named_pipe : my_bool;
    {  On connect, find out the replication role of the server, and
       establish connections to all the peers  }
            rpl_probe : my_bool;
    {  Each call to mysql_real_query() will parse it to tell if it is a read
       or a write, and direct it to the slave or the master      }
            rpl_parse : my_bool;
    {  If set, never read from a master, only from slave, when doing
       a read that is replication-aware    }
            no_master_reads : my_bool;
{ $if !defined(CHECK_EMBEDDED_DIFFERENCES) || defined(EMBEDDED_LIBRARY)}
            separate_thread : my_bool;
{ $endif}
            methods_to_use : mysql_option;
            ci: record case integer of  // C union
                {The ip/hostname to use when authenticating
                 client against embedded server built with
                 grant tables - only used in embedded server}
              0: (client_ip: PChar;);
                {The local address to bind when connecting to
                 remote server - not used in embedded server}
              1: (bind_address: PChar;);
            end;
            secure_auth : my_bool;           // Refuse client connecting to server if it uses old (pre-4.1.1) protocol
            report_data_truncation : my_bool;// 0 - never report, 1 - always report (default)
    { function pointers for local infile support  }
            local_infile_init : function (_para1:Ppointer; _para2:Pchar; _para3:pointer):cint;cdecl;
            local_infile_read : function (_para1:pointer; _para2:Pchar; _para3:cuint):cint;
            local_infile_end : procedure (_para1:pointer);
            local_infile_error : function (_para1:pointer; _para2:Pchar; _para3:cuint):cint;
            local_infile_userdata : pointer;
            extension : ^st_mysql_options_extention;
         end;

       mysql_status = (MYSQL_STATUS_READY,MYSQL_STATUS_GET_RESULT, MYSQL_STATUS_USE_RESULT
                       ,MYSQL_STATUS_STATEMENT_GET_RESULT
       );

       mysql_protocol_type = (MYSQL_PROTOCOL_DEFAULT,MYSQL_PROTOCOL_TCP,
         MYSQL_PROTOCOL_SOCKET,MYSQL_PROTOCOL_PIPE,
         MYSQL_PROTOCOL_MEMORY);

    { There are three types of queries - the ones that have to go to
      the master, the ones that go to a slave, and the adminstrative
      type which must happen on the pivot connectioin     }
       mysql_rpl_type = (MYSQL_RPL_MASTER,MYSQL_RPL_SLAVE,MYSQL_RPL_ADMIN
         );

       charset_info_st = record
            number : cuint;
            primary_number : cuint;
            binary_number : cuint;
            state : cuint;
            csname : ^char;
            name : ^char;
            comment : ^char;
            tailoring : ^char;
            ftype : ^cuchar;
            to_lower : ^cuchar;
            to_upper : ^cuchar;
            sort_order : ^cuchar;
            contractions : ^cuint16;
            sort_order_big : ^pword;
            tab_to_uni : ^cuint16;
            tab_from_uni : pointer; // was ^MY_UNI_IDX
            state_map : ^cuchar;
            ident_map : ^cuchar;
            strxfrm_multiply : cuint;
            mbminlen : cuint;
            mbmaxlen : cuint;
            min_sort_char : cuint16;
            max_sort_char : cuint16;
            escape_with_backslash_is_dangerous : my_bool;
            cset : pointer; // was ^MY_CHARSET_HANDLER
            coll : pointer; // was ^MY_COLLATION_HANDLER;
         end;
       CHARSET_INFO = charset_info_st;
       Pcharset_info_st = ^charset_info_st;

       Pcharacter_set = ^character_set;
       character_set = record
            number : cuint;
            state : cuint;
            csname : Pchar;
            name : Pchar;
            comment : Pchar;
            dir : Pchar;
            mbminlen : cuint;
            mbmaxlen : cuint;
         end;
       MY_CHARSET_INFO = character_set;
       PMY_CHARSET_INFO = ^MY_CHARSET_INFO;

       Pst_mysql_methods = ^st_mysql_methods;

       Pst_mysql = ^st_mysql;
       st_mysql = record
            net : NET;                   // Communication parameters
            connector_fd : gptr;         // ConnectorFd for SSL
            host : Pchar;
            user : Pchar;
            passwd : Pchar;
            unix_socket : Pchar;
            server_version : Pchar;
            host_info : Pchar;
            info : Pchar;
            db : Pchar;
            charset : Pcharset_info_st;
            fields : PMYSQL_FIELD;
            field_alloc : MEM_ROOT;
            affected_rows : my_ulonglong;
            insert_id : my_ulonglong;    // id if insert on table with NEXTNR
            extra_info : my_ulonglong;   // Used by mysqlshow, not used by mysql 5.0 and up
            thread_id : culong;          // Id for connection in server
            packet_length : culong;
            port : cuint;
            client_flag : culong;
            server_capabilities : culong;
            protocol_version : cuint;
            field_count : cuint;
            server_status : cuint;
            server_language : cuint;
            warning_count : cuint;
            options : st_mysql_options;
            status : mysql_status;
            free_me : my_bool;           // If free in mysql_close
            reconnect : my_bool;         // set to 1 if automatic reconnect
            scramble : array[0..(SCRAMBLE_LENGTH+1)-1] of char;  // session-wide random string
    {  Set if this is the original connection, not a master or a slave we have
       added though mysql_rpl_probe() or mysql_set_master()/ mysql_add_slave()      }
            rpl_pivot : my_bool;
    {   Pointers to the master, and the next slave connections, points to
        itself if lone connection.       }
            master : Pst_mysql;
            next_slave : Pst_mysql;
            last_used_slave : Pst_mysql; // needed for round-robin slave pick
            last_used_con : Pst_mysql;   // needed for send/read/store/use result to work correctly with replication
            stmts : Pointer;             // was PList, list of all statements
            methods : Pst_mysql_methods;
            thd : pointer;
    {   Points to boolean flag in MYSQL_RES  or MYSQL_STMT. We set this flag
        from mysql_stmt_close if close had to cancel result set of this object.       }
            unbuffered_fetch_owner : Pmy_bool;
            info_buffer: ^cchar;
            extension: pointer;
         end;
       MYSQL = st_mysql;
       PMYSQL = ^MYSQL;


       Pst_mysql_res = ^st_mysql_res;
       st_mysql_res = record
            row_count : my_ulonglong;
            fields : PMYSQL_FIELD;
            data : PMYSQL_DATA;
            data_cursor : PMYSQL_ROWS;
            lengths : pculong;           // column lengths of current row
            handle : PMYSQL;             // for unbuffered reads
            methods : Pst_mysql_methods;
            row : MYSQL_ROW;             // If unbuffered read
            current_row : MYSQL_ROW;     // buffer to current row
            field_alloc : MEM_ROOT;
            field_count, current_field : cuint;
            eof : my_bool;               // Used by mysql_fetch_row
            unbuffered_fetch_cancelled : my_bool;  // mysql_stmt_close() had to cancel this result
            extension : pointer;
         end;
       MYSQL_RES = st_mysql_res;
       PMYSQL_RES = ^MYSQL_RES;

       Pst_mysql_stmt = ^st_mysql_stmt;
       PMYSQL_STMT = ^MYSQL_STMT;

       st_mysql_methods = record
            read_query_result : function (mysql:PMYSQL):my_bool;cdecl;
            advanced_command : function (mysql:PMYSQL; command:enum_server_command; header:Pcuchar; header_length:culong; arg:Pcuchar;
                         arg_length:culong; skip_check:my_bool):my_bool;
            read_rows : function (mysql:PMYSQL; mysql_fields:PMYSQL_FIELD; fields:cuint):PMYSQL_DATA;
            use_result : function (mysql:PMYSQL):PMYSQL_RES;
            fetch_lengths : procedure (fto:pculong; column:MYSQL_ROW; field_count:cuint);
            flush_use_result : procedure (mysql:PMYSQL);
{ $if !defined(MYSQL_SERVER) || defined(EMBEDDED_LIBRARY)}
            list_fields : function (mysql:PMYSQL):PMYSQL_FIELD;
            read_prepare_result : function (mysql:PMYSQL; stmt:PMYSQL_STMT):my_bool;
            stmt_execute : function (stmt:PMYSQL_STMT):cint;
            read_binary_rows : function (stmt:PMYSQL_STMT):cint;
            unbuffered_fetch : function (mysql:PMYSQL; row:PPchar):cint;
            free_embedded_thd : procedure (mysql:PMYSQL);
            read_statistics : function (mysql:PMYSQL):Pchar;
            next_result : function (mysql:PMYSQL):my_bool;
            read_change_user_result : function (mysql:PMYSQL; buff:Pchar; passwd:Pchar):cint;
            read_rowsfrom_cursor : function (stmt:PMYSQL_STMT):cint;
{ $endif}
         end;
       MYSQL_METHODS = st_mysql_methods;
       PMYSQL_METHODS = ^MYSQL_METHODS;


       Pst_mysql_manager = ^st_mysql_manager;
       st_mysql_manager = record
            net : NET;
            host : Pchar;
            user : Pchar;
            passwd : Pchar;
            net_buf, net_buf_pos, net_data_end : pcchar;
            port : cuint;
            cmd_status : cint;
            last_errno : cint;
            net_buf_size : cint;
            free_me : my_bool;
            eof : my_bool;
            last_error : array[0..(MAX_MYSQL_MANAGER_ERR)-1] of cchar;
            extension : pointer;
         end;
       MYSQL_MANAGER = st_mysql_manager;
       PMYSQL_MANAGER = ^MYSQL_MANAGER;

       Pst_mysql_parameters = ^st_mysql_parameters;
       st_mysql_parameters = record
            p_max_allowed_packet : pculong;
            p_net_buffer_length : pculong;
            extension : pointer;
         end;
       MYSQL_PARAMETERS = st_mysql_parameters;
       PMYSQL_PARAMETERS = ^MYSQL_PARAMETERS;

    { The following definitions are added for the enhanced
      client-server protocol }

    { statement state  }

    enum_mysql_stmt_state = (MYSQL_STMT_INIT_DONE = 1,MYSQL_STMT_PREPARE_DONE, MYSQL_STMT_EXECUTE_DONE,MYSQL_STMT_FETCH_DONE);

    {
      Note: this info is from the mysql-5.0 version:

      This structure is used to define bind information, and
      internally by the client library.
      Public members with their descriptions are listed below
      (conventionally `On input' refers to the binds given to
      mysql_stmt_bind_param, `On output' refers to the binds given
      to mysql_stmt_bind_result):

      buffer_type    - One of the MYSQL_* types, used to describe
                       the host language type of buffer.
                       On output: if column type is different from
                       buffer_type, column value is automatically converted
                       to buffer_type before it is stored in the buffer.
      buffer         - On input: points to the buffer with input data.
                       On output: points to the buffer capable to store
                       output data.
                       The type of memory pointed by buffer must correspond
                       to buffer_type. See the correspondence table in
                       the comment to mysql_stmt_bind_param.

      The two above members are mandatory for any kind of bind.

      buffer_length  - the length of the buffer. You don't have to set
                       it for any fixed length buffer: float, double,
                       int, etc. It must be set however for variable-length
                       types, such as BLOBs or STRINGs.

      length         - On input: in case when lengths of input values
                       are different for each execute, you can set this to
                       point at a variable containining value length. This
                       way the value length can be different in each execute.
                       If length is not NULL, buffer_length is not used.
                       Note, length can even point at buffer_length if
                       you keep bind structures around while fetching:
                       this way you can change buffer_length before
                       each execution, everything will work ok.
                       On output: if length is set, mysql_stmt_fetch will
                       write column length into it.

      is_null        - On input: points to a boolean variable that should
                       be set to TRUE for NULL values.
                       This member is useful only if your data may be
                       NULL in some but not all cases.
                       If your data is never NULL, is_null should be set to 0.
                       If your data is always NULL, set buffer_type
                       to MYSQL_TYPE_NULL, and is_null will not be used.

      is_unsigned    - On input: used to signify that values provided for one
                       of numeric types are unsigned.
                       On output describes signedness of the output buffer.
                       If, taking into account is_unsigned flag, column data
                       is out of range of the output buffer, data for this column
                       is regarded truncated. Note that this has no correspondence
                       to the sign of result set column, if you need to find it out
                       use mysql_stmt_result_metadata.
      error          - where to write a truncation error if it is present.
                       possible error value is:
                       0  no truncation
                       1  value is out of range or buffer is too small

      Please note that MYSQL_BIND also has internals members.
    }
       Pst_mysql_bind = ^st_mysql_bind;

       st_mysql_bind = record
            length : pculong;               // output length pointer
            is_null : Pmy_bool;             // Pointer to null indicator
            buffer : pointer;               // buffer to get/put data
            error: pmy_bool;                // set this if you want to track data truncations happened during fetch
            row_ptr : PByte;                // for the current data position
            store_param_func : procedure (net:PNET; param:Pst_mysql_bind);cdecl;
            fetch_result : procedure (_para1:Pst_mysql_bind; _para2:PMYSQL_FIELD; row:PPbyte);
            skip_result : procedure (_para1:Pst_mysql_bind; _para2:PMYSQL_FIELD; row:PPbyte);
            buffer_length : culong;         // output buffer length, must be set when fetching str/binary
            offset : culong;                // offset position for char/binary fetch
            length_value : culong;          //  Used if length is 0
            param_number : cuint;           // For null count and error messages
            pack_length : cuint;            // Internal length for packed data
            buffer_type : enum_field_types; // buffer type
            error_value : my_bool;          // used if error is 0
            is_unsigned : my_bool;          // set if integer type is unsigned
            long_data_used : my_bool;       // If used with mysql_send_long_data
            is_null_value : my_bool;        // Used if is_null is 0
            extension : Pointer;
         end;
       MYSQL_BIND = st_mysql_bind;
       PMYSQL_BIND = ^MYSQL_BIND;

       { From  "my_list.h" }
       st_list = record
         prev, next : ^st_list;
         data : pointer;
       end;
       LIST = st_list;

       { statement handler  }

       st_mysql_stmt_extension = record end;

       st_mysql_stmt = record
            mem_root : MEM_ROOT;            // root allocations
            list : LIST;                    // list to keep track of all stmts
            mysql : PMYSQL;                 // connection handle
            params : PMYSQL_BIND;           // input parameters
            bind : PMYSQL_BIND;             // input parameters
            fields : PMYSQL_FIELD;          // result set metadata
            result : MYSQL_DATA;            // cached result set
            data_cursor : PMYSQL_ROWS;      // current row in cached result
            read_row_func : function (stmt:Pst_mysql_stmt; row:PPbyte):cint;cdecl;
            affected_rows : my_ulonglong;   // copy of mysql->affected_rows after statement execution
            insert_id : my_ulonglong;       // copy of mysql->insert_id
            stmt_id : culong;               // Id for prepared statement
            flags : culong;                 // i.e. type of cursor to open
            prefetch_rows : culong;         // number of rows per one COM_FETCH
            server_status : cuint;          // Copied from mysql->server_status after execute/fetch to know
                                            // server-side cursor status for this statement.
            last_errno : cuint;             // error code
            param_count : cuint;            // input parameter count
            field_count : cuint;            // number of columns in result set
            state : enum_mysql_stmt_state;  // statement state
            last_error : array[0..(MYSQL_ERRMSG_SIZE)-1] of char;  // error message
            sqlstate : array[0..(SQLSTATE_LENGTH+1)-1] of char;
            send_types_to_server : my_bool; // Types of input parameters should be sent to server
            bind_param_done : my_bool;      // input buffers were supplied
            bind_result_done : cuchar;      // output buffers were supplied

            unbuffered_fetch_cancelled : my_bool;   // mysql_stmt_close() had to cancel this result
    {   Is set to true if we need to calculate field->max_length for
        metadata fields when doing mysql_stmt_store_result.       }
            update_max_length : my_bool;
            extension: ^st_mysql_stmt_extension;
         end;
       MYSQL_STMT = st_mysql_stmt;
    {   When doing mysql_stmt_store_result calculate max_length attribute
        of statement metadata. This is to be consistent with the old API,
        where this was done automatically.
        In the new API we do that only by request because it slows down
        mysql_stmt_store_result sufficiently.       }
       enum_stmt_attr_type = (STMT_ATTR_UPDATE_MAX_LENGTH
                              ,STMT_ATTR_CURSOR_TYPE,  // unsigned long with combination of cursor flags (read only, for update, etc)
                              STMT_ATTR_PREFETCH_ROWS // Amount of rows to retrieve from server per one fetch if using cursors.
                                                      // Accepts unsigned long attribute in the range 1 - ulong_max
                             );


//#define max_allowed_packet (*mysql_get_parameters()->p_max_allowed_packet)
//#define net_buffer_length (*mysql_get_parameters()->p_net_buffer_length)

    var
      mysql_server_init: function (argc:cint; argv:PPchar; groups:PPchar):cint;extdecl;
      mysql_server_end: procedure ();extdecl;
      mysql_library_init: function (argc:cint; argv:PPchar; groups:PPchar):cint;extdecl;
      mysql_library_end: procedure ();extdecl;
      mysql_num_rows: function (res:PMYSQL_RES):my_ulonglong;extdecl;
      mysql_num_fields: function (res:PMYSQL_RES):cuint;extdecl;
      mysql_eof: function (res:PMYSQL_RES):my_bool;extdecl;
      mysql_fetch_field_direct: function (res:PMYSQL_RES; fieldnr:cuint):PMYSQL_FIELD;extdecl;
      mysql_fetch_fields: function (res:PMYSQL_RES):PMYSQL_FIELD;extdecl;
      mysql_row_tell: function (res:PMYSQL_RES):MYSQL_ROW_OFFSET;extdecl;
      mysql_field_tell: function (res:PMYSQL_RES):MYSQL_FIELD_OFFSET;extdecl;
      mysql_field_count: function (mysql:PMYSQL):cuint;extdecl;
      mysql_affected_rows: function (mysql:PMYSQL):my_ulonglong;extdecl;
      mysql_insert_id: function (mysql:PMYSQL):my_ulonglong;extdecl;
      mysql_errno: function (mysql:PMYSQL):cuint;extdecl;
      mysql_error: function (mysql:PMYSQL):Pchar;extdecl;
      mysql_sqlstate: function (mysql:PMYSQL):Pchar;extdecl;
      mysql_warning_count: function (mysql:PMYSQL):cuint;extdecl;
      mysql_info: function (mysql:PMYSQL):Pchar;extdecl;
      mysql_thread_id: function (mysql:PMYSQL):culong;extdecl;
      mysql_character_set_name: function (mysql:PMYSQL):Pchar;extdecl;
      mysql_set_character_set: function (mysql:PMYSQL; csname:Pchar):cint;extdecl;
      mysql_init: function (mysql:PMYSQL):PMYSQL;extdecl;
      mysql_ssl_set: function (mysql:PMYSQL; key:Pchar; cert:Pchar; ca:Pchar; capath:Pchar;
                 cipher:Pchar):my_bool;extdecl;
      mysql_change_user: function (mysql:PMYSQL; user:Pchar; passwd:Pchar; db:Pchar):my_bool;extdecl;
      mysql_real_connect: function (mysql:PMYSQL; host:Pchar; user:Pchar; passwd:Pchar; db:Pchar;
                 port:cuint; unix_socket:Pchar; clientflag:culong):PMYSQL;extdecl;
      mysql_select_db: function (mysql:PMYSQL; db:Pchar):cint;extdecl;
      mysql_query: function (mysql:PMYSQL; q:Pchar):cint;extdecl;
      mysql_send_query: function (mysql:PMYSQL; q:Pchar; length:culong):cint;extdecl;
      mysql_real_query: function (mysql:PMYSQL; q:Pchar; length:culong):cint;extdecl;
      mysql_store_result: function (mysql:PMYSQL):PMYSQL_RES;extdecl;
      mysql_use_result: function (mysql:PMYSQL):PMYSQL_RES;extdecl;
      mysql_get_character_set_info: procedure(mysql:PMYSQL; charset:PMY_CHARSET_INFO);extdecl;
      mysql_session_track_get_first: function(mysql:PMYSQL; typ:enum_session_state_type; data:ppcchar; length:psize_t):cint; extdecl;
      mysql_session_track_get_next: function(mysql:PMYSQL; typ:enum_session_state_type; data:ppcchar; length:psize_t):cint; extdecl;

    { local infile support  }

    const
       LOCAL_INFILE_ERROR_LEN = 512;

    var
      mysql_shutdown: function (mysql:PMYSQL; shutdown_level:mysql_enum_shutdown_level):cint;extdecl;
      mysql_dump_debug_info: function (mysql:PMYSQL):cint;extdecl;
      mysql_refresh: function (mysql:PMYSQL; refresh_options:cuint):cint;extdecl;
      mysql_kill: function (mysql:PMYSQL; pid:culong):cint;extdecl;
      mysql_set_server_option: function (mysql:PMYSQL; option:enum_mysql_set_option):cint;extdecl;
      mysql_ping: function (mysql:PMYSQL):cint;extdecl;
      mysql_stat: function (mysql:PMYSQL):Pchar;extdecl;
      mysql_get_server_info: function (mysql:PMYSQL):Pchar;extdecl;
      mysql_get_client_info: function :Pchar;extdecl;
      mysql_get_client_version: function :culong;extdecl;
      mysql_get_host_info: function (mysql:PMYSQL):Pchar;extdecl;
      mysql_get_server_version: function (mysql:PMYSQL):culong;extdecl;
      mysql_get_proto_info: function (mysql:PMYSQL):cuint;extdecl;
      mysql_list_dbs: function (mysql:PMYSQL; wild:Pchar):PMYSQL_RES;extdecl;

      mysql_list_tables: function (mysql:PMYSQL; wild:Pchar):PMYSQL_RES;extdecl;
      mysql_list_processes: function (mysql:PMYSQL):PMYSQL_RES;extdecl;
      mysql_options: function (mysql:PMYSQL; option:mysql_option; arg:Pchar):cint;extdecl;
      mysql_options4: function (mysql:PMYSQL; option:mysql_option; arg1,arg2:Pointer):cint;extdecl;
      mysql_get_option: function (mysql:PMYSQL; option:mysql_option; arg:Pointer):cint;extdecl;
      mysql_free_result: procedure (result:PMYSQL_RES);extdecl;
      mysql_data_seek: procedure (result:PMYSQL_RES; offset:my_ulonglong);extdecl;
      mysql_row_seek: function (result:PMYSQL_RES; offset:MYSQL_ROW_OFFSET):MYSQL_ROW_OFFSET;extdecl;
      mysql_field_seek: function (result:PMYSQL_RES; offset:MYSQL_FIELD_OFFSET):MYSQL_FIELD_OFFSET;extdecl;
      mysql_fetch_row: function (result:PMYSQL_RES):MYSQL_ROW;extdecl;
      mysql_fetch_lengths: function (result:PMYSQL_RES):pculong;extdecl;
      mysql_fetch_field: function (result:PMYSQL_RES):PMYSQL_FIELD;extdecl;
      mysql_list_fields: function (mysql:PMYSQL; table:Pchar; wild:Pchar):PMYSQL_RES;extdecl;
      mysql_escape_string: function (fto:Pchar; from:Pchar; from_length:culong):culong;extdecl;
      mysql_hex_string: function (fto:Pchar; from:Pchar; from_length:culong):culong;extdecl;
      mysql_real_escape_string: function (mysql:PMYSQL; fto:Pchar; from:Pchar; length:culong):culong;extdecl;
      mysql_real_escape_string_quote: function(mysql:PMYSQL; fto:pcchar; from:pcchar; length:culong; quote: cchar):culong;extdecl;
      mysql_reset_connection: function(mysql:PMYSQL):cint;extdecl;
      mysql_debug: procedure (debug:Pchar);extdecl;

      mysql_rollback: function (mysql:PMYSQL):my_bool;extdecl;
      mysql_autocommit: function (mysql:PMYSQL; auto_mode:my_bool):my_bool;extdecl;
      mysql_commit: function (mysql:PMYSQL):my_bool;extdecl;
      mysql_more_results: function (mysql:PMYSQL):my_bool;extdecl;
      mysql_next_result: function (mysql:PMYSQL):cint;extdecl;
      mysql_close: procedure (sock:PMYSQL);extdecl;

      mysql_stmt_init: function (mysql:PMYSQL):PMYSQL_STMT;extdecl;
      mysql_stmt_prepare: function (stmt:PMYSQL_STMT; query:Pchar; length:culong):cint;extdecl;
      mysql_stmt_execute: function (stmt:PMYSQL_STMT):cint;extdecl;
      mysql_stmt_fetch: function (stmt:PMYSQL_STMT):cint;extdecl;
      mysql_stmt_fetch_column: function (stmt:PMYSQL_STMT; bind:PMYSQL_BIND; column:cuint; offset:culong):cint;extdecl;
      mysql_stmt_store_result: function (stmt:PMYSQL_STMT):cint;extdecl;
      mysql_stmt_param_count: function (stmt:PMYSQL_STMT):culong;extdecl;
      mysql_stmt_attr_set: function (stmt:PMYSQL_STMT; attr_type:enum_stmt_attr_type; attr:pointer):my_bool;extdecl;
      mysql_stmt_attr_get: function (stmt:PMYSQL_STMT; attr_type:enum_stmt_attr_type; attr:pointer):my_bool;extdecl;
      mysql_stmt_bind_param: function (stmt:PMYSQL_STMT; bnd:PMYSQL_BIND):my_bool;extdecl;
      mysql_stmt_bind_result: function (stmt:PMYSQL_STMT; bnd:PMYSQL_BIND):my_bool;extdecl;
      mysql_stmt_close: function (stmt:PMYSQL_STMT):my_bool;extdecl;
      mysql_stmt_reset: function (stmt:PMYSQL_STMT):my_bool;extdecl;
      mysql_stmt_free_result: function (stmt:PMYSQL_STMT):my_bool;extdecl;
      mysql_stmt_send_long_data: function (stmt:PMYSQL_STMT; param_number:cuint; data:Pchar; length:culong):my_bool;extdecl;
      mysql_stmt_result_metadata: function (stmt:PMYSQL_STMT):PMYSQL_RES;extdecl;
      mysql_stmt_param_metadata: function (stmt:PMYSQL_STMT):PMYSQL_RES;extdecl;
      mysql_stmt_errno: function (stmt:PMYSQL_STMT):cuint;extdecl;
      mysql_stmt_error: function (stmt:PMYSQL_STMT):Pchar;extdecl;
      mysql_stmt_sqlstate: function (stmt:PMYSQL_STMT):Pchar;extdecl;
      mysql_stmt_row_seek: function (stmt:PMYSQL_STMT; offset:MYSQL_ROW_OFFSET):MYSQL_ROW_OFFSET;extdecl;
      mysql_stmt_row_tell: function (stmt:PMYSQL_STMT):MYSQL_ROW_OFFSET;extdecl;
      mysql_stmt_data_seek: procedure (stmt:PMYSQL_STMT; offset:my_ulonglong);extdecl;
      mysql_stmt_num_rows: function (stmt:PMYSQL_STMT):my_ulonglong;extdecl;
      mysql_stmt_affected_rows: function (stmt:PMYSQL_STMT):my_ulonglong;extdecl;
      mysql_stmt_insert_id: function (stmt:PMYSQL_STMT):my_ulonglong;extdecl;
      mysql_stmt_field_count: function (stmt:PMYSQL_STMT):cuint;extdecl;
      mysql_stmt_next_result: function (stmt:PMYSQL_STMT):cint;extdecl;


    { status return codes  }

    const
       MYSQL_NO_DATA = 100;
       MYSQL_DATA_TRUNCATED  = 101;

    function mysql_reload(mysql : PMySQL) : cint;

    { The following functions are mainly exported because of mysqlbinlog;
      They are not for general usage     }

    function simple_command(mysql,command,arg,length,skip_check : cint) : cint;

type
  TmncMySQLLib = class(TmnLibrary)
  protected
    procedure AssignLibrary; override;
  public
  end;

var
  MySQLLib: TmncMySQLLib = nil;

implementation

ResourceString
  SErrAlreadyLoaded  = 'MySQL interface already initialized from library %s.';
  SErrLoadFailed     = 'Can not load MySQL library "%s". Please check your installation.';
  SErrDefaultsFailed = 'Can not load default MySQL library ("%s" or "%s"). Check your installation.';

procedure TmncMySQLLib.AssignLibrary;
begin
// Only the procedure that are given in the c-library documentation are loaded, to
// avoid problems with 'incomplete' libraries
    my_init := GetAddress('my_init');
    my_thread_init := GetAddress('my_thread_init');
    my_thread_end := GetAddress('my_thread_end');

    mysql_affected_rows := GetAddress('mysql_affected_rows');
    mysql_autocommit := GetAddress('mysql_autocommit');
    mysql_change_user := GetAddress('mysql_change_user');
    mysql_close := GetAddress('mysql_close');
    mysql_commit := GetAddress('mysql_commit');
    mysql_data_seek := GetAddress('mysql_data_seek');
    mysql_debug := GetAddress('mysql_debug');
    mysql_dump_debug_info := GetAddress('mysql_dump_debug_info');
    mysql_eof := GetAddress('mysql_eof');
    mysql_errno := GetAddress('mysql_errno');
    mysql_error := GetAddress('mysql_error');
    mysql_escape_string := GetAddress('mysql_escape_string');
    mysql_fetch_field := GetAddress('mysql_fetch_field');
    mysql_fetch_field_direct := GetAddress('mysql_fetch_field_direct');
    mysql_fetch_fields := GetAddress('mysql_fetch_fields');
    mysql_fetch_lengths := GetAddress('mysql_fetch_lengths');
    mysql_fetch_row := GetAddress('mysql_fetch_row');
    mysql_field_seek := GetAddress('mysql_field_seek');
    mysql_field_count := GetAddress('mysql_field_count');
    mysql_field_tell := GetAddress('mysql_field_tell');
    mysql_free_result := GetAddress('mysql_free_result');
    mysql_get_client_info := GetAddress('mysql_get_client_info');
    mysql_get_client_version := GetAddress('mysql_get_client_version');
    mysql_get_host_info := GetAddress('mysql_get_host_info');
    mysql_get_server_version := GetAddress('mysql_get_server_version');
    mysql_get_proto_info := GetAddress('mysql_get_proto_info');
    mysql_get_server_info := GetAddress('mysql_get_server_info');
    mysql_info := GetAddress('mysql_info');
    mysql_init := GetAddress('mysql_init');
    mysql_insert_id := GetAddress('mysql_insert_id');
    mysql_kill := GetAddress('mysql_kill');
    mysql_library_end := GetAddress('mysql_server_end');
    mysql_library_init := GetAddress('mysql_server_init');
    mysql_list_dbs := GetAddress('mysql_list_dbs');
    mysql_list_fields := GetAddress('mysql_list_fields');
    mysql_list_processes := GetAddress('mysql_list_processes');
    mysql_list_tables := GetAddress('mysql_list_tables');
    mysql_more_results := GetAddress('mysql_more_results');
    mysql_next_result := GetAddress('mysql_next_result');
    mysql_num_fields := GetAddress('mysql_num_fields');
    mysql_num_rows := GetAddress('mysql_num_rows');
    mysql_options := GetAddress('mysql_options');
    mysql_ping := GetAddress('mysql_ping');
    mysql_query := GetAddress('mysql_query');
    mysql_real_connect := GetAddress('mysql_real_connect');
    mysql_real_escape_string := GetAddress('mysql_real_escape_string');
    mysql_real_query := GetAddress('mysql_real_query');
    mysql_refresh := GetAddress('mysql_refresh');
    mysql_rollback := GetAddress('mysql_rollback');
    mysql_row_seek := GetAddress('mysql_row_seek');
    mysql_row_tell := GetAddress('mysql_row_tell');
    mysql_select_db := GetAddress('mysql_select_db');
    mysql_server_end := GetAddress('mysql_server_end');
    mysql_server_init := GetAddress('mysql_server_init');
    mysql_set_character_set := GetAddress('mysql_set_character_set');
    mysql_set_server_option := GetAddress('mysql_set_server_option');
    mysql_sqlstate := GetAddress('mysql_sqlstate');
    mysql_shutdown := GetAddress('mysql_shutdown');
    mysql_stat := GetAddress('mysql_stat');
    mysql_store_result := GetAddress('mysql_store_result');
    mysql_thread_id := GetAddress('mysql_thread_id');
    mysql_use_result := GetAddress('mysql_use_result');
    mysql_warning_count := GetAddress('mysql_warning_count');
    mysql_stmt_init := GetAddress('mysql_stmt_init');
    mysql_stmt_prepare := GetAddress('mysql_stmt_prepare');
    mysql_stmt_execute := GetAddress('mysql_stmt_execute');
    mysql_stmt_fetch := GetAddress('mysql_stmt_fetch');
    mysql_stmt_fetch_column := GetAddress('mysql_stmt_fetch_column');
    mysql_stmt_store_result := GetAddress('mysql_stmt_store_result');
    mysql_stmt_param_count := GetAddress('mysql_stmt_param_count');
    mysql_stmt_attr_set := GetAddress('mysql_stmt_attr_set');
    mysql_stmt_attr_get := GetAddress('mysql_stmt_attr_get');
    mysql_stmt_bind_param := GetAddress('mysql_stmt_bind_param');
    mysql_stmt_bind_result := GetAddress('mysql_stmt_bind_result');
    mysql_stmt_close := GetAddress('mysql_stmt_close');
    mysql_stmt_reset := GetAddress('mysql_stmt_reset');
    mysql_stmt_free_result := GetAddress('mysql_stmt_free_result');
    mysql_stmt_send_long_data := GetAddress('mysql_stmt_send_long_data');
    mysql_stmt_result_metadata := GetAddress('mysql_stmt_result_metadata');
    mysql_stmt_param_metadata := GetAddress('mysql_stmt_param_metadata');
    mysql_stmt_errno := GetAddress('mysql_stmt_errno');
    mysql_stmt_error := GetAddress('mysql_stmt_error');
    mysql_stmt_sqlstate := GetAddress('mysql_stmt_sqlstate');
    mysql_stmt_row_seek := GetAddress('mysql_stmt_row_seek');
    mysql_stmt_row_tell := GetAddress('mysql_stmt_row_tell');
    mysql_stmt_data_seek := GetAddress('mysql_stmt_data_seek');
    mysql_stmt_num_rows := GetAddress('mysql_stmt_num_rows');
    mysql_stmt_affected_rows := GetAddress('mysql_stmt_affected_rows');
    mysql_stmt_insert_id := GetAddress('mysql_stmt_insert_id');
    mysql_stmt_field_count := GetAddress('mysql_stmt_field_count');
    mysql_stmt_next_result := GetAddress('mysql_stmt_next_result');
    mysql_real_escape_string_quote := GetAddress('mysql_real_escape_string_quote');
    mysql_reset_connection := GetAddress('mysql_reset_connection');
end;

function net_new_transaction(net : st_net) : st_net;
begin
  net.pkt_nr := 0;
  result := net;
end;

function IS_PRI_KEY(n : longint) : boolean;
begin
  IS_PRI_KEY:=(n and PRI_KEY_FLAG)<>0;
end;

function IS_NOT_NULL(n : longint) : boolean;
begin
 IS_NOT_NULL:=(n and NOT_NULL_FLAG)<>0;
end;

function IS_BLOB(n : longint) : boolean;
begin
 IS_BLOB:=(n and BLOB_FLAG)<>0;
end;

function IS_NUM_FIELD(f : pst_mysql_field) : boolean;
begin
   IS_NUM_FIELD:=((f^.flags) and NUM_FLAG)<>0;
end;

function IS_NUM(t : enum_field_types) : boolean;
begin
  IS_NUM := ((t <= FIELD_TYPE_INT24) and (t<>FIELD_TYPE_TIMESTAMP)) or (t=FIELD_TYPE_YEAR) or (t=FIELD_TYPE_NEWDECIMAL);
end;

function INTERNAL_NUM_FIELD(f : Pst_mysql_field) : boolean;
begin
  INTERNAL_NUM_FIELD := (f^.ftype <= FIELD_TYPE_INT24) and ((f^.ftype <> FIELD_TYPE_TIMESTAMP)
  or (f^.length = 14) or (f^.length=8)) or (f^.ftype=FIELD_TYPE_YEAR);
end;

function mysql_reload(mysql : PMySQL) : cint;
begin
  mysql_reload:=mysql_refresh(mysql,REFRESH_GRANT);
end;

function simple_command(mysql,command,arg,length,skip_check : longint) : longint;
begin
  //simple_command:=mysql^.(methods^.advanced_command)(mysqlcommandNullS0arglengthskip_check);
  result := -1;
end;

initialization
  MySQLLib := TmncMySQLLib.Create(cMySQLLibName);
finalization
  FreeAndNil(MySQLLib);
end.

