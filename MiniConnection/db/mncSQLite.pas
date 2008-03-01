unit mncSQLite;
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
//  ExecuteSQL('PRAGMA TEMP_STORE=MEMORY');//zaher

interface

uses
  Classes, SysUtils, Variants, 
  {$ifdef FPC}
  sqlite3,
  {$else}
  mncSQLiteHeader,
  {$endif}
  mnStreams, mncConnections, mncSQL;

type
  TmncSQLiteConnection = class(TmncConnection)
  private
    FDBHandle:PSqlite3;
    FUseAnsi: Boolean;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected:Boolean; override;
    class function GetMode:TmncTransactionMode; override;
  public
    constructor Create;
    function EncodeString(s: string): string; override; //eg. AnsiToUTF8
    function DecodeString(s: string): string; override; //eg. UTF8ToAnsi
    procedure CheckError(Error: Integer; ExtraMsg: string = '');
    property DBHandle:PSqlite3 read FDBHandle;
    property UseAnsi: Boolean read FUseAnsi write FUseAnsi default True;
  end;

  { TmncSQLiteSession }

  TmncSQLiteSession = class(TmncSession)
  private
    function GetConnection: TmncSQLiteConnection;
    procedure SetConnection(const AValue: TmncSQLiteConnection);
  protected
    procedure DoStart; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    procedure ExecuteSQL(SQL: string);
    function GetActive: Boolean; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    property Connection:TmncSQLiteConnection read GetConnection write SetConnection;
  end;

  TmncSQLiteCommand = class(TmncSQLCommand)
  private
    FStatment:PPsqlite3_stmt;
    FTail: pchar;
    FBOF: Boolean;
    FEOF: Boolean;
    function GetConnection: TmncSQLiteConnection;
    procedure FetchFields;
    procedure FetchValues;
    procedure ApplyValues;
    procedure CheckError(Error:longint);
  protected
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetEOF:Boolean; override;
    function GetActive:Boolean; override;
    procedure DoClose; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    property Connection:TmncSQLiteConnection read GetConnection;
  public
    constructor Create(vSession:TmncSession);
    destructor Destroy; override;
    procedure Clear; override;
    function RowsAffected: Integer; virtual;
  end;

implementation

procedure TmncSQLiteConnection.CheckError(Error:longint; ExtraMsg: string);
var
  s : String;
begin
  if (Error <> SQLITE_OK) then
  begin
    s := DecodeString(sqlite3_errmsg(FDBHandle));
    if ExtraMsg <> '' then
      s := s + ' - ' + ExtraMsg;
    raise EmncException.Create(s);
  end;
end;

{ TmncSQLiteConnection }

constructor TmncSQLiteConnection.Create;
begin
  inherited Create;
  FUseAnsi := True;
end;

function TmncSQLiteConnection.DecodeString(s: string): string;
begin
  if FUseAnsi then
    Result := Utf8ToAnsi(s)
  else
    Result := s;
end;

procedure TmncSQLiteConnection.DoConnect;
begin
  if not AutoCreate and not FileExists(Resource) then
    raise EmncException.Create('Database not exist: "' + Resource + '"');
  CheckError(sqlite3_open(PChar(Resource), @FDBHandle));
end;

function TmncSQLiteConnection.GetConnected: Boolean;
begin
  Result := FDBHandle <> nil;
end;

procedure TmncSQLiteConnection.DoDisconnect;
begin
  CheckError(sqlite3_close(FDBHandle));
  FDBHandle := nil;
end;

function TmncSQLiteConnection.EncodeString(s: string): string;
begin
  if FUseAnsi then
    Result := AnsiToUtf8(s)
  else                     
    Result := s;
end;

{ TmncSQLiteSession }

destructor TmncSQLiteSession.Destroy;
begin
  inherited;
end;
procedure TmncSQLiteSession.DoStart;
begin
  ExecuteSQL('BEGIN');
end;

procedure TmncSQLiteSession.DoCommit;
begin
  ExecuteSQL('COMMIT');
end;

procedure TmncSQLiteSession.DoRollback;
begin
  ExecuteSQL('ROLLBACK');
end;

procedure TmncSQLiteSession.ExecuteSQL(SQL: string);
var
 lMsg  : pchar;
 s : string;
 r  : integer;
begin
  lMSg := nil;
  s := Connection.EncodeString(SQL);
  r := sqlite3_exec(Connection.FDBHandle, PChar(s), nil, nil, @lMsg);
  if lMSg <> nil then
  begin
    s := Connection.DecodeString(lMSg);
    sqlite3_free(lMSg);
  end;
  Connection.CheckError(r, s);
end;

function TmncSQLiteSession.GetActive: Boolean;
begin
  Result:= inherited GetActive;
end;

constructor TmncSQLiteSession.Create(vConnection: TmncConnection);
begin
  inherited;
end;

function TmncSQLiteSession.GetConnection: TmncSQLiteConnection;
begin
  Result := inherited Connection as TmncSQLiteConnection;
end;

class function TmncSQLiteConnection.GetMode: TmncTransactionMode;
begin
  Result := smSingleTransactions;
end;

procedure TmncSQLiteSession.SetConnection(const AValue: TmncSQLiteConnection);
begin
  inherited Connection := AValue;
end;

{ TmncSQLiteCommand }

procedure TmncSQLiteCommand.CheckError(Error: Integer);
var
  s : String;
  ExtraMsg: string;
  r: Integer;
begin
  if (Error <> SQLITE_OK) then
  begin
    s := Connection.DecodeString(sqlite3_errmsg(Connection.DBHandle));
    if Active then
    begin
      r := sqlite3_finalize(FStatment);//without check error prevent the loop
      if (r <> SQLITE_OK) then
        ExtraMsg := Connection.DecodeString(sqlite3_errmsg(Connection.DBHandle))
      else
        ExtraMsg := '';
      if ExtraMsg <> '' then
        s := s + ' - ' + ExtraMsg;
      FStatment := nil;
    end;
    raise EmncException.Create(s);
  end;
end;

procedure TmncSQLiteCommand.Clear;
begin
  inherited;
  FBOF := True;
end;

constructor TmncSQLiteCommand.Create(vSession:TmncSession);
begin
  inherited Create(vSession);
end;

destructor TmncSQLiteCommand.Destroy;
begin
  inherited;
end;

function TmncSQLiteCommand.GetEOF: Boolean;
begin
  Result := (FStatment = nil) or FEOF; 
end;

function TmncSQLiteCommand.RowsAffected: Integer;
begin
  Result := 0;
end;

procedure TmncSQLiteCommand.ApplyValues;
var
  s: string;
  i: Integer;
  d: Double;
  c: Currency;
  t: Integer;
  t64: Int64;
begin
  for i := 0 to ParamList.Count - 1 do
  begin
    ParamList.Items[i].FreeBuffer;
  end;

  for i := 0 to ParamList.Count - 1 do
  begin
    if ParamList.Items[i].IsEmpty then
      CheckError(sqlite3_bind_null(FStatment, i + 1))
    else
    begin
      case VarType(ParamList.Items[i].Value) of
        varDate:
        begin
          d := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, d));
        end;
        varInteger:
        begin
          t := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_int(FStatment, i + 1, t));
        end;
        varint64:
        begin
          t64 := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_int64(FStatment, i + 1, t64));
        end;
        varCurrency:
        begin
          c := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, c));
        end;
        varDouble:
        begin
          d := ParamList.Items[i].Value;
          CheckError(sqlite3_bind_double(FStatment, i + 1, d));
        end;
        else
        begin
          if not ParamList.Items[i].BufferAllocated then
          begin
            s := Connection.EncodeString(ParamList.Items[i].Value);
            ParamList.Items[i].AllocBuffer(PChar(s)^, Length(s));
          end;
          CheckError(sqlite3_bind_text(FStatment, i + 1, PChar(ParamList.Items[i].Buffer), ParamList.Items[i].BufferSize, nil));
        end;
      end;
    end;
  end;
end;

procedure TmncSQLiteCommand.DoExecute;
begin
  if FStatment <> nil then
    CheckError(sqlite3_reset(FStatment));
  ApplyValues;
end;

procedure TmncSQLiteCommand.DoNext;
var
  r: Integer;
begin
//  CheckError(sqlite3_step(@FStatment));
  r:=sqlite3_step(FStatment);
  if (r = SQLITE_ROW) then
  begin
    if FBOF then
      FetchFields;
    FetchValues;
    FEOF := False;
  end
  else if (r = SQLITE_DONE) then
  begin
    FEOF := True;
    CheckError(sqlite3_reset(FStatment));
  end
  else
    CheckError(r);
  FBOF := False;
end;

procedure TmncSQLiteCommand.DoPrepare;
var
  s:string;
  r: Integer;
begin
  FBOF := True;
  s := ParseSQL([]);
//  s := Connection.EncodeString(ParseSQL([]));
//  sqlite3_prepare_v2
  r := sqlite3_prepare(Connection.DBHandle, PChar(s), -1 , @FStatment, @FTail);
  CheckError(r);
end;

procedure TmncSQLiteCommand.DoRollback;
begin
end;

procedure TmncSQLiteCommand.DoClose;
begin
  CheckError(sqlite3_finalize(FStatment));
//  CheckError(sqlite3_clear_bindings(FStatment));//not found in SQLite3 for WinCE
  FStatment := nil;  
end;

procedure TmncSQLiteCommand.DoCommit;
begin
end;

procedure TmncSQLiteCommand.FetchFields;
var
  i: Integer;
  c: Integer;
  aName: string;
begin
  Fields.Clear;
  c := sqlite3_column_count(FStatment);
  for i := 0 to c -1 do
  begin
//    sqlite3_column_type
    aName := Connection.DecodeString(sqlite3_column_name(FStatment, i));
    Fields.Add(aName);
  end;
end;

procedure TmncSQLiteCommand.FetchValues;
var
  i: Integer;
  c: Integer;
  int:Int64;
  str: string;
  flt: Double;
  aCurrent: TmncRecord;
  aType: Integer;
begin
  c := sqlite3_column_count(FStatment);
  if c > 0 then
  begin
    aCurrent := TmncRecord.Create(Fields);
    for i := 0 to c - 1 do
    begin
//TStorageType = (stNone,stInteger,stFloat,stText,stBlob,stNull);
      aType := sqlite3_column_type(FStatment, i);
      case aType of
        SQLITE_NULL:
        begin
          aCurrent.Add(i, Null);
        end;
        SQLITE_INTEGER:
        begin
          int := sqlite3_column_int(FStatment, i);
          aCurrent.Add(i, int);
        end;
        SQLITE_FLOAT:
        begin
          flt := sqlite3_column_double(FStatment, i);
          aCurrent.Add(i, flt);
        end;
        else
        begin
          str := Connection.DecodeString(sqlite3_column_text(FStatment, i));
          aCurrent.Add(i, str);
        end;
      end;
    end;
    Current := aCurrent;
  end;
end;

function TmncSQLiteCommand.GetActive: Boolean;
begin
  Result := FStatment <> nil; 
end;

function TmncSQLiteCommand.GetConnection: TmncSQLiteConnection;
begin
  Result := Session.Connection as TmncSQLiteConnection;
end;

end.

