unit mncFirebird;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{
  Only FirebirdSQL 2.5 Dialect 3
}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants,
  mncConnections, mncSQL,
  mncFBTypes, mncFBHeader, mncFBErrors, mncFBUtils, mncFBClient, mncFBStrings,
  mncSQLDA;

const
  USE_DIALECT = 3;

type

  { TmncFBConnection }

  TmncFBConnection = class(TmncConnection)
  private
    FHandle: TISC_DB_HANDLE;
    FDialect: Integer;
    FCachedPasswords: Boolean;
    FCharacterSet: string;
    FProtocol: TFBProtocol;
    FRole: string;
    FDPB: PChar;
    FDPBLength: Short;
    function GetIsReadOnly: Boolean;
    procedure SetProtocol(const AValue: TFBProtocol);
  protected
    function GetBaseLevel: Long;
    function GetDBSQLDialect: Integer;
    function GetLongDatabaseInfo(Command: Integer): Integer;

    function Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    class function GetMode: TmncSessionMode; override;
    procedure DoInit; override;
  public
    constructor Create;
    function GetVersion: string;
    procedure Execute(SQL: string);
    property CachedPasswords:Boolean read FCachedPasswords write FCachedPasswords default False;
    property Role: string read FRole write FRole;
    property CharacterSet: string read FCharacterSet write FCharacterSet;
    property Protocol: TFBProtocol read FProtocol write SetProtocol default dpTCP;
    property Handle: TISC_DB_HANDLE read FHandle;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  { TmncFBSession }

  TmncFBSession = class(TmncSession)
  private
    FHandle: TISC_TR_HANDLE;
    FStreamedActive: Boolean;
    FTPB: PChar;
    FTPBLength: Short;
    function GetConnection: TmncFBConnection;
    procedure SetConnection(const AValue: TmncFBConnection);
  protected
    function Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
    procedure DoInit; override;
    procedure DoStart; override;
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); override;
    function GetActive: Boolean; override;
  public
    constructor Create(vConnection: TmncConnection); override;
    destructor Destroy; override;
    procedure Execute(SQL: string);
    function GetLastInsertID: Int64;
    function GetRowsChanged: Integer;
    property Handle: TISC_TR_HANDLE read FHandle;
    property TPB: PChar read FTPB;
    property TPBLength: Short read FTPBLength;
    property Connection: TmncFBConnection read GetConnection write SetConnection;
  end;

  TmncFBTransaction = TmncFBSession;

  { TmncFBCommand }

  TmncFBCommand = class(TmncSQLCommand)
  private
    FHandle: TISC_STMT_HANDLE;
    FActive: Boolean;
    FBOF: Boolean;
    FEOF: Boolean;
    FCursor: string; { Cursor name }
    FSQLParams: TFBSQLDA; { Params }
    FSQLCurrent: TFBSQLDA; { The current record }
    FSQLType: TFBDSQLTypes;
    FGenerateParamNames: Boolean;
    FRecordCount: Integer;
    FParsedSQL: string;
    function GetConnection: TmncFBConnection;
    function GetDBHandle: PISC_DB_HANDLE;
    function GetSession: TmncFBSession;
    function GetTRHandle: PISC_TR_HANDLE;
    procedure SetSession(const AValue: TmncFBSession);
    procedure FreeHandle;
    procedure InternalPrepare(Full: Boolean);
  protected
    function Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
    procedure ValidStatement;
    procedure DoUnprepare; override;
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetEOF:Boolean; override;
    function GetActive:Boolean; override;
    procedure DoClose; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    property Connection: TmncFBConnection read GetConnection;
    property Session: TmncFBSession read GetSession write SetSession;
//    property Transaction: TmncFBSession read GetSession write SetSession;// Alias
    property TRHandle: PISC_TR_HANDLE read GetTRHandle;
    property DBHandle: PISC_DB_HANDLE read GetDBHandle;
  public

    procedure Clear; override;
    function GetRowsChanged: Integer; virtual;
    property SQLType: TFBDSQLTypes read FSQLType;
    property Handle: TISC_STMT_HANDLE read FHandle;
  end;

implementation

{ TmncFBConnection }

constructor TmncFBConnection.Create;
begin
  inherited Create;
end;

function TmncFBConnection.GetVersion: string;
var
  local_buffer: array[0..FBBigLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
  StatusVector:TStatusVector;
begin
  DatabaseInfoCommand := Char(isc_info_version);
  Call(FBClient.isc_database_info(@StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        FBBigLocalBufferLength, local_buffer), StatusVector, True);
  local_buffer[5 + Int(local_buffer[4])] := #0;
  Result := String(PChar(@local_buffer[5]));
end;

procedure TmncFBConnection.SetProtocol(const AValue: TFBProtocol);
begin
  if FProtocol =AValue then exit;
  FProtocol :=AValue;
end;

function TmncFBConnection.GetLongDatabaseInfo(Command: Integer): Integer;
var
  l: Integer;
  c: Char;
  StatusVector: TStatusVector;
  local_buffer: array[0..FBLocalBufferLength - 1] of Char;
begin
  c := Char(Command);
  Call(FBClient.isc_database_info(@StatusVector, @Handle, 1, @c, FBLocalBufferLength, local_buffer), StatusVector, True);
  l := FBClient.isc_vax_integer(@local_buffer[1], 2);
  Result := FBClient.isc_vax_integer(@local_buffer[3], l);
end;

function TmncFBConnection.GetIsReadOnly: Boolean;
begin
  Result := GetLongDatabaseInfo(isc_info_db_read_only) <> 0;
end;

function TmncFBConnection.GetBaseLevel: Long;
var
  local_buffer: array[0..FBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
  StatusVector: TStatusVector;
begin
  //CheckActive;
  DatabaseInfoCommand := Char(isc_info_base_level);
  Call(FBClient.isc_database_info(@StatusVector, @FHandle, 1, @DatabaseInfoCommand,
    FBLocalBufferLength, local_buffer), StatusVector, True);
  Result := FBClient.isc_vax_integer(@local_buffer[4], 1);
end;

function TmncFBConnection.GetDBSQLDialect: Integer;
var
  local_buffer: array[0..FBLocalBufferLength - 1] of Char;
  length: Integer;
  DatabaseInfoCommand: Char;
  StatusVector: TStatusVector;
begin
  DatabaseInfoCommand := Char(isc_info_db_SQL_Dialect);
  Call(FBClient.isc_database_info(@StatusVector, @FHandle, 1, @DatabaseInfoCommand,
    FBLocalBufferLength, local_buffer), StatusVector, True);
  if (local_buffer[0] <> Char(isc_info_db_SQL_dialect)) then
    Result := 1
  else
  begin
    length := FBClient.isc_vax_integer(@local_buffer[1], 2);
    Result := FBClient.isc_vax_integer(@local_buffer[3], length);
  end;
end;

function TmncFBConnection.Call(ErrCode: ISC_STATUS;  const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := ErrCode;
  if RaiseError and (CheckStatusVector(StatusVector, [isc_lost_db_connection]) or
    CheckStatusVector(StatusVector, [isc_net_read_err]) or
    CheckStatusVector(StatusVector, [isc_net_write_err])) then
  begin
    try
      FBRaiseError(StatusVector);
    finally
      Close;
    end;
  end;
  if RaiseError and (ErrCode > 0) then
    FBRaiseError(StatusVector);
end;

procedure TmncFBConnection.DoConnect;
var
  DPB: string;
  i: Integer;
  aDatabaseName: string;
  aParams: TStringList;
  StatusVector: TStatusVector;
begin
  FDialect := USE_DIALECT;
  { Generate a new DPB if necessary }
  aParams := TStringList.Create;
  try
    aParams.Assign(Params);
    FBHostInfo(Host, UserName, Password, Role, CharacterSet, aParams, CachedPasswords);
    GenerateDPB(aParams, DPB, FDPBLength);
  finally
    aParams.Free;
  end;
  FBAlloc(FDPB, 0, FDPBLength);
  Move(DPB[1], FDPB[0], FDPBLength);
  aDatabaseName := FBComposeConnectionString(Resource, Host, Port, FBClient.IsEmbed, Protocol);
  if Call(FBClient.isc_attach_database(@StatusVector, Length(aDatabaseName),
    PChar(aDatabaseName), @FHandle, FDPBLength, FDPB), StatusVector, False) > 0 then
  begin
    FHandle := nil;
    FBRaiseError(StatusVector);
  end;
  FDialect := GetDBSQLDialect;
  if (FDialect < USE_DIALECT) then
    raise EFBError.Create(-1, 'This database not dialect ' + IntToStr(USE_DIALECT))

{    for i := 0 to FEventNotifiers.Count - 1 do
      if IFBEventNotifier(FEventNotifiers[i]).GetAutoRegister then
        IFBEventNotifier(FEventNotifiers[i]).RegisterEvents;}
end;

function TmncFBConnection.GetConnected: Boolean;
begin
  Result := FHandle <> nil;
end;

procedure TmncFBConnection.DoDisconnect;
var
  i: Integer;
  StatusVector: TStatusVector;
begin
{  for i := 0 to FEventNotifiers.Count - 1 do
    IFBEventNotifier(FEventNotifiers[i]).UnRegisterEvents;}
  if (Call(FBClient.isc_detach_database(@StatusVector, @FHandle), StatusVector, False) > 0) then
    FBRaiseError(StatusVector)
  else
    FHandle := nil;
  FDialect := USE_DIALECT;
end;

{ TmncFBSession }

destructor TmncFBSession.Destroy;
begin
  FreeMem(FTPB);
  FTPB := nil;
  inherited;
end;

procedure TmncFBSession.Execute(SQL: string);
begin
  Connection.Execute(SQL);
end;

procedure TmncFBSession.DoStart;
var
  pteb: PISC_TEB_ARRAY;
  TPB: string;
  StatusVector: TStatusVector;
begin
  if ParamsChanged then
  begin
    ParamsChanged := False;
    GenerateTPB(Params, TPB, FTPBLength);
    if FTPBLength > 0 then
    begin
      FBAlloc(FTPB, 0, FTPBLength);
      Move(TPB[1], FTPB[0], FTPBLength);
    end;
  end;

  pteb := nil;
  FBAlloc(pteb, 0, SizeOf(TISC_TEB));
  try
    pteb^[0].db_handle := @(Connection.Handle);
    pteb^[0].tpb_length := FTPBLength;
    pteb^[0].tpb_address := FTPB;
    if Call(FBClient.isc_start_multiple(@StatusVector, @FHandle, 1, PISC_TEB(pteb)), StatusVector, False) > 0 then
    begin
      FHandle := nil;
      FBRaiseError(StatusVector);
    end;
  finally
    FreeMem(pteb);
  end;
end;

procedure TmncFBSession.DoStop(How: TmncSessionAction; Retaining: Boolean);
var
  status: ISC_STATUS;
  StatusVector: TStatusVector;
begin
  if not Retaining then
  try
    //EndSQLObjects;
  finally
  end;

  case How of
    sdaCommit:
    begin
      if not Retaining then
        Call(FBClient.isc_commit_transaction(@StatusVector, @FHandle), StatusVector, False)
      else
        Call(FBClient.isc_commit_retaining(@StatusVector, @FHandle), StatusVector, True);
    end;
    sdaRollback:
    begin
      if not Retaining then
        Call(FBClient.isc_rollback_transaction(@StatusVector, @FHandle), StatusVector, True)
      else
        Call(FBClient.isc_rollback_retaining(@StatusVector, @FHandle), StatusVector, True);
    end;
  end;
{  if ((Force) and (status > 0)) then
    status := Call(FBClient.isc_rollback_transaction(@StatusVector, @FHandle), StatusVector, True);
    if not Retaining and Force then
      FHandle := nil
    if (status > 0) then
    FBRaiseError(StatusVector);
}
end;

procedure TmncFBConnection.Execute(SQL: string);
var
  tr_handle: TISC_TR_HANDLE;
  StatusVector: TStatusVector;
begin
  tr_handle := nil;
  try
    Call(FBClient.isc_dsql_execute_immediate(@StatusVector, @FHandle, @tr_handle, 0, PChar(SQL), USE_DIALECT, nil), StatusVector, True);
  finally
  end;
end;


function TmncFBSession.GetLastInsertID: Int64;
begin
  CheckActive;
end;

function TmncFBSession.GetRowsChanged: Integer;
begin
  CheckActive;
end;

function TmncFBSession.GetActive: Boolean;
begin
  Result:= FHandle <> nil;
end;

constructor TmncFBSession.Create(vConnection: TmncConnection);
begin
  inherited;
  FHandle := nil;
  FTPB := nil;
  FTPBLength := 0;
end;

function TmncFBSession.GetConnection: TmncFBConnection;
begin
  Result := inherited Connection as TmncFBConnection;
end;

class function TmncFBConnection.GetMode: TmncSessionMode;
begin
  Result := smMultiple;
end;

procedure TmncFBConnection.DoInit;
begin
end;

procedure TmncFBSession.SetConnection(const AValue: TmncFBConnection);
begin
  inherited Connection := AValue;
end;

function TmncFBSession.Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := ErrCode;
  if CheckStatusVector(StatusVector, [isc_lost_db_connection]) then
    Connection.Call(ErrCode, StatusVector, RaiseError)
  else if RaiseError and (Result > 0) then
    FBRaiseError(StatusVector);
end;

procedure TmncFBSession.DoInit;
begin
end;

{ TmncFBCommand }

function TmncFBCommand.GetSession: TmncFBSession;
begin
  Result := inherited Session as TmncFBSession;
end;

function TmncFBCommand.GetTRHandle: PISC_TR_HANDLE;
begin

end;

procedure TmncFBCommand.SetSession(const AValue: TmncFBSession);
begin
  inherited Session := AValue;
end;

procedure TmncFBCommand.FreeHandle;
var
  isc_res: ISC_STATUS;
  StatusVector: TStatusVector;
begin
  try
    if (FHandle <> nil) then
    begin
      //if Connection.Connected then
      begin
        isc_res := Call(FBClient.isc_dsql_free_statement(@StatusVector, @FHandle, DSQL_drop), StatusVector, False);
        if (StatusVector[0] = 1) and (isc_res > 0) and (isc_res <> isc_bad_stmt_handle) and (isc_res <> isc_lost_db_connection) then
          FBRaiseError(StatusVector);
      end;
    end;
  finally
    FHandle := nil;
  end;
end;

procedure TmncFBCommand.Clear;
begin
  inherited;
end;

function TmncFBCommand.GetEOF: Boolean;
begin
end;

function TmncFBCommand.GetRowsChanged: Integer;
begin
  Result := Session.GetRowsChanged;
end;

procedure TmncFBCommand.DoExecute;
var
  fetch_res: ISC_STATUS;
  i: Integer;
  StatusVector: TStatusVector;
begin
  {if (FXParams <> nil) and (FXParams.Count > 0) and (Params.Count > 0) then
  begin
    for i := 0 to Params.Count - 1 do
      if FXParams.IsExist(Params[i].Name) then
        Params[i].Value := FXParams[Params[i].Name];
  end;}
  //Params.ApplyClones;
  ValidStatement;
  case FSQLType of
    SQLSelect:
      begin
        Call(FBClient.isc_dsql_execute2(@StatusVector,
          TRHandle,
          @FHandle,
          USE_DIALECT,
          FSQLParams.AsXSQLDA,
          nil), StatusVector, True);

        Call(FBClient.isc_dsql_set_cursor_name(@StatusVector, @FHandle, PAnsiChar(FCursor), 0), StatusVector, True);
        FActive := True;
        FBOF := True;
        FEOF := False;
        FRecordCount := 0;
        if NextOnExecute then
          Next;
      end;
    SQLExecProcedure:
      begin
        fetch_res := Call(FBClient.isc_dsql_execute2(@StatusVector,
          TRHandle,
          @FHandle,
          USE_DIALECT,
          FSQLParams.AsXSQLDA,
          FSQLCurrent.AsXSQLDA), StatusVector, False);
        if (fetch_res <> 0) then
        begin
          if (fetch_res <> isc_lock_conflict) then
          begin
           { Sometimes a prepared stored procedure appears to get
             off sync on the server ....This code is meant to try
             to work around the problem simply by "retrying". This
             need to be reproduced and fixed.
           }
            FBClient.isc_dsql_prepare(@StatusVector, TRHandle, @FHandle, 0,
              PAnsiChar(FParsedSQL), USE_Dialect, nil);
            Call(FBClient.isc_dsql_execute2(@StatusVector,
              TRHandle,
              @FHandle,
              USE_Dialect,
              FSQLParams.AsXSQLDA,
              FSQLCurrent.AsXSQLDA), StatusVector, True);
          end
          else
            FBRaiseError(StatusVector); // go ahead and raise the lock conflict
        end;
      end
  else
    Call(FBClient.isc_dsql_execute(@StatusVector,
      TRHandle,
      @FHandle,
      USE_Dialect,
      FSQLParams.AsXSQLDA), StatusVector, True)
  end;
end;

procedure TmncFBCommand.DoNext;
begin
end;

procedure TmncFBCommand.DoPrepare;
begin
  InternalPrepare(True);
end;

procedure TmncFBCommand.DoRollback;
begin
  Session.Rollback;
end;

procedure TmncFBCommand.DoClose;
begin
end;

procedure TmncFBCommand.DoCommit;
begin
  Session.Commit;
end;

function TmncFBCommand.GetActive: Boolean;
begin
end;

function TmncFBCommand.GetConnection: TmncFBConnection;
begin
  Result := Session.Connection as TmncFBConnection;
end;

function TmncFBCommand.GetDBHandle: PISC_DB_HANDLE;
begin
  //Result := Session.Connection.Handle;
end;

procedure TmncFBCommand.ValidStatement;
begin
  //CheckTransaction;
  if (FHandle = nil) then
    FBRaiseError(fbceInvalidStatementHandle, [nil]);
end;

procedure TmncFBCommand.DoUnprepare;
begin
  inherited;
  FreeHandle;
end;

function TmncFBCommand.Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := 0;
  if (ErrCode > 0) and (Session <> nil) then
    Result := Session.Call(ErrCode, StatusVector, RaiseError)
  else if RaiseError and (ErrCode > 0) then
    FBRaiseError(StatusVector);
end;

procedure TmncFBCommand.InternalPrepare(Full: Boolean);
var
  res_buffer: array[0..7] of Char;
  type_item: Char;
  sql_type:Integer;
  StatusVector: TStatusVector;
begin
  if not Prepared then
  begin
    if (SQL.Text = '') then
      FBRaiseError(fbceEmptyQuery, [nil]);
    FParsedSQL := ParseSQL([]);
    if (FParsedSQL = '') then
      FBRaiseError(fbceEmptyQuery, [nil]);
    try
      Call(FBClient.isc_dsql_alloc_statement2(@StatusVector, DBHandle, @FHandle), StatusVector, True);
      Call(FBClient.isc_dsql_prepare(@StatusVector, TRHandle, @FHandle, 0, PAnsiChar(FParsedSQL), USE_DIALECT, nil), StatusVector, True);
      { After preparing the statement, query the stmt type and possibly
        create a FSQLCurrent "holder" }
      { Get the type of the statement }
      type_item := Char(isc_info_sql_stmt_type);
      Call(FBClient.isc_dsql_sql_info(@StatusVector, @FHandle, 1, @type_item, SizeOf(res_buffer), res_buffer), StatusVector, True);
      if not GetInfoReqInteger(res_buffer, isc_info_sql_stmt_type, sql_type) then
        FBRaiseError(fbceUnknownError, [nil]);
      FSQLType := TFBDSQLTypes(sql_type);

      { Done getting the type }
      case FSQLType of
        SQLGetSegment,
          SQLPutSegment,
          SQLStartTransaction:
          begin
            FreeHandle;
            FBRaiseError(fbceNotPermitted, [nil]);
          end;
        SQLCommit,
          SQLRollback,
          SQLDDL, SQLSetGenerator,
          SQLInsert, SQLUpdate, SQLDelete, SQLSelect, SQLSelectForUpdate,
          SQLExecProcedure:
          begin
          { We already know how many inputs there are, so... }
            if (FSQLParams.Data <> nil) and
              (Call(FBClient.isc_dsql_describe_bind(@StatusVector, @FHandle, USE_DIALECT, FSQLParams.Data), StatusVector, True) > 0) then
              FBRaiseError(StatusVector);
            FSQLParams.Initialize;
            if Full and (FSQLType in [SQLSelect, SQLSelectForUpdate, SQLExecProcedure]) then
            begin
            { Allocate an initial output descriptor (with one column) }
              FSQLCurrent.Clear;
              FSQLCurrent.Count := 1;
            { Using isc_dsql_describe, get the right size for the columns... }
              Call(FBClient.isc_dsql_describe(@StatusVector, @FHandle, USE_DIALECT, FSQLCurrent.Data), StatusVector, True);
              if FSQLCurrent.Data^.sqld > FSQLCurrent.Data^.sqln then
              begin
                FSQLCurrent.Count := FSQLCurrent.Data^.sqld;
                Call(FBClient.isc_dsql_describe(@StatusVector, @FHandle, USE_DIALECT, FSQLCurrent.Data), StatusVector, True);
              end
              else if FSQLCurrent.Data^.sqld = 0 then
                FSQLCurrent.Clear;
              FSQLCurrent.Initialize;
            end;
          end;
      end;
    except
      on E: Exception do
      begin
        if (FHandle <> nil) then
          FreeHandle;
        raise;
      end;
    end;
  end;
end;

end.
