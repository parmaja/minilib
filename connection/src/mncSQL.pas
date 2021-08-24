unit mncSQL;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}
{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{*TODO
*  Bulk Insert (save it into memory before execute
*
*
*}
interface

uses
  Classes, SysUtils, Contnrs, StrUtils,
  mnClasses,
  mncConnections, mncCommons;

type
{TODO
  ESQLError = class(Exception)
  private
    FSQLCode: Integer;
    FErrorCode: Integer;
  public
    constructor Create(ASQLCode: Integer; Msg: string); overload;
    constructor Create(ASQLCode: Integer; AErrorCode: Integer; Msg: string); overload;
    property SQLCode: Integer read FSQLCode;
    property ErrorCode: Integer read FErrorCode;
  end;

  ESQLExceptionError = class(ESQLError)
  private
    FExceptionID: Integer;
    FExceptionMsg: string;
    FExceptionName: string;
  public
    constructor Create(ASQLCode: Integer; AErrorCode: Integer; AExceptionID: Integer; AExceptionName, AExceptionMsg: string; Msg: string); overload;
    property ExceptionID: Integer read FExceptionID;
    property ExceptionName: string read FExceptionName;
    property ExceptionMsg: string read FExceptionMsg;
  end;}

  TmncParseSQLOptions = set of (psoGenerateParams, psoAddParamsID, psoAddParamsNames);

  TmncSQLSession = class;
  TmncSQLCommand = class;

  { TmncSQLConnection }

  TmncSQLConnection = class abstract(TmncConnection)
  protected
    procedure DoClone(vConn: TmncSQLConnection); virtual;
  public
    constructor Create; override;
    function CreateSession: TmncSQLSession; virtual; abstract;

    function IsDatabaseExists(vName: string): Boolean; overload; virtual; abstract;
    function IsDatabaseExists: Boolean; overload;
    procedure CreateDatabase(const vName: string; CheckExists: Boolean = False); overload; virtual; abstract;
    procedure CreateDatabase(CheckExists: Boolean = False); overload;
    procedure DropDatabase(const vName: string; CheckExists: Boolean = False); overload; virtual; abstract;
    procedure DropDatabase(CheckExists: Boolean = False); overload;

    procedure Execute(vCommand: string); overload; virtual;
    procedure Execute(vCommand: string; vArgs: array of const); overload;

    function Clone(const vResource: string; AutoConnect: Boolean = True): TmncSQLConnection; overload;
    function Clone(AutoConnect: Boolean = True): TmncSQLConnection; overload;
    procedure CloneExecute(const vResource, vSQL: string; vArgs: array of const); overload;
    procedure CloneExecute(const vResource, vSQL: string); overload;
    procedure Vacuum; virtual;
    {
      GetParamChar: Called to take the real param char depend on the sql engine to replace it with this new one.
                    by default it is ?
    }
    function GetParamChar: string; virtual;
    function GetExtension: string; virtual;
    //ScriptSeperator
    function ScriptSeperator: string; virtual;
  end;

  { TmncSQLSession }

  TmncSQLSession = class abstract(TmncSession)
  private
    function GetConnection: TmncSQLConnection;
    procedure SetConnection(AValue: TmncSQLConnection);
  protected
    function InternalCreateCommand: TmncSQLCommand; virtual; abstract;
  public
    function CreateCommand(ASQL: string = ''): TmncSQLCommand;
    procedure ExecuteScript(AStrings: TStrings; AutoCommit: Boolean = False);
    property Connection: TmncSQLConnection read GetConnection write SetConnection;
  end;

  { TmncSQLName }

  TmncSQLName = class(TObject)
  public
    ID: Integer;
    Name: string;
    Position: Integer;
  end;

  { TmncProcessedSQL }

  TmncProcessedSQL = class(TmnObjectList<TmncSQLName>)
  private
  public
    SQL: string;
    procedure Clear; {$ifdef FPC} override; {$endif}    //TODO
    procedure Add(vID: Integer; vName:string; vPosition: Integer = -1);
  end;

  { TmncSQLCommand }

  TmncSQLCommand = class abstract(TmncCommand)
  private
    FFetchBlob: Boolean;
    FFetched: Int64;
    FParamPrefix: Char;
    FProcessSQL: Boolean;
    FReady: Boolean; //BOF
    FDone: Boolean; //EOF
    FProcessedSQL: TmncProcessedSQL;
    function GetSession: TmncSQLSession;
    procedure SetSession(AValue: TmncSQLSession);
    function GetSQL: TStrings;
    procedure SetParamPrefix(AValue: Char);
    property ProcessedSQL: TmncProcessedSQL read FProcessedSQL;
  protected
    function GetDone: Boolean; override;
    function GetParseOptions: TmncParseSQLOptions; virtual;
    procedure DoParse; override;
    procedure DoUnparse; override;
    procedure DoExecute; override;
    procedure ParseSQL(SQLOptions: TmncParseSQLOptions);
    procedure Fetch; override;
    procedure Clean; override; //Clean and reset stamemnt like Done or Ready called in Execute before DoExecute and after Prepare
    procedure HitDone;   //Make it FDone True
    procedure HitUnready; //Make it FReady False
    function GetProcessedSQL: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetLastRowID: Int64; virtual;
    function GetRowsChanged: Integer; virtual;
    property SQL: TStrings read GetSQL;//Alias of Request, autocomplete may add it in private becareful
    property Ready: Boolean read FReady;
    property Fetched: Int64 read FFetched;
    property FetchBlobs: Boolean read FFetchBlob write FFetchBlob default false;
    //ParamPrefix is ? to use it to open param and match it before convert it to engine paramprefix
    property ParamPrefix: Char read FParamPrefix write SetParamPrefix default '?';
    property ProcessSQL: Boolean read FProcessSQL write FProcessSQL default True; //TODO

    property Session: TmncSQLSession read GetSession write SetSession;
  end;

  //TODO
  TmncSQLText = class(TmnNamedObject)
  public
    Text: string;
  end;

  { TmncSQLResource }

  //TODO
  TmncSQLResource = class(TmnNamedObjectList<TmncSQLText>)
  protected
    procedure LoadLine(Line: string);
  public
    procedure LoadFromStrings(Strings: TStringList);
    procedure LoadFromFiles(Strings: TStringList);
  end;

implementation
(*
{ ESQLError }

constructor ESQLError.Create(ASQLCode: Integer; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
end;

constructor ESQLError.Create(ASQLCode: Integer; AErrorCode: Integer; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
  FErrorCode := AErrorCode;
end;

{ ESQLExceptionError }

constructor ESQLExceptionError.Create(ASQLCode: Integer; AErrorCode: Integer; AExceptionID: Integer; AExceptionName, AExceptionMsg: string; Msg: string);
begin
  inherited Create(ASQLCode, AErrorCode, Msg);
  FExceptionID := AExceptionID;
  FExceptionName := AExceptionName;
  FExceptionMsg := AExceptionMsg;
end;
*)
{ TmncSQLSession }

function TmncSQLSession.GetConnection: TmncSQLConnection;
begin
  Result := inherited Connection as TmncSQLConnection;
end;

procedure TmncSQLSession.SetConnection(AValue: TmncSQLConnection);
begin
  inherited Connection := AValue;
end;

function TmncSQLSession.CreateCommand(ASQL: string): TmncSQLCommand;
begin
  //CheckActive; nop, some commands can't run inside transaction like CREATE DATABASE
  Result := InternalCreateCommand;
  Result.Session := Self;
  if ASQL <> '' then
    Result.SQL.Text := ASQL;
end;

procedure TmncSQLSession.ExecuteScript(AStrings: TStrings; AutoCommit: Boolean);
var
  CMD: TmncSQLCommand;
  c, i: Integer;
  ScriptSeperator: string;
  procedure ExecuteNow;
  begin
    if CMD.SQL.Count > 0 then
    try
      CMD.Execute;
      Inc(c);
      if CMD.Active then
        CMD.Close;
      CMD.SQL.Clear;
      if AutoCommit then
        CommitRetaining;
    except
      on E: Exception do
      begin
        E.Message := E.Message + #13 + ' on script number ' + IntToStr(c);
        raise;
      end
      else
        Raise;
    end;
  end;
begin
  ScriptSeperator := Connection.ScriptSeperator;
  CMD := CreateCommand;
  try
    c := 1;
    CMD.ProcessSQL := False;
    for i := 0 to AStrings.Count -1 do
    begin
      if SameText(LeftStr(AStrings[i], Length(ScriptSeperator)), ScriptSeperator) then
        ExecuteNow
      else
        CMD.SQL.Add(AStrings[i]);
    end;
    if CMD.SQL.Count > 0 then
      ExecuteNow;
  finally
    CMD.Free;
  end;
end;

{ TmncSQLResource }

procedure TmncSQLResource.LoadLine(Line: string);
begin
end;

procedure TmncSQLResource.LoadFromStrings(Strings: TStringList);
begin
end;

procedure TmncSQLResource.LoadFromFiles(Strings: TStringList);
begin
end;

{ TmncSQLConnection }

function TmncSQLConnection.Clone(const vResource: string; AutoConnect: Boolean): TmncSQLConnection;
begin
  Result := TmncSQLConnection(TmncConnectionClass(ClassType).Create);
  try
    Result.Resource := vResource;
    Result.Port := Port;
    Result.Host := Host;
    Result.UserName := UserName;
    Result.Password := Password;
    DoClone(Result);
    if AutoConnect then
      Result.Connect;
  except
    FreeAndNil(Result);
  end;
end;

function TmncSQLConnection.Clone(AutoConnect: Boolean): TmncSQLConnection;
begin
  Result := Clone(Resource, AutoConnect);
end;

procedure TmncSQLConnection.CloneExecute(const vResource, vSQL: string; vArgs: array of const);
begin
  CloneExecute(vResource, Format(vSQL, vArgs));
end;

procedure TmncSQLConnection.CloneExecute(const vResource, vSQL: string);
var
  aConn: TmncSQLConnection;
begin
  aConn := Clone(vResource, False);
  try
    aConn.Connect;
    aConn.Execute(vSQL);
  finally
    aConn.Free;
  end;
end;

procedure TmncSQLConnection.Vacuum;
begin
end;

function TmncSQLConnection.GetParamChar: string;
begin
  Result := '?';
end;

function TmncSQLConnection.GetExtension: string;
begin
  Result := '';
end;

function TmncSQLConnection.ScriptSeperator: string;
begin
  Result := '^';
end;

procedure TmncSQLConnection.DoClone(vConn: TmncSQLConnection);
begin
end;

constructor TmncSQLConnection.Create;
begin
  inherited Create;
end;

function TmncSQLConnection.IsDatabaseExists: Boolean;
begin
  Result := IsDatabaseExists(Resource);
end;

procedure TmncSQLConnection.CreateDatabase(CheckExists: Boolean);
begin
  CreateDatabase(Resource, CheckExists);
end;

procedure TmncSQLConnection.DropDatabase(CheckExists: Boolean);
begin
  DropDatabase(Resource, CheckExists);
end;

procedure TmncSQLConnection.Execute(vCommand: string);
begin
  raise Exception.Create('Execute is not suported in ' + EngineName);
end;

procedure TmncSQLConnection.Execute(vCommand: string; vArgs: array of const);
begin
  Execute(Format(vCommand, vArgs));
end;

{ TmncProcessedSQL }

procedure TmncProcessedSQL.Clear;
begin
  inherited Clear;
  SQL := '';
end;

procedure TmncProcessedSQL.Add(vID: Integer; vName: string; vPosition: Integer);
var
  r: TmncSQLName;
begin
  r := TmncSQLName.Create;
  r.ID := vID;
  r.Name := vName;
  r.Position := vPosition;
  inherited Add(r);
end;

function TmncSQLCommand.GetSQL: TStrings;
begin
  Result := FRequest;//just alias
end;

function TmncSQLCommand.GetSession: TmncSQLSession;
begin
  Result := inherited Session as TmncSQLSession;
end;

procedure TmncSQLCommand.SetParamPrefix(AValue: Char);
begin
  if FParamPrefix =AValue then Exit;
  FParamPrefix :=AValue;
end;

procedure TmncSQLCommand.SetSession(AValue: TmncSQLSession);
begin
  inherited Session := AValue;
end;

function TmncSQLCommand.GetDone: Boolean;
begin
  Result := FDone;
end;

function TmncSQLCommand.GetParseOptions: TmncParseSQLOptions;
begin
  Result := [];
end;

procedure TmncSQLCommand.DoParse;
begin
  ParseSQL(GetParseOptions);
end;

procedure TmncSQLCommand.DoUnparse;
begin
  inherited;
  ProcessedSQL.Clear;
  //maybe clear params, idk
end;

procedure TmncSQLCommand.DoExecute;
begin
  inherited;
end;

procedure TmncSQLCommand.ParseSQL(SQLOptions: TmncParseSQLOptions);
var
  cCurChar, cNextChar, cQuoteChar: Char;
  sSQL, sParamName: string;
  i, LenSQL: Integer;
  iCurState, iCurParamState: Integer;
  iParam: Integer;
const
  DefaultState = 0;
  CommentState = 1;
  QuoteState = 2;
  ParamState = 3;
  ParamDefaultState = 0;
  ParamQuoteState = 1;

  procedure AddToSQL(s: string);
  begin
    ProcessedSQL.SQL := ProcessedSQL.SQL + s;
  end;
var
  aParam: TmncParam;
begin
  if (SQL.Text = '') then
    raise EmncException.Create('Empty SQL to parse!');
  if ProcessSQL then
  begin
    //TODO stored procedures and trigger must not check param in budy procedure
    ProcessedSQL.Clear;
    sParamName := '';
    try
      iParam := 1;
      cQuoteChar := '''';
      sSQL := Trim(SQL.Text) + ' ';//zaher that dummy
      i := 1;
      iCurState := DefaultState;
      iCurParamState := ParamDefaultState;
      { Now, traverse through the SQL string, character by character,
       picking out the parameters and formatting correctly for Firebird }
      LenSQL := Length(sSQL);
      while (i <= LenSQL) do
      begin
        { Get the current token and a look-ahead }
        cCurChar := sSQL[i];
        if i = LenSQL then
          cNextChar := #0
        else
          cNextChar := sSQL[i + 1];
        { Now act based on the current state }
        case iCurState of
          DefaultState:
            begin
              if (cCurChar = '''' )or (cCurChar = '"') then
              begin
                cQuoteChar := cCurChar;
                iCurState := QuoteState;
              end
              else if cCurChar = ParamPrefix then
              begin
                iCurState := ParamState;
                AddToSQL((Session.Connection as TmncSQLConnection).GetParamChar);//here we can replace it with new param char for example % for some sql engine
{                  if psoAddParamsID in SQLOptions then
                  AddToSQL();}
              end
              else if cCurChar = '/' then
              begin
                if (cNextChar = '*') then
                begin
                  AddToSQL(cCurChar);
                  Inc(i);
                  iCurState := CommentState;
                end;
              end;
            end;
          CommentState:
            begin
              if (cNextChar = #0) then
                raise EmncException.Create('Done in comment detected: ' + IntToStr(i))
              else if (cCurChar = '*') then
              begin
                if (cNextChar = '/') then
                  iCurState := DefaultState;
              end;
            end;
          QuoteState:
            begin
              if cNextChar = #0 then
                raise EmncException.Create('Done in string detected: ' + IntToStr(i))
              else if (cCurChar = cQuoteChar) then
              begin
                if (cNextChar = cQuoteChar) then
                begin
                  AddToSQL(cCurChar);
                  Inc(i);
                end
                else
                  iCurState := DefaultState;
              end;
            end;
          ParamState:
            begin
            { collect the name of the parameter }
              if iCurParamState = ParamDefaultState then
              begin
                if cCurChar = '"' then
                  iCurParamState := ParamQuoteState
                else if CharInSet(cCurChar, ['A'..'Z', 'a'..'z', '0'..'9', '_', ' ']) then //Quoted can include spaces
                begin
                  sParamName := sParamName + cCurChar;
                  if psoAddParamsNames in SQLOptions then
                    AddToSQL(cCurChar);
                end
                else if psoGenerateParams in SQLOptions then//if passed ? (ParamChar) without name of params
                begin
                  sParamName := '_Param_' + IntToStr(iParam);
                  Inc(iParam);
                  iCurState := DefaultState;
                  ProcessedSQL.Add(iParam, sParamName);
                  sParamName := '';
                end
                else
                  raise EmncException.Create('Parameter name expected');
              end
              else
              begin
              { determine if Quoted parameter name is finished }
                if cCurChar = '"' then
                begin
                  Inc(i);
                  ProcessedSQL.Add(iParam, sParamName);
                  SParamName := '';
                  iCurParamState := ParamDefaultState;
                  iCurState := DefaultState;
                end
                else
                  sParamName := sParamName + cCurChar
              end;
            { determine if the unquoted parameter name is finished }
              if (iCurParamState <> ParamQuoteState) and
                (iCurState <> DefaultState) then
              begin
                if not CharInSet(cNextChar, ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
                begin
                  Inc(i);
                  iCurState := DefaultState;
                  if cmdReplaceParams in Options then
                  begin
                    //AddToSQL(IntToStr(iParam));
                  end
                  else if psoAddParamsID in SQLOptions then
                  begin
                    AddToSQL(IntToStr(iParam));
                    Inc(iParam);
                  end;
                  ProcessedSQL.Add(iParam, sParamName, ProcessedSQL.SQL.Length);
                  sParamName := '';
                end;
              end;
            end;
        end;
        if iCurState <> ParamState then
          AddToSQL(sSQL[i]);
        Inc(i);
      end;
      Params.Clear;
      Binds.Clear;
      for i := 0 to ProcessedSQL.Count - 1 do
      begin
        aParam := Params.Found(ProcessedSQL[i].Name);//it will auto create it if not founded
        Binds.Add(aParam);
      end;
    finally
    end;
  end
  else
  begin
    ProcessedSQL.SQL := SQL.Text;
  end;
end;

procedure TmncSQLCommand.Fetch;
begin
  inherited;
  Inc(FFetched);
end;

procedure TmncSQLCommand.Clean;
begin
  inherited;
  FReady := True;
  FDone := False;
  FFetched := 0;
end;

procedure TmncSQLCommand.HitDone;
begin
  FDone := True;
end;

procedure TmncSQLCommand.HitUnready;
begin
  FReady := False;
end;

function TmncSQLCommand.GetProcessedSQL: string;
var
  i: Integer;
begin
  if cmdReplaceParams in Options then
  begin
    Result := '';
    for i := 0 to ProcessedSQL.Count -1 do
    begin
      //ProcessedSQL.SQL TODO replace Params ? with real Text
    end;
  end
  else
    Result := ProcessedSQL.SQL;
end;

constructor TmncSQLCommand.Create;
begin
  inherited Create;
  FProcessedSQL := TmncProcessedSQL.Create(True);
  FProcessSQL := True;
  FFetchBlob := False;
  FParamPrefix := '?';
end;

destructor TmncSQLCommand.Destroy;
begin
  inherited;
  FreeAndNil(FProcessedSQL);
end;

function TmncSQLCommand.GetLastRowID: Int64;
begin
  Result := 0;
end;

function TmncSQLCommand.GetRowsChanged: Integer;
begin
  Result := 0;
end;

end.

