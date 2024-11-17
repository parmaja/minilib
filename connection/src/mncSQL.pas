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
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
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
  mnClasses, mnUtils,
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

  TmncParseSQLOptions = set of (
    psoNoParams,
    psoGenerateParams,
    psoAddParamsID,
    psoAddParamsNames
  );

  TmncSQLConnection = class;
  TmncSQLTransaction = class;
  TmncSQLCommand = class;

  {$ifndef FPC}
  TReconnectProcedure = procedure(vConnection: TmncSQLConnection);

  TDBDispatcherProc = reference to procedure(vMessage: string; vPID: IntPtr; var vHandeled: Boolean);

  TDBChannel = class(TmncObject)
  private
    FName: string;
    FCount: Integer;
  public
    property Name: string read FName;
    property Count: Integer read FCount;
  end;

  TDBChannels = class(TmnObjectList<TDBChannel>)
  public
    function Find(const vName: string): TDBChannel;
    function Register(const vName: string): Boolean;
    function Unregister(const vName: string): Boolean;
  end;

  TDBDispatcher = class(TmncObject)
  private
    FName: string;
    FChannel: string;
    FEvent: TDBDispatcherProc;
  public
    property Name: string read FName;
    property Channel: string read FChannel;
    property Event: TDBDispatcherProc read FEvent;
    //property Channel: string read FChannel;
  end;

  TDBDispatchers = class(TmnObjectList<TDBDispatcher>)
  private
    FChannels: TDBChannels;
    FDB: TmncSQLConnection;
  public
    constructor Create(vDB: TmncSQLConnection);
    destructor Destroy; override;

    function RegisterDispatcher(const vName, vChannel: string; vEvent: TDBDispatcherProc): TDBDispatcher;
    procedure UnregisterDispatcher(const vName: string);

    procedure Process;
    property DB: TmncSQLConnection read FDB;
    property Channels: TDBChannels read FChannels;
    function Find(const vName: string): TDBDispatcher;
  end;
  {$endif}

  { TmncSQLConnection }

  TmncSQLConnection = class abstract(TmncConnection)
  private
    {$ifndef FPC}
    FIsReconnecting: Boolean;
    FDispatchers: TDBDispatchers;
    {$endif}
    function GetNextID(const vName: string; vStep: Integer): Integer;
  protected
    procedure DoClone(vConn: TmncSQLConnection); virtual;
    function DoGetNextIDSQL(const vName: string; vStep: Integer): string; virtual; deprecated; //TODO move it to dervied class should not be here, wrong place
    function GetSequenceSQL: string; virtual;
    procedure DoExecute(const vSQL: string); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function CreateTransaction: TmncSQLTransaction; virtual; abstract;

    property NextID[const vName: string; vStep: Integer]: Integer read GetNextID; //deprecated;

    function IsDatabaseExists(const vName: string): Boolean; overload; virtual; abstract;
    function IsDatabaseExists: Boolean; overload;

    procedure CreateDatabase(const vName: string; CheckExists: Boolean = False); overload; virtual; abstract;
    procedure CreateDatabase(CheckExists: Boolean = False); overload;

    procedure DropDatabase(const vName: string; CheckExists: Boolean = False); overload; virtual; abstract;
    procedure DropDatabase(CheckExists: Boolean = False); overload;

    procedure Execute(const vSQL: string); overload;
    procedure Execute(const vSQL: string; vArgs: array of const); overload;

    function Clone(const vResource: string; AutoConnect: Boolean = True): TmncSQLConnection; overload;
    function Clone(AutoConnect: Boolean = True): TmncSQLConnection; overload;

    procedure CloneExecute(const vResource, vSQL: string; vArgs: array of const); overload;
    procedure CloneExecute(const vResource, vSQL: string); overload;

    function UniqueDBName(const vBase: string): string; virtual; //TODO move to utils
    function EnumDatabases: TStrings; virtual;


    procedure Vacuum; virtual;
    {
      GetParamChar: Called to take the real param char depend on the sql engine to replace it with this new one.
                    by default it is ?
    }
    function GetParamChar: string; virtual;
    function GetExtension: string; virtual;
    //ScriptSeperator
    function ScriptSeperator: string; virtual;

    {$ifndef FPC}
    function TestConnected: Boolean; virtual;
    function IsReconnecting: Boolean; virtual;
    function ReceiveNotifications: TStrings; virtual;
    procedure Reconnect; virtual;
    procedure RecoverConnection; virtual;
    procedure StartListen(const vChannel: string); virtual; deprecated;
    procedure StopListen(const vChannel: string); virtual; deprecated;
    property Dispatchers: TDBDispatchers read FDispatchers;
    {$endif}
  end;

  { TmncSQLTransaction }

  TmncSQLTransaction = class abstract(TmncTransaction)
  private
    function GetConnection: TmncSQLConnection;
    procedure SetConnection(AValue: TmncSQLConnection);
  protected
    function DoCreateCommand: TmncSQLCommand; virtual; abstract;
  public
    function CreateCommand(ASQL: string = ''): TmncSQLCommand;
    procedure ExecuteScript(AStrings: TStrings; AutoCommit: Boolean = False);
    procedure Execute(const vSQL: string); overload; virtual;
    procedure Execute(const vSQL: string; vArgs: array of const); overload;
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

  TmncProcessedSQL = class abstract(TmnObjectList<TmncSQLName>)
  private
  protected
    function GetSQL: string; virtual; abstract;
    procedure ParseSQL(CMD: TmncSQLCommand; SQLOptions: TmncParseSQLOptions); virtual; abstract;
  public
    procedure Clear; {$ifdef FPC} override; {$else} virtual; {$endif}    //TODO
    procedure Add(vID: Integer; vName:string; vPosition: Integer = -1);
    property SQL: string read GetSQL;
  end;

  TmncProcessedSQLText = class(TmncProcessedSQL)
  private
  protected
    procedure ParseSQL(CMD: TmncSQLCommand; SQLOptions: TmncParseSQLOptions); override;
  public
    Text: string;
    procedure Clear; override;
    function GetSQL: string; override;
  end;

  { TmncSQLCommand }

  TmncSQLCommand = class abstract(TmncCommand)
  private
    FFetchBlob: Boolean;
    FParamPrefix: Char;
    FProcessSQL: Boolean;
    FProcessedSQL: TmncProcessedSQL;
    function GetTransaction: TmncSQLTransaction;
    procedure SetTransaction(AValue: TmncSQLTransaction);
    function GetSQL: TStrings;
    procedure SetParamPrefix(AValue: Char);
  protected
    function GetParseOptions: TmncParseSQLOptions; virtual;
    procedure DoParse; override;
    procedure ParseSQL(SQLOptions: TmncParseSQLOptions);

    function GetProcessedSQL: string;
    procedure DoRequestChanged(Sender: TObject); override;

    property ProcessedSQL: TmncProcessedSQL read FProcessedSQL;
    function CreateProcessedSQL: TmncProcessedSQL; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Prepare(vSQL: string): TmncCommand; overload;
    function GetLastRowID: Int64; virtual;
    function GetRowsChanged: Integer; virtual;
    property SQL: TStrings read GetSQL;//Alias of Request, autocomplete may add it in private becareful
    property FetchBlobs: Boolean read FFetchBlob write FFetchBlob default false;
    //ParamPrefix is ? to use it to open param and match it before convert it to engine paramprefix
    property ParamPrefix: Char read FParamPrefix write SetParamPrefix default '?';
    property ProcessSQL: Boolean read FProcessSQL write FProcessSQL default True; //TODO

    property Transaction: TmncSQLTransaction read GetTransaction write SetTransaction;
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

{$ifndef FPC}
var
  ReconnectProc: TReconnectProcedure = nil;
{$endif}

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

{ TmncSQLTransaction }

function TmncSQLConnection.DoGetNextIDSQL(const vName: string; vStep: Integer): string;
begin
  Result := '';
end;

function TmncSQLConnection.GetSequenceSQL: string;
begin
  Result := '';
end;

function TmncSQLConnection.GetNextID(const vName: string; vStep: Integer): Integer;
var
  aCmd: TmncSQLCommand;
  aSQL: string;
  aTR: TmncSQLTransaction;
begin
  aSQL := DoGetNextIDSQL(vName, vStep);
  if aSQL<>'' then
  begin
    aTR := CreateTransaction;
    try
      aCmd := aTR.CreateCommand;
      try
        aCmd.SQL.Text := aSQL;
        if aCmd.Execute then
          Result := aCmd.Fields.Items[0].AsInteger
        else
          Result := 0;
      finally
        aCmd.Free;
      end;
    finally
      aTR.Free;
    end;
  end
  else
    Result := 0;
end;

function TmncSQLTransaction.GetConnection: TmncSQLConnection;
begin
  Result := inherited Connection as TmncSQLConnection;
end;

procedure TmncSQLTransaction.SetConnection(AValue: TmncSQLConnection);
begin
  inherited Connection := AValue;
end;

function TmncSQLTransaction.CreateCommand(ASQL: string): TmncSQLCommand;
begin
  //CheckActive; nop, some commands can't run inside transaction like CREATE DATABASE
  Result := DoCreateCommand;
  Result.Transaction := Self;
  if ASQL <> '' then
    Result.SQL.Text := ASQL;
end;

procedure TmncSQLTransaction.Execute(const vSQL: string);
var
  aCmd: TmncSQLCommand;
begin
  aCmd := CreateCommand;
  try
    aCmd.SQL.Text := vSQL;
    aCmd.Execute;
  finally
    aCmd.Free;
  end;
end;

procedure TmncSQLTransaction.Execute(const vSQL: string; vArgs: array of const);
begin
  Execute(Format(vSQL, vArgs));
end;

procedure TmncSQLTransaction.ExecuteScript(AStrings: TStrings; AutoCommit: Boolean);
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

function TmncSQLConnection.UniqueDBName(const vBase: string): string;
begin
  Result := '';
end;

procedure TmncSQLConnection.DoClone(vConn: TmncSQLConnection);
begin
end;

procedure TmncSQLConnection.DoExecute(const vSQL: string);
begin
  raise Exception.Create('Execute is not suported in ' + EngineName);
end;

constructor TmncSQLConnection.Create;
begin
  inherited Create;
  {$ifndef FPC}
  FDispatchers := TDBDispatchers.Create(Self);
  {$endif}
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

procedure TmncSQLConnection.Execute(const vSQL: string);
begin
  if Connected then
    DoExecute(vSQL);
end;

function TmncSQLConnection.EnumDatabases: TStrings;
begin
  Result := nil;
end;

procedure TmncSQLConnection.Execute(const vSQL: string; vArgs: array of const);
begin
  Execute(Format(vSQL, vArgs));
end;

{ TmncProcessedSQL }

procedure TmncProcessedSQL.Clear;
begin
  inherited Clear;
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

function TmncSQLCommand.GetTransaction: TmncSQLTransaction;
begin
  Result := inherited Transaction as TmncSQLTransaction;
end;

procedure TmncSQLCommand.SetParamPrefix(AValue: Char);
begin
  if FParamPrefix =AValue then Exit;
  FParamPrefix :=AValue;
end;

procedure TmncSQLCommand.SetTransaction(AValue: TmncSQLTransaction);
begin
  inherited Transaction := AValue;
end;

function TmncSQLCommand.GetParseOptions: TmncParseSQLOptions;
begin
  Result := [];
end;

procedure TmncSQLCommand.DoParse;
begin
  if not (psoNoParams in GetParseOptions) then
    ParseSQL(GetParseOptions);
end;

procedure TmncSQLCommand.DoRequestChanged(Sender: TObject);
begin
  inherited;
  if ProcessedSQL <> nil then { TODO : need discuss }
    ProcessedSQL.Clear;

end;

procedure TmncSQLCommand.ParseSQL(SQLOptions: TmncParseSQLOptions);
begin
  ProcessedSQL.ParseSQL(Self, SQLOptions);
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
    Result := ProcessedSQL.GetSQL;
end;

constructor TmncSQLCommand.Create;
begin
  inherited Create;
  FProcessedSQL := CreateProcessedSQL;
  FProcessSQL := True;
  FFetchBlob := False;
  FParamPrefix := '?';
end;

function TmncSQLCommand.CreateProcessedSQL: TmncProcessedSQL;
begin
  Result := TmncProcessedSQLText.Create(True);
end;

destructor TmncSQLCommand.Destroy;
begin
  FreeAndNil(FProcessedSQL); //if you moved it after inherited say WHY
  inherited;
end;

function TmncSQLCommand.Prepare(vSQL: string): TmncCommand;
begin
  SQL.Text:= vSQL;
  Prepare();
  Result := Self;
end;

function TmncSQLCommand.GetLastRowID: Int64;
begin
  Result := 0;
end;

function TmncSQLCommand.GetRowsChanged: Integer;
begin
  Result := 0;
end;

{ TmncProcessedSQLText }

procedure TmncProcessedSQLText.Clear;
begin
  inherited;
  Text := '';
end;

function TmncProcessedSQLText.GetSQL: string;
begin
  Result := Text;
end;

procedure TmncProcessedSQLText.ParseSQL(CMD: TmncSQLCommand; SQLOptions: TmncParseSQLOptions);
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
    Text := Text + s;
  end;
var
  aParam: TmncParam;
begin
  begin
    if (CMD.SQL.Text = '') then
      raise EmncException.Create('Empty SQL to parse!');
    if CMD.ProcessSQL then
    begin
      //TODO stored procedures and trigger must not check param in budy procedure
      Clear;
      sParamName := '';
      try
        iParam := 1;
        cQuoteChar := '''';
        sSQL := Trim(CMD.SQL.Text) + ' ';//zaher that dummy
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
                else if cCurChar = CMD.ParamPrefix then
                begin
                  iCurState := ParamState;
                  AddToSQL((CMD.Transaction.Connection as TmncSQLConnection).GetParamChar);//here we can replace it with new param char for example % for some sql engine
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
                    Add(iParam, sParamName);
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
                    Add(iParam, sParamName);
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
                    if cmdReplaceParams in CMD.Options then
                    begin
                      //AddToSQL(IntToStr(iParam));
                    end
                    else if psoAddParamsID in SQLOptions then
                    begin
                      AddToSQL(IntToStr(iParam));
                      Inc(iParam);
                    end;
                    Add(iParam, sParamName, Text.Length);
                    sParamName := '';
                  end;
                end;
              end;
          end;
          if iCurState <> ParamState then
            AddToSQL(sSQL[i]);
          Inc(i);
        end;
        CMD.Params.Clear;
        CMD.Binds.Clear;
        for i := 0 to Count - 1 do
        begin
          aParam := CMD.Params.Require(Self[i].Name);//it will auto create it if not founded
          CMD.Binds.Add(aParam);
        end;
      finally
      end;
    end
    else
    begin
      Text := CMD.SQL.Text;
    end;
  end;
end;

destructor TmncSQLConnection.Destroy;
begin
  {$ifndef FPC}
  FreeAndNil(FDispatchers);
  {$endif}
  inherited;
end;

{$ifndef FPC}
function TmncSQLConnection.IsReconnecting: Boolean;
begin
  Result := FIsReconnecting;
end;

function TmncSQLConnection.ReceiveNotifications: TStrings;
begin
  Result := nil;
end;

procedure TmncSQLConnection.Reconnect;
begin
  if not TestConnected then
  begin
    FIsReconnecting := True;
    try
      if Assigned(ReconnectProc) then
        ReconnectProc(Self);
    finally
      if TestConnected then
        FIsReconnecting := False;
    end;
  end;
end;

procedure TmncSQLConnection.RecoverConnection;
begin
end;

procedure TmncSQLConnection.StartListen(const vChannel: string);
begin
end;

procedure TmncSQLConnection.StopListen(const vChannel: string);
begin
end;

function TmncSQLConnection.TestConnected: Boolean;
begin
  Result := True;
end;

{ TDBDispatchers }

constructor TDBDispatchers.Create(vDB: TmncSQLConnection);
begin
  inherited Create(True);
  FDB := vDB;

  FChannels := TDBChannels.Create;
end;

destructor TDBDispatchers.Destroy;
begin
  FreeAndNil(FChannels);
  inherited;
end;

function TDBDispatchers.Find(const vName: string): TDBDispatcher;
begin
  Result := nil;
  for var itm in Self do
    if itm.Name=vName then
    begin
      Result := itm;
      Break;
    end;
end;

procedure TDBDispatchers.Process;
var
  st: TStrings;
  i, j: Integer;
  aName: string;
  aValue: string;
  aHandeled: Boolean;
begin
  if Count<>0 then
  begin
    st := DB.ReceiveNotifications;
    try
      if st.Count<>0 then
      begin
        //for var s: stringex in st do
        while st.Count<>0 do
        begin
          i := st.Count-1;
          aName := st.Names[i];
          aValue := st.ValueFromIndex[i];
          aHandeled := False;

          for var itm in Self do
            if SameText(aName, itm.Channel) then
              itm.FEvent(aValue, IntPtr(st.Objects[i]), aHandeled);
              { TODO : improve: aHandeled := aHandeled or }

          if aHandeled then
          begin
            j := st.Count-1;
            while j>=0 do
            begin
              if (SubStr(st[i], '=') = aName) then
                Delete(i);
            end;
          end
          else
            st.Delete(i);
        end;
      end;
    finally
      st.Free;
    end;
  end;
end;

function TDBDispatchers.RegisterDispatcher(const vName, vChannel: string; vEvent: TDBDispatcherProc): TDBDispatcher;
begin
  if Find(vName)<>nil then raise Exception.Create('duplicate name');

  Result := TDBDispatcher.Create;
  Result.FName := vName;
  Result.FChannel := vChannel;
  Result.FEvent := vEvent;
  Add(Result);

  if Channels.Register(vChannel) then
    DB.StartListen(vChannel);
end;

procedure TDBDispatchers.UnregisterDispatcher(const vName: string);
begin
  var itm := Find(vName);
  if (itm<>nil) then
  begin
    if Channels.Unregister(itm.Channel) then
      DB.StopListen(itm.Channel);
    Remove(itm);
  end;



  {RemoveItems(procedure(vItem: TDBDispatcher; var vCheck: Boolean)
  begin
    vCheck := vItem.Name = vName;
  end);}
end;

{ TDBChannels }

function TDBChannels.Find(const vName: string): TDBChannel;
begin
  Result := nil;
  for var itm in Self do
    if itm.Name=vName then
    begin
      Result := itm;
      break;
    end;
end;

function TDBChannels.Register(const vName: string): Boolean;
begin
  var itm := Find(vName);
  Result := itm = nil;
  if Result then
  begin
    itm := TDBChannel.Create;
    itm.FName := vName;
    itm.FCount := 0;
    Add(itm);
  end;
  itm.FCount := itm.FCount + 1;
end;

function TDBChannels.Unregister(const vName: string): Boolean;
begin
  Result := False;

  var itm := Find(vName);
  if itm<>nil then
  begin
    itm.FCount := itm.FCount-1;
    if itm.Count=0 then
    begin
      Result := True;
      Remove(itm);
    end;
  end;
  {$ifopt D+}
  //raise exception
  {$endif}
end;
{$endif}
end.

