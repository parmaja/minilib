unit ScatCommands;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of Scat://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, syncobjs,
  mnFields, mnUtils, mnSockets, mnServers, mnCommandServers, mnStreams, mnXML;

type
  TscatServer = class;

{**
  Base classes
*}

  { TscatCommand }

  TscatCommand = class(TmnCommand)
  private
    //FParams: TStringList;
    FParams: TmnFields;
    function GetServer: TscatServer;
  protected
    procedure DoExecute; virtual;
    procedure Execute; override;
  public
    property Server:TscatServer read GetServer;
    constructor Create(Connection: TmnCommandConnection; const Params: string); override;
    destructor Destroy; override;
    property Params: TmnFields read FParams;
  end;

{**
  Files Commands
*}

  TscatGetCommand = class(TscatCommand)
  protected
  public
    constructor Create(Connection: TmnCommandConnection; const Params: string); override;
    procedure DoExecute; override;
  end;

  TscatPutCommand = class(TscatCommand)
  protected
  public
    constructor Create(Connection: TmnCommandConnection; const Params: string); override;
    procedure DoExecute; override;
  end;

  TscatServerInfoCommand = class(TscatCommand)
  protected
    procedure DoExecute; override;
  public
  end;

  TscatDirCommand = class(TscatCommand)
  protected
    procedure DoExecute; override;
  public
  end;

  TscatDeleteFileCommand = class(TscatCommand)
  protected
    procedure DoExecute; override;
  public
  end;

{**
  Server
*}

  { TscatServer }

  TscatServer = class(TmnCommandServer)
  private
    FDocumentRoot: string;
    FDefaultDocument: TStringList;
  protected
    procedure SetDefaultDoc(const Value: TStringList);
    procedure DoBeforeOpen; override;
    procedure DoAfterClose; override;
    function CreateListener: TmnListener; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DocumentRoot: string read FDocumentRoot write FDocumentRoot;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDoc;
  published
  end;

var
  scatLock: TCriticalSection = nil;

implementation

uses
  mnXMLUtils, mnXMLRttiProfile;

var
  FscatServer: TscatServer = nil;

function scatServer: TscatServer;
begin
  if FscatServer = nil then
    FscatServer := TscatServer.Create(nil);
  Result := FscatServer;
end;

{ TscatServer }

constructor TscatServer.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultDocument := TStringList.Create;
  Port := '80';
  FDefaultDocument.Add('index.html');
  FDefaultDocument.Add('index.htm');
  FDefaultDocument.Add('default.html');
  FDefaultDocument.Add('default.htm');
  FDocumentRoot := '';

  RegisterCommand('Info', TscatServerInfoCommand);
  RegisterCommand('GET', TscatGetCommand);
  RegisterCommand('PUT', TscatPutCommand);
  RegisterCommand('DIR', TscatDirCommand);
  RegisterCommand('DEL', TscatDeleteFileCommand);
end;

destructor TscatServer.Destroy;
begin
  FreeAndNil(FDefaultDocument);
  inherited;
end;

procedure TscatServer.SetDefaultDoc(const Value: TStringList);
begin
  FDefaultDocument.Assign(Value);
end;

procedure TscatServer.DoBeforeOpen;
begin
  inherited;
end;

procedure TscatServer.DoAfterClose;
begin
  inherited;
end;

function TscatServer.CreateListener: TmnListener;
begin
  Result := inherited CreateListener;
{  Result := TmnScatListener.Create;
  TmnScatListener(Result).DocumentRoot := ExcludeTrailingPathDelimiter(FDocumentRoot);
  TmnScatListener(Result).DefaultDocument.Assign(FDefaultDocument);}
end;

{ TscatGetCommand }

constructor TscatGetCommand.Create(Connection: TmnCommandConnection; const Params: string);
begin
  inherited;
end;

procedure TscatGetCommand.DoExecute;
var
  aFile: TFileStream;
  aFileName: string;
begin
  Connection.Stream.WriteCommand('OK');
  aFileName := Params.Values['FileName'];
  {aFile := TFileStream.Create(DocumentRoot + aFileName, fmOpenRead or fmShareDenyWrite);
  try
    Connection.Stream.WriteStream(aFile);
  finally
    aFile.Free;
  end;}
end;

procedure FieldsCallBack(S: string; vObject: TObject);
var
  Name, Value: string;
  p: Integer;
begin
  p := pos('=', s);
  Name := Copy(s, 1, p - 1);
  Value := DequoteStr(Copy(s, p + 1, MaxInt));
  (vObject as TmnFields).Add(Name, Value);
end;

{ TscatCommand }

constructor TscatCommand.Create(Connection: TmnCommandConnection; const Params: string);
begin
  inherited;
  Connection.Stream.Timeout := -1;
  FParams := TmnFields.Create;
  StrToStringsCallback(FParams, @FieldsCallBack, Params, [#0, #13, #10], [' ']);
end;

destructor TscatCommand.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TscatCommand.GetServer: TscatServer;
begin
  Result := (inherited Server as TscatServer);
end;

procedure TscatCommand.DoExecute;
begin
end;

procedure TscatCommand.Execute;
begin
  inherited;
  {$ifdef DEBUG_MODE}
//    Server.Listener.Log(Connection, GetCommandName + ': Started on port ' + Server.Port);
  try
  {$endif}
    DoExecute;
  {$ifdef DEBUG_MODE}
  except
    on E:Exception do
    begin
      Server.Listener.Log(Connection, GetCommandName + ': Error ' + E.Message);
      raise;
    end;
  end;
//    Server.Listener.Log(Connection, GetCommandName + ': Finished');
  {$endif}
end;

{ TscatServerInfoCommand }

procedure TscatServerInfoCommand.DoExecute;
begin
  Connection.Stream.WriteCommand('OK');
  Connection.Stream.WriteLn('Server is running on port: ' + Server.Port);
  //Connection.Stream.WriteLn('the server is: "' + Application.ExeName + '"');
  Connection.Stream.WriteLn('');
end;

{ TscatPutCommand }

constructor TscatPutCommand.Create(Connection: TmnCommandConnection;
  const Params: string);
begin
  inherited;
end;

procedure TscatPutCommand.DoExecute;
var
  aFile: TFileStream;
  aFileName: string;
begin
  Connection.Stream.WriteCommand('OK');
  aFileName := Params.Values['FileName'];
  {aFile := TFileStream.Create(DocumentRoot + aFileName, fmCreate);
  try
    Connection.Stream.ReadStream(aFile);
  finally
    aFile.Free;
  end;}
end;

{ TscatDirCommand }

procedure TscatDirCommand.DoExecute;
var
//  i: Integer;
//  aStrings: TStringList;
  aPath, aFilter: string;
begin
  Connection.Stream.WriteCommand('OK');
  aFilter := Params.Values['Filter'];
  //aPath := IncludeTrailingPathDelimiter(DocumentRoot);
  if aFilter = '' then
    aFilter := '*.*';
{   aStrings := TStringList.Create;
  try
    EnumFileList(aPath + aFilter, aStrings);
    for i := 0 to aStrings.Count - 1 do
    begin
      Connection.Stream.WriteLn(IntToStr(i) + ': ' + aStrings[i]);
    end;
  finally
    aStrings.Free;
  end;}
end;

{ TscatDeleteFileCommand }

procedure TscatDeleteFileCommand.DoExecute;
var
  aFileName: string;
begin
  {aFileName := IncludeTrailingPathDelimiter(DocumentRoot) + Params.Values['FileName'];
  if FileExists(aFileName) then
    DeleteFile(aFileName);}
  Connection.Stream.WriteCommand('OK');
end;

initialization
  scatLock := TCriticalSection.Create;
finalization
  FreeAndNil(FscatServer);
  scatLock.Free;
end.
