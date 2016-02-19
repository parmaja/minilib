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
  mnFields, mnUtils, mnSockets, mnServers, mnCommandServers, mnStreams, mnSocketStreams;

{**
-------------------
GET http://localhost/index.html HTTP/1.1
Host: localhost
Connection: Close

Post Body
-------------------

-------------------
method URI[path?params] http_version
headers[0]->Host: localhost
headers[1]->Connection: Close
headers[2]
-------------------

*}

{**
  Ref: https://www.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html
*}

type
  TscatServer = class;

{**
  Base classes
*}

  { TscatCommand }

  TscatCommand = class(TmnCommand)
  private
    FMethod: string;
    FURI: string;
    FParams: TmnFields;
    FVersion: string;
    FRequestHeader: TmnFields;
    FRespondHeader: TmnFields;
    FHeaderSent: Boolean;
    function GetServer: TscatServer;
    function GetStream: TmnSocketStream;
    procedure Enter;
    procedure Leave;
  protected
    procedure DoExecute; virtual;
    procedure Execute; override;
  public
    property Server: TscatServer read GetServer;
    property Stream: TmnSocketStream read GetStream;
    constructor Create(Connection: TmnCommandConnection); override;
    destructor Destroy; override;
    property Method: string read FMethod;
    property URI: string read FURI;
    property Params: TmnFields read FParams;
    property Version: string read FVersion;
    property RequestHeader: TmnFields read FRequestHeader;
    procedure StartHeader(AValue: string); virtual;
    procedure SendHeader(AName, AValue: string); virtual;
    procedure EndHeader;
  end;

  { TscatWebCommand }

  TscatWebCommand = class(TscatCommand)
  private
    procedure ParseURI;
  protected
    function GetDefaultDocument(Root: string): string;
    procedure Answer404;
  public
    Root: string; //Document root folder
    Path: string;
    Host: string;
    procedure Respond; virtual;
    procedure DoExecute; override;
  end;

{**
  Files Commands
*}

  { TscatGetCommand }

  TscatGetCommand = class(TscatWebCommand)
  protected
  public
    procedure Respond; override;
  end;

  TscatPutCommand = class(TscatCommand)
  protected
  public
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

  { TScatListener }

  TScatListener = class(TmnCommandListener)
  private
  protected
    function CreateStream(Socket: TmnCustomSocket): TmnSocketStream; override;
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
    procedure SetDefaultDocument(const Value: TStringList);
    procedure DoBeforeOpen; override;
    procedure DoAfterClose; override;
    function CreateListener: TmnListener; override;
  public
    constructor Create;
    destructor Destroy; override;
    property DocumentRoot: string read FDocumentRoot write FDocumentRoot;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDocument;
  published
  end;

var
  scatLock: TCriticalSection = nil;

implementation

procedure ParamsCallBack(vObject: TObject; S: string);
var
  Name, Value: string;
  p: Integer;
begin
  p := pos('=', s);
  Name := Copy(s, 1, p - 1);
  Value := DequoteStr(Copy(s, p + 1, MaxInt));
  (vObject as TmnFields).Add(Name, Value);
end;

function TScatListener.CreateStream(Socket: TmnCustomSocket): TmnSocketStream;
begin
  Result := inherited CreateStream(Socket);
  Result.EOFOnError := True;
  Result.EndOfLine := sWinEndOfLine;
end;

{ TscatWebCommand }

procedure TscatWebCommand.Answer404;
var
  Body: string;
begin
  StartHeader('HTTP/1.1 200 OK');
  SendHeader('Content-Type', 'text/html');
  EndHeader;
  Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' + //FDocument +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>';
  Connection.Stream.WriteString(Body);
end;

procedure TscatWebCommand.ParseURI;
var
  I, J: Integer;
  aParams: string;
begin
  I := 1;
  while (I <= Length(URI)) and (URI[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(URI)) and (URI[I] <> ' ') do
    Inc(I);

  Path := Copy(URI, J, I - J);

  Inc(I);
  while (I <= Length(URI)) and (URI[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(URI)) and (URI[I] <> ' ') do
    Inc(I);
  FVersion := Trim(UpperCase(Copy(URI, J, I - J)));
  if FVersion = '' then
    FVersion := 'HTTP/1.0';

  if Path[1] = '/' then //Not sure
    Delete(Path, 1, 1);

    { Find parameters }
  J := Pos('?', Path);
  if J <= 0 then
    aParams := ''
  else
  begin
    aParams := Copy(Path, J + 1, Length(Path));
    Path := Copy(Path, 1, J - 1);
    StrToStringsCallback(aParams, Params, @ParamsCallBack);
  end;
end;

function TscatWebCommand.GetDefaultDocument(Root: string): string;
var
  i: Integer;
  aFile: string;
begin
  //TODO baaad you need to luck before access
  for i := 0 to Server.DefaultDocument.Count - 1 do
  begin
    aFile := Root + Server.DefaultDocument[i];
    if FileExists(aFile) then
    begin
      Result := aFile;
    end;
  end;
end;


procedure TscatWebCommand.Respond;
begin
  Stream.WriteString('HTTP/1.0 404 Not Found');
end;

procedure TscatWebCommand.DoExecute;
var
  l: string;
begin
  inherited;
  while Connected do
  begin
    l := Stream.ReadLine;
    RequestHeader.AddItem(l, ':');
    if l = '' then
      break;
  end;

  Root := Server.DocumentRoot;
  Host := RequestHeader['Host'];
  try
    ParseURI;
    Respond;
  finally
  end;
  if Connected then
    Shutdown;
end;

{ TscatServer }

constructor TscatServer.Create;
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

procedure TscatServer.SetDefaultDocument(const Value: TStringList);
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
  Result := TScatListener.Create;
end;

{ TscatGetCommand }

function DocumentToContentType(FileName: string): string;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if Length(Ext) > 1 then
    Ext := Copy(Ext, 2, Length(Ext));
  if (Ext = 'htm') or (Ext = 'html') or (Ext = 'shtml') or (Ext = 'dhtml') then
    Result := 'text/html'
  else if Ext = 'css' then
    Result := 'text/css'
  else if Ext = 'gif' then
    Result := 'image/gif'
  else if Ext = 'bmp' then
    Result := 'image/bmp'
  else if (Ext = 'jpg') or (Ext = 'jpeg') then
    Result := 'image/jpeg'
  else if Ext = 'txt' then
    Result := 'text/plain'
  else
    Result := 'application/binary';
end;

procedure TscatGetCommand.Respond;
var
  DocSize: Int64;
  aDocStream: TFileStream;
  aDocument: string;
begin
  aDocument := IncludeTrailingPathDelimiter(Root) + Path;
  aDocument := StringReplace(aDocument, '/', PathDelim, [rfReplaceAll]);//correct it for linux

 if aDocument[Length(aDocument)] = PathDelim then //get the default file if it not defined
    aDocument := GetDefaultDocument(aDocument);

  if FileExists(aDocument) then
  begin
    if Connected then
    begin
      aDocStream := TFileStream.Create(aDocument, fmOpenRead or fmShareDenyWrite);
      try
        DocSize := aDocStream.Size;
        if Connected then
        begin
          StartHeader('HTTP/1.1 200 OK');
          SendHeader('Content-Type', DocumentToContentType(aDocument));
          SendHeader('Content-Length', IntToStr(DocSize));
          EndHeader;
        end;

        if Connected then
          Stream.WriteStream(aDocStream);
      finally
        aDocStream.Free;
      end;
    end;
  end
  else
    Answer404;
end;

{ TscatCommand }

constructor TscatCommand.Create(Connection: TmnCommandConnection);
begin
  inherited;
  Locking := False;
  FParams := TmnFields.Create;
  FRequestHeader := TmnFields.Create;
  FRespondHeader := TmnFields.Create;
end;

destructor TscatCommand.Destroy;
begin
  FParams.Free;
  FRequestHeader.Free;
  FRespondHeader.Free;
  inherited;
end;

procedure TscatCommand.StartHeader(AValue: string);
begin
  if FHeaderSent then
    raise Exception.Create('Header is sent');
  Stream.WriteLine(AValue);
end;

procedure TscatCommand.SendHeader(AName, AValue: string);
begin
  Stream.WriteLine(AName + ': ' + AValue);
end;

procedure TscatCommand.EndHeader;
begin
  Stream.WriteLine('');
  FHeaderSent := True;
end;

function TscatCommand.GetServer: TscatServer;
begin
  Result := (inherited Server as TscatServer);
end;

function TscatCommand.GetStream: TmnSocketStream;
begin
  Result := Connection.Stream;
end;

procedure TscatCommand.Enter;
begin
  Connection.Listener.Enter;
end;

procedure TscatCommand.Leave;
begin
  Connection.Listener.Leave;
end;

procedure TscatCommand.DoExecute;
begin
end;

procedure TscatCommand.Execute;
var
  aRequests: TStringList;
begin
  inherited;
  aRequests := TStringList.Create;
  try
    StrToStrings(Request, aRequests, [' '], []);
    FMethod := aRequests[0];
    FURI := aRequests[1];
    FVersion := aRequests[2];
  finally
    aRequests.Free;
  end;
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
  Connection.Stream.WriteLine('Server is running on port: ' + Server.Port);
  //Connection.Stream.WriteLine('the server is: "' + Application.ExeName + '"');
  Connection.Stream.WriteLine('');
end;

{ TscatPutCommand }

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
  scatLock.Free;
end.
