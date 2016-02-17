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
  mnFields, mnUtils, mnSockets, mnServers, mnCommandServers, mnStreams, mnSocketStreams, mnXML;

{**
-------------------
GET http://localhost/index.html HTTP/1.1
Host: localhost
Connection: Close

Post Body
-------------------

-------------------
method path?params http_version
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
    FHeader: TmnFields;
    function GetServer: TscatServer;
  protected
    procedure DoExecute; virtual;
    procedure Execute; override;
  public
    property Server: TscatServer read GetServer;
    constructor Create(Connection: TmnCommandConnection); override;
    destructor Destroy; override;
    property Method: string read FMethod;
    property URI: string read FURI;
    property Params: TmnFields read FParams;
    property Version: string read FVersion;
    property Header: TmnFields read FHeader;
  end;

  { TscatWebCommand }

  TscatWebCommand = class(TscatCommand)
  private
    procedure ParseRequest;
  protected
    procedure Answer404;
  public
    Root: string; //Document root folder
    Path: string;
    Host: string;
    procedure Respond(RequestStream: TmnCustomStream); virtual;
    procedure DoExecute; override;
  end;

{**
  Files Commands
*}

  { TscatGetCommand }

  TscatGetCommand = class(TscatWebCommand)
  protected
  public
    procedure Respond(RequestStream: TmnCustomStream); override;
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

  { TScatConnection }

  TScatConnection = class(TmnCommandConnection)
  protected
    function CreateStream(Socket: TmnCustomSocket): TmnSocketStream; override;
  public
  end;

  { TScatListener }

  TScatListener = class(TmnCommandListener)
  private
  protected
    function CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection; override;
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

{ TScatConnection }

function TScatConnection.CreateStream(Socket: TmnCustomSocket): TmnSocketStream;
begin
  Result :=inherited CreateStream(Socket);
  Result.EndOfLine := sWinEndOfLine;
end;

{ TScatListener }

function TScatListener.CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection;
begin
  Result := TScatConnection.Create(vSocket);
end;

{ TscatWebCommand }

procedure TscatWebCommand.Answer404;
var
  Body: string;
begin
  Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' + //FDocument +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>' + sEndOfLine;
  Connection.Stream.WriteString(Body);
end;

procedure TscatWebCommand.ParseRequest;
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

  if Path[1] = '/' then
    Delete(Path, 1, 1);

    { Find parameters }
  J := Pos('?', Path);
  if J <= 0 then
    aParams := ''
  else
  begin
    aParams := Copy(Path, J + 1, Length(Path));
    Path := Copy(Path, 1, J - 1);
  end;
end;

procedure TscatWebCommand.Respond(RequestStream: TmnCustomStream);
begin
  RequestStream.WriteString('HTTP/1.0 404 Not Found');
end;

procedure TscatWebCommand.DoExecute;
var
  l: string;
begin
  inherited;
  while Connected do
  begin
    l := Connection.Stream.ReadLine;

    Header.AddItem(l, ':');
    if l = '' then
      break;
  end;

  Root := Server.DocumentRoot;
  Host := Header['Host'];
  try
    Respond(Connection.Stream);
  finally
  end;
  if Connected then
    Shutdown;
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

procedure TscatGetCommand.Respond(RequestStream: TmnCustomStream);
var
  DocSize: Int64;
  aDocStream: TFileStream;
  aDocument: string;
  aAnswerContentType: string;
  aProxyRequest: Boolean;
begin
  aDocument := IncludeTrailingPathDelimiter(Root) + Path;
  aDocument := StringReplace(aDocument, '/', PathDelim, [rfReplaceAll]);//correct it for linux

{  if aDocument[Length(aDocument)] = PathDelim then //get the default file if it not defined
    aDocument := GetDocument(aDocument);}

  if FileExists(aDocument) then
  begin
    if Connected then
    begin
      aAnswerContentType := DocumentToContentType(aDocument);
      aDocStream := TFileStream.Create(aDocument, fmOpenRead or fmShareDenyWrite);
      DocSize := aDocStream.Size;
      if Connected then
        RequestStream.WriteString('HTTP/1.1 200 OK' + sEndOfLine +
          'Content-Type: ' + DocumentToContentType(aDocument) + sEndOfLine +
          'Content-Length: ' + IntToStr(DocSize) + sEndOfLine +
          sEndOfLine);
      if Connected then
        RequestStream.WriteStream(aDocStream);
      aDocStream.Free;
    end;
  end
  else
    Answer404;
end;

procedure FieldsCallBack(vObject: TObject; S: string);
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

constructor TscatCommand.Create(Connection: TmnCommandConnection);
begin
  inherited;
  Connection.Stream.Timeout := -1;
  FParams := TmnFields.Create;
  FHeader := TmnFields.Create;
end;

destructor TscatCommand.Destroy;
begin
  FParams.Free;
  FHeader.Free;
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
  Connection.Stream.WriteLn('Server is running on port: ' + Server.Port);
  //Connection.Stream.WriteLn('the server is: "' + Application.ExeName + '"');
  Connection.Stream.WriteLn('');
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
