unit mnWebModules;
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


interface

uses
  SysUtils, Classes, syncobjs,
  mnFields, mnUtils, mnSockets, mnServers, mnStreams,
  mnModules;

type

  TscatWebModule = class;

  { TscatWebCommand }

  TscatWebCommand = class(TmnCommand)
  private
    function GetModule: TscatWebModule;
  protected
    URIPath: string;
    URIParams: TStringList;
    function GetDefaultDocument(Root: string): string;
    procedure Shutdown;
    procedure Answer404;
    procedure Respond; override;
    procedure Prepare; override;
    procedure Unprepare; override;
    procedure Execute; override;
    procedure Created; override;
  public
    Root: string; //Document root folder
    Host: string;
    Connection : TmnCommandConnection;
    destructor Destroy; override;
    property Module: TscatWebModule read GetModule;
  end;

  TscatWebServer = class;

  { TscatWebModule }

  TscatWebModule = class(TmnModule)
  private
    FServer: TscatWebServer;
    procedure SetDefaultDocument(AValue: TStringList);
    procedure SetDocumentRoot(AValue: string);
  protected
    FDocumentRoot: string;
    FDefaultDocument: TStringList;
    function GetActive: Boolean; override;
    procedure Created; override;
    procedure CreateCommands; override;

    procedure ParseRequest(var ARequest: TmnRequest); override;
    function Match(ARequest: TmnRequest): Boolean; override;
  public
    destructor Destroy; override;
    property Server: TscatWebServer read FServer;
    property DocumentRoot: string read FDocumentRoot write SetDocumentRoot;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDocument;
  end;

  { TscatWebServer }

  TscatWebServer = class(TmnCommandServer)
  private
    FWebModule: TscatWebModule;
  protected
    procedure DoBeforeOpen; override;
    procedure DoAfterClose; override;
  public
    constructor Create;
    destructor Destroy; override;
    property WebModule: TscatWebModule read FWebModule;
  end;

  {**
    Files Commands
  *}

    { TscatGetFileCommand }

    TscatGetFileCommand = class(TscatWebCommand)
    protected
    public
      procedure Respond; override;
    end;

    { TscatPutCommand }

    TscatPutCommand = class(TscatWebCommand)
    protected
    public
      procedure Respond; override;
    end;

    TscatServerInfoCommand = class(TscatWebCommand)
    protected
      procedure Respond; override;
    public
    end;

    TscatDirCommand = class(TscatWebCommand)
    protected
      procedure Respond; override;
    public
    end;

    TscatDeleteFileCommand = class(TscatWebCommand)
    protected
      procedure Respond; override;
    public
    end;

var
  scatLock: TCriticalSection = nil;

function ParsePath(aRequest: string; out Name: string; out URIPath: string; URIParams: TStringList): Boolean;

implementation

function ParsePath(aRequest: string; out Name: string; out URIPath: string; URIParams: TStringList): Boolean;
begin
  ParseURI(aRequest, URIPath, URIParams);
  Name := SubStr(URIPath, '/', 0);
  URIPath := Copy(URIPath, Length(Name) + 1, MaxInt);
end;

{ TscatWebModule }

procedure TscatWebModule.SetDocumentRoot(AValue: string);
begin
  if FDocumentRoot =AValue then Exit;
  FDocumentRoot :=AValue;
end;

procedure TscatWebModule.SetDefaultDocument(AValue: TStringList);
begin
  FDefaultDocument.Assign(AValue);
end;

function TscatWebModule.GetActive: Boolean;
begin
  Result := Server.Active;
end;

procedure TscatWebModule.Created;
begin
  inherited;
  FDefaultDocument := TStringList.Create;
end;

procedure TscatWebModule.CreateCommands;
begin
  inherited;
  Commands.Add('GET', TscatGetFileCommand);
end;

destructor TscatWebModule.Destroy;
begin
  FreeAndNil(FDefaultDocument);
  inherited Destroy;
end;

procedure TscatWebModule.ParseRequest(var ARequest: TmnRequest);
var
  aName, aPath: string;
begin
  ParsePath(ARequest.Path, aName, aPath, nil);
  ARequest.Command := ARequest.Method;
end;

function TscatWebModule.Match(ARequest: TmnRequest): Boolean;
var
  aName, aPath: string;
begin
  ParsePath(ARequest.Path, aName, aPath, nil);
  Result := SameText(Name, aName) and SameText(SubStr(ARequest.Protcol, '/', 0),  'http');
end;

{ TscatWebCommand }

procedure TscatWebCommand.Answer404;
var
  Body: string;
begin
  SendHead('HTTP/1.1 200 OK');
  PostHeader('Content-Type', 'text/html');
  Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' + //FDocument +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>';
  RespondStream.WriteString(Body);
end;

function TscatWebCommand.GetModule: TscatWebModule;
begin
  Result := Module as TscatWebModule;
end;

procedure TscatWebCommand.Unprepare;
begin
end;

function TscatWebCommand.GetDefaultDocument(Root: string): string;
var
  i: Integer;
  aFile: string;
begin
  //TODO baaad you need to luck before access
  for i := 0 to Module.DefaultDocument.Count - 1 do
  begin
    aFile := Root + Module.DefaultDocument[i];
    if FileExists(aFile) then
    begin
      Result := aFile;
    end;
  end;
end;

procedure TscatWebCommand.Shutdown;
begin
  if (Connection <> nil) and Connection.Connected and (Connection.Stream <> nil) then
    Connection.Stream.Drop;
end;

procedure TscatWebCommand.Respond;
begin
  RespondStream.WriteString('HTTP/1.0 404 Not Found');
end;

procedure TscatWebCommand.Prepare;
var
  aName: string;
begin
  inherited Prepare;
  ParsePath(Request.Path, aName, URIPath, URIParams);
end;

procedure TscatWebCommand.Execute;
var
  l: string;
begin
  inherited;
  Root := Module.DocumentRoot;
  Host := RequestHeader['Host'];
  try
    Respond;
  finally
  end;
  Shutdown;
end;

procedure TscatWebCommand.Created;
begin
  inherited Created;
  URIParams := TStringList.Create;
end;

{ TscatGetFileCommand }

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

procedure TscatGetFileCommand.Respond;
var
  DocSize: Int64;
  aDocStream: TFileStream;
  aDocument: string;
begin
  aDocument := IncludeTrailingPathDelimiter(Root) + URIPath;
  aDocument := StringReplace(aDocument, '/', PathDelim, [rfReplaceAll]);//correct it for linux

 if aDocument[Length(aDocument)] = PathDelim then //get the default file if it not defined
    aDocument := GetDefaultDocument(aDocument);

  if FileExists(aDocument) then
  begin
    if Active then
    begin
      aDocStream := TFileStream.Create(aDocument, fmOpenRead or fmShareDenyWrite);
      try
        DocSize := aDocStream.Size;
        if Active then
        begin
          SendHead('HTTP/1.1 200 OK');
          PostHeader('Content-Type', DocumentToContentType(aDocument));
          PostHeader('Content-Length', IntToStr(DocSize));
        end;

        if Active then
          RespondStream.WriteStream(aDocStream);
      finally
        aDocStream.Free;
      end;
    end;
  end
  else
    Answer404;
end;

{ TscatServerInfoCommand }

procedure TscatServerInfoCommand.Respond;
begin
  SendHead('OK');
  Connection.Stream.WriteLine('Server is running on port: ' + Module.Server.Port);
  //Connection.Stream.WriteLine('the server is: "' + Application.ExeName + '"');
end;

{ TscatPutCommand }

procedure TscatPutCommand.Respond;
var
  aFile: TFileStream;
  aFileName: string;
begin
  Connection.Stream.WriteCommand('OK');
  aFileName := URIParams.Values['FileName'];
  {aFile := TFileStream.Create(DocumentRoot + aFileName, fmCreate);
  try
    Connection.Stream.ReadStream(aFile);
  finally
    aFile.Free;
  end;}
end;

{ TscatDirCommand }

procedure TscatDirCommand.Respond;
var
//  i: Integer;
//  aStrings: TStringList;
  aPath, aFilter: string;
begin
  Connection.Stream.WriteCommand('OK');
  aFilter := URIParams.Values['Filter'];
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

procedure TscatDeleteFileCommand.Respond;
var
  aFileName: string;
begin
  {aFileName := IncludeTrailingPathDelimiter(DocumentRoot) + Params.Values['FileName'];
  if FileExists(aFileName) then
    DeleteFile(aFileName);}
  Connection.Stream.WriteCommand('OK');
end;
{
RegisterCommand('Info', TscatServerInfoCommand);
RegisterCommand('GET', TscatGetFileCommand);
RegisterCommand('PUT', TscatPutCommand);
RegisterCommand('DIR', TscatDirCommand);
RegisterCommand('DEL', TscatDeleteFileCommand);
}

{ TscatWebCommand }

destructor TscatWebCommand.Destroy;
begin
  URIParams.Free;
  inherited;
end;

{procedure TScatListener.DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket);
begin
  inherited;
  Result.EOFOnError := True;
  Result.EndOfLine := sWinEndOfLine;
end;

function TScatListener.GetCommandClass(var CommandName: string): TmnCommandClass;
begin
  inherited;
end;}


{ TscatWebServer }

constructor TscatWebServer.Create;
begin
  inherited;
  FWebModule := TscatWebModule.Create('web', Modules, 'http/1.0');
  FWebModule.FServer := Self;
  Port := '80';
  with FWebModule do
  begin
    FDocumentRoot := '';
    FDefaultDocument.Add('index.html');
    FDefaultDocument.Add('index.htm');
    FDefaultDocument.Add('default.html');
    FDefaultDocument.Add('default.htm');
  end;
end;

destructor TscatWebServer.Destroy;
begin
  inherited;
end;

procedure TscatWebServer.DoBeforeOpen;
begin
  inherited;
end;

procedure TscatWebServer.DoAfterClose;
begin
  inherited;
end;

{function TscatWebServer.DoCreateListener: TmnListener;
begin
  Result := TScatListener.Create;
end;}

initialization
  scatLock := TCriticalSection.Create;
finalization
  scatLock.Free;
end.
