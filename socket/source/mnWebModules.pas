unit mnWebModules;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of mod://www.gnu.org/licenses/lgpl.html)
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

  TmodWebModule = class;

  { TmodHttpCommand }

  TmodHttpCommand = class(TmnCommand)
  private
    FCookies: TmnParams;
    FURIParams: TmnParams;
  protected
    URIPath: string;
    procedure Created; override;
    procedure SendHeader; override;
    procedure Prepare; override;
    procedure Respond(var Result: TmnExecuteResults); override;
  public
    destructor Destroy; override;
    property Cookies: TmnParams read FCookies;
    property URIParams: TmnParams read FURIParams;
  end;

  { TmodURICommand }

  TmodURICommand = class(TmodHttpCommand)
  private
    function GetModule: TmodWebModule;
  protected
    function GetDefaultDocument(Root: string): string;
    procedure RespondNotFound;
    procedure Respond(var Result: TmnExecuteResults); override;
    procedure Prepare; override;
    procedure Unprepare; override;
    procedure Created; override;
  public
    Root: string; //Document root folder
    Host: string;
    destructor Destroy; override;
    property Module: TmodWebModule read GetModule;
  end;

  TmodWebServer = class;

  { TmodWebModule }

  TmodWebModule = class(TmnModule)
  private
    FServer: TmodWebServer;
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
    property Server: TmodWebServer read FServer;
    property DocumentRoot: string read FDocumentRoot write SetDocumentRoot;
    property DefaultDocument: TStringList read FDefaultDocument write SetDefaultDocument;
  end;

  { TmodWebServer }

  TmodWebServer = class(TmnModuleServer)
  private
    FWebModule: TmodWebModule;
  protected
  public
    constructor Create; override;
    property WebModule: TmodWebModule read FWebModule;
  end;

  {**
    Files Commands
  *}

  { TmodHttpGetCommand }

  TmodHttpGetCommand = class(TmodURICommand)
  protected
  public
    procedure Respond(var Result: TmnExecuteResults); override;
  end;

  { TmodPutCommand }

  TmodPutCommand = class(TmodURICommand)
  protected
  public
    procedure Respond(var Result: TmnExecuteResults); override;
  end;

  { TmodServerInfoCommand }

  TmodServerInfoCommand = class(TmodURICommand)
  protected
    procedure Respond(var Result: TmnExecuteResults); override;
  public
  end;

  { TmodDirCommand }

  TmodDirCommand = class(TmodURICommand)
  protected
    procedure Respond(var Result: TmnExecuteResults); override;
  public
  end;

  { TmodDeleteFileCommand }

  TmodDeleteFileCommand = class(TmodURICommand)
  protected
    procedure Respond(var Result: TmnExecuteResults); override;
  public
  end;

var
  modLock: TCriticalSection = nil;

implementation

{ TmodWebModule }

procedure TmodWebModule.SetDocumentRoot(AValue: string);
begin
  if FDocumentRoot =AValue then Exit;
  FDocumentRoot :=AValue;
end;

procedure TmodWebModule.SetDefaultDocument(AValue: TStringList);
begin
  FDefaultDocument.Assign(AValue);
end;

function TmodWebModule.GetActive: Boolean;
begin
  Result := Server.Active;
end;

procedure TmodWebModule.Created;
begin
  inherited;
  FDefaultDocument := TStringList.Create;
  UseKeepAlive := true;
end;

procedure TmodWebModule.CreateCommands;
begin
  inherited;
  RegisterCommand('GET', TmodHttpGetCommand, true);
  RegisterCommand('Info', TmodServerInfoCommand);
  {
  RegisterCommand('GET', TmodHttpGetCommand);
  RegisterCommand('PUT', TmodPutCommand);
  RegisterCommand('DIR', TmodDirCommand);
  RegisterCommand('DEL', TmodDeleteFileCommand);
  }
end;

destructor TmodWebModule.Destroy;
begin
  FreeAndNil(FDefaultDocument);
  inherited Destroy;
end;

procedure TmodWebModule.ParseRequest(var ARequest: TmnRequest);
var
  aName, aPath: string;
begin
  ParsePath(ARequest.URI, aName, aPath, nil);
  ARequest.Command := ARequest.Method;
end;

function TmodWebModule.Match(ARequest: TmnRequest): Boolean;
var
  aName, aPath: string;
begin
  ParsePath(ARequest.URI, aName, aPath, nil);
  Result := SameText(Name, aName) and SameText(SubStr(ARequest.Protcol, '/', 0),  'http');
end;

{ TmodURICommand }

procedure TmodURICommand.RespondNotFound;
var
  Body: string;
begin
  SendRespond('HTTP/1.1 200 OK');
  PostHeader('Content-Type', 'text/html');
  Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>';
  RespondStream.WriteString(Body);
end;

function TmodURICommand.GetModule: TmodWebModule;
begin
  Result := (inherited Module) as TmodWebModule;
end;

procedure TmodURICommand.Unprepare;
begin

end;

function TmodURICommand.GetDefaultDocument(Root: string): string;
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

procedure TmodURICommand.Respond(var Result: TmnExecuteResults);
begin
  inherited;
end;

procedure TmodURICommand.Prepare;
var
  S: string;
begin
  inherited;
  ParsePath(Request.URI, S, URIPath, URIParams);
  Root := Module.DocumentRoot;
  Host := RequestHeader['Host'].AsString;
end;

procedure TmodURICommand.Created;
begin
  inherited Created;
end;

{ TmodHttpGetCommand }

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
  else if (Ext = 'png') then
    Result := 'image/png'
  else if Ext = 'txt' then
    Result := 'text/plain'
  else if Ext = 'svg' then
    Result := 'image/svg+xml'
  else if Ext = 'css' then
    Result := 'text/css'
  else if Ext = 'json' then
    Result := 'application/json'
  else if Ext = 'js' then
    Result := 'text/js'
  else
    Result := 'application/binary';
end;

procedure TmodHttpGetCommand.Respond(var Result: TmnExecuteResults);
var
  DocSize: Int64;
  aDocStream: TFileStream;
  aDocument: string;
begin
  inherited;
  aDocument := IncludeTrailingPathDelimiter(Root) + '.' + URIPath;
  aDocument := StringReplace(aDocument, '/', PathDelim, [rfReplaceAll]);//correct it for linux
  aDocument := ExpandFileName(aDocument);

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
          SendRespond('HTTP/1.1 200 OK');
          PostHeader('Content-Type', DocumentToContentType(aDocument));
          PostHeader('Content-Length', IntToStr(DocSize));
        end;

        SendHeader;

        if Active then
          RespondStream.WriteStream(aDocStream);
      finally
        aDocStream.Free;
      end;
    end;
  end
  else
    RespondNotFound;
end;

{ TmodServerInfoCommand }

procedure TmodServerInfoCommand.Respond(var Result: TmnExecuteResults);
begin
  inherited;
  SendRespond('OK');
  RespondStream.WriteLine('Server is running on port: ' + Module.Server.Port);
  //RespondStream.WriteLine('the server is: "' + Application.ExeName + '"');
end;

{ TmodPutCommand }

procedure TmodPutCommand.Respond(var Result: TmnExecuteResults);
var
  aFile: TFileStream;
  aFileName: string;
begin
  inherited;
  RespondStream.WriteCommand('OK');
  aFileName := URIParams.Values['FileName'];
  {aFile := TFileStream.Create(DocumentRoot + aFileName, fmCreate);
  try
    Connection.Stream.ReadStream(aFile);
  finally
    aFile.Free;
  end;}
end;

{ TmodDirCommand }

procedure TmodDirCommand.Respond(var Result: TmnExecuteResults);
var
//  i: Integer;
//  aStrings: TStringList;
  aPath, aFilter: string;
begin
  inherited;
  RespondStream.WriteCommand('OK');
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

{ TmodDeleteFileCommand }

procedure TmodDeleteFileCommand.Respond(var Result: TmnExecuteResults);
var
  aFileName: string;
begin
  inherited;
  {aFileName := IncludeTrailingPathDelimiter(DocumentRoot) + Params.Values['FileName'];
  if FileExists(aFileName) then
    DeleteFile(aFileName);}
  RespondStream.WriteCommand('OK');
end;

{ TmodURICommand }

destructor TmodURICommand.Destroy;
begin
  inherited;
end;

{ TmodWebServer }

constructor TmodWebServer.Create;
begin
  inherited;
  FWebModule := TmodWebModule.Create('web', 'http/1.1', Modules);
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

{ TmodHttpCommand }

procedure TmodHttpCommand.Created;
begin
  inherited;
  FCookies := TmnParams.Create;
  FURIParams := TmnParams.Create;
end;

destructor TmodHttpCommand.Destroy;
begin
  FreeAndNil(FCookies);
  FreeAndNil(FURIParams);
  inherited;
end;

procedure TmodHttpCommand.Prepare;
var
  aName: string;
begin
  inherited;
  ParsePath(Request.URI, aName, URIPath, URIParams);
end;

procedure TmodHttpCommand.Respond(var Result: TmnExecuteResults);
var
  aParams: TmnParams;
begin
  inherited;
  if Module.UseKeepAlive and RequestHeader.IsExists('Connection') then
  begin
    Result.Timout := Module.KeepAliveTimeOut;

    if SameText(RequestHeader['Connection'].AsString, 'Keep-Alive') then
    begin
      if RequestHeader.IsExists('Keep-Alive') then
      begin
        aParams := TmnParams.Create;
        try
          //Keep-Alive: timeout=5, max=1000
          aParams.Seperator := '=';
          aParams.Delimiter := ',';
          aParams.AsString := RequestHeader['Keep-Alive'].AsString;
          Result.Timout := aParams['timeout'].AsInteger;
        finally
          aParams.Free;
        end;
      end;
      Result.Status := Result.Status + [erKeepAlive];
    end;
  end;
end;

procedure TmodHttpCommand.SendHeader;
begin
  PostHeader('Cookies', Cookies.ToString);
  inherited;
end;

initialization
  modLock := TCriticalSection.Create;
finalization
  modLock.Free;
end.
