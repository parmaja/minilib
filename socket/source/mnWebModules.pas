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

  { TmodWebCommand }

  TmodWebCommand = class(TmnCommand)
  private
    function GetModule: TmodWebModule;
  protected
    Path: string;
    URIParams: TStringList;
    function GetDefaultDocument(Root: string): string;
    procedure Close;
    procedure RespondNotFound;
    procedure Respond; override;
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
    procedure DoBeforeOpen; override;
    procedure DoAfterClose; override;
  public
    constructor Create;
    destructor Destroy; override;
    property WebModule: TmodWebModule read FWebModule;
  end;

  {**
    Files Commands
  *}

  { TmodGetFileCommand }

  TmodGetFileCommand = class(TmodWebCommand)
  protected
  public
    procedure Respond; override;
  end;

  { TmodPutCommand }

  TmodPutCommand = class(TmodWebCommand)
  protected
  public
    procedure Respond; override;
  end;

  TmodServerInfoCommand = class(TmodWebCommand)
  protected
    procedure Respond; override;
  public
  end;

  TmodDirCommand = class(TmodWebCommand)
  protected
    procedure Respond; override;
  public
  end;

  TmodDeleteFileCommand = class(TmodWebCommand)
  protected
    procedure Respond; override;
  public
  end;

var
  modLock: TCriticalSection = nil;

function ParsePath(aRequest: string; out Name: string; out URIPath: string; URIParams: TStringList): Boolean;

implementation

function ParsePath(aRequest: string; out Name: string; out URIPath: string; URIParams: TStringList): Boolean;
begin
  ParseURI(aRequest, URIPath, URIParams);
  Name := SubStr(URIPath, '/', 0);
  URIPath := Copy(URIPath, Length(Name) + 1, MaxInt);
end;

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
end;

procedure TmodWebModule.CreateCommands;
begin
  inherited;
  RegisterCommand('GET', TmodGetFileCommand, true);
  RegisterCommand('Info', TmodServerInfoCommand);
  {
  RegisterCommand('GET', TmodGetFileCommand);
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

{ TmodWebCommand }

procedure TmodWebCommand.RespondNotFound;
var
  Body: string;
begin
  SendRespond('HTTP/1.1 200 OK');
  PostHeader('Content-Type', 'text/html');
  Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
    '<BODY><H1>404 Not Found</H1>The requested URL ' + //FDocument +
    ' was not found on this server.<P><h1>Powerd by Mini Web Server</h3></BODY></HTML>';
  RespondStream.WriteString(Body);
end;

function TmodWebCommand.GetModule: TmodWebModule;
begin
  Result := (inherited Module) as TmodWebModule;
end;

procedure TmodWebCommand.Unprepare;
begin
  RespondStream.Close;
end;

function TmodWebCommand.GetDefaultDocument(Root: string): string;
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

procedure TmodWebCommand.Close;
begin
  RespondStream.Close;
end;

procedure TmodWebCommand.Respond;
begin
  RespondNotFound;
end;

procedure TmodWebCommand.Prepare;
var
  aName: string;
begin
  inherited Prepare;
  ParsePath(Request.URI, aName, Path, URIParams);
  Root := Module.DocumentRoot;
  Host := RequestHeader['Host'];
end;

procedure TmodWebCommand.Created;
begin
  inherited Created;
  URIParams := TStringList.Create;
end;

{ TmodGetFileCommand }

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

procedure TmodGetFileCommand.Respond;
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
        aDocStream.Seek(0, soFromBeginning);
        if Active then
          RespondStream.WriteStream(aDocStream);
      finally
        aDocStream.Free;
      end;
    end;
  end
  else
    inherited;
end;

{ TmodServerInfoCommand }

procedure TmodServerInfoCommand.Respond;
begin
  SendRespond('OK');
  RespondStream.WriteLine('Server is running on port: ' + Module.Server.Port);
  //RespondStream.WriteLine('the server is: "' + Application.ExeName + '"');
end;

{ TmodPutCommand }

procedure TmodPutCommand.Respond;
var
  aFile: TFileStream;
  aFileName: string;
begin
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

procedure TmodDirCommand.Respond;
var
//  i: Integer;
//  aStrings: TStringList;
  aPath, aFilter: string;
begin
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

procedure TmodDeleteFileCommand.Respond;
var
  aFileName: string;
begin
  {aFileName := IncludeTrailingPathDelimiter(DocumentRoot) + Params.Values['FileName'];
  if FileExists(aFileName) then
    DeleteFile(aFileName);}
  RespondStream.WriteCommand('OK');
end;

{ TmodWebCommand }

destructor TmodWebCommand.Destroy;
begin
  URIParams.Free;
  inherited;
end;

{ TmodWebServer }

constructor TmodWebServer.Create;
begin
  inherited;
  FWebModule := TmodWebModule.Create('web', Modules, 'http/1.0');
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

destructor TmodWebServer.Destroy;
begin
  inherited;
end;

procedure TmodWebServer.DoBeforeOpen;
begin
  inherited;
end;

procedure TmodWebServer.DoAfterClose;
begin
  inherited;
end;

initialization
  modLock := TCriticalSection.Create;
finalization
  modLock.Free;
end.
