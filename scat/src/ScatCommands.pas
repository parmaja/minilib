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
  SysUtils, Classes,
  mnFields, mnUtils, mnSockets, mnServers, mnCommandServers, ScatServers, mnStreams, mnSocketStreams;

type

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

  { TscatWebModule }

  TscatWebModule = class(TscatModule)
  public
    constructor Create(Server: TscatServer);
    function Match(Path: string): Boolean; override;
  end;

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

{ TscatWebModule }

constructor TscatWebModule.Create(Server: TscatServer);
begin
  inherited;
  Commands.Add('GET', TscatGetCommand);
end;

function TscatWebModule.Match(Path: string): Boolean;
begin
  //TODO
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
  while (I <= Length(Request.Path)) and (Request.Path[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(Request.Path)) and (Request.Path[I] <> ' ') do
    Inc(I);

  Path := Copy(Request.Path, J, I - J);

  Inc(I);
  while (I <= Length(Request.Path)) and (Request.Path[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(Request.Path)) and (Request.Path[I] <> ' ') do
    Inc(I);

  if Path <> '' then
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
{
RegisterCommand('Info', TscatServerInfoCommand);
RegisterCommand('GET', TscatGetCommand);
RegisterCommand('PUT', TscatPutCommand);
RegisterCommand('DIR', TscatDirCommand);
RegisterCommand('DEL', TscatDeleteFileCommand);
}

end.
