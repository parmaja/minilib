unit ScatServers;
{$M+}{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of Scat://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, Classes, syncobjs,
  mnFields, mnUtils, mnClasses, mnSockets, mnServers, mnConnectionCommands, mnStreams, mnSocketStreams;

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
    FParams: TmnFields;
    FRequestHeader: TmnFields;
    FRespondHeader: TmnFields;
    FHeaderSent: Boolean;
    function GetServer: TscatServer;
    function GetStream: TmnConnectionStream;
    procedure Enter;
    procedure Leave;
  protected
    procedure DoExecute; virtual;
    procedure Execute; override;
  public
    property Server: TscatServer read GetServer;
    property Stream: TmnConnectionStream read GetStream;
    constructor Create(Connection: TmnCommandConnection); override;
    destructor Destroy; override;
    property Params: TmnFields read FParams;
    property RequestHeader: TmnFields read FRequestHeader;
    procedure StartHeader(AValue: string); virtual;
    procedure SendHeader(AName, AValue: string); virtual;
    procedure EndHeader;
  end;

  { TscatModule }

  TscatModule = class(TObject)
  private
    FCommands: TmnCommandClasses;
  public
    constructor Create(const Server: TscatServer);
    destructor Destroy; override;
    property Commands: TmnCommandClasses read FCommands;
    function Match(Path: string): Boolean; virtual; abstract;
  end;

  TscatModuleClass = class of TscatModule;

  { TscatModuleClassItem }

  TscatModuleClassItem = class(TObject)
  private
    FModuleClass: TscatModuleClass;
    FName: string;
  public
    property Name: string read FName;
    property ModuleClass: TscatModuleClass read FModuleClass;
  end;

  TscatModules = class(TmnObjectList<TscatModuleClassItem>)
  end;

  { TScatListener }

  TScatListener = class(TmnCommandListener)
  private
  protected
    procedure DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket); override;
  public
    function GetCommandClass(var CommandName: string): TmnCommandClass; override;
  public
  end;

{**
  Server
*}

  { TscatServer }

  TscatServer = class(TmnCommandServer)
  public
    //FModules: TmnModuleClasses;
    constructor Create;
    destructor Destroy; override;
    procedure RegisterModuleClass(AModule: TscatModuleClass);
  end;

  { TscatWebServer }

  TscatWebServer = class(TscatServer)
  private
    FDocumentRoot: string;
    FDefaultDocument: TStringList;
  protected
    procedure SetDefaultDocument(const Value: TStringList);
    procedure DoBeforeOpen; override;
    procedure DoAfterClose; override;
    function DoCreateListener: TmnListener; override;
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

{ TscatServer }

constructor TscatServer.Create;
begin
  inherited;
  //FModules := TmnModuleClasses.Create(True);
end;

destructor TscatServer.Destroy;
begin
//  FreeAndNil(FModules);
  inherited Destroy;
end;

procedure TscatServer.RegisterModuleClass(AModule: TscatModuleClass);
begin

end;

{ TscatModule }

constructor TscatModule.Create(const Server: TscatServer);
begin
  inherited Create;
  FCommands := TmnCommandClasses.Create(True);
end;

destructor TscatModule.Destroy;
begin
  FreeAndNil(FCommands);
  inherited Destroy;
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
  Stream.WriteLine(utf8string(''));
  FHeaderSent := True;
end;

function TscatCommand.GetServer: TscatServer;
begin
  Result := (inherited Server as TscatServer);
end;

function TscatCommand.GetStream: TmnConnectionStream;
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

procedure TScatListener.DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket);
begin
  inherited;
  Result.EOFOnError := True;
  Result.EndOfLine := sWinEndOfLine;
end;

function TScatListener.GetCommandClass(var CommandName: string): TmnCommandClass;
begin
  inherited;
end;


{ TscatWebServer }

constructor TscatWebServer.Create;
begin
  inherited;
  FDefaultDocument := TStringList.Create;
  Port := '80';
  FDefaultDocument.Add('index.html');
  FDefaultDocument.Add('index.htm');
  FDefaultDocument.Add('default.html');
  FDefaultDocument.Add('default.htm');
  FDocumentRoot := '';
end;

destructor TscatWebServer.Destroy;
begin
  FreeAndNil(FDefaultDocument);
  inherited;
end;

procedure TscatWebServer.SetDefaultDocument(const Value: TStringList);
begin
  FDefaultDocument.Assign(Value);
end;

procedure TscatWebServer.DoBeforeOpen;
begin
  inherited;
end;

procedure TscatWebServer.DoAfterClose;
begin
  inherited;
end;

function TscatWebServer.DoCreateListener: TmnListener;
begin
  Result := TScatListener.Create;
end;

initialization
  scatLock := TCriticalSection.Create;
finalization
  scatLock.Free;
end.
