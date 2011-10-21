unit mnCommClasses;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
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
  Classes, SysUtils,
  mnStreams;

const
  cTimeout = 30000;

type
  ECommError = class(Exception)
  end;

  TDataBits = (dbFive, dbSix, dbSeven, dbEight);
  TParityBits = (prNone, prOdd, prEven, prMark, prSpace);
  TStopBits = (sbOneStopBit, sbOneAndHalfStopBits, sbTwoStopBits);
  TFlowControl = (fcHardware, fcXonXoff, fcNone);

  TmnCommConnectMode = (ccmReadWrite, ccmRead, ccmWrite);

  { TmnCustomCommStream }

  TmnCustomCommStream = class(TmnCustomStream)
  private
    FBaudRate: Int64;
    FConnectMode: TmnCommConnectMode;
    FParity: TParityBits;
    FStopBits: TStopBits;
    FPort: string;
    FFlowControl: TFlowControl;
    FDataBits: TDataBits;
    FEventChar: AnsiChar;
    FDiscardNull: Boolean;
    FTimeout: Cardinal;
    FFailTimeout: Boolean;
    FWaitMode: Boolean;
    procedure SetTimeout(const Value: Cardinal);
  protected
    procedure DoConnect; virtual; abstract;
    procedure DoDisconnect; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    procedure CheckConnected;
    procedure Created; virtual;
    function InternalRead(var Buffer; Count: Integer): Integer; virtual; abstract;
    function InternalWrite(const Buffer; Count: Integer): Integer; virtual; abstract;
  public
    constructor Create(Suspend: Boolean; Port: string; BaudRate: Int64; DataBits: TDataBits = dbEight; Parity: TParityBits = prNone; StopBits: TStopBits = sbOneStopBit; FlowControl: TFlowControl = fcHardware); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override; final;
    function Write(const Buffer; Count: Integer): Integer; override; final;
    procedure Connect;
    procedure Disconnect;
    procedure Open; //Alias for Connect
    procedure Close; //Alias for Disconnect
    procedure Flush; virtual;
    procedure Purge; virtual;
    function WaitRead: Boolean; virtual;
    function WaitWrite: Boolean; virtual;
    property Connected: Boolean read GetConnected;
    property Port: string read FPort;
    property BaudRate: Int64 read FBaudRate;
    property DataBits: TDataBits read FDataBits;
    property Parity: TParityBits read FParity;
    property StopBits: TStopBits read FStopBits;
    property FlowControl: TFlowControl read FFlowControl write FFlowControl;

    property EventChar: AnsiChar read FEventChar write FEventChar default #13;
    property DiscardNull: Boolean read FDiscardNull write FDiscardNull default False;

    property ConnectMode: TmnCommConnectMode read FConnectMode write FConnectMode;
    //WaitMode: Make WaitEvent before read buffer
    property WaitMode: Boolean read FWaitMode write FWaitMode;
    property Timeout: Cardinal read FTimeout write SetTimeout default cTimeout;
    //FailTimeout: raise an exception when timeout accord
    property FailTimeout: Boolean read FFailTimeout write FFailTimeout default True;
  end;

implementation

{ TmnCustomCommStream }

procedure TmnCustomCommStream.Close;
begin
  Disconnect;
end;

constructor TmnCustomCommStream.Create(Suspend: Boolean; Port: string; BaudRate: Int64;
  DataBits: TDataBits; Parity: TParityBits; StopBits: TStopBits; FlowControl: TFlowControl);
begin
  inherited Create;
  FTimeout := cTimeout;
  FFailTimeout := True;
  FPort := Port;
  FBaudRate := BaudRate;
  FDataBits := DataBits;
  FParity := Parity;
  FStopBits := StopBits;
  FFlowControl := FlowControl;
  FEventChar := #13;
  Created;
  if not Suspend then
    Open;
end;

procedure TmnCustomCommStream.Connect;
begin
  DoConnect;
end;

destructor TmnCustomCommStream.Destroy;
begin
  if Connected then
    Disconnect;
  inherited;
end;

procedure TmnCustomCommStream.Disconnect;
begin
  DoDisconnect;
end;

procedure TmnCustomCommStream.Flush;
begin
end;

procedure TmnCustomCommStream.CheckConnected;
begin
  if not Connected then
    raise ECommError.Create('Port to ' + Port + ' not connected');
end;

procedure TmnCustomCommStream.Created;
begin
end;

procedure TmnCustomCommStream.Open;
begin
  if Connected then
    raise ECommError.Create('Already connected');
  Connect;
end;

procedure TmnCustomCommStream.Purge;
begin
end;

function TmnCustomCommStream.Read(var Buffer; Count: Integer): Integer;
begin
  CheckConnected;
  if WaitMode then
  begin
    if WaitRead then
      Result := InternalRead(Buffer, Count)
    else
      Result := 0;
  end
  else
    Result := InternalRead(Buffer, Count);
end;

function TmnCustomCommStream.Write(const Buffer; Count: Integer): Integer;
begin
  CheckConnected;
  Result := InternalWrite(Buffer, Count);
end;

function TmnCustomCommStream.WaitRead: Boolean;
begin
  Result := False;
end;

function TmnCustomCommStream.WaitWrite: Boolean;
begin
  Result := False;
end;

procedure TmnCustomCommStream.SetTimeout(const Value: Cardinal);
begin
  FTimeout := Value;
end;

end.

