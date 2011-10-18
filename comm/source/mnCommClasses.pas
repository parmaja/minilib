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
  TFlowControl = (fcHardware, fcSoftware, fcNone, fcCustom);

  TDTRFlowControl = (dtrDisable, dtrEnable, dtrHandshake);
  TRTSFlowControl = (rtsDisable, rtsEnable, rtsHandshake, rtsToggle);

  TFlowControlFlags = record
    OutCTSFlow: Boolean;
    OutDSRFlow: Boolean;
    ControlDTR: TDTRFlowControl;
    ControlRTS: TRTSFlowControl;
    XonXoffOut: Boolean;
    XonXoffIn: Boolean;
    DSRSensitivity: Boolean;
    TxContinueOnXoff: Boolean;
    XonChar: AnsiChar;
    XoffChar: AnsiChar;
  end;

  TParityFlags = record
    Check: Boolean;
    Replace: Boolean;
    ReplaceChar: AnsiChar;
  end;

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
    procedure SetEventChar(const Value: AnsiChar);
    procedure SetDiscardNull(const Value: Boolean);
    procedure SetTimeout(const Value: Cardinal);
  protected
    procedure DoConnect; virtual; abstract;
    procedure DoDisconnect; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    function GetFlowControlFlags: TFlowControlFlags; virtual;
    function GetParityFlags: TParityFlags; virtual;
    function DoRead(var Buffer; Count: Integer): Integer; virtual; abstract;
    function DoWrite(const Buffer; Count: Integer): Integer; virtual; abstract;
    procedure CheckConnected;
    procedure Created; virtual;
  public
    constructor Create(Suspend: Boolean; Port: string; BaudRate: Int64; DataBits: TDataBits = dbEight; Parity: TParityBits = prNone; StopBits: TStopBits = sbOneStopBit; FlowControl: TFlowControl = fcHardware); overload;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Open; //Alias for Connect
    procedure Close; //Alias for Disconnect
    procedure Flush; virtual;
    procedure Purge; virtual;
    function ReadString: string;
    function WaitRead: Boolean; virtual;
    function WaitWrite: Boolean; virtual;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    property Connected: Boolean read GetConnected;
    property Port: string read FPort;
    property BaudRate: Int64 read FBaudRate;
    property DataBits: TDataBits read FDataBits;
    property Parity: TParityBits read FParity;
    property StopBits: TStopBits read FStopBits;
    property FlowControl: TFlowControl read FFlowControl write FFlowControl;
    property EventChar: AnsiChar read FEventChar write SetEventChar default #13;
    property DiscardNull: Boolean read FDiscardNull write SetDiscardNull default False;

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

function TmnCustomCommStream.GetFlowControlFlags: TFlowControlFlags;
begin
  Result.XonChar := #17;
  Result.XoffChar := #19;
  Result.DSRSensitivity := False;
  Result.ControlRTS := rtsDisable;
  Result.ControlDTR := dtrDisable;//or enable like as Synaser
  Result.TxContinueOnXoff := False;
  Result.OutCTSFlow := False;
  Result.OutDSRFlow := False;
  Result.XonXoffIn := False;
  Result.XonXoffOut := False;
  case FlowControl of
    fcHardware:
      begin
        Result.ControlRTS := rtsHandshake;
        Result.OutCTSFlow := True;
      end;
    fcSoftware:
      begin
        Result.XonXoffIn := True;
        Result.XonXoffOut := True;
      end;
  end;
end;

function TmnCustomCommStream.GetParityFlags: TParityFlags;
begin
  Result.Check := False;
  Result.Replace := False;
  Result.ReplaceChar := #0;
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
      Result := DoRead(Buffer, Count)
    else
      Result := 0;
  end
  else
    Result := DoRead(Buffer, Count);
end;

function TmnCustomCommStream.ReadString: string;
var
  c: Integer;
begin
  c := 255;
  SetLength(Result, c);
  c := Read(Result[1], c);
  SetLength(Result, c);
end;

function TmnCustomCommStream.WaitRead: Boolean;
begin
  Result := False;
end;

function TmnCustomCommStream.WaitWrite: Boolean;
begin
  Result := False;
end;

procedure TmnCustomCommStream.SetDiscardNull(const Value: Boolean);
begin
  FDiscardNull := Value;
end;

procedure TmnCustomCommStream.SetEventChar(const Value: AnsiChar);
begin
  FEventChar := Value;
end;

procedure TmnCustomCommStream.SetTimeout(const Value: Cardinal);
begin
  FTimeout := Value;
end;

function TmnCustomCommStream.Write(const Buffer; Count: Integer): Integer;
begin
  Result := DoWrite(Buffer, Count);
end;

end.

