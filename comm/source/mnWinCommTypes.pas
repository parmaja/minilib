unit mnWinCommTypes;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * Some function ported from ComPort at sourceforge thanks for them
 *}

 {*
   This file to share between Windows Platforms
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ifndef WINDOWS}
Error: for other platforms
{$endif}
{$ENDIF}

interface

uses
  Windows,
  Classes, SysUtils,
  mnCommClasses;

type
  TComEvent = (evRxChar, evTxEmpty, evRxFlag, evRing, evBreak, evCTS, evDSR,
    evError, evRLSD, evRx80Full);
  TComEvents = set of TComEvent;

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

const
  // auxilary constants used not defined in windows.pas
  dcb_Binary           = $00000001;
  dcb_Parity           = $00000002;
  dcb_OutxCTSFlow      = $00000004;
  dcb_OutxDSRFlow      = $00000008;
  dcb_DTRControl       = $00000030;
  dcb_DSRSensivity     = $00000040;
  dcb_TxContinueOnXoff = $00000080;
  dcb_OutX             = $00000100;
  dcb_InX              = $00000200;
  dcb_ErrorChar        = $00000400;
  dcb_Null             = $00000800;
  dcb_RTSControl       = $00003000;
  dcb_AbortOnError     = $00004000;

  cOverlapped: array[Boolean] of Cardinal = (0, FILE_FLAG_OVERLAPPED);
  cWriteThrough: array[Boolean] of Cardinal = (0, FILE_FLAG_WRITE_THROUGH);//not Integer

  cParityBits: array[TParityBits] of Integer =
    (NOPARITY, ODDPARITY, EVENPARITY, MARKPARITY, SPACEPARITY);
  cStopBits: array[TStopBits] of Integer =
    (ONESTOPBIT, ONE5STOPBITS, TWOSTOPBITS);
    
{  cBaudRate: array[TBaudRate] of Integer =
    (0, CBR_110, CBR_300, CBR_600, CBR_1200, CBR_2400, CBR_4800, CBR_9600,
     CBR_14400, CBR_19200, CBR_38400, CBR_56000, CBR_57600, CBR_115200,
     CBR_128000, CBR_256000);}
     
  cDataBits: array[TDataBits] of Integer = (5, 6, 7, 8);

  cControlRTS: array[TRTSFlowControl] of DWORD =
    (RTS_CONTROL_DISABLE shl 12,
     RTS_CONTROL_ENABLE shl 12,
     RTS_CONTROL_HANDSHAKE shl 12,
     RTS_CONTROL_TOGGLE shl 12);
     
  cControlDTR: array[TDTRFlowControl] of DWORD =
    (DTR_CONTROL_DISABLE shl 4,
     DTR_CONTROL_ENABLE shl 4,
     DTR_CONTROL_HANDSHAKE shl 4);

  { TmnWinCustomCommStream }

type
  TmnWinCustomCommStream = class(TmnCustomCommStream)
  private
    FReceiveBuffer: Integer;
    FReadTimeout: Cardinal;
    FWriteTimeout: Cardinal;
    procedure SetReceiveBuffer(AValue: Integer);
  protected
    //FHandle: THandle;
    procedure Created; override;
    function GetFlowControlFlags: TFlowControlFlags; virtual;
    function GetParityFlags: TParityFlags; virtual;
  public
    function WaitEvent(const Events: TComEvents): TComEvents; virtual;
    property ReceiveBuffer: Integer read FReceiveBuffer write SetReceiveBuffer;
    property ReadTimeout: Cardinal read FReadTimeout write FReadTimeout;
    property WriteTimeout: Cardinal read FWriteTimeout write FWriteTimeout;
  end;

const
  AllComEvents: TComEvents = [Low(TComEvent)..High(TComEvent)];

function EventsToInt(const Events: TComEvents): Integer;
function IntToEvents(Mask: Integer): TComEvents;

implementation

function EventsToInt(const Events: TComEvents): Integer;
begin
  Result := 0;
  if evRxChar in Events then
    Result := Result or EV_RXCHAR;
  if evRxFlag in Events then
    Result := Result or EV_RXFLAG;
  if evTxEmpty in Events then
    Result := Result or EV_TXEMPTY;
  if evRing in Events then
    Result := Result or EV_RING;
  if evCTS in Events then
    Result := Result or EV_CTS;
  if evDSR in Events then
    Result := Result or EV_DSR;
  if evRLSD in Events then
    Result := Result or EV_RLSD;
  if evError in Events then
    Result := Result or EV_ERR;
  if evBreak in Events then
    Result := Result or EV_BREAK;
  if evRx80Full in Events then
    Result := Result or EV_RX80FULL;
end;

function IntToEvents(Mask: Integer): TComEvents;
begin
  Result := [];
  if (EV_RXCHAR and Mask) <> 0 then
    Result := Result + [evRxChar];
  if (EV_TXEMPTY and Mask) <> 0 then
    Result := Result + [evTxEmpty];
  if (EV_BREAK and Mask) <> 0 then
    Result := Result + [evBreak];
  if (EV_RING and Mask) <> 0 then
    Result := Result + [evRing];
  if (EV_CTS and Mask) <> 0 then
    Result := Result + [evCTS];
  if (EV_DSR and Mask) <> 0 then
    Result := Result + [evDSR];
  if (EV_RXFLAG and Mask) <> 0 then
    Result := Result + [evRxFlag];
  if (EV_RLSD and Mask) <> 0 then
    Result := Result + [evRLSD];
  if (EV_ERR and Mask) <> 0 then
    Result := Result + [evError];
  if (EV_RX80FULL and Mask) <> 0 then
    Result := Result + [evRx80Full];
end;

{ TmnWinCustomCommStream }

procedure TmnWinCustomCommStream.SetReceiveBuffer(AValue: Integer);
begin
  if FReceiveBuffer <> AValue then
  begin
    FReceiveBuffer := AValue;
  end;
end;

procedure TmnWinCustomCommStream.Created;
begin
  inherited Created;
  FReceiveBuffer := 4096;
  FReadTimeout := Timeout;
  FWriteTimeout := Timeout;
end;

function TmnWinCustomCommStream.WaitEvent(const Events: TComEvents): TComEvents;
begin
  Result := [];
end;

function TmnWinCustomCommStream.GetFlowControlFlags: TFlowControlFlags;
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
  case Handshake of
    hsHardware:
      begin
        Result.ControlRTS := rtsHandshake;
        Result.OutCTSFlow := True;
      end;
    hsSoftware:
      begin
        Result.XonXoffIn := True;
        Result.XonXoffOut := True;
      end;
    hsBoth:
      begin
        Result.ControlRTS := rtsHandshake;
        Result.OutCTSFlow := True;
        Result.XonXoffIn := True;
        Result.XonXoffOut := True;
      end;
  end;
end;

function TmnWinCustomCommStream.GetParityFlags: TParityFlags;
begin
  Result.Check := False;
  Result.Replace := False;
  Result.ReplaceChar := #0;
end;

end.

