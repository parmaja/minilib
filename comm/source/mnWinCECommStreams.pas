unit mnWinCECommStreams;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * Some function ported from ComPort at sourceforge thanks for them
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Windows,
  Classes, SysUtils,
  mnStreams, mnCommClasses;

type
  { TmnOSCommStream }

  TmnOSCommStream = class(TmnCustomCommStream)
  private
    FHandle: THandle;
    FCancelEvent: THandle;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    function DoWrite(const Buffer; Count: Integer): Integer; override;
    function DoRead(var Buffer; Count: Integer): Integer; override;
    function GetFlowControlFlags: TFlowControlFlags; override;
  public
    function WaitEvent(const Events: TComEvents): TComEvents; override;
    function GetInQue: Integer; override;
    procedure Flush; override;
    procedure Purge; override;
    procedure Reset; override;
    procedure Cancel; override;
  end;

implementation

uses
  mnWinCommTypes;

procedure TmnOSCommStream.Cancel;
begin
  inherited;
  if FCancelEvent <> 0 then
  begin
    SetEvent(FCancelEvent);
    Sleep(0);
  end;
end;

procedure TmnOSCommStream.DoConnect;
var
  f: THandle;
  DCB: TDCB;
  aTimeouts: TCommTimeouts;
  P:Pointer;
  aMode: Cardinal;
  aShare: Cardinal;
begin
  {$ifdef WINCE}
  P := PWideChar(UTF8Decode((Port+':')));
  {$else}
  P := PChar('\\.\' + Port);
  {$endif}

  aMode := 0;
  case ConnectMode of
    ccmReadWrite: aMode := GENERIC_READ or GENERIC_WRITE;
    ccmRead: aMode := GENERIC_READ;
    ccmWrite: aMode := GENERIC_WRITE;
  end;

  f := CreateFile(P, aMode, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
//  cWriteThrough[WriteThrough]

  if (f = INVALID_HANDLE_VALUE) then
  begin
    RaiseLastOSError;
  end;

  FHandle := f;
  try
{    if not SetupComm(FHandle, BufferSize, BufferSize) then //some devices may not support this API.
    begin
      RaiseLastOSError;
    end;}

    FillChar(DCB, SizeOf(DCB), #0);

    DCB.DCBlength := SizeOf(TDCB);
    DCB.XonLim := BufferSize div 4;
    DCB.XoffLim := DCB.XonLim;
    DCB.EvtChar := EventChar;

    DCB.Flags := dcb_Binary;
    if DiscardNull then
      DCB.Flags := DCB.Flags or dcb_Null;

    with GetFlowControlFlags do
    begin
      DCB.XonChar := XonChar;
      DCB.XoffChar := XoffChar;
      if OutCTSFlow then
        DCB.Flags := DCB.Flags or dcb_OutxCTSFlow;
      if OutDSRFlow then
        DCB.Flags := DCB.Flags or dcb_OutxDSRFlow;
      DCB.Flags := DCB.Flags or CControlDTR[ControlDTR]
        or CControlRTS[ControlRTS];
      if XonXoffOut then
        DCB.Flags := DCB.Flags or dcb_OutX;
      if XonXoffIn then
        DCB.Flags := DCB.Flags or dcb_InX;
      if DSRSensitivity then
        DCB.Flags := DCB.Flags or dcb_DSRSensivity;
      if TxContinueOnXoff then
        DCB.Flags := DCB.Flags or dcb_TxContinueOnXoff;
    end;

    DCB.Parity := CParityBits[Parity];
    DCB.StopBits := CStopBits[StopBits];
    DCB.BaudRate := BaudRate;
    DCB.ByteSize := cDataBits[DataBits];

    with GetParityFlags do
      if Check then
      begin
        DCB.Flags := DCB.Flags or dcb_Parity;
        if Replace then
        begin
          DCB.Flags := DCB.Flags or dcb_ErrorChar;
          DCB.ErrorChar := AnsiChar(ReplaceChar);
        end;
      end;

    // apply settings
    if not SetCommState(FHandle, DCB) then
    begin
      raise EComPort.Create('Error in SetCommState '+ IntToStr(GetLastError));
//      RaiseLastOSError;
    end;

    aTimeouts.ReadIntervalTimeout := MAXWORD;
    aTimeouts.ReadTotalTimeoutMultiplier := ReadTimeout;
    aTimeouts.ReadTotalTimeoutConstant := ReadTimeoutConst;
    aTimeouts.WriteTotalTimeoutMultiplier := WriteTimeout;
    aTimeouts.WriteTotalTimeoutConstant := WriteTimeoutConst;

    if not SetCommTimeouts(FHandle, aTimeouts) then
    begin
      raise EComPort.Create('Error in SetCommTimeouts'+ IntToStr(GetLastError));
//      RaiseLastOSError;
    end;

  except
    if FHandle <> 0 then
      CloseHandle(FHandle);
    FHandle := 0;
    raise;
  end;
end;

procedure TmnOSCommStream.DoDisconnect;
begin
  if FHandle <> 0 then
  try
    FileClose(FHandle);
  finally
    FHandle := 0;
  end;
  if FCancelEvent <> 0 then
  begin
    CloseHandle(FCancelEvent);
    FCancelEvent := 0;
  end;
end;

procedure TmnOSCommStream.Flush;
begin
  inherited;
  if not Flushfilebuffers(FHandle) then
    RaiseLastOSError;
end;

function TmnOSCommStream.GetConnected: Boolean;
begin
  Result := FHandle <> 0;
end;

function TmnOSCommStream.GetInQue: Integer;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  if not ClearCommError(FHandle, Errors, @ComStat) then
    raise EComPort.Create('Clear Com Failed');
  Result := ComStat.cbInQue;
end;

procedure TmnOSCommStream.Purge;
var
  F: integer;
begin
  inherited;
  F := PURGE_TXCLEAR or PURGE_TXABORT or PURGE_RXABORT or PURGE_RXCLEAR;
  if not PurgeComm(FHandle, F) then
    RaiseLastOSError;
end;


procedure TmnOSCommStream.Reset;
begin
  inherited;
end;

function TmnOSCommStream.WaitEvent(const Events: TComEvents): TComEvents;
var
  Mask: DWord;
  E: Boolean;
begin
  Result := [];
  try
    SetCommMask(FHandle, EventsToInt(Events));
    Mask := 0;
    E := WaitCommEvent(FHandle, Mask, nil);
    if not E then
    begin
      Result := [];
      //raise EComPort.Create('Wait Failed');
    end;
      Result := IntToEvents(Mask);
  finally
    SetCommMask(FHandle, 0);
  end;
end;

function TmnOSCommStream.GetFlowControlFlags: TFlowControlFlags;
begin
  Result := inherited GetFlowControlFlags;
  //Result.ControlDTR := dtrEnable;
end;

function TmnOSCommStream.DoRead(var Buffer; Count: Integer): Integer;
var
  Bytes: DWORD;
  E: Cardinal;
begin
  Bytes := 0;
  Result := 0;
  try
    if ReadFile(FHandle, Buffer, Count, Bytes, nil) then
    begin
      E := 0;
    end
    else
      E := GetLastError;

    if E > 0 then
      RaiseLastOSError
    else
      Result := Bytes;
  finally
  end;
end;

function TmnOSCommStream.DoWrite(const Buffer; Count: Integer): Integer;
var
  Bytes: DWORD;
  E: Cardinal;
begin
  Bytes := 0;
  Result := 0;
  try
    if WriteFile(FHandle, Buffer, Count, Bytes, nil) then
    begin
      E := 0;
    end
    else
      E := GetLastError;

    if E > 0 then
      RaiseLastOSError
    else
      Result := Bytes;
  finally
  end;
end;

end.
