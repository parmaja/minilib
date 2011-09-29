unit mnWinCommStreams;
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
begin
  if UseOverlapped then
    FCancelEvent := CreateEvent(nil, True, False, nil);

  P := PChar('\\.\' + Port);

  aMode := 0;
  case ConnectMode of
    ccmReadWrite: aMode := GENERIC_READ or GENERIC_WRITE;
    ccmRead: aMode := GENERIC_READ;
    ccmWrite: aMode := GENERIC_WRITE;
  end;
  
  f := CreateFile(P, aMode, 0, nil, OPEN_EXISTING, cWriteThrough[WriteThrough] or cOverlapped[UseOverlapped], 0);

  if (f = INVALID_HANDLE_VALUE) then
  begin
    RaiseLastOSError;
  end;

  FHandle := f;
  try
    if not SetupComm(FHandle, BufferSize, BufferSize) then
      RaiseLastOSError;

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
      RaiseLastOSError;

    aTimeouts.ReadIntervalTimeout := MAXWORD;
    aTimeouts.ReadTotalTimeoutMultiplier := ReadTimeout;
    aTimeouts.ReadTotalTimeoutConstant := ReadTimeoutConst;
    aTimeouts.WriteTotalTimeoutMultiplier := WriteTimeout;
    aTimeouts.WriteTotalTimeoutConstant := WriteTimeoutConst;

    if not SetCommTimeouts(FHandle, aTimeouts) then
      RaiseLastOSError;

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
  if Connected then
  begin
    if not ClearCommError(FHandle, Errors, @ComStat) then
      raise EComPort.Create('Clear Com Failed');
    Result := ComStat.cbInQue;
  end
  else
    Result := 0;
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
  Overlapped: TOverlapped;
  EventHandles: array[0..1] of THandle;
  Mask: DWord;
  Count: Integer;
  E: Boolean;
  R: Integer;
begin
  Result := [];
  FillChar(Overlapped, SizeOf(TOverlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, False, nil);
  EventHandles[0] := Overlapped.hEvent;
  if FCancelEvent <> 0 then
  begin
    EventHandles[1] := FCancelEvent;
    Count := 2;
  end
  else
    Count := 1;

  try
    Mask := EventsToInt(Events);
    SetCommMask(FHandle, Mask);
    E := WaitCommEvent(FHandle, Mask, @Overlapped);
    if (E) or (GetLastError = ERROR_IO_PENDING) then
    begin
      R := WaitForMultipleObjects(Count, @EventHandles,  False, Timeout);
      if (R = WAIT_OBJECT_0) then
      begin
        GetOverlappedResult(FHandle, Overlapped, Mask, False);
        Result := IntToEvents(Mask);
      end;
      E := (R = WAIT_OBJECT_0)
        or (R = WAIT_OBJECT_0 + 1) or (R = WAIT_TIMEOUT);
      SetCommMask(FHandle, 0);
    end;

    if not E then
    begin
      raise EComPort.Create('Wait Failed');
    end;
  finally
    CloseHandle(Overlapped.hEvent);
  end;
end;

function TmnOSCommStream.DoRead(var Buffer; Count: Integer): Integer;
var
  Bytes: DWORD;
  Overlapped: TOverlapped;
  EventHandles: array[0..1] of THandle;
  P: POverlapped;
  E: Cardinal;
  R: Integer;
begin
  Bytes := 0;
  Result := 0;
  P := nil;
  try
    if UseOverlapped then
    begin
      FillChar(Overlapped, Sizeof(Overlapped), 0);
      Overlapped.hEvent := CreateEvent(nil, True, True, nil);
      EventHandles[0] := Overlapped.hEvent;
      EventHandles[1] := FCancelEvent;
      P := @Overlapped;   
    end
    else
      P := nil;

    if ReadFile(FHandle, Buffer, Count, Bytes, P) then
    begin
      E := 0;
    end
    else
      E := GetLastError;

    if UseOverlapped and (E = ERROR_IO_PENDING) then
    begin
      R:= WaitForMultipleObjects(2, @EventHandles, False, Timeout);
      if R = WAIT_TIMEOUT then
      begin
        if FailTimeout then
          raise EComPort.Create('Read Timeout')
        else
          Result := 0;
      end
      else if (R = WAIT_OBJECT_0) then
      begin
        GetOverlappedResult(FHandle, Overlapped, Bytes, False);
        Result := Bytes;
      end;
    end
    else
    begin
      if E > 0 then
        RaiseLastOSError
      else
        Result := Bytes;
    end;
  finally
    if P <> nil then //it is Overlapped
      CloseHandle(Overlapped.hEvent);
  end;
end;

function TmnOSCommStream.DoWrite(const Buffer; Count: Integer): Integer;
var
  Bytes: DWORD;
  Overlapped: TOverlapped;
  EventHandles: array[0..1] of THandle;
  P: POverlapped;
  E: Cardinal;
  R: Integer;
begin
  Bytes := 0;
  Result := 0;
  P := nil;
  try
    if UseOverlapped then
    begin
      FillChar(Overlapped, Sizeof(Overlapped), 0);
      Overlapped.hEvent := CreateEvent(nil, True, True, nil);
      EventHandles[0] := Overlapped.hEvent;
      EventHandles[1] := FCancelEvent;
      P := @Overlapped;
    end
    else
      P := nil;

    if WriteFile(FHandle, Buffer, Count, Bytes, P) then
    begin
      E := 0;
    end
    else
      E := GetLastError;

    if UseOverlapped and (E = ERROR_IO_PENDING) then
    begin
      R:= WaitForMultipleObjects(2, @EventHandles, False, Timeout);
      if R = WAIT_TIMEOUT then
      begin
        if FailTimeout then
          raise EComPort.Create('Read Timeout')
        else
          Result := 0;
      end
      else if (R = WAIT_OBJECT_0) then
      begin
        GetOverlappedResult(FHandle, Overlapped, Bytes, False);
        Result := Bytes;
      end;
    end
    else
    begin
      if E > 0 then
        RaiseLastOSError
      else
        Result := Bytes;
    end;
  finally
    if P <> nil then //it is Overlapped
      CloseHandle(Overlapped.hEvent);
  end;
end;

end.
