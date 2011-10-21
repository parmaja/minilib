unit mnWinCommStreams;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * Some function ported from ComPort at sourceforge thanks for them

   http://www.codeproject.com/KB/system/SerialPortComm.aspx

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
  mnWinCommTypes, mnCommClasses;

type

  { TmnOSCommStream }

  TmnOSCommStream = class(TmnWinCustomCommStream)
  private
    FHandle: THandle;
    FCancelEvent: THandle;
    FUseOverlapped: Boolean;
    FWriteThrough: Boolean;
    procedure SetUseOverlapped(const Value: Boolean);
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    function InternalWrite(const Buffer; Count: Integer): Integer; override;
    function InternalRead(var Buffer; Count: Integer): Integer; override;
    procedure Created; override;
  public
    function WaitRead: Boolean; override;
    function WaitWrite: Boolean; override;
    function WaitEvent(const Events: TComEvents): TComEvents; override;
    procedure Flush; override;
    procedure Purge; override;
    function GetInQue: Integer;
    procedure Cancel;
    property WriteThrough: Boolean read FWriteThrough write FWriteThrough;
    property UseOverlapped: Boolean read FUseOverlapped write SetUseOverlapped;
  end;

implementation

procedure TmnOSCommStream.Cancel;
begin
  inherited;
  if FCancelEvent <> 0 then
  begin
    SetEvent(FCancelEvent);
    Sleep(0);
  end;
end;

procedure TmnOSCommStream.SetUseOverlapped(const Value: Boolean);
begin
  if FUseOverlapped <> Value then
  begin
    if Connected then
      raise ECommError.Create('Already connected');
    FUseOverlapped := Value;
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
    RaiseLastOSError;

  FHandle := f;
  try
    if (ReceiveBuffer > 0) then
      if not SetupComm(FHandle, ReceiveBuffer, 0) then
        RaiseLastOSError;

    DCB.DCBlength := SizeOf(TDCB);
    DCB.XonLim := ReceiveBuffer div 4;
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

    //Apply settings
    if not SetCommState(FHandle, DCB) then
      RaiseLastOSError;

    aTimeouts.ReadIntervalTimeout := MAXWORD;
    aTimeouts.ReadTotalTimeoutMultiplier := 0;
    aTimeouts.ReadTotalTimeoutConstant := ReadTimeout;
    aTimeouts.WriteTotalTimeoutMultiplier := 0;
    aTimeouts.WriteTotalTimeoutConstant := WriteTimeout;

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
  if not FlushFileBuffers(FHandle) then
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
    Errors := 0;
    if not ClearCommError(FHandle, Errors, @ComStat) then
      raise ECommError.Create('Clear Com Failed');
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

function TmnOSCommStream.WaitEvent(const Events: TComEvents): TComEvents;
var
  Overlapped: TOverlapped;
  P: POverlapped;
  EventHandles: array[0..1] of THandle;
  Mask: DWord;
  Count: Integer;
  E: Boolean;
  R: Integer;
begin
  Result := [];
  if UseOverlapped then
  begin
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
    P := @Overlapped;
  end
  else
    P := nil;

  try
    Mask := EventsToInt(Events);
    SetCommMask(FHandle, Mask);
    Mask := 0;
    E := WaitCommEvent(FHandle, Mask, P);
    if UseOverlapped then
    begin
      if E or (GetLastError = ERROR_IO_PENDING) then
      begin
        R := WaitForMultipleObjects(Count, @EventHandles,  False, Timeout);
        if (R = WAIT_OBJECT_0) then
        begin
          GetOverlappedResult(FHandle, Overlapped, Mask, False);
          Result := IntToEvents(Mask);
        end;
        E := (R = WAIT_OBJECT_0) or (R = WAIT_OBJECT_0 + 1) or (R = WAIT_TIMEOUT);
        SetCommMask(FHandle, 0);
      end;
      if not E then
        raise ECommError.Create('Wait Failed');
    end
    else
    begin
      if not E then
        raise ECommError.Create('Wait Failed');
      Result := IntToEvents(Mask);
    end;
  finally
    if UseOverlapped then
      CloseHandle(Overlapped.hEvent);
  end;
end;

function TmnOSCommStream.InternalRead(var Buffer; Count: Integer): Integer;
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
      E := 0
    else
      E := GetLastError;

    if UseOverlapped and (E = ERROR_IO_PENDING) then
    begin
      R:= WaitForMultipleObjects(2, @EventHandles, False, Timeout);
      if R = WAIT_TIMEOUT then
      begin
        if FailTimeout then
          raise ECommError.Create('Read Timeout')
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

procedure TmnOSCommStream.Created;
begin
  inherited;
end;

function TmnOSCommStream.WaitRead: Boolean;
var
  Ev: TComEvents;
begin
  Ev := WaitEvent([evRxChar]);
  Result := Ev <> [];//(Ev = [evRxChar]) or (Ev = [evTxEmpty]);
end;

function TmnOSCommStream.WaitWrite: Boolean;
var
  Ev: TComEvents;
begin
  Ev := WaitEvent([evTxEmpty]);
  Result := Ev = [evTxEmpty];
end;

function TmnOSCommStream.InternalWrite(const Buffer; Count: Integer): Integer;
var
  Bytes: DWORD;
  Overlapped: TOverlapped;
  EventHandles: array[0..1] of THandle;
  P: POverlapped;
  E: Cardinal;
  R: Integer;
{  Errors: DWORD;
  ComStat: TComStat;}
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
{      Errors := 0;
      if not ClearCommError(FHandle, Errors, @ComStat) then
        raise ECommError.Create('Clear Com Failed');}
    end
    else
      E := GetLastError;

    if UseOverlapped and (E = ERROR_IO_PENDING) then
    begin
      R:= WaitForMultipleObjects(2, @EventHandles, False, Timeout);
      if R = WAIT_TIMEOUT then
      begin
        if FailTimeout then
          raise ECommError.Create('Read Timeout')
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

