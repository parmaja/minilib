program stream_check;

{$H+}
{$ifdef fpc}
{$mode delphi}
{$endif}

{
  Minimal repro for the response-buffer corruption:
  TStreamHelper.WriteUTF8String used ByteLength() before the local overloads
  were declared, so FPC picked SysUtils.ByteLength(UnicodeString) (Length*2)
  for UTF8 strings and every written line was followed by garbage bytes.
  Run after building; the output below TidyWriter must be exactly the lines
  written, with no extra bytes.
}

uses
  SysUtils, Classes, mnUtils, mnStreams, mnDON;

type
  TMemBufStream = class(TmnBufferStream)
  private
    FMem: TMemoryStream;
  protected
    function DoRead(var Buffer; Count: Longint): Longint; override;
    function DoWrite(const Buffer; Count: Longint): Longint; override;
    function GetConnected: Boolean; override;
  public
    constructor Create(AMem: TMemoryStream);
  end;

constructor TMemBufStream.Create(AMem: TMemoryStream);
begin
  FMem := AMem;
  inherited Create(#13#10);
end;

function TMemBufStream.DoRead(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TMemBufStream.DoWrite(const Buffer; Count: Longint): Longint;
begin
  Result := FMem.Write(Buffer, Count);
end;

function TMemBufStream.GetConnected: Boolean;
begin
  Result := True;
end;

const
  Lines: array[0..3] of string =
    ('<!DOCTYPE html>', '<head>', '  <title>readme.md</title>', '  <link rel="icon" href="data:,">');
var
  aMem: TMemoryStream;
  aBuf: TMemBufStream;
  aWr: TmnTidyWriter;
  s: string;
  b: TBytes;
  aExpected: string;
begin
  aMem := TMemoryStream.Create;
  aBuf := TMemBufStream.Create(aMem);
  aWr := TmnTidyWriter.Create('t', aBuf);
  try
    aExpected := '';
    for s in Lines do
    begin
      aWr.WriteLn(s);
      aExpected := aExpected + s + #13#10;
    end;
    aBuf.Flush;
    SetLength(b, aMem.Size);
    if aMem.Size > 0 then
      Move(aMem.Memory^, b[0], aMem.Size);
    if aExpected = TEncoding.UTF8.GetString(b) then
    begin
      WriteLn('OK   buffer content is clean, ', aMem.Size, ' bytes');
      Halt(0);
    end
    else
    begin
      WriteLn('FAIL expected ', Length(aExpected), ' bytes, got ', aMem.Size);
      WriteLn('AS TEXT:');
      WriteLn(TEncoding.UTF8.GetString(b));
      Halt(1);
    end;
  finally
    aWr.Free;
    aBuf.Free;
    aMem.Free;
  end;
end.
