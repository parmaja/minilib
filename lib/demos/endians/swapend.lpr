program swapend;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  mnUtils;

var
  I: SmallInt;
  Q: Int64;

procedure Reset;
begin
  I := 1;
  Q := 1 + 2 shl 8;
end;

begin

  Reset;
	Writeln(DataToBinStr(Q, SizeOf(Q), '-'));
  Reset;
	Q := SwapEndian(Q);
  Writeln(DataToBinStr(Q, SizeOf(Q), '-'));
  Reset;
  Q := SwapBytes(Q);
  Writeln(DataToBinStr(Q, SizeOf(Q), '-'));

  Writeln();

  Reset;
  Writeln(DataToBinStr(I, SizeOf(I), '-'));
  Reset;
  I := SwapEndian(I);
  Writeln(DataToBinStr(I, SizeOf(I), '-'));
  Reset;
  I := SwapBytes(I);
  Writeln(DataToBinStr(I, SizeOf(I), '-'));

  readln();
end.

