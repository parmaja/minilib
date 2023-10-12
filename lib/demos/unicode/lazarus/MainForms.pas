unit MainForms;

{$mode delphi}{$H+}

interface

uses
  Windows,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  cputils, cp1250, cp1252, cp1256;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure HexIt(var h; c: Integer);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.HexIt(var h; c: Integer);
var
  i: Integer;
  b: PByte;
  s: string;
begin
  s:='';
  b := Pointer(h);
  for i := 0 to c - 1  do
  begin
    if s <> '' then
      s := s + ' ';
    s := s + IntToHex(b^, 2);
    Inc(b);
  end;
  Memo1.Lines.Add(s);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  R: TRect;
  w: widestring;
  s: ansistring;
  
  c:Char;
  wc:WideChar;
begin
  R := Rect(0,0,100, 100);
//  s:='ZAHER';
  s := 'าวๅั';
//  w := UTF8Decode(AnsiToUtf8(s));
//  c := 'า';
//  wc:= widechar(c);
//  HexIt(@wc, SizeOf(wc));
  w := AnsiToUnicode(s, cp1256_mbtowc);
  HexIt(w, Length(w) * 2);
  DrawTextW(Canvas.Handle, PWideChar(w), Length(s), R, 0);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  w: widestring;
  s: ansistring;
begin
  s := 'าวๅั';
  HexIt(s, StrLen(Pchar(s)));
  w := AnsiToUnicode(s, cp1256_mbtowc);
  HexIt(w, Length(w) * 2);
  s := UnicodeToAnsi(w, cp1256_wctomb);
  HexIt(s, Length(s));
end;

initialization
  {$I MainForms.lrs}
end.

