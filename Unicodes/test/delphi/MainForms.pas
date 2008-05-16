unit MainForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cp1256, cp1252, cputils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    procedure HexIt(h: PChar);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  R: TRect;
  w: widestring;
  s: AnsiString;
begin
  R := Rect(0,0,100, 100);
//  s:='ZAHER';
  s:='าวๅั';
  w := UTF8Decode(AnsiToUtf8(s));
//  w := AnsiToUnicode(s, cp1256_mbtowc);
  DrawTextW(Canvas.Handle, PWideChar(w), Length(s), R, 0);
  HexIt(PChar(w));
end;

procedure TForm1.HexIt(h: PChar);
var
  i: Integer;
  b: Byte;
  s: string;
begin
  s:='';
  for i := 0 to strlen(h) do
  begin
    b := Ord(h[i]);
    if s <> '' then
      s := s+' ';
    s := s + IntToHex(b, 2);
  end;
  Edit1.Text:=s;
end;

end.
