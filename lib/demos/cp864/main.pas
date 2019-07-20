unit main;

{$mode objfpc}{$H+}
{$codepage utf-8}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ucputils, ucp864, ucp1256, minibidi;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure HexStr(const s; c, n: Integer);
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;
procedure TForm1.Button3Click(Sender: TObject);
var
  ws: widestring;
  s: string;
begin
  Memo1.Lines.Add('');
  ws := UTF8Decode(Edit1.Text);
  HexStr(ws, Length(ws), 2);
  BidiString(ws);
  HexStr(ws, Length(ws), 2);
  s := ucpUnicodeToCP864(ws);
  HexStr(s, length(s), 1);
end;


procedure TForm1.HexStr(const s; c, n: Integer);
var
  i, j: Integer;
  h: string;
begin
  h := '';
  i := 0;
  while i < c do
  begin
    for j := (i * n) + n - 1 downto (i * n) do
    begin
      h := h + IntToHex(PByte(s)[j], 2);
    end;
    h := h + ' ';
    i:=i+1;
  end;
  Memo1.Lines.Add(h);
end;

end.

