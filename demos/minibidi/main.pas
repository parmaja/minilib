unit main;
{$codepage utf8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,  minibidi;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
  public
    procedure Draw(s: WideString);
  end;

var
  Form1: TForm1; 

implementation

uses
  mnUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  w: widestring;
begin
  Panel1.Canvas.Clear;
  w := UTF8Decode(Edit1.Text);
  BidiString(w, true, false, bdnContext, bdpRightToLeft);
  Draw(w);
end;

procedure TForm1.Draw(s: WideString);
var
  t: widestring;
  u: utf8string;
  i: Integer;
  x, y, w: Integer;
  st: TTextStyle;
begin
  x := 0;
  y := 50;
  InitMemory(st, SizeOf(st));
  for i := 1 to Length(s) do
  begin
    t := s[i];
    u := UTF8Encode(t);
    w := Panel1.Canvas.TextWidth(u);
    Panel1.Canvas.TextRect(Panel1.ClientRect, x, y, u, st);
    x := x + w;
  end;
end;

end.

