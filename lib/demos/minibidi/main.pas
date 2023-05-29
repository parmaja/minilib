unit main;

{$codepage utf8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Clipbrd,
  ExtCtrls, minibidi;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    TemplatesCbo: TComboBox;
    Edit1: TEdit;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure TemplatesCboClick(Sender: TObject);
    procedure TemplatesCboSelect(Sender: TObject);
  private
    procedure DrawNow;
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
begin
  DrawNow;
end;

procedure TForm1.DrawNow;
var
  w: WideString;
begin
  Panel1.Canvas.Clear;
  w := UTF8Decode(Edit1.Text);
  w := BidiString(w, [bdoApplyShape], bdnContext, bdpRightToLeft);
  Draw(w);
end;

procedure TForm1.TemplatesCboClick(Sender: TObject);
begin
end;

procedure TForm1.TemplatesCboSelect(Sender: TObject);
begin
  if TemplatesCbo.ItemIndex >= 0 then
  begin
    Edit1.Text := TemplatesCbo.Items[TemplatesCbo.ItemIndex];
    DrawNow;
  end;
end;

procedure TForm1.Draw(s: WideString);
var
  t: WideString;
  u: utf8string;
  x, y, w: Integer;
  st: TTextStyle;
begin
  Clipbrd.Clipboard.AsText := s;
  x := 0;
  y := 50;

  Panel1.Canvas.Font := Panel1.Font;
  Initialize(st);
  st.Opaque := False;
  st.Alignment := taLeftJustify;
  for t in s do
  begin
    u := UTF8Encode(t);
    w := Panel1.Canvas.TextWidth(u);

    Panel1.Canvas.TextRect(Panel1.ClientRect, x, y, u, st);
    x := x + w;
  end;
end;

end.
