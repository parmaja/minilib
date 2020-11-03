unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Clipbrd, MiniBidi;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    InputEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OutputEdit: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  ws, ows: widestring;
  c: widechar;
begin
  ws := InputEdit.Text;
  BidiString(ws, [bdoApplyShape, bdoReorderCombining], bdnContext, bdpRightToLeft);
  ows := '';
  for c in ws do
  begin
    if c <> widestring(' ') then
      ows := c + ows;
  end;
  OutputEdit.Text := ows;
  Clipboard.AsText := oWS;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ws: widestring;
  c: widechar;
begin
  ws := InputEdit.Text;
  BidiString(ws, [bdoApplyShape, bdoReorderCombining], bdnContext, bdpRightToLeft);
  OutputEdit.Text := ws;
  Clipboard.AsText := WS;
end;

end.

