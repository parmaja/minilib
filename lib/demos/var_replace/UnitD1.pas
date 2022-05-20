unit UnitD1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  mnUtils;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Memo2: TMemo;
    Memo3: TMemo;
    Button2: TButton;
    InEdit: TEdit;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure Run(Prefix: string; Suffix: string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Run('$', '');
end;

procedure TForm1.Run(Prefix: string; Suffix: string);
var
  i: Integer;
begin
  Memo3.Clear;
  for i := 0 to Memo2.Lines.Count -1 do
    Memo3.Lines.Add(VarReplace(Memo2.Lines[i], Memo1.Lines, Prefix, Suffix));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo3.Lines.Add(VarReplace(InEdit.Text, Memo1.Lines, '$', ''));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Run('{#', '}');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Run('$', '');
end;

end.
