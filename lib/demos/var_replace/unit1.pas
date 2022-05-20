unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, mnUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    InEdit: TEdit;
    OutEdit: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}

{**
*  Use name values
*}


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  OutEdit.Text := VarReplace(InEdit.Text, Memo1.Lines, '$');
end;

end.

//DO $Value1,$Value2,$Value3

