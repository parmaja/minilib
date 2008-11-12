unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Tea;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  v: QWord;
  k:TTeaKey;
begin
  k[0] := 10;
  k[1] := 10;
  k[2] := 10;
  k[3] := 10;
  v := StrToInt64Def(Edit1.Text, 0);
  TeaEncrypt(v, k);
  Edit2.Text:= IntToStr(v);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  v: QWord;
  k:TTeaKey;
begin
  k[0] := 10;
  k[1] := 10;
  k[2] := 10;
  k[3] := 10;
  v := StrToInt64Def(Edit2.Text, 0);
  TeaDecrypt(v, k);
  Edit1.Text:= IntToStr(v);
end;

initialization
  {$I Unit1.lrs}

end.

