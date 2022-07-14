unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    ListBox1: TListBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
  public
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
  edit2.Text := SubStr(Edit1.Text, ',', StrToIntDef(Edit3.Text, 0));
end;

procedure AddOutput(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);
begin
  with TObject(Sender) as TForm1 do
  begin
    if (Index = 0) and (ListBox1.Items.Count > 0) then
      ListBox1.Items[ListBox1.Items.Count -1] := ListBox1.Items[ListBox1.Items.Count -1] + S
    else
      ListBox1.Items.Add(S);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s: string;
begin
  s := 'test1'#13'test2'#13'test3'#13;
  StrToStringsCallback(s, self, @AddOutput, [#13]);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  s: string;
begin
  s := 'tt'#13'test4';
  StrToStringsCallback(s, self, @AddOutput, [#13]);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ListBox1.Items.Clear;
  StrToStringsExCallback(Memo1.Text, self, @AddOutput, ['::', ';', #13#10, #13, #10, #0]);
end;

end.

