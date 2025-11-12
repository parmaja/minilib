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
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    ListBox1: TListBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
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
  Edit2.Text := SubStr(Edit1.Text, ',', StrToIntDef(Edit3.Text, 0));
end;

procedure AddOutput(Sender: Pointer; Index: Integer; S: string; var Resume: Boolean);
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

procedure AddOutputEx(Sender: Pointer; Index, CharIndex, NextIndex: Integer; S: string; var Resume: Boolean);
begin
  with TObject(Sender) as TForm1 do
  begin
    if (Index = 0) and (ListBox1.Items.Count > 0) then
      ListBox1.Items[ListBox1.Items.Count -1] := ListBox1.Items[ListBox1.Items.Count -1] + S
    else
      ListBox1.Items.Add(S);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  C: Integer;
begin
  ListBox1.Items.Clear;
  StrToStringsExCallback(Memo1.Text, 1, self, ['::', ';', #13#10, #13, #10, #0], C, @AddOutputEx);
end;

var
  CharIndex: Integer = 0;

procedure TForm1.Button5Click(Sender: TObject);
var
  s: string;
  C, Start: Integer;
begin
  StrScanTo(Memo1.Text, CharIndex, S, Start, CharIndex, C, ['::', ';', #13#10, #13, #10, #0]);
  ListBox1.Items.Add(S + ' at ' + IntToStr(CharIndex));
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  ListBox1.Clear;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  l, r: string;
begin
  l := SplitPath(Edit1.Text, r, StrToIntDef(Edit3.Text, 0));
  Memo1.Clear;;
  edit2.Text := l;
  Memo1.Lines.Add(l);
  Memo1.Lines.Add(r);
end;

end.

