unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.Edit, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.DialogService,
  SendAndRecv, FMX.Layouts, FMX.ListBox;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

  procedure OnWriteLn(S: string);
  procedure OnWrite(S: string);
  procedure OnReadLn(var S: UTF8String);

implementation

{$R *.fmx}

procedure OnWriteLn(S: string);
begin
  Form1.ListBox1.Items.Add(s);
  Form1.ListBox1.ItemIndex := Form1.ListBox1.Items.Count-1;
end;

procedure OnWrite(S: string);
begin
  Form1.ListBox1.Items.Add(s);
  Form1.ListBox1.ItemIndex := Form1.ListBox1.Items.Count-1;
end;

procedure OnReadLn(var S: utf8string);
begin
  s := InputBox('Test', 'Enter Value', '')
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SendAndRecv.Application := TTestStream.Create;
  SendAndRecv.Application.Run;
  SendAndRecv.Application.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WriteLn := Onwriteln;
  ReadLn := Onreadln;
  Write := OnWrite;
end;

end.
