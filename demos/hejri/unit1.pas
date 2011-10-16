unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DateUtils, HejriDates, HejriUtils, UniDates;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button3: TButton;
    ToCbo: TComboBox;
    FromCbo: TComboBox;
    FromEdit: TEdit;
    ToEdit: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  UniDate.EnumItems(FromCbo.Items);
  UniDate.EnumItems(ToCbo.Items);
  FromCbo.ItemIndex := 0;
  ToCbo.ItemIndex := 0;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
  dt, dt2: TDateTime;
  y, m, d: word;
  y2, m2, d2: word;
begin
  dt := Trunc(Now);
  for i := 0 to 1000 - 1 do
  begin
    Hejri_DecodeDate(dt, y, m, d);
    y2:=y;
    m2:=m;
    d2:=d;
    dt2 := Trunc(Hejri_EncodeDate(y, m, d));
    if dt2 <> dt then
    begin
       memo1.Lines.Add(FormatDateTime('YYYY-MM-DD', dt2) + ' -> '+ IntToStr(y) +'-'+ IntToStr(m)+'-'+IntToStr(d) + ' <> '+ IntToStr(y) +'-'+ IntToStr(m)+'-'+IntToStr(d));
    end;
    dt := dt - 1;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  dt: TDateTime;
begin
  dt := udtISOStrToDate(UniDate.Items[FromCbo.ItemIndex], FromEdit.Text);
  ToEdit.Text := udtISODateToStr(UniDate.Items[ToCbo.ItemIndex], dt);
end;

end.

