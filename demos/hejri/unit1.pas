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
    Button4: TButton;
    ToCbo: TComboBox;
    FromCbo: TComboBox;
    FromEdit: TEdit;
    ToEdit: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
  if ToCbo.Items.Count > 1 then
    ToCbo.ItemIndex := 1;
end;

{
  This important test the formula of Hejri functions date convert
  It test Days in Month and convert back to TDateTime days.
}
procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
  dt, old_dt, dt2: TDateTime;
  y, m, d: word;
  count: Integer;
  c, c2, old_y, old_m: Integer;
{  procedure nothing;
  begin
    //ok it is nothing :P
  end;}
begin
  Memo1.Clear;
  count := 0;
  old_m := 0;
  old_y := 0;
  old_dt := 0;
  dt := HejriEncodeDate(1,1,1);
  for i := Trunc(dt) to Trunc(Now) do
  begin
{    if count = 6 then
      nothing;}
    Hejri_DecodeDate(dt, y, m, d);

    //check the wrong of MonthDays in this month
    if (old_y <> y) or (old_m <> m) then
    begin
      if (old_m <> 0) then
      begin
        c := trunc(dt) - trunc(old_dt);
        c2 := Hejri_MonthDays(old_y, old_m);
        if c <> c2 then
        begin
          memo1.Lines.Add(IntToStr(count) + ': ' + FormatDateTime('YYYY-MM-DD', dt) + ' -> '+ IntToStr(y) +'-'+ IntToStr(m)+'-'+IntToStr(d) + ' <> '+ ' C=' + IntToStr(c) + ' C2=' + IntToStr(c2));
          exit;
        end;
      end;
      old_m := m;
      old_y := y;
      old_dt := dt;
    end;

    //check the reverse of encode/decode
    dt2 := Hejri_EncodeDate(y, m, d);
    if dt2 <> dt then
    begin
      memo1.Lines.Add(IntToStr(count) + ': ' + FormatDateTime('YYYY-MM-DD', dt) + ' -> '+ IntToStr(y) +'-'+ IntToStr(m)+'-'+IntToStr(d) + ' <> '+ IntToStr(y) +'-'+ IntToStr(m)+'-'+IntToStr(d));
      exit;
    end;
    dt := dt + 1;
    inc(count);
  end;
  memo1.Lines.Add('Test Finished');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  dt: TDateTime;
begin
  dt := Hejri_EncodeDate(1317, 8, 28);
  dt := Hejri_EncodeDate(1317, 8, 29);
  dt := Hejri_EncodeDate(1317, 8, 30);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  dt: TDateTime;
begin
  dt := udtISOStrToDate(UniDate.Items[FromCbo.ItemIndex], FromEdit.Text);
  ToEdit.Text := udtISODateToStr(UniDate.Items[ToCbo.ItemIndex], dt);
end;

end.

