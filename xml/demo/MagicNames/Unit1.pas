unit Unit1;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

//stupid idea i ignored it

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function EncodeMagicName(const Name: string): string;
var
  i, p: Integer;
  Last:Boolean;
begin
  Result := '';
  Last:=False;
  p := 1;
  for i := 1 to Length(Name) do
  begin
    if (Name[i] >= 'A') and (Name[i] <= 'Z') then
    begin
      if not Last then
      begin
        if Result <> '' then
          Result := Result + '_';
        Result := Result + LowerCase(MidStr(Name, p, I - p));
        p := i;
        Last:=True;
      end;
    end
    else
      Last:=False;
  end;
  if Result <> '' then
    Result := Result + '_';
  Result := Result + LowerCase(MidStr(Name, p, MaxInt));
end;

function DecodeMagicName(const Name: string): string;
begin
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit2.Text := EncodeMagicName(Edit1.Text);

end;

end.

