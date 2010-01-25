unit CSVIEForms;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls,
  sqlvClasses, mncCSVExchanges;

type

  { TCSVIEForm }

  TCSVIEForm = class(TForm)
    CancelBtn: TButton;
    ANSIFileChk: TCheckBox;
    HeaderList: TComboBox;
    EOLCharList: TComboBox;
    Label1: TLabel;
    QuoteCharLbl: TLabel;
    QuoteCharLbl1: TLabel;
    QuoteCharLbl2: TLabel;
    QuoteCharList: TComboBox;
    OkBtn: TButton;
    DelimiterList: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

function ShowCSVIEOptions(Title: string; CSVIE: TmncCSVIE): Boolean;

implementation

{$R *.lfm}

function ShowCSVIEOptions(Title: string; CSVIE: TmncCSVIE): Boolean;
var
  s: string;
  c: Char;
begin
  with TCSVIEForm.Create(Application) do
  begin
    Caption := Title;
    QuoteCharList.Text := sqlvEngine.Setting.CSVQuoteChar;
    DelimiterList.Text := sqlvEngine.Setting.CSVDelimiterChar;
    HeaderList.ItemIndex := Ord(sqlvEngine.Setting.CSVHeader);
    ANSIFileChk.Checked := sqlvEngine.Setting.CSVANSIContents;
    Result := ShowModal = mrOK;
    if Result then
    begin
      case HeaderList.ItemIndex of
        1: CSVIE.Header := hdrNormal;
        2: CSVIE.Header := hdrIgnore;
        else
          CSVIE.Header := hdrNone;
      end;
      s := DelimiterList.Text;
      if s = '' then
        c := #0
      else if LeftStr(s, 1) = '#' then
        c := Char(StrToIntDef(Copy(s, 2, MaxInt), 0))
      else
        c := s[1];
      CSVIE.Delimiter := c;

      s := QuoteCharList.Text;
      if s = '' then
        CSVIE.QuoteChar := #0
      else
        CSVIE.QuoteChar := s[1];


      case HeaderList.ItemIndex of
        1: CSVIE.EndOfLine := #10;
        2: CSVIE.EndOfLine := #13;
        else
          CSVIE.EndOfLine := #13#10;
      end;
      CSVIE.ANSIContents := ANSIFileChk.Checked;

      sqlvEngine.Setting.CSVQuoteChar := CSVIE.QuoteChar;
      sqlvEngine.Setting.CSVDelimiterChar := CSVIE.Delimiter;
      sqlvEngine.Setting.CSVHeader := CSVIE.Header;
      sqlvEngine.Setting.CSVANSIContents := CSVIE.ANSIContents;
      sqlvEngine.SaveSetting;
    end;
  end;
end;

{ TCSVIEForm }

procedure TCSVIEForm.FormCreate(Sender: TObject);
begin
  HeaderList.Items.Add('No Header');
  HeaderList.Items.Add('Header contain fields');
  HeaderList.Items.Add('Ignoer header in import');
  HeaderList.ItemIndex := 0;

  DelimiterList.Items.Add(';');
  DelimiterList.Items.Add(',');
  DelimiterList.Items.Add('|');
  DelimiterList.Items.Add('#9');
  DelimiterList.Text := ';';
  DelimiterList.ItemIndex := 0;

  EOLCharList.Items.Add('Windows');
  EOLCharList.Items.Add('Unix');
  EOLCharList.Items.Add('Mac');
  EOLCharList.ItemIndex := 0;

  QuoteCharList.Items.Add('');
  QuoteCharList.Items.Add('"');
  QuoteCharList.Items.Add('''');
  EOLCharList.ItemIndex := 0;
end;

end.

