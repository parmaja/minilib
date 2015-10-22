unit CSVOptionsForms;
{$mode objfpc}{$H+}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls,
  mnStreams, mncCSV;

type

  { TCSVOptionsForm }

  TCSVOptionsForm = class(TForm)
    CancelBtn: TButton;
    ANSIFileChk: TCheckBox;
    SkipColumnEdit: TEdit;
    HeaderList: TComboBox;
    EOLCharList: TComboBox;
    Label1: TLabel;
    QuoteCharLbl: TLabel;
    QuoteCharLbl1: TLabel;
    QuoteCharLbl2: TLabel;
    QuoteCharLbl3: TLabel;
    QuoteCharList: TComboBox;
    OkBtn: TButton;
    DelimiterList: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
  public
  end;

function ShowCSVOptions(Title: string; var vCSVIE: TmncCSVOptions): Boolean;

implementation

{$R *.lfm}

function ShowCSVOptions(Title: string; var vCSVIE: TmncCSVOptions): Boolean;
var
  s: string;
  c: Char;
begin
  with TCSVOptionsForm.Create(Application) do
  begin
    Caption := Title;

    QuoteCharList.Text := vCSVIE.QuoteChar;
    DelimiterList.Text := vCSVIE.DelimiterChar;
    HeaderList.ItemIndex := Ord(vCSVIE.HeaderLine);
    ANSIFileChk.Checked := vCSVIE.ANSIContents;
    SkipColumnEdit.Text := IntToStr(vCSVIE.SkipColumn);
    if vCSVIE.EndOfLine = sWinEndOfLine then
      EOLCharList.ItemIndex := 0
    else if vCSVIE.EndOfLine = sUnixEndOfLine then
      EOLCharList.ItemIndex := 1
    else
      EOLCharList.ItemIndex := 0;

    Result := ShowModal = mrOK;
    if Result then
    begin
      case HeaderList.ItemIndex of
        1: vCSVIE.HeaderLine := hdrNormal;
        2: vCSVIE.HeaderLine := hdrIgnore;
        else
          vCSVIE.HeaderLine := hdrNone;
      end;
      s := DelimiterList.Text;
      if s = '' then
        c := #0
      else if LeftStr(s, 1) = '#' then
        c := Char(StrToIntDef(Copy(s, 2, MaxInt), 0))
      else
        c := s[1];
      vCSVIE.DelimiterChar := c;

      s := QuoteCharList.Text;
      if s = '' then
        vCSVIE.QuoteChar := #0
      else
        vCSVIE.QuoteChar := s[1];
      if vCSVIE.QuoteChar < #32 then
        vCSVIE.QuoteChar := #0;


      case EOLCharList.ItemIndex of
        1: vCSVIE.EndOfLine := #10;
        2: vCSVIE.EndOfLine := #13;
        else
          vCSVIE.EndOfLine := #13#10;
      end;
      vCSVIE.ANSIContents := ANSIFileChk.Checked;
      vCSVIE.SkipColumn := StrToIntDef(SkipColumnEdit.Text, 0);
    end;
  end;
end;

{ TCSVOptionsForm }

procedure TCSVOptionsForm.FormCreate(Sender: TObject);
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

procedure TCSVOptionsForm.OkBtnClick(Sender: TObject);
begin

end;

end.

