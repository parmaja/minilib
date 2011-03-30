unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  LangClasses, LangUtils, PO_LangParser;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure ListLang(Contents: TPO_LangContents);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  StringHashList;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Lang: TLanguage;
  Parser: TPO_Parser;
  Contents: TPO_LangContents;
  Strings: TStringList;
begin
  Lang := TLanguage.Create;
  Contents := TPO_LangContents.Create;
  Strings := TStringList.Create;
  Parser := TPO_Parser.Create(Contents);
  try
    Parser.Contents := Contents;
    Strings.LoadFromFile(ExtractFilePath(Application.ExeName) + 'test.ar.po');
    Parser.Parse(Strings);
    Strings.Clear;
    Parser.Generate(Strings);
    Strings.SaveToFile(ExtractFilePath(Application.ExeName) + 'test2.ar.po');
    ListLang(Contents);
  finally
    Strings.Free;
    Contents.Free;
    Lang.Free;
    Parser.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ListLang(Contents: TPO_LangContents);
var
  i: Integer;
begin
  Memo1.Clear;
  for i := 0 to Contents.Count -1 do
  begin
    if Contents[i].Reference <> '' then
      Memo1.Lines.Add('#: '+Contents[i].Reference);
    Memo1.Lines.Add(Contents[i].ID + ':' + Contents[i].Text);
  end;
end;

end.

