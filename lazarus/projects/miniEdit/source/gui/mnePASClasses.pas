unit mnePASClasses;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  LCLintf, LCLType,
  Dialogs, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit,
  Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterPas, SynHighlighterLFM;

type

  { TmneSynPASSyn }

  TmneSynPASSyn = class(TSynPASSyn) //Only need for add sample source
  public
    function GetSampleSource: string; override;
  end;

  { TPASFile }

  TPASFile = class(TEditorFile)
  protected
    procedure NewSource; override;
  public
  end;

  TLFMFile = class(TEditorFile)
  protected
    //procedure NewSource; override;
  public
  end;

  { TPASFileCategory }

  TPASFileCategory = class(TFileCategory)
  private
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  { TLFMFileCategory }

  TLFMFileCategory = class(TFileCategory)
  private
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  { TPascalPerspective }

  TPascalPerspective = class(TEditorPerspective)
  protected
    procedure Init; override;
  public
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils;

{ TmneSynPASSyn }

function TmneSynPASSyn.GetSampleSource: string;
begin
  Result := '{ Syntax highlighting }'#13#10 +
             'procedure TForm1.Button1Click(Sender: TObject);'#13#10 +
             'var'#13#10 +
             '  Number, I, X: Integer;'#13#10 +
             'begin'#13#10 +
             '  Number := 123456;'#13#10 +
             '  Caption := ''The Number is'' + #32 + IntToStr(Number);'#13#10 +
             '  for I := 0 to Number do'#13#10 +
             '  begin'#13#10 +
             '    Inc(X);'#13#10 +
             '    Dec(X);'#13#10 +
             '    X := X + 1.0;'#13#10 +
             '    X := X - $5E;'#13#10 +
             '  end;'#13#10 +
             '  {$R+}'#13#10 +
             '  asm'#13#10 +
             '    mov AX, 1234H'#13#10 +
             '    mov Number, AX'#13#10 +
             '  end;'#13#10 +
             '  {$R-}'#13#10 +
             'end;';
end;

{ TLFMFileCategory }

function TLFMFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynLFMSyn.Create(nil);
end;

{ TPascalPerspective }

procedure TPascalPerspective.Init;
begin
  FName := 'Pascal';
  FTitle := 'Pascal project';
  FDescription := 'Pascal/FPC/Lazarus Files, *.pas, *.pp *.inc';
  FImageIndex := -1;
  AddGroup('pas', 'pas');
  AddGroup('dpr', 'pas');
  AddGroup('lpr', 'pas');
  AddGroup('ppr', 'pas');
  AddGroup('lfm', 'lfm');
  //AddGroup('inc');
end;

{ TPASFileCategory }

function TPASFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmneSynPASSyn.Create(nil);
end;

{ TPASFile }

procedure TPASFile.NewSource;
begin
  inherited NewSource;
  SynEdit.Text := 'unit ';
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('interface');
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('uses');
  SynEdit.Lines.Add('  SysUtils;');
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('implementation');
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('end.');
  SynEdit.CaretY := 1;
  SynEdit.CaretX := 5;
end;

initialization
  with Engine do
  begin
    Categories.Add('pas', TPASFile, TPASFileCategory);
    Categories.Add('lfm', TLFMFile, TLFMFileCategory);
    Groups.Add('ppr', 'Pascal Project Files', 'pas', ['ppr'], [fgkExecutable, fgkMember, fgkBrowsable], [fgsFolding]);//PPR meant Pascal project
    Groups.Add('lpr', 'Lazarus Project Files', 'pas', ['lpr'], [fgkExecutable, fgkMember, fgkBrowsable], [fgsFolding]);
    Groups.Add('dpr', 'Delphi Project Files', 'pas', ['dpr'], [fgkExecutable, fgkMember, fgkBrowsable], [fgsFolding]);
    Groups.Add('pas', 'Pascal Files', 'pas', ['pas', 'pp', 'p', 'inc'], [fgkExecutable, fgkMember, fgkBrowsable], [fgsFolding]);
    Groups.Add('lfm', 'Lazarus Form Files', 'lfm', ['lfm'], [fgkMember, fgkBrowsable], [fgsFolding]);

    Perspectives.Add(TPascalPerspective);
  end;
end.
