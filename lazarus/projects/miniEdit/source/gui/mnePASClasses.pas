unit mnePASClasses;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

{$DEFINE SYN_HEREDOC}

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  LCLintf, LCLType,
  Dialogs, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit,
  Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterCSS, SynHighlighterSQL, SynHighlighterXML, SynHighlighterApache,
  SynHighlighterJScript, SynHighlighterHTMLPHP, SynHighlighterPas;

type
  { TPASFile }

  TPASFile = class(TEditorFile)
  protected
    procedure NewSource; override;
  public
  end;

  { TPASFileCategory }

  TPASFileCategory = class(TFileCategory)
  private
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  { TPascalPerspective }

  TPascalPerspective = class(TEditorPerspective)
  public
    class procedure GetAttributes(var PerspectiveAttributes: TPerspectiveAttributes); override;
  end;

implementation

uses
  IniFiles, mnXMLStreams, mnUtils;

{ TPascalPerspective }

class procedure TPascalPerspective.GetAttributes(var PerspectiveAttributes: TPerspectiveAttributes);
begin
  PerspectiveAttributes.Title := 'Pascal project';
  PerspectiveAttributes.Description := 'Pascal/FPC/Lazarus Files, *.pas, *.pp *.inc';
  PerspectiveAttributes.Name := 'Pascal';
  PerspectiveAttributes.ImageIndex := -1;
end;

{ TPASFileCategory }

function TPASFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynPASSyn.Create(nil);
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
    Categories.Add('PAS', TPASFile, TPASFileCategory);
    Perspectives.Add(TPascalPerspective);
  end;
end.

