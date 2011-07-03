unit mnePHPClasses;

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
  SynHighlighterJScript, SynHighlighterXHTML, SynHighlighterPas,
  mneClasses;

type

  { TphpFile }

  TphpFile = class(TEditorFile)
  protected
    procedure NewSource; override;
  public
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
    function Run: Boolean; override;
  end;

  { TCssFile }

  TCssFile = class(TEditorFile)
  public
  end;

  { TJSFile }

  TJSFile = class(TEditorFile)
  public
  end;

  { THTMLFile }

  THTMLFile = class(TEditorFile)
  public
  end;

  { TPHPFileCategory }

  TPHPFileCategory = class(TFileCategory)
  private
    procedure ExtractKeywords(Files, Variables, Identifiers: TStringList);
  protected
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddCompletion(AKeyword: string; AKind: integer);
    function CreateHighlighter: TSynCustomHighlighter; override;
    procedure OnExecuteCompletion(Sender: TObject); override;
  public
    constructor Create; override;
  end;

  { TCSSFileCategory }

  TCSSFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  { TJSFileCategory }

  TJSFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  { TPHPPerspective }

  TPHPPerspective = class(TEditorPerspective)
  public
    constructor Create; override;
  end;

implementation

uses
  IniFiles, mnXMLStreams, mnUtils;

{ TPHPPerspective }

constructor TPHPPerspective.Create;
begin
  inherited;
  FTitle := 'PHP project';
  FDescription := 'PHP Files, *.php, *.inc';
  FName := 'PHP';
  FImageIndex := -1;
end;

{ TphpFile }

procedure TphpFile.NewSource;
begin
  SynEdit.Text := '<?php';
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('?>');
  SynEdit.CaretY := 2;
  SynEdit.CaretX := 3;
end;

procedure TphpFile.OpenInclude;
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;

  function TryOpen: boolean;
  begin
    aToken := Engine.ExpandFileName(aToken);
    Result := FileExists(aToken);
    if Result then
      Engine.Files.OpenFile(aToken);
  end;

begin
  inherited;
  if Engine.Files.Current <> nil then
  begin
    if Engine.Files.Current.Group.Category.Name = 'HTML/PHP' then
    begin
      P := Engine.Files.Current.SynEdit.CaretXY;
      Engine.Files.Current.SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      aToken := DequoteStr(aToken);
      if (aToken <> '') and (TtkTokenKind(aTokenType) = tkString) then
      begin
        aToken := StringReplace(aToken, '/', '\', [rfReplaceAll, rfIgnoreCase]);
        if not TryOpen then
        begin
          aToken := ExtractFileName(aToken);
          TryOpen;
        end;
      end;
    end;
  end;
end;

function TphpFile.CanOpenInclude: Boolean;
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;
begin
  Result := False;
  if (Group <> nil) then
  begin
    if Group.Category.Name = 'HTML/PHP' then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

function TphpFile.Run: Boolean;
var
  aFile: string;
  aRoot: string;
  aUrlMode: TRunMode;
begin
  Result := False;
  aFile := Name;
  if (Engine.Session.IsOpened) then
  begin
    aFile := ExpandToPath(aFile, Engine.Session.Project.RootDir);
    aUrlMode := Engine.Session.Project.RunMode;
  end
  else
  begin
    aUrlMode := prunNone;
  end;

  case aUrlMode of
    prunUrl:
    begin
      if Engine.Session.IsOpened then
      begin
        aRoot := IncludeTrailingPathDelimiter(Engine.Session.Project.RootDir);
        if SameText((Copy(aFile, 1, Length(aRoot))), aRoot) then
        begin
          aFile := Copy(aFile, Length(aRoot) + 1, MaxInt);
          aFile := IncludeSlash(Engine.Session.Project.RootUrl) + aFile;
          //ShellExecute(0, 'open', PChar(aFile), '', PChar(ExtractFilePath(aFile)), SW_SHOWNOACTIVATE);//TODO Jihad
          Result := True;
        end;
      end;
    end;
    prunConsole:
    begin
      if Engine.Options.CompilerFolder <> '' then
        aRoot := IncludeTrailingPathDelimiter(Engine.Options.CompilerFolder) + 'php.exe'
      else
        aRoot := 'php.exe';
      Result := True;
      //        ShellExecute(0, '', PChar(aRoot), PChar(aFile), PChar(ExtractFilePath(aFile)), SW_SHOWNOACTIVATE);
    end;
  end;
end;

{ TCSSFileCategory }

function TCSSFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynCSSSyn.Create(nil);
end;

{ TPHPFileCategory }

function TPHPFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynXHTMLSyn.Create(nil);
end;

procedure TPHPFileCategory.DoAddCompletion(AKeyword: string; AKind: integer);
begin
  Completion.ItemList.Add(AKeyword);
end;

procedure TPHPFileCategory.OnExecuteCompletion(Sender: TObject);
var
  aVariables: THashedStringList;
  aIdentifiers: THashedStringList;
  s: string;
  i, r: integer;
  aSynEdit: TCustomSynEdit;
  aProcessor: byte;
  aHTMLProcessor, aPHPProcessor: integer;
  aTokenType, aStart: integer;
  aRange: pointer;
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aFiles: TStringList;
begin
  inherited;
  Screen.Cursor := crHourGlass;
  try
    Completion.ItemList.Clear;
    aSynEdit := (Sender as TSynCompletion).TheForm.CurrentEditor as TCustomSynEdit;
    if (aSynEdit <> nil) and (Highlighter is TSynXHTMLSyn) then
    begin
      aPHPProcessor := (Highlighter as TSynXHTMLSyn).Processors.IndexOf('php');
      aHTMLProcessor := (Highlighter as TSynXHTMLSyn).Processors.IndexOf('html');
      P := aSynEdit.CaretXY;
      GetHighlighterAttriAtRowColEx2(aSynEdit, P, S, aTokenType, aStart, Attri, aRange);
      aProcessor := RangeToProcessor(PtrUInt(aRange));
      if aTokenType = Ord(tkProcessor) then
        Abort
      //CanExecute := False
      else if aProcessor = aHTMLProcessor then
      begin
        Completion.TheForm.Caption := 'HTML';
        EnumerateKeywords(Ord(tkKeyword), sHTMLKeywords, Highlighter.IdentChars, @DoAddCompletion);
      end
      else if aProcessor = aPHPProcessor then
      begin
        if aTokenType = Ord(tkComment) then
          Abort
        //CanExecute := False
        else if aTokenType = Ord(tkString) then
        begin
          EnumerateKeywords(Ord(tkSQL), sSQLKeywords, Highlighter.IdentChars, @DoAddCompletion);
        end
        else
        begin
          Completion.TheForm.Caption := 'PHP';
          //load keyowrds
          EnumerateKeywords(Ord(tkKeyword), sPHPControls, Highlighter.IdentChars, @DoAddCompletion);
          EnumerateKeywords(Ord(tkKeyword), sPHPKeywords, Highlighter.IdentChars, @DoAddCompletion);
          EnumerateKeywords(Ord(tkFunction), sPHPFunctions, Highlighter.IdentChars, @DoAddCompletion);
          EnumerateKeywords(Ord(tkValue), sPHPConstants, Highlighter.IdentChars, @DoAddCompletion);
          // load a variable
          aVariables := THashedStringList.Create;
          aIdentifiers := THashedStringList.Create;

          //Add system variables
          ExtractStrings([','], [], PChar(sPHPVariables), aVariables);
          for i := 0 to aVariables.Count - 1 do
            aVariables[i] := '$' + aVariables[i];

          //extract keywords from external files
          if (Engine.Session.IsOpened) and (Engine.Session.Project.RootDir <> '') then
          begin
            if Engine.Options.CollectAutoComplete then
            begin
              if ((GetTickCount - Engine.Session.Project.CachedAge) > (Engine.Options.CollectTimeout * 1000)) then
              begin
                Engine.Session.Project.CachedVariables.Clear;
                Engine.Session.Project.CachedIdentifiers.Clear;
                aFiles := TStringList.Create;
                try
                  EnumFileList('', Engine.Session.Project.RootDir, '*.php', aFiles, 1000, Engine.Session.IsOpened);
                  r := aFiles.IndexOf(Engine.Files.Current.Name);
                  if r >= 0 then
                    aFiles.Delete(r);
                  ExtractKeywords(aFiles, Engine.Session.Project.CachedVariables, Engine.Session.Project.CachedIdentifiers);
                finally
                  aFiles.Free;
                end;
              end;
              aVariables.AddStrings(Engine.Session.Project.CachedVariables);
              aIdentifiers.AddStrings(Engine.Session.Project.CachedIdentifiers);
              Engine.Session.Project.CachedAge := GetTickCount;
            end;
          end;
          //add current file variables
          try
            Highlighter.ResetRange;
            for i := 0 to aSynEdit.Lines.Count - 1 do
            begin
              Highlighter.SetLine(aSynEdit.Lines[i], 1);
              while not Highlighter.GetEol do
              begin
                if (Highlighter.GetTokenPos <> (aStart - 1)) and (RangeToProcessor(PtrUInt(Highlighter.GetRange)) = aPHPProcessor) then
                begin
                  if (Highlighter.GetTokenKind = Ord(tkVariable)) then
                  begin
                    s := Highlighter.GetToken;
                    if (s <> '$') and (aVariables.IndexOf(s) < 0) then
                    begin
                      aVariables.Add(s);
                    end;
                  end
                  else if (Highlighter.GetTokenKind = Ord(tkIdentifier)) then
                  begin
                    s := Highlighter.GetToken;
                    if aIdentifiers.IndexOf(s) < 0 then
                    begin
                      aIdentifiers.Add(s);
                    end;
                  end;
                end;
                Highlighter.Next;
              end;
            end;

            for i := 0 to aVariables.Count - 1 do
              DoAddCompletion(aVariables[i], Ord(tkVariable));

            for i := 0 to aIdentifiers.Count - 1 do
              DoAddCompletion(aIdentifiers[i], Ord(tkIdentifier));
          finally
            aIdentifiers.Free;
            aVariables.Free;
          end;
        end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

constructor TPHPFileCategory.Create;
begin
  inherited Create;
end;

procedure TPHPFileCategory.ExtractKeywords(Files, Variables, Identifiers: TStringList);
var
  aFile: TStringList;
  s: string;
  i, f: integer;
  aPHPProcessor: integer;
  aHighlighter: TSynXHTMLSyn;
begin
  aHighlighter := TSynXHTMLSyn.Create(nil);
  aFile := TStringList.Create;
  try
    aPHPProcessor := aHighlighter.Processors.Find('php').Index;
    for f := 0 to Files.Count - 1 do
    begin
      aFile.LoadFromFile(Files[f]);
      aHighlighter.ResetRange;
      for i := 0 to aFile.Count - 1 do
      begin
        aHighlighter.SetLine(aFile[i], 1);
        while not aHighlighter.GetEol do
        begin
          if (RangeToProcessor(PtrUInt(aHighlighter.GetRange)) = aPHPProcessor) then
          begin
            if (aHighlighter.GetTokenKind = Ord(tkVariable)) then
            begin
              s := aHighlighter.GetToken;
              if (s <> '$') and (Variables.IndexOf(s) < 0) then
                Variables.Add(s);
            end
            else if (aHighlighter.GetTokenKind = Ord(tkIdentifier)) then
            begin
              s := aHighlighter.GetToken;
              if Identifiers.IndexOf(s) < 0 then
                Identifiers.Add(s);
            end;
          end;
          aHighlighter.Next;
        end;
      end;
    end;
  finally
    aHighlighter.Free;
    aFile.Free;
  end;
end;

procedure TPHPFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  FCompletion := TSynCompletion.Create(nil);
  FCompletion.Width := 340;
  FCompletion.EndOfTokenChr := '{}()[].<>/\:!$&*+-=%';
  FCompletion.OnExecute := @OnExecuteCompletion;
  FCompletion.ShortCut := scCtrl + VK_SPACE;
  FCompletion.CaseSensitive := False;
  //Result.Options := [scoLimitToMatchedText, {scoCaseSensitive, }scoUseInsertList, scoUsePrettyText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter];
  //Result.DefaultType := ctCode;
  FCompletion.AddEditor(vSynEdit);
end;

{ TJSFileCategory }

function TJSFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynJScriptSyn.Create(nil);
end;

initialization
  with Engine do
  begin
    Categories.Add('HTML/PHP', TphpFile, TPHPFileCategory, [fckPublish]);
    Categories.Add('CSS', TCssFile, TCSSFileCategory, [fckPublish]);
    Categories.Add('JS', TJSFile, TJSFileCategory, [fckPublish]);
    Groups.Add('PHP Files', 'php', 'HTML/PHP', ['php'], [fgkExecutable, fgkPublish, fgkBrowsable, fgkMainIcon]);
    Groups.Add('PHPX Files', 'phpx', 'HTML/PHP', ['phpx'], [fgkExecutable, fgkPublish, fgkBrowsable, fgkMainIcon]);
    Groups.Add('HTML Files', 'html', 'HTML/PHP', ['html', 'tpl'], [fgkPublish, fgkBrowsable]);
    Groups.Add('CSS Files', 'css', 'CSS', ['css'], [fgkPublish, fgkBrowsable]);
    Groups.Add('Java Script Files', 'js', 'JS', ['js'], [fgkPublish, fgkBrowsable]);
    Perspectives.Add(TPHPPerspective);
    DefaultGroup := 'PHP';
  end;
end.

