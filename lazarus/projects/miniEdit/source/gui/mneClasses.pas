unit mneClasses;

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

  { TphpFile }

  TphpFile = class(TEditorFile)
  protected
    procedure NewSource; override;
  public
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
    function Run: Boolean; override;
  end;

  TCssFile = class(TEditorFile)
  public
  end;

  TJSFile = class(TEditorFile)
  public
  end;

  THTMLFile = class(TEditorFile)
  public
  end;

  TSQLFile = class(TEditorFile)
  public
  end;

  TApacheFile = class(TEditorFile)
  public
  end;

  TINIFile = class(TEditorFile)
  public
  end;

  TTXTFile = class(TEditorFile)
  public
  end;

  TXMLFile = class(TEditorFile)
  public
    procedure NewSource; override;
  end;

  { TPASFile }

  TPASFile = class(TEditorFile)
  protected
    procedure NewSource; override;
  public
  end;

  { TPHPFileCategory }

  TPHPFileCategory = class(TFileCategory)
  private
    procedure ExtractKeywords(Files, Variables, Identifiers: TStringList);
  protected
    procedure DoAddCompletion(AKeyword: string; AKind: integer);
    function CreateHighlighter: TSynCustomHighlighter; override;
    procedure OnExecuteCompletion(Sender: TObject); override;
  public
    constructor Create; override;
  end;

  TCSSFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TJSFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TSQLFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TApacheFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TINIFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TTXTFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TXMLFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  { TPASFileCategory }

  TPASFileCategory = class(TFileCategory)
  private
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  { TPHPPerspective }

  TPHPPerspective = class(TEditorPerspective)
  public
    class procedure GetAttributes(var PerspectiveAttributes: TPerspectiveAttributes); override;
  end;

  { TPascalPerspective }

  TPascalPerspective = class(TEditorPerspective)
  public
    class procedure GetAttributes(var PerspectiveAttributes: TPerspectiveAttributes); override;
  end;

  { TmneEngine }

  TmneEngine = class(TEditorEngine)
  protected
  public
    constructor Create; override;
  end;

function ColorToRGBHex(Color: TColor): string;
function RGBHexToColor(Value: string): TColor;

const
  sSoftwareRegKey = 'Software\miniEdit\';
{$ifdef WINDOWS}
  SysPlatform = 'WINDOWS';
{$else}
  SysPlatform = 'LINUX';
{$endif}

function GetFileImageIndex(const FileName: string): integer;

function GetWordAtRowColEx(SynEdit: TCustomSynEdit; XY: TPoint; IdentChars: TSynIdentChars; Select: boolean): string;
function GetHighlighterAttriAtRowColEx2(SynEdit: TCustomSynEdit; const XY: TPoint; var Token: string; var TokenType, Start: integer; var Attri: TSynHighlighterAttributes; var Range: Pointer): boolean;

implementation

uses
  IniFiles, mnXMLStreams, mnUtils;

function ColorToRGBHex(Color: TColor): string;
var
  aRGB: TColorRef;
begin
  aRGB := ColorToRGB(Color);
  FmtStr(Result, '%s%.2x%.2x%.2x', ['#', GetRValue(aRGB), GetGValue(aRGB), GetBValue(aRGB)]);
end;

function RGBHexToColor(Value: string): TColor;
var
  R, G, B: byte;
begin
  if LeftStr(Value, 1) = '#' then
    Delete(Value, 1, 1);
  if Value <> '' then
  begin
    if Length(Value) = 3 then
    begin
      R := StrToIntDef('$' + Copy(Value, 1, 1) + Copy(Value, 1, 1), 0);
      G := StrToIntDef('$' + Copy(Value, 2, 1) + Copy(Value, 2, 1), 0);
      B := StrToIntDef('$' + Copy(Value, 3, 1) + Copy(Value, 3, 1), 0);
      Result := RGB(R, G, B);
    end
    else
    begin
      R := StrToIntDef('$' + Copy(Value, 1, 2), 0);
      G := StrToIntDef('$' + Copy(Value, 3, 2), 0);
      B := StrToIntDef('$' + Copy(Value, 5, 2), 0);
      Result := RGB(R, G, B);
    end;
  end
  else
    Result := clBlack;
end;

type
  TSynCustomHighlighterHack = class(TSynCustomHighlighter);

function GetHighlighterAttriAtRowColEx2(SynEdit: TCustomSynEdit; const XY: TPoint; var Token: string; var TokenType, Start: integer; var Attri: TSynHighlighterAttributes; var Range: Pointer): boolean;
var
  PosX, PosY: integer;
  Line: string;
  aToken: string;
begin
  with SynEdit do
  begin
    TokenType := 0;
    Token := '';
    Attri := nil;
    Result := False;
    PosY := XY.Y - 1;
    if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then
    begin
      Line := Lines[PosY];
      if PosY = 0 then
        Highlighter.ResetRange
      else
        Highlighter.SetRange(TSynCustomHighlighterHack(Highlighter).CurrentRanges.Range[PosY - 1]);
      Highlighter.SetLine(Line, PosY);
      PosX := XY.X;
      Range := Highlighter.GetRange;
      if PosX > 0 then
        while not Highlighter.GetEol do
        begin
          Start := Highlighter.GetTokenPos + 1;
          aToken := Highlighter.GetToken;
          Range := Highlighter.GetRange;
          if (PosX >= Start) and (PosX < Start + Length(aToken)) then
          begin
            Attri := Highlighter.GetTokenAttribute;
            TokenType := Highlighter.GetTokenKind;
            Token := aToken;
            Result := True;
            exit;
          end;
          Highlighter.Next;
        end;
    end;
  end;
end;

function GetWordAtRowColEx(SynEdit: TCustomSynEdit; XY: TPoint; IdentChars: TSynIdentChars; Select: boolean): string;
var
  Line: string;
  Len, Stop: integer;
begin
  Result := '';
  if (XY.Y >= 1) and (XY.Y <= SynEdit.Lines.Count) then
  begin
    Line := SynEdit.Lines[XY.Y - 1];
    Len := Length(Line);
    if Len <> 0 then
    begin
      if (XY.X > 1) and (XY.X <= Len + 1) and not (Line[XY.X] in IdentChars) then
        XY.X := XY.X - 1;
      if (XY.X >= 1) and (XY.X <= Len + 1) and (Line[XY.X] in IdentChars) then
      begin
        Stop := XY.X;
        while (Stop <= Len) and (Line[Stop] in IdentChars) do
          Inc(Stop);
        while (XY.X > 1) and (Line[XY.X - 1] in IdentChars) do
          Dec(XY.X);
        if Stop > XY.X then
        begin
          Result := Copy(Line, XY.X, Stop - XY.X);
          if Select then
          begin
            SynEdit.CaretXY := XY;
            SynEdit.BlockBegin := XY;
            SynEdit.BlockEnd := Point(XY.x + Length(Result), XY.y);
          end;
        end;
      end;
    end;
  end;
end;

{ TmneEngine }

function GetFileImageIndex(const FileName: string): integer;
var
  AExtensions: TStringList;
  s: string;
begin
  s := ExtractFileExt(FileName);
  if LeftStr(s, 1) = '.' then
    s := Copy(s, 2, MaxInt);

  AExtensions := TStringList.Create;
  try
    Engine.Groups[0].EnumExtensions(AExtensions);
    if AExtensions.IndexOf(s) >= 0 then
      Result := 2
    else
      Result := 1;//any file
  finally
    AExtensions.Free;
  end;
end;

{ TPascalPerspective }

class procedure TPascalPerspective.GetAttributes(var PerspectiveAttributes: TPerspectiveAttributes);
begin
  PerspectiveAttributes.Title := 'Pascal project';
  PerspectiveAttributes.Description := 'Pascal/FPC/Lazarus Files, *.pas, *.pp *.inc';
  PerspectiveAttributes.Name := 'Pascal';
  PerspectiveAttributes.ImageIndex := -1;
end;

{ TPHPPerspective }

class procedure TPHPPerspective.GetAttributes(var PerspectiveAttributes: TPerspectiveAttributes);
begin
  PerspectiveAttributes.Title := 'PHP project';
  PerspectiveAttributes.Description := 'PHP Files, *.php, *.inc';
  PerspectiveAttributes.Name := 'PHP';
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

constructor TmneEngine.Create;
begin
  inherited;
  //Categories.Add('', TTXTFile, TTXTFileCategory);
  Categories.Add('TXT', TTXTFile, TTXTFileCategory);
  Categories.Add('HTML/PHP', TphpFile, TPHPFileCategory, [fckPublish]);
  Categories.Add('CSS', TCssFile, TCSSFileCategory, [fckPublish]);
  Categories.Add('JS', TJSFile, TJSFileCategory, [fckPublish]);
  Categories.Add('htaccess', TApacheFile, TApacheFileCategory, []);
  Categories.Add('SQL', TSQLFile, TSQLFileCategory);
  Categories.Add('INI', TINIFile, TINIFileCategory);
  Categories.Add('XML', TXMLFile, TXMLFileCategory);
  Categories.Add('PAS', TPASFile, TPASFileCategory);

  Groups.Add('PHP Files', 'php', 'HTML/PHP', ['php'], [fgkExecutable, fgkPublish, fgkBrowsable, fgkMainIcon]);
  Groups.Add('PHPX Files', 'phpx', 'HTML/PHP', ['phpx'], [fgkExecutable, fgkPublish, fgkBrowsable, fgkMainIcon]);
  Groups.Add('HTML Files', 'html', 'HTML/PHP', ['html', 'tpl'], [fgkPublish, fgkBrowsable]);
  Groups.Add('CSS Files', 'css', 'CSS', ['css'], [fgkPublish, fgkBrowsable]);
  Groups.Add('Java Script Files', 'js', 'JS', ['js'], [fgkPublish, fgkBrowsable]);
  Groups.Add('SQL files', 'sql', 'SQL', ['sql'], [fgkPublish, fgkBrowsable]);
  Groups.Add('htaccess files', 'Apache', 'htaccess', ['htaccess', 'conf'], [fgkBrowsable]);
  Groups.Add('XML files', 'XML', 'XML', ['xml'], [fgkPublish, fgkBrowsable]);
  Groups.Add('INI files', 'ini', 'ini', ['ini'], []);
  Groups.Add('TXT files', 'TXT', 'TXT', ['txt'], []);

  Perspectives.Add(TPascalPerspective);
  Perspectives.Add(TPHPPerspective);

  Extenstion := 'mne-project';
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
  Result := TSynHTMLPHPSyn.Create(nil);
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
    if (aSynEdit <> nil) and (Highlighter is TSynHTMLPHPSyn) then
    begin
      aPHPProcessor := (Highlighter as TSynHTMLPHPSyn).Processors.IndexOf('php');
      aHTMLProcessor := (Highlighter as TSynHTMLPHPSyn).Processors.IndexOf('html');
      P := aSynEdit.CaretXY;
      GetHighlighterAttriAtRowColEx2(aSynEdit, P, S, aTokenType, aStart, Attri, aRange);
      aProcessor := RangeToProcessor(aRange);
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
                if (Highlighter.GetTokenPos <> (aStart - 1)) and (RangeToProcessor(Highlighter.GetRange) = aPHPProcessor) then
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
  Completion.CaseSensitive := False;
end;

procedure TPHPFileCategory.ExtractKeywords(Files, Variables, Identifiers: TStringList);
var
  aFile: TStringList;
  s: string;
  i, f: integer;
  aPHPProcessor: integer;
  aHighlighter: TSynHTMLPHPSyn;
begin
  aHighlighter := TSynHTMLPHPSyn.Create(nil);
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
          if (RangeToProcessor(aHighlighter.GetRange) = aPHPProcessor) then
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

{ TJSFileCategory }

function TJSFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynJScriptSyn.Create(nil);
end;

{ TSQLFileCategory }

function TSQLFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynSQLSyn.Create(nil);
end;

{ TTApacheFileCategory }

function TApacheFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynApacheSyn.Create(nil);
end;

{ TINIFileCategory }

function TINIFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

{ TTXTFileCategory }

function TTXTFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

{ TXMLFileCategory }

function TXMLFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynXMLSyn.Create(nil);
end;

{ TXMLFile }

procedure TXMLFile.NewSource;
begin
  SynEdit.Text := '<?xml version="1.0" encoding="iso-8859-1" ?>';
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('');
  SynEdit.CaretY := 2;
  SynEdit.CaretX := 3;
end;

end.

