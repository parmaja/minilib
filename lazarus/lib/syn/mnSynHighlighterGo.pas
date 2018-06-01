unit mnSynHighlighterGo;
{$mode objfpc}{$H+}
{**
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{

}

interface

uses
  Classes, SysUtils,
  SynEdit, SynEditTypes,
  SynEditHighlighter, SynHighlighterHashEntries, mnSynHighlighterMultiProc;

type

  { TGoProcessor }

  TGoProcessor = class(TCommonSynProcessor)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
  public
    procedure QuestionProc;
    procedure SlashProc;
    procedure IdentProc;
    procedure GreaterProc;
    procedure LowerProc;

    procedure Next; override;

    procedure InitIdent; override;
    procedure MakeProcTable; override;
    procedure MakeIdentTable; override;
  end;

  { TSynDSyn }

  TSynGoSyn = class(TSynMultiProcSyn)
  private
  protected
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitProcessors; override;
  published
  end;

const

  SYNS_LangGo = 'Go';
  SYNS_FilterGo = 'Go Lang Files (*.go)|*.go';

  cGoSample =
      'package main'#13#10+
      '//line comment'#13#10+
      'import "fmt"'#13#10+
      'func main() {'#13#10+
      '    fmt.Println("hello world")'#13#10+
      '    /* All rest of program here'#13#10+
      '    */'#13#10+
      '}'#13#10;

{$INCLUDE 'GoKeywords.inc'}

implementation

uses
  mnUtils;

procedure TGoProcessor.MakeIdentTable;
var
  c: char;
begin
  InitMemory(Identifiers, SizeOf(Identifiers));
  for c := 'a' to 'z' do
    Identifiers[c] := True;
  for c := 'A' to 'Z' do
    Identifiers[c] := True;
  for c := '0' to '9' do
    Identifiers[c] := True;
  Identifiers['_'] := True;

  InitMemory(HashCharTable, SizeOf(HashCharTable));
  HashCharTable['_'] := 1;
  for c := 'a' to 'z' do
    HashCharTable[c] := 2 + Ord(c) - Ord('a');
  for c := 'A' to 'Z' do
    HashCharTable[c] := 2 + Ord(c) - Ord('A');
end;

procedure TGoProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TGoProcessor.IdentProc;
begin
  Parent.FTokenID := IdentKind((Parent.FLine + Parent.Run));
  inc(Parent.Run, FStringLen);
  if Parent.FTokenID = tkComment then
  begin
    while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
      Inc(Parent.Run);
  end
  else
    while Identifiers[Parent.FLine[Parent.Run]] do
      inc(Parent.Run);
end;

procedure TGoProcessor.LowerProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '=': Inc(Parent.Run);
    '<':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '=' then
          Inc(Parent.Run);
      end;
  end;
end;

procedure TGoProcessor.SlashProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '/':
      begin
        SLCommentProc;
      end;
    '*':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '*' then
          DocumentProc
        else
          CommentProc;
      end;
    '+':
      begin
        Inc(Parent.Run);
{        if Parent.FLine[Parent.Run] = '+' then
          DocumentProc
        else}
          GrandCommentProc;
      end
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TGoProcessor.MakeProcTable;
var
  I: Char;
begin
  inherited;
  for I := #0 to #255 do
    case I of
      '?': ProcTable[I] := @QuestionProc;
      '''': ProcTable[I] := @StringSQProc;
      '"': ProcTable[I] := @StringDQProc;
      '`': ProcTable[I] := @StringBQProc;
      '/': ProcTable[I] := @SlashProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
    end;
end;

procedure TGoProcessor.QuestionProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '>':
      begin
        Parent.Processors.Switch(Parent.Processors.MainProcessor);
        Inc(Parent.Run);
        Parent.FTokenID := tkProcessor;
      end
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TGoProcessor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
    ProcTable[Parent.FLine[Parent.Run]]
  else case Range of
    rscComment:
    begin
      CommentProc;
    end;
    rscGrandComment:
    begin
      GrandCommentProc;
    end;
    rscDocument:
    begin
      DocumentProc;
    end;
    rscStringSQ, rscStringDQ, rscStringBQ:
      StringProc;
  else
    if ProcTable[Parent.FLine[Parent.Run]] = nil then
      UnknownProc
    else
      ProcTable[Parent.FLine[Parent.Run]];
  end;
end;

procedure TGoProcessor.InitIdent;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sGoKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sGoFunctions, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TGoProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

function TGoProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['$'];
end;

constructor TSynGoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterGo;
end;

procedure TSynGoSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TGoProcessor.Create(Self, 'Go'));

  Processors.MainProcessor := 'Go';
  Processors.DefaultProcessor := 'Go';
end;

class function TSynGoSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGo;
end;

function TSynGoSyn.GetSampleSource: string;
begin
  Result := cGoSample;
end;

end.
