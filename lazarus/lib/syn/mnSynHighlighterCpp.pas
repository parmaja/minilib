unit mnSynHighlighterCpp;
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

  { TCppProcessor }

  TCppProcessor = class(TCommonSynProcessor)
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
  public
    procedure QuestionProc;
    procedure DirectiveProc;
    procedure SlashProc;
    procedure IdentProc;
    procedure GreaterProc;
    procedure LowerProc;

    procedure Next; override;

    procedure InitIdent; override;
    procedure MakeProcTable; override;
    procedure MakeIdentTable; override;
  end;

  { TmnSynCppSyn }

  TmnSynCppSyn = class(TSynMultiProcSyn)
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

  SYNS_LangCpp = 'Cpp';
  SYNS_FilterCpp = 'Cpp Lang Files (*.c;*.cpp;*.h;*.ino)|*.c;*.cpp;*.h;*.ino';

  cCppSample =
      'import std.stdio;'#13#10+
      '// Computes average line length for standard input.'#13#10+
      ''#13#10+
      'void main()'#13#10+
      '{'#13#10+
      '    ulong lines = 0;'#13#10+
      '    double sumLength = 0;'#13#10+
      '    foreach (line; stdin.byLine())'#13#10+
      '    {'#13#10+
      '        ++lines;'#13#10+
      '        sumLength += line.length;'#13#10+
      '    }'#13#10+
      '    writeln("Average line length: ",'#13#10+
      '        lines ? sumLength / lines : 0);'#13#10+
      '}'#13#10;

{$INCLUDE 'CppKeywords.inc'}

implementation

uses
  mnUtils;

procedure TCppProcessor.MakeIdentTable;
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

procedure TCppProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TCppProcessor.IdentProc;
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

procedure TCppProcessor.LowerProc;
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

procedure TCppProcessor.SlashProc;
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
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TCppProcessor.MakeProcTable;
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
      '#': ProcTable[I] := @DirectiveProc;
      '/': ProcTable[I] := @SlashProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
    end;
end;

procedure TCppProcessor.QuestionProc;
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

procedure TCppProcessor.DirectiveProc;
begin
  Parent.FTokenID := tkProcessor;
  WordProc;
end;

procedure TCppProcessor.Next;
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

procedure TCppProcessor.InitIdent;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sCppKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sCppFunctions, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TCppProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

function TCppProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['$'];
end;

constructor TmnSynCppSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterCpp;
end;

procedure TmnSynCppSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TCppProcessor.Create(Self, 'Cpp'));

  Processors.MainProcessor := 'Cpp';
  Processors.DefaultProcessor := 'Cpp';
end;

class function TmnSynCppSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCpp;
end;

function TmnSynCppSyn.GetSampleSource: string;
begin
  Result := cCppSample;
end;

end.

