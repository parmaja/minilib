unit mnSynHighlighterD;
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

  { TDProcessor }

  TDProcessor = class(TCommonSynProcessor)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
  public
    procedure QuestionProc;
    procedure SlashProc;
    procedure AtProc;

    procedure GreaterProc;
    procedure LowerProc;

    procedure Next; override;

    procedure Prepare; override;
    procedure MakeProcTable; override;
  end;

  { TSynDSyn }

  TSynDSyn = class(TSynMultiProcSyn)
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

  SYNS_LangD = 'D';
  SYNS_FilterD = 'D Lang Files (*.d;*.dd)|*.d;*.dd';

  cDSample =
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

{$INCLUDE 'DKeywords.inc'}

implementation

uses
  mnUtils;

procedure TDProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TDProcessor.LowerProc;
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

procedure TDProcessor.SlashProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '/':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '*' then
          SLDocumentProc
        else if ScanMatch('TODO') then
          SLDocumentProc
        else
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
        if Parent.FLine[Parent.Run] = '+' then
          SpecialDocumentProc
        else
          SpecialCommentProc;
      end
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TDProcessor.AtProc;
begin
  Inc(Parent.Run);
  Parent.FTokenID := tkVariable;
  WordProc;
end;

procedure TDProcessor.MakeProcTable;
var
  I: Char;
begin
  inherited;
  for I := #33 to #255 do
    case I of
      '?': ProcTable[I] := @QuestionProc;
      '@': ProcTable[I] := @AtProc;
      '''': ProcTable[I] := @StringSQProc;
      '"': ProcTable[I] := @StringDQProc;
      '`': ProcTable[I] := @StringBQProc;
      '/': ProcTable[I] := @SlashProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
    end;
end;

procedure TDProcessor.QuestionProc;
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

procedure TDProcessor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
    ProcTable[Parent.FLine[Parent.Run]]
  else case Range of
    rscComment:
    begin
      CommentProc;
    end;
    rscSpecialComment:
    begin
      SpecialCommentProc;
    end;
    rscDocument:
    begin
      DocumentProc;
    end;
    rscSpecialDocument:
    begin
      SpecialDocumentProc;
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

procedure TDProcessor.Prepare;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sDKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sDFunctions, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkType), sDTypes, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TDProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range in [rscDocument, rscSpecialDocument]) or (LastRange in [rscDocument, rscSpecialDocument]) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

function TDProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['$', '.'];
end;

constructor TSynDSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterD;
end;

procedure TSynDSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TDProcessor.Create(Self, 'D'));

  Processors.MainProcessor := 'D';
  Processors.DefaultProcessor := 'D';
end;

class function TSynDSyn.GetLanguageName: string;
begin
  Result := SYNS_LangD;
end;

function TSynDSyn.GetSampleSource: string;
begin
  Result := cDSample;
end;

end.
