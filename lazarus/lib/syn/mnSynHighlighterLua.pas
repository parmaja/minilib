unit mnSynHighlighterLua;
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

interface

uses
  Classes, SysUtils,
  SynEdit, SynEditTypes,
  SynEditHighlighter, SynHighlighterHashEntries, mnSynHighlighterMultiProc;

type

  { TLuaProcessor }

  TLuaProcessor = class(TCommonSynProcessor)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
    procedure Created; override;
  public
    procedure QuestionProc;
    procedure DirectiveProc;
    procedure DashProc;
    procedure BracketProc;
    procedure SpecialStringProc;

    procedure GreaterProc;
    procedure LowerProc;

    procedure Next; override;

    procedure Prepare; override;
    procedure MakeProcTable; override;
  end;

  { TmnSynLuaSyn }

  TmnSynLuaSyn = class(TSynMultiProcSyn)
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

  SYNS_LangLua = 'Lua';
  SYNS_FilterLua = 'Lua Lang Files (*.c;*.lua)|*.c;*.lua';

  cLuaSample =
      '-- defines a factorial function'#13#10+
      'function fact(n)'#13#10+
      '  if n == 0 then'#13#10+
      '    return 1'#13#10+
      '  else'#13#10+
      '    return n * fact(n-1)'#13#10+
      '  end'#13#10+
      'end'#13#10;

{$INCLUDE 'LuaKeywords.inc'}

implementation

uses
  mnUtils;

procedure TLuaProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TLuaProcessor.LowerProc;
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

procedure TLuaProcessor.DashProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '-':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '*' then
          SLDocumentProc
        else if ScanMatch('TODO') then
          SLDocumentProc
        else if ScanMatch('[[') then
        begin
          if Parent.FLine[Parent.Run] = '*' then
            DocumentProc
          else
            CommentProc;
        end
        else
          SLCommentProc;
      end;
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TLuaProcessor.BracketProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '[':
      begin
        SetRange(rscSpecialString);
        Inc(Parent.Run);
        SpecialStringProc;
      end
  else
    Parent.FTokenID := tkSymbol;
  end;

end;

procedure TLuaProcessor.SpecialStringProc;
begin
  Parent.FTokenID := tkString;
  SetRange(rscSpecialString);
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if ScanMatch(']]') then
    begin
      SetRange(rscUnKnown);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure TLuaProcessor.MakeProcTable;
var
  I: Char;
begin
  inherited;
  for I := #0 to #255 do
    case I of
      '?': ProcTable[I] := @QuestionProc;
      '''': ProcTable[I] := @StringSQProc;
      '"': ProcTable[I] := @StringDQProc;
      '[': ProcTable[I] := @BracketProc;
      '-': ProcTable[I] := @DashProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
    end;
end;

procedure TLuaProcessor.QuestionProc;
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

procedure TLuaProcessor.DirectiveProc;
begin
  Parent.FTokenID := tkProcessor;
  WordProc;
end;

procedure TLuaProcessor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
    ProcTable[Parent.FLine[Parent.Run]]
  else case Range of
    rscComment:
    begin
      CommentProc;
    end;
    rscDocument:
    begin
      DocumentProc;
    end;
    rscStringSQ, rscStringDQ, rscStringBQ:
      StringProc;
    rscSpecialString:
      SpecialStringProc;
  else
    if ProcTable[Parent.FLine[Parent.Run]] = nil then
      UnknownProc
    else
      ProcTable[Parent.FLine[Parent.Run]];
  end;
end;

procedure TLuaProcessor.Prepare;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sLuaKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkType), sLuaTypes, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sLuaFunctions, TSynValidStringChars + ['.'], @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TLuaProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

procedure TLuaProcessor.Created;
begin
  inherited Created;
  CloseComment := ']]';
end;

function TLuaProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['.'];
end;

constructor TmnSynLuaSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterLua;
end;

procedure TmnSynLuaSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TLuaProcessor.Create(Self, 'Lua'));

  Processors.MainProcessor := 'Lua';
  Processors.DefaultProcessor := 'Lua';
end;

class function TmnSynLuaSyn.GetLanguageName: string;
begin
  Result := 'Lua';
end;

function TmnSynLuaSyn.GetSampleSource: string;
begin
  Result := cLuaSample;
end;

end.
