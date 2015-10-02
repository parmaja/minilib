unit SynHighlighterD;
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
  SynEditHighlighter, SynHighlighterHashEntries, SynHighlighterMultiProc;

type
  TDRangeState = (rsDUnknown, rsDComment, rsDCommentPlus, rsDDocument, rsDStringSQ, rsDStringDQ);

  { TDProcessor }

  TDProcessor = class(TSynProcessor)
  private
  protected
    FRange: TDRangeState;
    //LastRange: Bad Idea but let us try
    LastRange: TDRangeState;
    function GetIdentChars: TSynIdentChars; override;
    procedure ResetRange; override;
    function GetRange: Byte; override;
    procedure SetRange(Value: Byte); override;
    procedure SetRange(Value: TDRangeState); overload;
    function KeyHash(ToHash: PChar): Integer; override;
    procedure InternalCommentProc;
    procedure InternalCommentPlusProc;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
  public
    procedure QuestionProc;
    procedure AndSymbolProc;
    procedure HashLineCommentProc;
    procedure CommentProc;
    procedure CommentPlusProc;
    procedure DocumentProc;
    procedure SlashProc;
    procedure StringProc;
    procedure StringSQProc;
    procedure StringDQProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure CRProc;
    procedure LFProc;
    procedure GreaterProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
    procedure VariableProc;
    procedure UnknownProc;
    procedure SetLine(const NewValue: string; LineNumber: integer); override;
    procedure Next; override;

    property Range: TDRangeState read FRange;

    procedure InitIdent; override;
    procedure MakeMethodTables; override;
    procedure MakeIdentTable; override;
  end;

  { TSynDSyn }

  TSynDSyn = class(TSynMultiProcSyn)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
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

procedure TDProcessor.MakeIdentTable;
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

procedure TDProcessor.AndSymbolProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '&'] then
    Inc(Parent.Run);
end;

procedure TDProcessor.HashLineCommentProc;
begin
  Parent.FTokenID := tkComment;
  Inc(Parent.Run);
  repeat
    Inc(Parent.Run);
  until Parent.FLine[Parent.Run] in [#0, #10, #13];
end;

procedure TDProcessor.StringProc;

  function IsEscaped: boolean;
  var
    iFirstSlashPos: integer;
  begin
    iFirstSlashPos := Parent.Run - 1;
    while (iFirstSlashPos > 0) and (Parent.FLine[iFirstSlashPos] = '\') do
      Dec(iFirstSlashPos);
    Result := (Parent.Run - iFirstSlashPos + 1) mod 2 <> 0;
  end;

var
  iCloseChar: char;
begin
  Parent.FTokenID := tkString;
  if Range = rsDStringSQ then
    iCloseChar := ''''
  else
    iCloseChar := '"';
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = iCloseChar) and (not IsEscaped) then
    begin
      SetRange(rsDUnKnown);
      inc(Parent.Run);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure TDProcessor.CRProc;
begin
  Parent.FTokenID := tkSpace;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] = #10 then
    Inc(Parent.Run);
end;

procedure TDProcessor.EqualProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TDProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TDProcessor.IdentProc;
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

procedure TDProcessor.LFProc;
begin
  Parent.FTokenID := tkSpace;
  inc(Parent.Run);
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

procedure TDProcessor.MinusProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '-'] then
    Inc(Parent.Run);
end;

procedure TDProcessor.NullProc;
begin
  Parent.FTokenID := tkNull;
end;

procedure TDProcessor.NumberProc;
begin
  inc(Parent.Run);
  Parent.FTokenID := tkNumber;
  while Parent.FLine[Parent.Run] in ['0'..'9', '.', '-'] do
  begin
    case Parent.FLine[Parent.Run] of
      '.':
        if Parent.FLine[Parent.Run + 1] = '.' then
          break;
    end;
    inc(Parent.Run);
  end;
end;

procedure TDProcessor.OrSymbolProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '|'] then
    Inc(Parent.Run);
end;

procedure TDProcessor.PlusProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '+'] then
    Inc(Parent.Run);
end;

procedure TDProcessor.SlashProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '/':
      begin
        Parent.FTokenID := tkComment;
        repeat
          Inc(Parent.Run);
        until Parent.FLine[Parent.Run] in [#0, #10, #13];
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
          DocumentProc
        else
          CommentPlusProc;
      end;
    '=':
      begin
        Inc(Parent.Run);
        Parent.FTokenID := tkSymbol;
      end;
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TDProcessor.SpaceProc;
begin
  Parent.FTokenID := tkSpace;
  repeat
    Inc(Parent.Run);
  until (Parent.FLine[Parent.Run] > #32) or (Parent.FLine[Parent.Run] in [#0, #10, #13]);
end;

procedure TDProcessor.SymbolProc;
begin
  Inc(Parent.Run);
  Parent.FTokenID := tkSymbol;
end;

procedure TDProcessor.SymbolAssignProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] = '=' then
    Inc(Parent.Run);
end;

procedure TDProcessor.VariableProc;
var
  i: integer;
begin
  Parent.FTokenID := tkVariable;
  i := Parent.Run;
  repeat
    Inc(i);
  until not (Identifiers[Parent.FLine[i]]);
  Parent.Run := i;
end;

procedure TDProcessor.UnknownProc;
begin
  inc(Parent.Run);
  Parent.FTokenID := tkUnknown;
end;

procedure TDProcessor.SetLine(const NewValue: string; LineNumber: integer);
begin
  inherited;
  LastRange := rsDUnknown;
end;

procedure TDProcessor.CommentProc;
begin
  Parent.FTokenID := tkComment;
  SetRange(rsDComment);
  InternalCommentProc;
end;

procedure TDProcessor.CommentPlusProc;
begin
  Parent.FTokenID := tkComment;
  SetRange(rsDCommentPlus);
  InternalCommentPlusProc;
end;

procedure TDProcessor.DocumentProc;
begin
  Parent.FTokenID := tkDocument;
  SetRange(rsDDocument);
  InternalCommentProc;
end;

procedure TDProcessor.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: ProcTable[I] := @NullProc;
      #10: ProcTable[I] := @LFProc;
      #13: ProcTable[I] := @CRProc;
      '?': ProcTable[I] := @QuestionProc;
      '''': ProcTable[I] := @StringSQProc;
      '"': ProcTable[I] := @StringDQProc;
      '#': ProcTable[I] := @HashLineCommentProc;
      '/': ProcTable[I] := @SlashProc;
      '=': ProcTable[I] := @EqualProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      '-': ProcTable[I] := @MinusProc;
      '|': ProcTable[I] := @OrSymbolProc;
      '+': ProcTable[I] := @PlusProc;
      '&': ProcTable[I] := @AndSymbolProc;
      '$': ProcTable[I] := @VariableProc;
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        ProcTable[I] := @SpaceProc;
      '^', '%', '*', '!':
        ProcTable[I] := @SymbolAssignProc;
      '{', '}', '.', ',', ';', '(', ')', '[', ']', '~':
        ProcTable[I] := @SymbolProc;
    else
      ProcTable[I] := @UnknownProc;
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
  case Range of
    rsDComment:
    begin
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        CommentProc;
    end;
    rsDCommentPlus:
    begin
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        CommentPlusProc;
    end;
    rsDDocument:
    begin
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        DocumentProc;
    end;
    rsDStringSQ, rsDStringDQ:
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        StringProc;
  else
    ProcTable[Parent.FLine[Parent.Run]];
  end;
end;

procedure TDProcessor.StringDQProc;
begin
  SetRange(rsDStringDQ);
  Inc(Parent.Run);
  StringProc;
end;

procedure TDProcessor.StringSQProc;
begin
  SetRange(rsDStringSQ);
  Inc(Parent.Run);
  StringProc;
end;

function TDProcessor.GetRange: Byte;
begin
  Result := Byte(Range);
end;

procedure TDProcessor.ResetRange;
begin
  inherited;
  SetRange(rsDUnknown);
  LastRange := rsDUnknown;
end;

procedure TDProcessor.SetRange(Value: Byte);
begin
  SetRange(TDRangeState(Value));
end;

procedure TDProcessor.SetRange(Value: TDRangeState);
begin
  if FRange <> Value then
    LastRange := FRange;
  FRange := Value;
end;

procedure TDProcessor.InitIdent;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sDKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sDFunctions, TSynValidStringChars, @DoAddKeyword);
  SetRange(rsDUnknown);
end;

function TDProcessor.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, HashCharTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

procedure TDProcessor.InternalCommentProc;
begin
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = '*') and (Parent.FLine[Parent.Run + 1] = '/') then
    begin
      SetRange(rsDUnknown);
      Inc(Parent.Run, 2);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure TDProcessor.InternalCommentPlusProc;
begin
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = '+') and (Parent.FLine[Parent.Run + 1] = '/') then
    begin
      SetRange(rsDUnknown);
      Inc(Parent.Run, 2);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

function TDProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rsDDocument) or (LastRange = rsDDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

function TDProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['$'];
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

function TSynDSyn.GetIdentChars: TSynIdentChars;
begin
  //  Result := TSynValidStringChars + ['&', '#', ';', '$'];
  Result := TSynValidStringChars + ['&', '#', '$'];
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

