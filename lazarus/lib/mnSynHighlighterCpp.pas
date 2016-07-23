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
  SynEditHighlighter, SynHighlighterHashEntries, SynHighlighterMultiProc;

type
  TCppRangeState = (rsCppUnknown, rsCppComment, rsCppCommentPlus, rsCppDocument, rsCppStringSQ, rsCppStringDQ, rsCppStringBQ); //BackQuote

  { TDProcessor }

  TCppProcessor = class(TSynProcessor)
  private
  protected
    FRange: TCppRangeState;
    //LastRange: Bad Idea but let us try
    LastRange: TCppRangeState;
    function GetIdentChars: TSynIdentChars; override;
    procedure ResetRange; override;
    function GetRange: Byte; override;
    procedure SetRange(Value: Byte); override;
    procedure SetRange(Value: TCppRangeState); overload;
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
    procedure StringBQProc;
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

    property Range: TCppRangeState read FRange;

    procedure InitIdent; override;
    procedure MakeMethodTables; override;
    procedure MakeIdentTable; override;
  end;

  { TmnSynCppSyn }

  TmnSynCppSyn = class(TSynMultiProcSyn)
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

{$INCLUDE 'DKeywords.inc'}

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

procedure TCppProcessor.AndSymbolProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '&'] then
    Inc(Parent.Run);
end;

procedure TCppProcessor.HashLineCommentProc;
begin
  Parent.FTokenID := tkComment;
  Inc(Parent.Run);
  repeat
    Inc(Parent.Run);
  until Parent.FLine[Parent.Run] in [#0, #10, #13];
end;

procedure TCppProcessor.StringProc;

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
  case Range of
    rsCppStringSQ: iCloseChar := '''';
    rsCppStringDQ: iCloseChar := '"';
    rsCppStringBQ: iCloseChar := '`';
  end;

  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = iCloseChar) and (not IsEscaped) then
    begin
      SetRange(rsCppUnKnown);
      inc(Parent.Run);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure TCppProcessor.CRProc;
begin
  Parent.FTokenID := tkSpace;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] = #10 then
    Inc(Parent.Run);
end;

procedure TCppProcessor.EqualProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
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

procedure TCppProcessor.LFProc;
begin
  Parent.FTokenID := tkSpace;
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

procedure TCppProcessor.MinusProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '-'] then
    Inc(Parent.Run);
end;

procedure TCppProcessor.NullProc;
begin
  Parent.FTokenID := tkNull;
end;

procedure TCppProcessor.NumberProc;
begin
  inc(Parent.Run);
  Parent.FTokenID := tkNumber;
  while Parent.FLine[Parent.Run] in ['0'..'9', '.', '-', 'E', 'x'] do
  begin
    case Parent.FLine[Parent.Run] of
      '.':
        if Parent.FLine[Parent.Run + 1] = '.' then
          break;
    end;
    inc(Parent.Run);
  end;
end;

procedure TCppProcessor.OrSymbolProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '|'] then
    Inc(Parent.Run);
end;

procedure TCppProcessor.PlusProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '+'] then
    Inc(Parent.Run);
end;

procedure TCppProcessor.SlashProc;
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

procedure TCppProcessor.SpaceProc;
begin
  Parent.FTokenID := tkSpace;
  repeat
    Inc(Parent.Run);
  until (Parent.FLine[Parent.Run] > #32) or (Parent.FLine[Parent.Run] in [#0, #10, #13]);
end;

procedure TCppProcessor.SymbolProc;
begin
  Inc(Parent.Run);
  Parent.FTokenID := tkSymbol;
end;

procedure TCppProcessor.SymbolAssignProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] = '=' then
    Inc(Parent.Run);
end;

procedure TCppProcessor.VariableProc;
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

procedure TCppProcessor.UnknownProc;
begin
  inc(Parent.Run);
  Parent.FTokenID := tkUnknown;
end;

procedure TCppProcessor.SetLine(const NewValue: string; LineNumber: integer);
begin
  inherited;
  LastRange := rsCppUnknown;
end;

procedure TCppProcessor.CommentProc;
begin
  Parent.FTokenID := tkComment;
  SetRange(rsCppComment);
  InternalCommentProc;
end;

procedure TCppProcessor.CommentPlusProc;
begin
  Parent.FTokenID := tkComment;
  SetRange(rsCppCommentPlus);
  InternalCommentPlusProc;
end;

procedure TCppProcessor.DocumentProc;
begin
  Parent.FTokenID := tkDocument;
  SetRange(rsCppDocument);
  InternalCommentProc;
end;

procedure TCppProcessor.MakeMethodTables;
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
      '`': ProcTable[I] := @StringBQProc;
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

procedure TCppProcessor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  case Range of
    rsCppComment:
    begin
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        CommentProc;
    end;
    rsCppCommentPlus:
    begin
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        CommentPlusProc;
    end;
    rsCppDocument:
    begin
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        DocumentProc;
    end;
    rsCppStringSQ, rsCppStringDQ, rsCppStringBQ:
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        StringProc;
  else
    ProcTable[Parent.FLine[Parent.Run]];
  end;
end;

procedure TCppProcessor.StringDQProc;
begin
  SetRange(rsCppStringDQ);
  Inc(Parent.Run);
  StringProc;
end;

procedure TCppProcessor.StringBQProc;
begin
  SetRange(rsCppStringBQ);
  Inc(Parent.Run);
  StringProc;
end;

procedure TCppProcessor.StringSQProc;
begin
  SetRange(rsCppStringSQ);
  Inc(Parent.Run);
  StringProc;
end;

function TCppProcessor.GetRange: Byte;
begin
  Result := Byte(Range);
end;

procedure TCppProcessor.ResetRange;
begin
  inherited;
  SetRange(rsCppUnknown);
  LastRange := rsCppUnknown;
end;

procedure TCppProcessor.SetRange(Value: Byte);
begin
  SetRange(TCppRangeState(Value));
end;

procedure TCppProcessor.SetRange(Value: TCppRangeState);
begin
  if FRange <> Value then
    LastRange := FRange;
  FRange := Value;
end;

procedure TCppProcessor.InitIdent;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sDKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sDFunctions, TSynValidStringChars, @DoAddKeyword);
  SetRange(rsCppUnknown);
end;

function TCppProcessor.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, HashCharTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

procedure TCppProcessor.InternalCommentProc;
begin
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = '*') and (Parent.FLine[Parent.Run + 1] = '/') then
    begin
      SetRange(rsCppUnknown);
      Inc(Parent.Run, 2);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure TCppProcessor.InternalCommentPlusProc;
begin
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = '+') and (Parent.FLine[Parent.Run + 1] = '/') then
    begin
      SetRange(rsCppUnknown);
      Inc(Parent.Run, 2);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

function TCppProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rsCppDocument) or (LastRange = rsCppDocument) then
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

function TmnSynCppSyn.GetIdentChars: TSynIdentChars;
begin
  //  Result := TSynValidStringChars + ['&', '#', ';', '$'];
  Result := TSynValidStringChars + ['&', '#', '$'];
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

