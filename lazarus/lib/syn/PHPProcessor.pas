unit PHPProcessor;
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
  http://flatdev.republika.pl/php-functions-lastest.zip
}

interface

uses
  Classes, SysUtils,
  SynEdit, SynEditTypes,
  SynEditHighlighter, SynHighlighterHashEntries, mnSynHighlighterMultiProc;

type
  TPHPRangeState = (rsphpUnknown, rsphpComment, rsphpDocument, rsphpStringSQ, rsphpStringDQ, rsphpVarExpansion);

  { TPHPProcessor }

  TPHPProcessor = class(TSynProcessor)
  protected
    FRange: TPHPRangeState;
    //LastRange: Bad Idea but let us try
    LastRange: TPHPRangeState;
    function GetIdentChars: TSynIdentChars; override;
    procedure ResetRange; override;
    function GetRange: Byte; override;
    procedure SetRange(Value: Byte); override;
    procedure SetRange(Value: TPHPRangeState); overload;
    function KeyHash(ToHash: PChar): Integer; override;
    procedure InternalCommentProc;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
  public
    procedure QuestionProc;
    procedure AndSymbolProc;
    procedure HashLineCommentProc;
    procedure CommentProc;
    procedure DocumentProc;
    procedure SlashProc;
    procedure StringProc;
    procedure StringSQProc;
    procedure StringDQProc;
    procedure VarExpansionProc;
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

    property Range: TPHPRangeState read FRange;

    procedure InitIdent; override;
    procedure MakeMethodTables; override;
    procedure MakeIdentTable; override;
  end;

const
  {$INCLUDE 'PHPKeywords.inc'}

implementation

uses
  mnUtils;

procedure TPHPProcessor.MakeIdentTable;
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

procedure TPHPProcessor.AndSymbolProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '&'] then
    Inc(Parent.Run);
end;

procedure TPHPProcessor.HashLineCommentProc;
begin
  Parent.FTokenID := tkComment;
  Inc(Parent.Run);
  repeat
    Inc(Parent.Run);
  until Parent.FLine[Parent.Run] in [#0, #10, #13];
end;

procedure TPHPProcessor.StringProc;

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
  if Range = rsphpStringSQ then
    iCloseChar := ''''
  else
    iCloseChar := '"';
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = iCloseChar) and (not IsEscaped) then
    begin
      SetRange(rsphpUnKnown);
      inc(Parent.Run);
      break;
    end;
    if (iCloseChar = '"') and (Parent.FLine[Parent.Run] = '$') and
       ((Parent.FLine[Parent.Run + 1] = '{') or Identifiers[Parent.FLine[Parent.Run + 1]]) then
    begin
      if (Parent.Run > 1) and (Parent.FLine[Parent.Run - 1] = '{') then { complex syntax }
        Dec(Parent.Run);
      if not IsEscaped then
      begin
        { break the token to process the variable }
        SetRange(rsphpVarExpansion);
        break;
      end
      else if Parent.FLine[Parent.Run] = '{' then
        Inc(Parent.Run); { restore Run if we previously deincremented it }
    end;
    Inc(Parent.Run);
  end;
end;

procedure TPHPProcessor.CRProc;
begin
  Parent.FTokenID := tkSpace;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] = #10 then
    Inc(Parent.Run);
end;

procedure TPHPProcessor.EqualProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TPHPProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TPHPProcessor.IdentProc;
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

procedure TPHPProcessor.LFProc;
begin
  Parent.FTokenID := tkSpace;
  inc(Parent.Run);
end;

procedure TPHPProcessor.LowerProc;
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

procedure TPHPProcessor.MinusProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '-'] then
    Inc(Parent.Run);
end;

procedure TPHPProcessor.NullProc;
begin
  Parent.FTokenID := tkNull;
end;

procedure TPHPProcessor.NumberProc;
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

procedure TPHPProcessor.OrSymbolProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '|'] then
    Inc(Parent.Run);
end;

procedure TPHPProcessor.PlusProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '+'] then
    Inc(Parent.Run);
end;

procedure TPHPProcessor.SlashProc;
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
    '=':
      begin
        Inc(Parent.Run);
        Parent.FTokenID := tkSymbol;
      end;
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TPHPProcessor.SpaceProc;
begin
  Parent.FTokenID := tkSpace;
  repeat
    Inc(Parent.Run);
  until (Parent.FLine[Parent.Run] > #32) or (Parent.FLine[Parent.Run] in [#0, #10, #13]);
end;

procedure TPHPProcessor.SymbolProc;
begin
  Inc(Parent.Run);
  Parent.FTokenID := tkSymbol;
end;

procedure TPHPProcessor.SymbolAssignProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] = '=' then
    Inc(Parent.Run);
end;

procedure TPHPProcessor.VariableProc;
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

procedure TPHPProcessor.UnknownProc;
begin
  inc(Parent.Run);
  Parent.FTokenID := tkUnknown;
end;

procedure TPHPProcessor.SetLine(const NewValue: string; LineNumber: integer);
begin
  inherited;
  LastRange := rsphpUnknown;
end;

procedure TPHPProcessor.CommentProc;
begin
  Parent.FTokenID := tkComment;
  SetRange(rsphpComment);
  InternalCommentProc;
end;

procedure TPHPProcessor.DocumentProc;
begin
  Parent.FTokenID := tkDocument;
  SetRange(rsphpDocument);
  InternalCommentProc;
end;

procedure TPHPProcessor.MakeMethodTables;
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

procedure TPHPProcessor.QuestionProc;
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

procedure TPHPProcessor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  case Range of
    rsphpComment:
    begin
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        CommentProc;
    end;
    rsphpDocument:
    begin
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        DocumentProc;
    end;
    rsphpStringSQ, rsphpStringDQ:
      if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
        ProcTable[Parent.FLine[Parent.Run]]
      else
        StringProc;
    rsphpVarExpansion:
      VarExpansionProc;
  else
    ProcTable[Parent.FLine[Parent.Run]];
  end;
end;

procedure TPHPProcessor.VarExpansionProc;
type
  TExpansionSyntax = (esNormal, esComplex, esBrace);
var
  iSyntax: TExpansionSyntax;
  iOpenBraces: integer;
  iOpenBrackets: integer;
  iTempRun: integer;
begin
  SetRange(rsphpStringDQ); { var expansion only occurs in double quoted strings }
  Parent.FTokenID := tkVariable;
  if Parent.FLine[Parent.Run] = '{' then
  begin
    iSyntax := esComplex;
    Inc(Parent.Run, 2); // skips '{$'
  end
  else
  begin
    Inc(Parent.Run);
    if Parent.FLine[Parent.Run] = '{' then
    begin
      iSyntax := esBrace;
      Inc(Parent.Run);
    end
    else
      iSyntax := esNormal;
  end;
  if iSyntax in [esBrace, esComplex] then
  begin
    iOpenBraces := 1;
    while Parent.FLine[Parent.Run] <> #0 do
    begin
      if Parent.FLine[Parent.Run] = '}' then
      begin
        Dec(iOpenBraces);
        if iOpenBraces = 0 then
        begin
          Inc(Parent.Run);
          break;
        end;
      end;
      if Parent.FLine[Parent.Run] = '{' then
        Inc(iOpenBraces);
      Inc(Parent.Run);
    end;
  end
  else
  begin
    while Identifiers[Parent.FLine[Parent.Run]] do
      Inc(Parent.Run);
    iOpenBrackets := 0;
    iTempRun := Parent.Run;
    { process arrays and objects }
    while Parent.FLine[iTempRun] <> #0 do
    begin
      if Parent.FLine[iTempRun] = '[' then
      begin
        Inc(iTempRun);
        if Parent.FLine[iTempRun] = '''' then
        begin
          Inc(iTempRun);
          while (Parent.FLine[iTempRun] <> '''') and (Parent.FLine[iTempRun] <> #0) do
            Inc(iTempRun);
          if (Parent.FLine[iTempRun] = '''') and (Parent.fLine[iTempRun + 1] = ']') then
          begin
            Inc(iTempRun, 2);
            Parent.Run := iTempRun;
            continue;
          end
          else
            break;
        end
        else
          Inc(iOpenBrackets);
      end
      else if (Parent.FLine[iTempRun] = '-') and (Parent.FLine[iTempRun + 1] = '>') then
        Inc(iTempRun, 2)
      else
        break;

      if not Identifiers[Parent.FLine[iTempRun]] then
        break
      else
        repeat
          Inc(iTempRun);
        until not Identifiers[Parent.FLine[iTempRun]];

      while Parent.FLine[iTempRun] = ']' do
      begin
        if iOpenBrackets = 0 then
          break;
        Dec(iOpenBrackets);
        Inc(iTempRun);
      end;
      if iOpenBrackets = 0 then
        Parent.Run := iTempRun;
    end;
  end;
end;

procedure TPHPProcessor.StringDQProc;
begin
  SetRange(rsphpStringDQ);
  Inc(Parent.Run);
  StringProc;
end;

procedure TPHPProcessor.StringSQProc;
begin
  SetRange(rsphpStringSQ);
  Inc(Parent.Run);
  StringProc;
end;

function TPHPProcessor.GetRange: Byte;
begin
  Result := Byte(Range);
end;

procedure TPHPProcessor.ResetRange;
begin
  inherited;
  SetRange(rsphpUnknown);
  LastRange := rsphpUnknown;
end;

procedure TPHPProcessor.SetRange(Value: Byte);
begin
  SetRange(TPHPRangeState(Value));
end;

procedure TPHPProcessor.SetRange(Value: TPHPRangeState);
begin
  if FRange <> Value then
    LastRange := FRange;
  FRange := Value;
end;

procedure TPHPProcessor.InitIdent;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sPHPControls, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKeyword), sPHPKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sPHPFunctions, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkValue), sPHPConstants, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkVariable), sPHPVariables, TSynValidStringChars, @DoAddKeyword);
  SetRange(rsphpUnknown);
end;

function TPHPProcessor.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, HashCharTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

procedure TPHPProcessor.InternalCommentProc;
begin
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = '*') and (Parent.FLine[Parent.Run + 1] = '/') then
    begin
      SetRange(rsphpUnKnown);
      Inc(Parent.Run, 2);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

function TPHPProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rsphpDocument) or (LastRange = rsphpDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

function TPHPProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['$'];
end;

end.

