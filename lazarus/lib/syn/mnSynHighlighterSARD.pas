unit mnSynHighlighterSard;
{$mode objfpc}{$H+}
{**
 * NOT COMPLETED
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *}

interface

uses
  Classes, SysUtils,
  SynEdit, SynEditTypes,
  SynEditHighlighter, mnSynHighlighterMultiProc;

type

  { TSardProcessor }

  TSardProcessor = class(TCommonSynProcessor)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
  public
    procedure Created; override;
    procedure QuestionProc;
    procedure SlashProc;
    procedure BlockProc;

    procedure GreaterProc;
    procedure LowerProc;
    procedure DeclareProc;

    procedure Next; override;

    procedure Prepare; override;
    procedure MakeProcTable; override;
  end;

  { TSynDSyn }

  TSynSardSyn = class(TSynMultiProcSyn)
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

  SYNS_LangSard = 'Sard';
  SYNS_FilterSard = 'Sard Lang Files (*.sard)|*.sard';

  cSardSample =  '/*'
                +'    This examples are worked, and this comment will ignored, not compiled or parsed as we say.'#13
                +'  */'#13
                +''#13
                +'  //Single Line comment'#13
                +'  CalcIt:Integer(p1, p2){'#13
                +'      :=p1 * p2 / 2;'#13
                +'    };'#13
                +''#13
                +'  x := {'#13
                +'        y := 0;'#13
                +'        x := CalcIt(x, y);'#13
                +'        := y + x+ 500 * %10; //this is a result return of the block'#13
                +'    }; //do not forget to add ; here'#13
                +''#13
                +'  f := 10.0;'#13
                +'  f := z + 5.5;'#13
                +''#13
                +'  {* Embeded block comment *};'#13
                +''#13
                +'  := "Result:" + x + '' It is an example:'#13
                +'    Multi Line String'#13
                +'  '';'#13;

implementation

uses
  mnUtils;

procedure TSardProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TSardProcessor.LowerProc;
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

procedure TSardProcessor.DeclareProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '=': Inc(Parent.Run);
    ':':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '=' then
          Inc(Parent.Run);
      end;
  end;
end;

procedure TSardProcessor.SlashProc;
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

procedure TSardProcessor.BlockProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '*': SpecialCommentProc;
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TSardProcessor.MakeProcTable;
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
      '{': ProcTable[I] := @BlockProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      ':': ProcTable[I] := @DeclareProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
    //else
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
    end;
end;

procedure TSardProcessor.QuestionProc;
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

procedure TSardProcessor.Next;
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
  else
    if ProcTable[Parent.FLine[Parent.Run]] = nil then
      UnknownProc
    else
      ProcTable[Parent.FLine[Parent.Run]];
  end;
end;

procedure TSardProcessor.Prepare;
begin
  inherited;
//  EnumerateKeywords(Ord(tkKeyword), sSardKeywords, TSynValidStringChars, @DoAddKeyword);
//  EnumerateKeywords(Ord(tkFunction), sSardFunctions, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TSardProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

procedure TSardProcessor.Created;
begin
  inherited Created;
  CloseSpecialComment := '*}';
end;

function TSardProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

constructor TSynSardSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterSard;
end;

procedure TSynSardSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TSardProcessor.Create(Self, 'Sard'));

  Processors.MainProcessor := 'Sard';
  Processors.DefaultProcessor := 'Sard';
end;

class function TSynSardSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSard;
end;

function TSynSardSyn.GetSampleSource: string;
begin
  Result := cSardSample;
end;

end.

initialization
  RegisterPlaceableHighlighter(TSynSardSyn);
finalization
  FreeAndNil(FSardSyn);
end.

