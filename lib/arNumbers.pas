unit arNumber;
{
/**
 * Extended Arabic figures numbers
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author     Zaher Dirkey
 */
}
interface

uses
  Windows, Messages, SysUtils;

const
  COUNTED_NUM = 5;

type
  TCurrencyNames = array[0..COUNTED_NUM - 1] of string;

const
  Figures: array[0..38] of string =
  (
    'æÇÍÏ', //		0
    'ÃÍÏ', //		1
    'ÇËäí', //		2
    'ËáÇËÉ', //		3
    'ÃÑÈÚÉ', //		4
    'ÎãÓÉ', //		5
    'ÓÊÉ', //		6
    'ÓÈÚÉ', //		7
    'ËãÇäíÉ', //		8
    'ÊÓÚÉ', //		9
    'ÇËäíä', //		10
    'æÇÍÏ', //		11
    'ÅÍÏì', //		12
    'ÇËäÊí', //		13
    'ËáÇË', //		14
    'ÃÑÈÚ', //		15
    'ÎãÓ', //		16
    'ÓÊ', //		17
    'ÓÈÚ', //		18
    'ËãÇä', //		19
    'ÊÓÚ', //		20
    'ÇËäÊíä', //		21
    'ÚÔÑÉ', //		22
    'ÚÔÑ', //		23
    'ÚÔÑíä', //		24
    'ËáÇËíä', //		25
    'ÃÑÈÚíä', //		26
    'ÎãÓíä', //		27
    'ÓÊíä', //		28
    'ÓÈÚíä', //		29
    'ËãÇäíä', //		30
    'ÊÓÚíä', //		31
    'ãÆÉ', //	 32
    'ãÆÊíä', //	33
    'ãÆÊí', //	34
    'ãÆÇÊ', //	35
    'áÇÔÆ', // 36
    ' ', //	37	96
    ' æ ' // 38
    );

  CountedFigures: array[0..29] of string =
  (
    'M', //
    'ÃáÝ', //
    'ÃáÝíä', //
    'ÃáÝí', //
    'ÂáÇÝ', //
    'M', //
    'ãáíæä', //
    'ãáíæäíä', //
    'ãáíæäÇ', //
    'ãáÇííä', //
    'M', //
    'ãáíÇÑ', //
    'ãáíÇÑíä', //
    'ãáíÇÑÇ', //
    'ãáíÇÑÇÊ', //
    'M', //
    'Èáíæä', //
    'Èáíæäíä', //
    'ÈáíæäÇ', //
    'ÈáÇííä', //
    'M', //
    'ÈáíÇÑ', //
    'ÈáíÇÑíä', //
    'ÈáíÇÑÇ', //
    'ÈáíÇÑÇÊ', //
    'M', //
    'ÊÑíáíæä', //
    'ÊÑíáíæäíä', //
    'ÊÑíáíæäÇ', //
    'ÊÑíáÇííä' //
    );

function GetArabicFigure(Num: Int64; const CurrencyNames: TCurrencyNames): string;

implementation

function GetArabicFigurePart(Num: Int64; Counted: array of string; HasOther: Boolean; LastCount: Boolean): string;
var
  I: Integer;
  a1, a2, a3: Int64;
  f1, f2, f3: Int64;
  fa, fm, Mas, Mas11: Integer;

  Phrase: array[0..8] of Int64;

  PhraseStr: string;
  j: integer;
begin
  Result := '';

  Mas := Ord(Counted[0] = 'M');
  Mas11 := Mas * 11;
  a3 := (Num div 100);
  a2 := (Num div 10) mod 10;
  a1 := Num - a2 * 10 - a3 * 100;

  if (a1 > 2) then
    f1 := 3
  else
    f1 := a1;

  if (a2 > 1) then
    f2 := 2
  else
    f2 := a2;

  if (a3 > 2) then
    f3 := 3
  else
    f3 := a3;

  fa := (f2 * 4) + f1;

  fm := Integer(fa > 0) * 4 + f3;

  for j := 0 to Length(Phrase) - 1 do
    Phrase[j] := -1;

  Phrase[0] := 1;

  case fa of
    0:
      begin
        if (a3 = 0) then
          if LastCount then
            Phrase[1] := 37
          else
            Phrase[0] := -1;
      end;
    1:
      begin
        if (not LastCount) and (not (a3 = 0)) then
        begin
          Phrase[1] := 37;
          Phrase[2] := Mas11;
        end;
      end;
    2:
      begin
        if (LastCount) or (a3 = 0) then
        begin
          Phrase[0] := 3 - Integer(HasOther);
          Phrase[1] := -1;
          Phrase[2] := -1;
        end
        else
        begin
          Phrase[1] := 37;
          Phrase[2] := Mas11 + a1;
        end;
      end;
    3:
      begin
        Phrase[0] := 4;
        Phrase[1] := 37;
        Phrase[2] := Mas11 + a1;
      end;
    4:
      begin
        Phrase[0] := 4;
        Phrase[1] := 37;
        Phrase[2] := 22 + Mas;
      end;
    5, 6, 7:
      begin
        Phrase[1] := 37;
        Phrase[2] := 22 + Mas;
        Phrase[3] := 37;
        Phrase[4] := Mas11 + a1;
      end;
    8:
      begin
        Phrase[1] := 37;
        Phrase[2] := 22 + a2;
      end;
    9:
      begin
        Phrase[1] := 37;
        Phrase[2] := 22 + a2;
        Phrase[3] := 38;
        Phrase[4] := Mas11 + Mas;
      end;
    10:
      begin
        Phrase[1] := 37;
        Phrase[2] := 22 + a2;
        Phrase[3] := 38;
        Phrase[4] := Mas11 + 10;
      end;
    11:
      begin
        Phrase[1] := 37;
        Phrase[2] := 22 + a2;
        Phrase[3] := 38;
        Phrase[4] := Mas11 + a1;
      end;
  end;

  case fm of
    1:
      begin
        Phrase[5] := 37;
        Phrase[6] := 32;
      end;
    2:
      begin
        Phrase[5] := 37;
        Phrase[6] := 34;
      end;
    3:
      begin
        Phrase[5] := 37;
        Phrase[6] := 32;
        Phrase[7] := 11 + a3;
      end;
    5:
      begin
        Phrase[5] := 38;
        Phrase[6] := 32;
      end;
    6:
      begin
        Phrase[5] := 38;
        Phrase[6] := 33;
      end;
    7:
      begin
        Phrase[5] := 38;
        Phrase[6] := 32;
        Phrase[7] := 11 + a3;
      end;
  end;
  for I := 7 downto 1 do
    if (Phrase[i] >= 0) then
    begin
      PhraseStr := Figures[Phrase[I]];
      Result := Result + PhraseStr;
    end;

  if (Phrase[0] >= 0) then
    Result := Result + Counted[Phrase[0]];
  if (HasOther) then
    Result := Result + ' æ ';
end;

function GetArabicFigure(Num: Int64; const CurrencyNames: TCurrencyNames): string;
var
  I, X: Integer;
  CurValue, ModValue: Int64;
  HasOther, LastCount: Boolean;
  Counted: TCurrencyNames;
  BufStr, StrRes: string;
  j: integer;
begin
  if Num = 0 then
    Result := Figures[36]
  else
  begin
    for j := 0 to 4 do
      Counted[j] := CurrencyNames[j];
    LastCount := True;
    StrRes := '';
    ModValue := Num;
    HasOther := False;
    CurValue := ModValue mod 1000;
    ModValue := (ModValue - CurValue) div 1000;

    StrRes := GetArabicFigurePart(CurValue, Counted, HasOther, LastCount);
    BufStr := StrRes;
    LastCount := False;
    I := 0;
    while (ModValue <> 0) do
    begin
      HasOther := (CurValue <> 0);
      CurValue := ModValue mod 1000;
      ModValue := (ModValue - CurValue) div 1000;
      X := 0;
      while X < COUNTED_NUM do
      begin
        Counted[X] := CountedFigures[I];
        X := X + 1;
        I := I + 1;
      end;
      StrRes := GetArabicFigurePart(CurValue, Counted, HasOther, LastCount);
      StrRes := StrRes + BufStr;
      BufStr := StrRes;
    end;
    Result := StrRes;
  end;
end;

end.

