unit calculators;

//ported from  \fpc\ide\fpcalc.pas with many modifications
//ported by Zaher Dirkey Zaher at parmaja dot com

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StrUtils, Math;

type
  TCalcState = (csFirst, csValid, csError);

  { TFPCalculator }

  TCalculator = class(TObject)
  private
    FMaxDecimals: Integer;
    FMaxDigits: Integer;
    procedure GetDisplay(var R: Extended);
    procedure SetDisplay(R: Extended; ShouldKeepZeroes: boolean);
    procedure Error;
  protected
    Started: Boolean;
    Status: TCalcState;
    Number: string;
    Sign: Char;
    CurrentOperator,
    LastOperator: Char;
    LastResult,
    Operand: Extended;
    HaveMemory: Boolean;
    Memory: Extended;
    DispNumber: Extended;
    HexShown: Boolean;
  public
    constructor Create;
    function Process(Key: string): boolean; virtual;
    procedure Clear; virtual;
    procedure Reset; virtual;

    procedure Log(const S: string); virtual;
    procedure Refresh; virtual;

    property MaxDecimals: Integer read FMaxDecimals write FMaxDecimals default 10;
    property MaxDigits: Integer read FMaxDigits write FMaxDigits default 30;
  end;

implementation

constructor TCalculator.Create;
begin
  inherited Create;
  FMaxDecimals := 10;
  FMaxDigits := 30;
  Clear;
  HexShown := False;
end;

procedure TCalculator.GetDisplay(var R: Extended);
begin
  R := DispNumber;
end;

procedure TCalculator.Refresh;
begin
end;

procedure TCalculator.Reset;
begin
  Clear;
  HaveMemory := False;
  Memory := 0;
end;

procedure TCalculator.Log(const s: string);
begin
end;

procedure TCalculator.SetDisplay(R: Extended; ShouldKeepZeroes: Boolean);
var
  S: string;
  i, KeepZeroes: Integer;
  p: Integer;
begin
  DispNumber := R;
  KeepZeroes := 0;
  p := pos('.', Number);
  if (ShouldKeepZeroes and (p > 0)) then
  begin
    for i := Length(Number) downto p + 1 do
      if Number[i] = '0' then
        inc(KeepZeroes)
      else
        break;
  end;

  S := FloatToStrF(R, ffGeneral, MaxDecimals, MaxDigits);

  if KeepZeroes > 0 then
    S := S + AddChar('0', S, KeepZeroes);

  if S[1] <> '-' then
    Sign := ' '
  else
  begin
    Delete(S, 1, 1);
    Sign := '-';
  end;

  if Length(S) > MaxDigits + 1 + MaxDecimals then
    Error
  else
  begin
    if RightStr(S, 1) = '.' then
     S := LeftStr(S, Length(S) - 1);
    Number := S;
  end;
end;

procedure TCalculator.Error;
begin
  Status := csError;
  Number := 'Error';
  Sign := ' ';
  Refresh;
end;

function TCalculator.Process(Key: string): boolean;
var
  R: Extended;
  X: cardinal;
  procedure CheckFirst;
  begin
    if Status = csFirst then
    begin
      Status := csValid;
      SetDisplay(0, False);
    end;
  end;
var
  s: string;
begin
  Result := True;
  Key := UpperCase(Key);
  if (Status = csError) and (Key <> 'C') then
    Key := ' ';
  R := 0;
  if HexShown then
  begin
    GetDisplay(R);
    SetDisplay(R, False);
    HexShown := False;
    if Key = 'H' then
      Key := ' ';
  end;
  if Key = 'X^Y' then
    Key := '^'
  else if Key = #13 then
    Key := '='
  else if Key = #7 then
    Key := 'C'
  else if Key = #27 then
    Key := 'AC'
  else if (Key = '_') or (Key = #241) then
    Key := '+/-';

  if Length(Key) > 1 then
  begin
{        if Status = csFirst then}
    begin
{          Status := csValid;}
      GetDisplay(R);
      if Key = 'AC' then
        Clear
      else if Key = 'ON' then
        Reset
      else if Key = '1/X' then
      begin
        if R = 0 then
          Error
        else
          SetDisplay(1 / R, False);
      end
      else if Key = 'SQRT' then
      begin
        if R < 0 then
          Error
        else
          SetDisplay(sqrt(R), False)
      end
      else if Key = 'LOG' then
      begin
        if R <= 0 then
          Error else SetDisplay(ln(R), False)
      end
      else if Key = 'X^2' then
        SetDisplay(R * R, False)
      else if (Key = '+/-') then
      begin
        if Sign = ' ' then
          Sign := '-'
        else
          Sign := ' ';
        GetDisplay(R);
        SetDisplay(-R, True);
      end
      else if Key = 'M+' then
      begin
        Memory := Memory + R;
        HaveMemory := True;
      end
      else if Key = 'M-' then
      begin
        Memory := Memory - R;
        HaveMemory := True;
      end
      else if Key = 'MR' then
      begin
        CheckFirst;
        SetDisplay(Memory, False)
      end
      else if Key = 'MC' then
      begin
        Memory := 0;
        HaveMemory := False;
      end;
    end;
  end
  else
    case Key[1] of
      '0'..'9':
        if Length(Number) < MaxDigits then
        begin
          CheckFirst;
          if Number = '0' then
             Number := '';
          Number := Number + Key;
          DispNumber := StrToFloat(Number);
          //SetDisplay(StrToFloat(Number), True);
        end;
      '.':
        begin
          CheckFirst;
          if Pos('.', Number) = 0 then
            Number := Number + '.';
        end;
      #8:
        begin
          CheckFirst;
          if Length(Number) = 1 then
            Number := '0'
          else
            Number := LeftStr(Number, Length(Number) - 1);
          SetDisplay(StrToFloat(Number), True); { !!! }
        end;
      'H':
        begin
          GetDisplay(R);
          X := trunc(abs(R));
          Number := IntToHex(longint(X), 8);
          HexShown := True;
        end;
      'C':
        begin
          CheckFirst;
          SetDisplay(0, True);
        end;
      '^', '+', '-', '*', '/', '%', '=':
        begin
          if (Key[1] = '=') and (Status = csFirst) then //for repeat last operator
          begin
            Status := csValid;
            R := LastResult;
            CurrentOperator := LastOperator;
          end
          else
            GetDisplay(R);

          if (Status = csValid) then
          begin
            Started := True;
            if CurrentOperator = '=' then
              s := ' '
            else
              s := CurrentOperator;
            Log(s + FloatToStrF(R, ffGeneral, MaxDecimals, MaxDigits));
            Status := csFirst;
            LastOperator := CurrentOperator;
            LastResult := R;
            if Key = '%' then
            begin
              case CurrentOperator of
                '+', '-': R := Operand * R / 100;
                '*', '/': R := R / 100;
              end;
            end;

            case CurrentOperator of
              '^': if (Operand = 0) and (R <= 0) then Error else SetDisplay(Power(Operand, R), False);
              '+': SetDisplay(Operand + R, False);
              '-': SetDisplay(Operand - R, False);
              '*': SetDisplay(Operand * R, False);
              '/': if R = 0 then Error else SetDisplay(Operand / R, False);
            end;
            if (Key[1] = '=') then
              Log('=' + Number);
          end;
          CurrentOperator := Key[1];
          GetDisplay(Operand);
        end;
    else
      Result := False;
    end;
  Refresh;
end;

procedure TCalculator.Clear;
begin
  if Started then
    Log(AddChar('-', '', MaxDigits + 1 + MaxDecimals));
  Started := False;
  Status := csFirst;
  Number := '0';
  Sign := ' ';
  CurrentOperator := '=';
end;

end.

