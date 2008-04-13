unit FPCalculators;

interface

uses
  SysUtils, Math;

const
  MaxDecimals = 10;
  MaxDigits   = 30;

type
  TCalcState = (csFirst, csValid, csError);

  TFPCalculator = class(TObject)
  private
    procedure GetDisplay(var R: Extended);
    procedure SetDisplay(R: Extended;ShouldKeepZeroes : boolean);
    procedure Error;
  protected
    Status: TCalcState;
    Number: string[MaxDigits];
    Sign: Char;
    CurrentOperator,
    LastOperator: Char;
    LastR,
    Operand: Extended;
    HaveMemory: Boolean;
    Memory: Extended;
    DispNumber: Extended;
    HexShown : Boolean;
  public
    constructor Create;
    procedure Clear;
    procedure Reset;
    procedure Refresh; virtual;
    function CalcKey(Key: string): boolean;
  end;

implementation

constructor TFPCalculator.Create;
begin
  inherited Create;
  Clear;
  HexShown:=False;
end;

procedure TFPCalculator.GetDisplay(var R: Extended);
begin
  R := DispNumber;
end;

procedure TFPCalculator.Refresh;
begin
end;

procedure TFPCalculator.Reset;
begin
  Clear;
  HaveMemory := False;
  Memory := 0;
end;

procedure TFPCalculator.SetDisplay(R: Extended;ShouldKeepZeroes : boolean);
var
  S: string[MaxDigits];
  i,KeepZeroes : byte;
begin
  DispNumber:=R;
  KeepZeroes:=0;
  if ShouldKeepZeroes and (pos('.',Number)>0) then
    for i:=Length(Number) downto pos('.',Number)+1 do
      if Number[i]='0' then
        inc(KeepZeroes)
      else
        break;

  Str(R: 0: MaxDecimals, S);
  if Pos('.',S) <> 0 then
     while (Length(S)>1) and (S[Length(S)]='0') do Dec(S[0]);
  if KeepZeroes > 0 then
    for i:=1 to KeepZeroes do
      S:=S + '0';
  if S[1] <> '-' then Sign := ' ' else
  begin
    Delete(S, 1, 1);
    Sign := '-';
  end;
  
  if Length(S) > MaxDigits + 1 + MaxDecimals then Error
  else
  begin
    if S[Length(S)] = '.' then Dec(S[0]);
    Number := S;
  end;
end;

procedure TFPCalculator.Error;
begin
  Status := csError;
  Number := 'Error';
  Sign := ' ';
  Refresh;
end;

function TFPCalculator.CalcKey(Key: string): boolean;
var
  R: Extended;
  X : cardinal;
  procedure CheckFirst;
  begin
    if Status = csFirst then
    begin
      Status := csValid;
      SetDisplay(0,False);
    end;
  end;

begin
  Result := True;
  Key := UpperCase(Key);
  if (Status = csError) and (Key <> 'C') then Key := ' ';
  if HexShown then
    begin
      GetDisplay(R);
      SetDisplay(R,False);
      HexShown := False;
      if Key = 'H' then
        Key := ' ';
    end;
  if Key='X^Y' then
    Key:='^'
  else if Key=#13 then
    Key:='='
  else if Key=#27 then
    Key:='C'
  else if (Key = '_') or (Key = #241) then
    Key := '+/-';

  if Length(Key)>1 then
     begin
{        if Status = csFirst then}
        begin
{          Status := csValid;}
          GetDisplay(R);
          if Key='AC' then
            Clear
          else if Key='ON' then
            Reset
          else if Key='1/X' then
          begin
            if R=0 then
              Error
            else
              SetDisplay(1/R,False);
          end
          else if Key='SQRT' then begin if R < 0 then Error else SetDisplay(sqrt(R),False) end else
          if Key='LOG' then begin if R<=0 then Error else SetDisplay(ln(R),False) end else
          if Key='X^2' then SetDisplay(R*R,False) else
          if (Key='+/-') then
          begin
            if Sign = ' ' then
              Sign := '-'
            else
              Sign := ' ';
            GetDisplay(R);
            SetDisplay(-R,True);
          end
          else if Key='M+' then
          begin
            Memory := Memory + R;
            HaveMemory := True;
          end
          else if Key='M-' then
          begin
            Memory:=Memory - R;
            HaveMemory := True;
          end
          else if Key='MR' then
          begin
            CheckFirst;
            SetDisplay(Memory, False)
          end
          else if Key='MC' then
          begin
            Memory := 0;
            HaveMemory := False;
          end;
        end;
     end
  else
  case Key[1] of
    '0'..'9':
    if Length(Number)<MaxDigits then
      begin
        CheckFirst;
        if Number = '0' then
          Number := '';
        Number := Number + Key;
        SetDisplay(StrToFloat(Number),True);
      end;
    '.':
      begin
        CheckFirst;
        if Pos('.', Number) = 0 then Number := Number + '.';
      end;
    #8:
      begin
        CheckFirst;
        if Length(Number) = 1 then
          Number := '0'
        else
          Dec(Number[0]);
        SetDisplay(StrToFloat(Number),True); { !!! }
      end;
    'H':
      begin
        GetDisplay(R);
        X:=trunc(abs(R));
        Number:=IntToHex(longint(X),8);
        HexShown:=True;
      end;
    '+', '-', '*', '/', '=', '%', '^':
      begin
        if (Key[1]='=') and (Status=csFirst) then
          begin
            Status:=csValid;
            R:=LastR;
            CurrentOperator:=LastOperator;
          end
        else
          GetDisplay(R);
        if (Status = csValid)  then
        begin
          Status := csFirst;
          LastR:=R;
          LastOperator:=CurrentOperator;
          if Key = '%' then
            case CurrentOperator of
              '+', '-': R := Operand * R / 100;
              '*', '/': R := R / 100;
            end;
          case CurrentOperator of
            '^': if (Operand = 0)and(R <= 0) then Error else SetDisplay(Power(Operand,R),False);
            '+': SetDisplay(Operand + R,False);
            '-': SetDisplay(Operand - R,False);
            '*': SetDisplay(Operand * R,False);
            '/': if R = 0 then Error else SetDisplay(Operand / R,False);
          end;
        end;
        CurrentOperator := Key[1];
        GetDisplay(Operand);
      end;
    'C':
      begin
        CheckFirst;
        SetDisplay(0,True);
      end;
    else
      Result := False;
  end;
  Refresh;
end;

procedure TFPCalculator.Clear;
begin
  Status := csFirst;
  Number := '0';
  Sign := ' ';
  CurrentOperator := '=';
end;

end.
