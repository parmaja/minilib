unit mnrNodes;

{$IFDEF FPC}
{$MODE delphi}
{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, mnrClasses;

type

  TmnrBooleanLayout = class(TmnrLayout)
  protected
    function CreateCell(vRow: TmnrRow): TmnrCell; override;
  end;

  TmnrTextLayout = class(TmnrLayout)
  protected
    function CreateCell(vRow: TmnrRow): TmnrCell; override;
  end;

  TmnrDataLayout = class(TmnrLayout)
  protected
    function CreateCell(vRow: TmnrRow): TmnrCell; override;
  end;

  TmnrIntegerLayout = class(TmnrLayout)
  protected
    procedure ScaleCell(vCell: TmnrCell; Invert: Boolean); override;
  protected
    function CreateCell(vRow: TmnrRow): TmnrCell; override;
  end;

  TmnrCurrencyLayout = class(TmnrIntegerLayout)
  protected
    function CreateCell(vRow: TmnrRow): TmnrCell; override;
  end;

  TmnrDoubleLayout = class(TmnrIntegerLayout)
  protected
    function CreateCell(vRow: TmnrRow): TmnrCell; override;
  end;

  TmnrDateTimeLayout = class(TmnrLayout)
  protected
    function CreateCell(vRow: TmnrRow): TmnrCell; override;
  end;

  TmnrTimeLayout = class(TmnrLayout)
  protected
    function CreateCell(vRow: TmnrRow): TmnrCell; override;
  end;

//************************ CELLS ************************


  TmnrBooleanReportCell = class(TmnrCell)
  private
    FValue: Boolean;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure DoSumCell(vCell: TmnrCell); override;

  end;

  TmnrTextReportCell = class(TmnrCell)
  private
    FValue: string;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  end;


  TmnrIntegerReportCell = class(TmnrCell)
  private
    FValue: Integer;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure DoSumCell(vCell: TmnrCell); override;


  public
    function DisplayText: string; override;
  end;

  TmnrDataReportCell = class(TmnrTextReportCell)
  private
    FData: Integer;
  protected
    function GetAsData: Integer; override;
    procedure SetAsData(const Value: Integer); override;
  end;

  TmnrDateTimeReportCell = class(TmnrCell)
  private
    FValue: TDateTime;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;

  public
    function DisplayText: string; override;
  end;

  TmnrTimeReportCell = class(TmnrDateTimeReportCell)
  protected
    function GetAsString: string; override;
  end;

  TmnrBaseCurrencyReportCell = class(TmnrCell)
  private
    FData: Integer;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;
    function GetAsData: Integer; override;


    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure SetAsData(const Value: Integer); override;
    procedure DoSumCell(vCell: TmnrCell); override;

  public
    function DisplayText: string; override;
  end;

  TmnrCurrencyReportCell = class(TmnrBaseCurrencyReportCell)
  private
    FValue: Currency;
  protected
    function GetAsCurrency: Currency; override;
    procedure SetAsCurrency(const Value: Currency); override;
  end;

  TmnrReportTotalCell = class(TmnrBaseCurrencyReportCell)
  protected
    function GetAsCurrency: Currency; override;
  end;

  TmnrPageTotalCell = class(TmnrBaseCurrencyReportCell)
  protected
    function GetAsCurrency: Currency; override;
  end;

  TmnrToPageTotalCell = class(TmnrBaseCurrencyReportCell)
  protected
    function GetAsCurrency: Currency; override;
  end;

  TmnrDoubleReportCell = class(TmnrCell)
  private
    FValue: Double;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;


    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsDouble(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure DoSumCell(vCell: TmnrCell); override;

  end;

implementation

{ TmnrTextReportCell }

function TmnrTextReportCell.GetAsBoolean: Boolean;
begin
  Result := StrToBoolDef(AsString, False);
end;

function TmnrTextReportCell.GetAsCurrency: Currency;
begin
  Result := StrToCurrDef(AsString, 0);
end;

function TmnrTextReportCell.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTimeDef(AsString, 0);
end;

function TmnrTextReportCell.GetAsDouble: Double;
begin
  Result := StrToFloatDef(AsString, 0);
end;

function TmnrTextReportCell.GetAsInteger: Longint;
begin
  Result := StrToIntDef(AsString, 0);
end;

function TmnrTextReportCell.GetAsString: string;
begin
  Result := FValue;
end;

function TmnrTextReportCell.GetAsVariant: Variant;
begin
  Result := FValue;
end;

function TmnrTextReportCell.GetIsNull: Boolean;
begin
  Result := FValue <> '';
end;

procedure TmnrTextReportCell.SetAsBoolean(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TmnrTextReportCell.SetAsCurrency(const Value: Currency);
begin
  FValue := CurrToStr(Value);
end;

procedure TmnrTextReportCell.SetAsDateTime(const Value: TDateTime);
begin
  FValue := DateTimeToStr(Value);
end;

procedure TmnrTextReportCell.SetAsDouble(const Value: Double);
begin
  FValue := FloatToStr(Value);
end;

procedure TmnrTextReportCell.SetAsInteger(const Value: Integer);
begin
  FValue := IntToStr(Value);
end;

procedure TmnrTextReportCell.SetAsString(const Value: string);
begin
  FValue := Value;
end;

procedure TmnrTextReportCell.SetAsVariant(const Value: Variant);
begin
  FValue := Value;
end;

{ TmnrTextLayout }

function TmnrTextLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := TmnrTextReportCell.Create(vRow);
end;

{ TmnrIntegerLayout }

function TmnrIntegerLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := TmnrIntegerReportCell.Create(vRow);
end;

procedure TmnrIntegerLayout.ScaleCell(vCell: TmnrCell; Invert: Boolean);
var
  v: Double;
begin
  with vCell.DesignCell do
  begin
    if AppendTotals then
    begin
      v           := vCell.AsDouble;
      if Invert then
        v := -v;
      SubTotal    := SubTotal + v;
      Total       := Total + v;
      PageTotal   := PageTotal + v;
      ToPageTotal := ToPageTotal + v;
      if (Count=0) or (v<MinValue) then MinValue := v;
      if (Count=0) or (v>MaxValue) then MaxValue := v;
    end;
  end;
end;

{ TmnrIntegerReportCell }

function TmnrIntegerReportCell.DisplayText: string;
begin
  if AsInteger=0 then
    Result:= ''
  else
    Result := inherited DisplayText;
end;

procedure TmnrIntegerReportCell.DoSumCell(vCell: TmnrCell);
begin
  AsInteger := AsInteger + vCell.AsInteger;
end;

function TmnrIntegerReportCell.GetAsBoolean: Boolean;
begin
  Result := AsInteger <> 0;
end;

function TmnrIntegerReportCell.GetAsCurrency: Currency;
begin
  Result := AsInteger;
end;

function TmnrIntegerReportCell.GetAsDateTime: TDateTime;
begin
  Result := AsInteger;
end;

function TmnrIntegerReportCell.GetAsDouble: Double;
begin
  Result := AsInteger;
end;

function TmnrIntegerReportCell.GetAsInteger: Longint;
begin
  Result := FValue;
end;

function TmnrIntegerReportCell.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

function TmnrIntegerReportCell.GetAsVariant: Variant;
begin
  Result := FValue;
end;

function TmnrIntegerReportCell.GetIsNull: Boolean;
begin
  Result := False;
end;

procedure TmnrIntegerReportCell.SetAsBoolean(const Value: Boolean);
begin
  FValue := Ord(Value);
end;

procedure TmnrIntegerReportCell.SetAsCurrency(const Value: Currency);
begin
  FValue := Trunc(Value);
end;

procedure TmnrIntegerReportCell.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Trunc(Value);
end;

procedure TmnrIntegerReportCell.SetAsDouble(const Value: Double);
begin
  FValue := Trunc(Value);
end;

procedure TmnrIntegerReportCell.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TmnrIntegerReportCell.SetAsString(const Value: string);
begin
  FValue := StrToIntDef(Value, 0);
end;

procedure TmnrIntegerReportCell.SetAsVariant(const Value: Variant);
begin
  FValue := Value;
end;

{ TmnrDateTimeLayout }

function TmnrDateTimeLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := TmnrDateTimeReportCell.Create(vRow);
end;

{ TmnrDateTimeReportCell }

function TmnrDateTimeReportCell.DisplayText: string;
begin
  if AsDateTime=0 then
    Result := ''
  else
    Result := inherited DisplayText;
end;

function TmnrDateTimeReportCell.GetAsBoolean: Boolean;
begin
  Result := AsDateTime <> 0;
end;

function TmnrDateTimeReportCell.GetAsCurrency: Currency;
begin
  Result := AsDateTime;
end;

function TmnrDateTimeReportCell.GetAsDateTime: TDateTime;
begin
  Result := FValue;
end;

function TmnrDateTimeReportCell.GetAsDouble: Double;
begin
  Result := AsDateTime;
end;

function TmnrDateTimeReportCell.GetAsInteger: Longint;
begin
  Result := Trunc(AsDateTime);
end;

function TmnrDateTimeReportCell.GetAsString: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', AsDateTime);
end;

function TmnrDateTimeReportCell.GetAsVariant: Variant;
begin
  Result := FValue;
end;

function TmnrDateTimeReportCell.GetIsNull: Boolean;
begin
  Result := FValue <> 0;
end;

procedure TmnrDateTimeReportCell.SetAsBoolean(const Value: Boolean);
begin
  FValue := Ord(Value);
end;

procedure TmnrDateTimeReportCell.SetAsCurrency(const Value: Currency);
begin
  FValue := Value;
end;

procedure TmnrDateTimeReportCell.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value;
end;

procedure TmnrDateTimeReportCell.SetAsDouble(const Value: Double);
begin
  FValue := Value;
end;

procedure TmnrDateTimeReportCell.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TmnrDateTimeReportCell.SetAsString(const Value: string);
begin
  FValue := StrToDateTimeDef(Value, 0);
end;

procedure TmnrDateTimeReportCell.SetAsVariant(const Value: Variant);
begin
  FValue := Value;
end;

{ TmnrCurrencyLayout }

function TmnrCurrencyLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := TmnrCurrencyReportCell.Create(vRow);
end;

{ TmnrBaseCurrencyReportCell }

function TmnrBaseCurrencyReportCell.DisplayText: string;
begin
  {if AsCurrency=0 then
    Result := ''
  else}
    Result := inherited DisplayText;
end;

procedure TmnrBaseCurrencyReportCell.DoSumCell(vCell: TmnrCell);
begin
  AsCurrency := AsCurrency + vCell.AsCurrency;
end;

function TmnrBaseCurrencyReportCell.GetAsBoolean: Boolean;
begin
  Result := AsCurrency <> 0;
end;

function TmnrBaseCurrencyReportCell.GetAsCurrency: Currency;
begin
  Result := 0; //abstract
end;

function TmnrBaseCurrencyReportCell.GetAsData: Integer;
begin
  Result := FData;
end;

function TmnrBaseCurrencyReportCell.GetAsDateTime: TDateTime;
begin
  Result := AsCurrency;
end;

function TmnrBaseCurrencyReportCell.GetAsDouble: Double;
begin
  Result := AsCurrency;
end;

function TmnrBaseCurrencyReportCell.GetAsInteger: Longint;
begin
  Result := Trunc(AsCurrency);
end;

function TmnrBaseCurrencyReportCell.GetAsString: string;
begin
  Result := CurrToStr(AsCurrency);
end;

function TmnrBaseCurrencyReportCell.GetAsVariant: Variant;
begin
  Result := AsCurrency;
end;

function TmnrBaseCurrencyReportCell.GetIsNull: Boolean;
begin
  Result := False;
end;

procedure TmnrBaseCurrencyReportCell.SetAsBoolean(const Value: Boolean);
begin
  AsCurrency := Ord(Value);
end;

procedure TmnrBaseCurrencyReportCell.SetAsCurrency(const Value: Currency);
begin
end;

procedure TmnrBaseCurrencyReportCell.SetAsData(const Value: Integer);
begin
  FData := Value;
end;

procedure TmnrBaseCurrencyReportCell.SetAsDateTime(const Value: TDateTime);
begin
  AsCurrency := Value;
end;

procedure TmnrBaseCurrencyReportCell.SetAsDouble(const Value: Double);
begin
  AsCurrency := Value;
end;

procedure TmnrBaseCurrencyReportCell.SetAsInteger(const Value: Integer);
begin
  AsCurrency := Value;
end;

procedure TmnrBaseCurrencyReportCell.SetAsString(const Value: string);
begin
  AsCurrency := StrToCurrDef(Value, 0);
end;

procedure TmnrBaseCurrencyReportCell.SetAsVariant(const Value: Variant);
begin
  AsCurrency := Value;
end;

{ TmnrDoubleLayout }

function TmnrDoubleLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := TmnrDoubleReportCell.Create(vRow);
end;

{ TmnrDoubleReportCell }

procedure TmnrDoubleReportCell.DoSumCell(vCell: TmnrCell);
begin
  AsDouble := AsDouble + vCell.AsDouble;
end;

function TmnrDoubleReportCell.GetAsBoolean: Boolean;
begin
  Result := AsDouble <> 0;
end;

function TmnrDoubleReportCell.GetAsCurrency: Currency;
begin
  Result := AsDouble;
end;

function TmnrDoubleReportCell.GetAsDateTime: TDateTime;
begin
  Result := AsDouble;
end;

function TmnrDoubleReportCell.GetAsDouble: Double;
begin
  Result := FValue;
end;

function TmnrDoubleReportCell.GetAsInteger: Longint;
begin
  Result := Trunc(FValue);
end;

function TmnrDoubleReportCell.GetAsString: string;
begin
  Result := FloatToStr(FValue);
end;

function TmnrDoubleReportCell.GetAsVariant: Variant;
begin
  Result := FValue;
end;

function TmnrDoubleReportCell.GetIsNull: Boolean;
begin
  Result := False;
end;

procedure TmnrDoubleReportCell.SetAsBoolean(const Value: Boolean);
begin
  FValue := Ord(Value);
end;

procedure TmnrDoubleReportCell.SetAsCurrency(const Value: Currency);
begin
  FValue := Value;
end;

procedure TmnrDoubleReportCell.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value;
end;

procedure TmnrDoubleReportCell.SetAsDouble(const Value: Double);
begin
  FValue := Value;
end;

procedure TmnrDoubleReportCell.SetAsInteger(const Value: Integer);
begin
  FValue := Trunc(Value);
end;

procedure TmnrDoubleReportCell.SetAsString(const Value: string);
begin
  FValue := StrToFloatDef(Value, 0);
end;

procedure TmnrDoubleReportCell.SetAsVariant(const Value: Variant);
begin
  FValue := Value;
end;

{ TmnrBooleanLayout }

function TmnrBooleanLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := TmnrBooleanReportCell.Create(vRow);
end;

{ TmnrBooleanReportCell }

procedure TmnrBooleanReportCell.DoSumCell(vCell: TmnrCell);
begin
  //inherited;

end;

function TmnrBooleanReportCell.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TmnrBooleanReportCell.GetAsCurrency: Currency;
begin
  Result := Ord(AsBoolean);
end;

function TmnrBooleanReportCell.GetAsDateTime: TDateTime;
begin
  Result := 0;
end;

function TmnrBooleanReportCell.GetAsDouble: Double;
begin
  Result := Ord(AsBoolean);
end;

function TmnrBooleanReportCell.GetAsInteger: Longint;
begin
  Result := Ord(AsBoolean);
end;

function TmnrBooleanReportCell.GetAsString: string;
begin
  Result := BoolToStr(FValue);
end;

function TmnrBooleanReportCell.GetAsVariant: Variant;
begin
  Result := FValue;
end;

function TmnrBooleanReportCell.GetIsNull: Boolean;
begin
  Result := False;
end;

procedure TmnrBooleanReportCell.SetAsBoolean(const Value: Boolean);
begin
  FValue := Value;
end;

procedure TmnrBooleanReportCell.SetAsCurrency(const Value: Currency);
begin
  FValue := Value <> 0;
end;

procedure TmnrBooleanReportCell.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value <> 0;
end;

procedure TmnrBooleanReportCell.SetAsDouble(const Value: Double);
begin
  FValue := Value <> 0;
end;

procedure TmnrBooleanReportCell.SetAsInteger(const Value: Integer);
begin
  FValue := Value <> 0;
end;

procedure TmnrBooleanReportCell.SetAsString(const Value: string);
begin
  FValue := StrToBoolDef(Value, False);
end;

procedure TmnrBooleanReportCell.SetAsVariant(const Value: Variant);
begin
  FValue := Value;
end;

{ TmnrTimeLayout }

function TmnrTimeLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := TmnrTimeReportCell.Create(vRow);
end;

{ TmnrTimeReportCell }

function TmnrTimeReportCell.GetAsString: string;
begin
  Result := FormatDateTime('hh:nn:ss', AsDateTime);
end;

{ TmnrDataReportCell }

function TmnrDataReportCell.GetAsData: Integer;
begin
  Result := FData;
end;

procedure TmnrDataReportCell.SetAsData(const Value: Integer);
begin
  FData := Value;
end;

{ TmnrDataLayout }

function TmnrDataLayout.CreateCell(vRow: TmnrRow): TmnrCell;
begin
  Result := TmnrDataReportCell.Create(vRow);
end;

{ TmnrPageTotalCell }

function TmnrPageTotalCell.GetAsCurrency: Currency;
begin
  Result := DesignCell.PageTotal;
end;

{ TmnrToPageTotalCell }

function TmnrToPageTotalCell.GetAsCurrency: Currency;
begin
  Result := DesignCell.ToPageTotal;
end;

{ TmnrTotalCell }

function TmnrReportTotalCell.GetAsCurrency: Currency;
begin
  Result := DesignCell.Total;
end;

{ TmnrCurrencyReportCell }

function TmnrCurrencyReportCell.GetAsCurrency: Currency;
begin
  Result := FValue;
end;

procedure TmnrCurrencyReportCell.SetAsCurrency(const Value: Currency);
begin
  FValue := Value;
end;

initialization
  DefaultCellClass := TmnrTextReportCell;

end.

