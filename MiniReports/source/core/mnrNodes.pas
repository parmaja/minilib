unit mnrNodes;

{$IFDEF FPC}
{$MODE delphi}
{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Contnrs, mnrClasses;

type

  TmnrTextReportCell = class(TmnrReportCell)
  private
    FValue: string;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  end;

  TmnrTextLayout = class(TmnrLayout)
  protected
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; override;
  end;

  TmnrIntegerReportCell = class(TmnrReportCell)
  private
    FValue: Integer;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  end;

  TmnrIntegerLayout = class(TmnrLayout)
  protected
    procedure ScaleCell(vCell: TmnrCustomReportCell); override;
  protected
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; override;
  end;

  TmnrDateTimeReportCell = class(TmnrReportCell)
  private
    FValue: TDateTime;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  end;

  TmnrDateTimeLayout = class(TmnrLayout)
  protected
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; override;
  end;

  TmnrCurrencyReportCell = class(TmnrReportCell)
  private
    FValue: Currency;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  end;

  TmnrCurrencyLayout = class(TmnrIntegerLayout)
  protected
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; override;
  end;

  TmnrDoubleReportCell = class(TmnrReportCell)
  private
    FValue: Double;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  end;

  TmnrDoubleLayout = class(TmnrIntegerLayout)
  protected
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; override;
  end;

  TmnrBooleanReportCell = class(TmnrReportCell)
  private
    FValue: Boolean;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull: Boolean; override;

    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsCurrency(const Value: Currency); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Longint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  end;

  TmnrBooleanLayout = class(TmnrLayout)
  protected
    function CreateCell(vCells: TmnrReportCells): TmnrReportCell; override;
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

function TmnrTextReportCell.GetAsFloat: Double;
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
  Result := FValue<>'';
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

procedure TmnrTextReportCell.SetAsFloat(const Value: Double);
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

function TmnrTextLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := TmnrTextReportCell.Create(vCells);
end;

{ TmnrIntegerLayout }

function TmnrIntegerLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := TmnrIntegerReportCell.Create(vCells);
end;

procedure TmnrIntegerLayout.ScaleCell(vCell: TmnrCustomReportCell);
begin
  inherited;
  if vCell.Reference<>nil then
  begin
    vCell.Reference.Total := vCell.Reference.Total + vCell.AsFloat;
  end;
end;

{ TmnrIntegerReportCell }

function TmnrIntegerReportCell.GetAsBoolean: Boolean;
begin
  Result := AsInteger<>0;
end;

function TmnrIntegerReportCell.GetAsCurrency: Currency;
begin
  Result := AsInteger;
end;

function TmnrIntegerReportCell.GetAsDateTime: TDateTime;
begin
  Result := AsInteger;
end;

function TmnrIntegerReportCell.GetAsFloat: Double;
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

procedure TmnrIntegerReportCell.SetAsFloat(const Value: Double);
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

function TmnrDateTimeLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := TmnrDateTimeReportCell.Create(vCells);
end;

{ TmnrDateTimeReportCell }

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

function TmnrDateTimeReportCell.GetAsFloat: Double;
begin
  Result := AsDateTime;
end;

function TmnrDateTimeReportCell.GetAsInteger: Longint;
begin
  Result := Trunc(AsDateTime);
end;

function TmnrDateTimeReportCell.GetAsString: string;
begin
  Result := DateToStr(AsDateTime);
end;

function TmnrDateTimeReportCell.GetAsVariant: Variant;
begin
  Result := FValue;
end;

function TmnrDateTimeReportCell.GetIsNull: Boolean;
begin
  Result := FValue<>0;
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

procedure TmnrDateTimeReportCell.SetAsFloat(const Value: Double);
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

function TmnrCurrencyLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := TmnrCurrencyReportCell.Create(vCells);
end;

{ TmnrCurrencyReportCell }

function TmnrCurrencyReportCell.GetAsBoolean: Boolean;
begin
  Result := AsCurrency<>0;
end;

function TmnrCurrencyReportCell.GetAsCurrency: Currency;
begin
  Result := FValue;
end;

function TmnrCurrencyReportCell.GetAsDateTime: TDateTime;
begin
  Result := AsCurrency;
end;

function TmnrCurrencyReportCell.GetAsFloat: Double;
begin
  Result := AsCurrency;
end;

function TmnrCurrencyReportCell.GetAsInteger: Longint;
begin
  Result := Trunc(FValue);
end;

function TmnrCurrencyReportCell.GetAsString: string;
begin
  Result := CurrToStr(FValue);
end;

function TmnrCurrencyReportCell.GetAsVariant: Variant;
begin
  Result := FValue;
end;

function TmnrCurrencyReportCell.GetIsNull: Boolean;
begin
  Result := False;
end;

procedure TmnrCurrencyReportCell.SetAsBoolean(const Value: Boolean);
begin
  FValue := Ord(Value);
end;

procedure TmnrCurrencyReportCell.SetAsCurrency(const Value: Currency);
begin
  FValue := Value;
end;

procedure TmnrCurrencyReportCell.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value;
end;

procedure TmnrCurrencyReportCell.SetAsFloat(const Value: Double);
begin
  FValue := Value;
end;

procedure TmnrCurrencyReportCell.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TmnrCurrencyReportCell.SetAsString(const Value: string);
begin
  FValue := StrToCurrDef(Value, 0);
end;

procedure TmnrCurrencyReportCell.SetAsVariant(const Value: Variant);
begin
  FValue := Value;
end;

{ TmnrDoubleLayout }

function TmnrDoubleLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := TmnrDoubleReportCell.Create(vCells);
end;

{ TmnrDoubleReportCell }

function TmnrDoubleReportCell.GetAsBoolean: Boolean;
begin
  Result := AsFloat<>0;
end;

function TmnrDoubleReportCell.GetAsCurrency: Currency;
begin
  Result := AsFloat;
end;

function TmnrDoubleReportCell.GetAsDateTime: TDateTime;
begin
  Result := AsFloat;
end;

function TmnrDoubleReportCell.GetAsFloat: Double;
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

procedure TmnrDoubleReportCell.SetAsFloat(const Value: Double);
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

function TmnrBooleanLayout.CreateCell(vCells: TmnrReportCells): TmnrReportCell;
begin
  Result := TmnrBooleanReportCell.Create(vCells);
end;

{ TmnrBooleanReportCell }

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

function TmnrBooleanReportCell.GetAsFloat: Double;
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
  FValue := Value<>0;
end;

procedure TmnrBooleanReportCell.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value<>0;
end;

procedure TmnrBooleanReportCell.SetAsFloat(const Value: Double);
begin
  FValue := Value<>0;
end;

procedure TmnrBooleanReportCell.SetAsInteger(const Value: Integer);
begin
  FValue := Value<>0;
end;

procedure TmnrBooleanReportCell.SetAsString(const Value: string);
begin
  FValue := StrToBoolDef(Value, False);
end;

procedure TmnrBooleanReportCell.SetAsVariant(const Value: Variant);
begin
  FValue := Value;
end;

initialization
  DefaultCellClass := TmnrTextReportCell;

end.