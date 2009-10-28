unit posEdits;

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, Types,
  posUtils, posControls;

type
  TposEdit = class(TposLabeledFrame)
  private
    FText: TCaption;
    FMuliLine: Boolean;
    FReadOnly: Boolean;
    FTextMargin: Integer;
    FPassword: Boolean;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    procedure SetText(Value: TCaption);
    procedure SetTextMargin(const Value: Integer);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetLayout(const Value: TTextLayout);
  protected
    function DoKeyPress(var Key: Char): Boolean; override;
    procedure GetText(var vText: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTextStyle: TTextStyle; override;
    function GetInputs: TposFrameInputs; override;
    procedure PaintInner(Canvas: TCanvas; const Rect: TRect; Color: TColor); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property TextMargin: Integer read FTextMargin write SetTextMargin default 1;
    property MuliLine: Boolean read FMuliLine write FMuliLine default False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Text: TCaption read FText write SetText;
    property Password:Boolean read FPassword write FPassword default False; 
  end;

implementation

procedure TposEdit.SetText(Value: TCaption);
begin
  if FText <> Value then
  begin
    FText := Value;
    Invalidate;
  end;
end;

constructor TposEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption] + [csOpaque];
  Style := Style + [fsOpaque];
  Width := 60;
  Height := 22;
  FTextMargin := 1;
  FLayout := tlCenter;
  TabStop := True;
end;

destructor TposEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TposEdit.GetText(var vText: string);
begin
  vText := FText;
end;

function TposEdit.GetTextStyle: TTextStyle;
begin
  Result := inherited GetTextStyle;
  if FMuliLine then
  begin
    Result.SingleLine := False;
    Result.Wordbreak := True;
  end
  else
  begin
    Result.SingleLine := True;
    Result.Wordbreak := False;
  end;
  Result.Alignment := FAlignment;
  BidiAlignment(Result);
end;

function TposEdit.GetInputs: TposFrameInputs;
begin
  Result := inherited GetInputs;
  Result := Result + [fiText];
  if ReadOnly then
    Result := Result - [fiFocus] + [fiReadOnly];
end;

procedure TposEdit.SetTextMargin(const Value: Integer);
begin
  if FTextMargin <> Value then
  begin
    FTextMargin := Value;
    Refresh;
  end;
end;

procedure TposEdit.PaintInner(Canvas: TCanvas; const Rect: TRect; Color: TColor);
var
  aRect: TRect;
  aStyle: TTextStyle;
  aText: string;
begin
  inherited;
  aStyle := GetTextStyle;
  aRect := Rect;
  GetText(aText);
  if Password then
    aText := StringOfChar('*', Length(aText));
  InflateRect(aRect, -TextMargin, -TextMargin);
  PaintText(Canvas, aText, aRect, aStyle);
end;

procedure TposEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Refresh;
  end;
end;

procedure TposEdit.SetLayout(const Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Refresh;
  end;
end;

function TposEdit.DoKeyPress(var Key: Char): Boolean;
begin
  case Key of
    #8:
      begin
        Text := Copy(Text, 1, Length(Text) - 1);
        Result := True;
      end;
    #32..#255:
      begin
        Text := Text + Key;
        Result := True;
      end;
  else
    Result := inherited DoKeyPress(Key);
  end;
end;

initialization
finalization
end.

