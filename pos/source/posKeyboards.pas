unit posKeyboards;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, Types, Contnrs,
{$IFDEF FPC}
  LCLType,
  LConvEncoding,
{$ELSE}
  Windows,
{$ENDIF}
  posTypes, posControls, posButtons;

const
  cKeyboardButtonColor = $00AA8264;
  VK_EQUALS = $bb;

type
  TCtrlKey = (ckNone, ckTab, ckCtrl, ckAlt, ckSpace, ckLanguage, ckEscape, ckClear, ckEnter, ckShift, ckCapsLock, ckBackspace, ckPAD);

  TposKeyboard = class;

  TposKeyLanguages = class;
  TposKeyLanguage = class;

  TposKeyInfo = record
    VirtualKey: Word;
  end;

  TposKeyLanguageItem = class(TObject)
  private
    FLanguage: TposKeyLanguage;
    FShiftCaption: string;
    FNormalCaption: string;
    FName: string;
  public
    constructor Create(Language: TposKeyLanguage; Name, Normal, Shift: string; VirtualKey: Word = 0);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property NormalCaption: string read FNormalCaption write FNormalCaption;
    property ShiftCaption: string read FShiftCaption write FShiftCaption;
  end;

  TposKeyLanguage = class(TObjectList)
  private
    FLanguages: TposKeyLanguages;
    FName: string;
    FShortName: string;
    function GetItem(Index: Integer): TposKeyLanguageItem;
    procedure SetItem(Index: Integer; const Value: TposKeyLanguageItem);
  public
    constructor Create(Name, ShortName: string; Languages: TposKeyLanguages);
    destructor Destroy; override;
    function Find(vName: string): TposKeyLanguageItem;
    property Items[Index: Integer]: TposKeyLanguageItem read GetItem write SetItem; default;
    property Name: string read FName write FName;
    property ShortName: string read FShortName write FShortName;
  end;

  TposKeyLanguages = class(TObjectList)
  private
    function GetItem(Index: Integer): TposKeyLanguage;
    procedure SetItem(Index: Integer; const Value: TposKeyLanguage);
  public
    function FindByName(vName: string): TposKeyLanguage;
    function FindByShortName(vShortName: string): TposKeyLanguage;
    function IndexOfName(vName: string): Integer;
    function IndexOfShortName(vShortName: string): Integer;
    property Items[Index: Integer]: TposKeyLanguage read GetItem write SetItem; default;
  end;

  TposCustomKeyboardKey = class(TObject)
  private
    FKeyboard: TposKeyboard;
    FSize: Integer;
    FRow: Integer;
    FBoundsRect: TRect;
    FVisible: Boolean;
    FState: Boolean;
    FName: string;
    FVirtualKey: Word;
    procedure SetBoundsRect(const Value: TRect);
    procedure SetVisible(const Value: Boolean);
    procedure SetName(const Value: string);
  protected
    procedure Paint(Canvas: TCanvas; Rect: TRect); virtual;
    function GetButtonColor: TColor; virtual;
    procedure Click; virtual;
    procedure Pressed;
    procedure Release;
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
  public
    constructor Create(AKeyboard: TposKeyboard);
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property Size: Integer read FSize write FSize;
    property Keyboard: TposKeyboard read FKeyboard;
    property Row: Integer read FRow write FRow;
    property Visible: Boolean read FVisible write SetVisible;
    property VirtualKey: Word read FVirtualKey write FVirtualKey;
  end;

  TposKeyboardKey = class(TposCustomKeyboardKey)
  private
    FLangKey: TposKeyLanguageItem;
    FLangTR: Integer;
  protected
    function GetLangKey: TposKeyLanguageItem;
    procedure Paint(Canvas: TCanvas; Rect: TRect); override;
    procedure Click; override;
  public
  end;

  TposPADKeyboardKey = class(TposCustomKeyboardKey)
  private
  protected
    procedure Paint(Canvas: TCanvas; Rect: TRect); override;
    procedure Click; override;
  public
  end;

  TposCtrlKeyboardKey = class(TposCustomKeyboardKey)
  private
    FCtrlKey: TCtrlKey;
  protected
    procedure Paint(Canvas: TCanvas; Rect: TRect); override;
    procedure Click; override;
  public
    property CtrlKey: TCtrlKey read FCtrlKey write FCtrlKey;
  end;

  TKeyboardButtonList = class(TObjectList)
  private
    function GetItem(Index: Integer): TposCustomKeyboardKey;
    procedure SetItem(Index: Integer; const Value: TposCustomKeyboardKey);
  public
    function InRect(X, Y: Integer): TposCustomKeyboardKey;
    procedure Paint(Canvas: TCanvas; vRect: TRect); virtual;
    property Items[Index: Integer]: TposCustomKeyboardKey read GetItem write SetItem; default;
  end;

  TposShiftState = (pssShift, pssAlt, pssCtrl, pssCapsLock);
  TposShiftStates = set of TposShiftState;

  TOnButtonPress = procedure(Sender: TObject; Value: string) of object;
//  TOnKeyPress = procedure(Sender: TObject; Value: string) of object;
  TOnSendKey = procedure(Sender: TObject; Key: TposKeyInfo) of object;

  { TposKeyboard }
  TposKeyboardKind = (keyNormal, keyNumberPAD);

  TposKeyboard = class(TposFocusFrame)
  private
    FInteractive: Boolean;
    FMultiPAD: Boolean;
    FNormalButtons: TKeyboardButtonList;
    FPADButtons: TKeyboardButtonList;
    FCtrlFont: TFont;
    FBorderColor: TColor;
    FButtonColor: TColor;
    FLanguages: TposKeyLanguages;
    FCurrentLanguage: string;
    FShiftState: TposShiftStates;
    FActiveButton: TposCustomKeyboardKey;
    FOnButtonPress: TOnButtonPress;
    FOnSendKey: TOnSendKey;
    FKind: TposKeyboardKind;
    FButtons: TKeyboardButtonList;
    FPressedColor: TColor;
    FSizeFactor: Integer;
    procedure SetActiveButton(const Value: TposCustomKeyboardKey);
    procedure SetCtrlFont(const Value: TFont);
    procedure SetBorderColor(const Value: TColor);
    procedure SetButtonColor(const Value: TColor);
    procedure SetCurrentLanguage(const Value: string);
    procedure SetMultiPAD(const AValue: Boolean);
    procedure SetShiftState(const Value: TposShiftStates);
    procedure SetKind(const Value: TposKeyboardKind);
    procedure SetButtons(const Value: TKeyboardButtonList);
    procedure SetPressedColor(const Value: TColor);
    procedure CreatePads; virtual;
  protected
    FLangTR: Integer;
    procedure ChangeScale(M, D: Integer); override;
    procedure Resized; override;
    procedure PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor); override;
    procedure CalcButtons;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoButtonPress(Value: string); virtual;
    procedure DoSendKey(Key: TposKeyInfo); virtual;
    procedure InitLanguages;
    procedure CtrlFontChanged(Sender: TObject);
    property ActiveButton: TposCustomKeyboardKey read FActiveButton write SetActiveButton;
    property Buttons: TKeyboardButtonList read FButtons write SetButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendKey(Key: Word);
    function GetNextLanguage:string;
    procedure FlipCurrentLanguage;
    procedure ChooseLanguageByName(Name:string);
    procedure ChooseLanguageByShortName(Name:string);
    property CurrentLanguage: string read FCurrentLanguage write SetCurrentLanguage;
    property Languages: TposKeyLanguages read FLanguages;
    property ShiftState: TposShiftStates read FShiftState write SetShiftState;
  published
    property Kind: TposKeyboardKind read FKind write SetKind;
    property SizeFactor: Integer read FSizeFactor write FSizeFactor default 4;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clDefault;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default cKeyboardButtonColor;
    property PressedColor: TColor read FPressedColor write SetPressedColor default clDefault;
    property CtrlFont: TFont read FCtrlFont write SetCtrlFont;
    property MultiPAD: Boolean read FMultiPAD write SetMultiPAD default True;
// todo
//    property ShowDisplay: Boolean read FShowDisplay write FShowDisplay default False;
    property Interactive: Boolean read FInteractive write FInteractive default False;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnButtonPress: TOnButtonPress read FOnButtonPress write FOnButtonPress;
    property OnSendKey: TOnSendKey read FOnSendKey write FOnSendKey;
  end;

implementation

uses
  posUtils, Math, posDraws;

const
  cSizeFactor = 4;
  cDownDarker = -50;

{ TposKeyboard }

procedure TposKeyboard.CreatePads;
var
  aRow: Integer;
  procedure AddKey(List: TKeyboardButtonList; AName: string; ASize: Integer; AVirtualKey: Word);
  var
    aButton: TposKeyboardKey;
  begin
    aButton := TposKeyboardKey.Create(Self);
    with aButton do
    begin
      Name := AName;
      VirtualKey := AVirtualKey;
      FSize := ASize;
      Row := aRow;
    end;
    List.Add(aButton);
  end;

  procedure AddPADKey(List:TKeyboardButtonList; AName: string; ASize: Integer; AVirtualKey: Word);
  var
    aButton: TposPADKeyboardKey;
  begin
    aButton := TposPADKeyboardKey.Create(Self);
    with aButton do
    begin
      Name := AName;
      VirtualKey := AVirtualKey;
      FSize := ASize;
      Row := aRow;
    end;
    List.Add(aButton);
  end;

  procedure AddCtrlKey(List:TKeyboardButtonList; AName: string; ACtrlKey: TCtrlKey; ASize: Integer; AVirtualKey: Word);
  var
    aButton:TposCtrlKeyboardKey;
  begin
    aButton := TposCtrlKeyboardKey.Create(Self);
    with aButton do
    begin
      FCtrlKey := ACtrlKey;
      Name := AName;
      VirtualKey := AVirtualKey;
      FSize := ASize;
      Row := aRow;
    end;
    List.Add(aButton);
  end;
begin
  FNormalButtons.Clear;
  FPADButtons.Clear;

  aRow := 0;
  AddPADKey(FPADButtons, '7', 4, Ord('7'));
  AddPADKey(FPADButtons, '8', 4, Ord('8'));
  AddPADKey(FPADButtons, '9', 4, Ord('9'));
  AddPADKey(FPADButtons, '*', 4, VK_MULTIPLY);
  AddCtrlKey(FPADButtons, '<-', ckBackspace, 8, VK_BACK);
  Inc(aRow);
  AddPADKey(FPADButtons, '4', 4, Ord('4'));
  AddPADKey(FPADButtons, '5', 4, Ord('5'));
  AddPADKey(FPADButtons, '6', 4, Ord('6'));
  AddPADKey(FPADButtons, '/', 4, VK_DIVIDE);
  if MultiPAD then
    AddCtrlKey(FPADButtons, '123', ckPAD, 8, 0)
  else
    AddCtrlKey(FPADButtons, 'Escape', ckEscape, 8, VK_Escape);
  Inc(aRow);
  AddPADKey(FPADButtons, '1', 4, Ord('1'));
  AddPADKey(FPADButtons, '2', 4, Ord('2'));
  AddPADKey(FPADButtons, '3', 4, Ord('3'));
  AddPADKey(FPADButtons, '+', 4, VK_ADD);
  AddCtrlKey(FPADButtons, 'Tab', ckTab, 8, VK_TAB);
  Inc(aRow);
  AddPADKey(FPADButtons, '0', 4, Ord('0'));
  AddPADKey(FPADButtons, '.', 4, VK_DECIMAL);
  AddPADKey(FPADButtons, '=', 4, VK_EQUALS);
  AddPADKey(FPADButtons, '-', 4, VK_SUBTRACT);
  AddCtrlKey(FPADButtons, 'Enter', ckEnter, 8, VK_RETURN);

  aRow := 0;
//  AddCtrlKey('ESC', #27, ckChar, 4);
  AddKey(FNormalButtons, '`', 4, $C0);
  AddKey(FNormalButtons, '1', 4, Ord('1'));
  AddKey(FNormalButtons, '2', 4, Ord('2'));
  AddKey(FNormalButtons, '3', 4, Ord('3'));
  AddKey(FNormalButtons, '4', 4, Ord('4'));
  AddKey(FNormalButtons, '5', 4, Ord('5'));
  AddKey(FNormalButtons, '6', 4, Ord('6'));
  AddKey(FNormalButtons, '7', 4, Ord('7'));
  AddKey(FNormalButtons, '8', 4, Ord('8'));
  AddKey(FNormalButtons, '9', 4, Ord('9'));
  AddKey(FNormalButtons, '0', 4, Ord('0'));
  AddKey(FNormalButtons, '-', 4, VK_SUBTRACT);
  AddKey(FNormalButtons, '=', 4, VK_EQUALS);
  AddKey(FNormalButtons, '\', 4, $DC);
  AddCtrlKey(FNormalButtons, '<-', ckBackspace, 4, VK_BACK);
  Inc(aRow);
  AddCtrlKey(FNormalButtons, 'Tab', ckTab, 6, VK_TAB);
  AddKey(FNormalButtons, 'Q', 4, Ord('Q'));
  AddKey(FNormalButtons, 'W', 4, Ord('W'));
  AddKey(FNormalButtons, 'E', 4, Ord('E'));
  AddKey(FNormalButtons, 'R', 4, Ord('R'));
  AddKey(FNormalButtons, 'T', 4, Ord('T'));
  AddKey(FNormalButtons, 'Y', 4, Ord('Y'));
  AddKey(FNormalButtons, 'U', 4, Ord('U'));
  AddKey(FNormalButtons, 'I', 4, Ord('I'));
  AddKey(FNormalButtons, 'O', 4, Ord('O'));
  AddKey(FNormalButtons, 'P', 4, Ord('P'));
  AddKey(FNormalButtons, '[', 4, $DB);
  AddKey(FNormalButtons, ']', 4, $DD);
  if MultiPAD then
    AddCtrlKey(FNormalButtons, '123', ckPAD, 6, 0)
  else
    AddCtrlKey(FNormalButtons, 'Escape', ckEscape, 6, VK_ESCAPE);
  Inc(aRow);
  AddCtrlKey(FNormalButtons, 'Caps', ckCapsLock, 8, VK_CAPITAL);
  AddKey(FNormalButtons, 'A', 4, Ord('A'));
  AddKey(FNormalButtons, 'S', 4, Ord('S'));
  AddKey(FNormalButtons, 'D', 4, Ord('D'));
  AddKey(FNormalButtons, 'F', 4, Ord('F'));
  AddKey(FNormalButtons, 'G', 4, Ord('G'));
  AddKey(FNormalButtons, 'H', 4, Ord('H'));
  AddKey(FNormalButtons, 'J', 4, Ord('J'));
  AddKey(FNormalButtons, 'K', 4, Ord('K'));
  AddKey(FNormalButtons, 'L', 4, Ord('L'));
  AddKey(FNormalButtons, ';', 4, $BA);
  AddKey(FNormalButtons, '''', 4, $DE);
  AddCtrlKey(FNormalButtons, 'Enter', ckEnter, 8, VK_RETURN);
  Inc(aRow);
  AddCtrlKey(FNormalButtons, 'Shift', ckShift, 10, VK_SHIFT);
  AddKey(FNormalButtons, 'Z', 4, Ord('Z'));
  AddKey(FNormalButtons, 'X', 4, Ord('X'));
  AddKey(FNormalButtons, 'C', 4, Ord('C'));
  AddKey(FNormalButtons, 'V', 4, Ord('V'));
  AddKey(FNormalButtons, 'B', 4, Ord('B'));
  AddKey(FNormalButtons, 'N', 4, Ord('N'));
  AddKey(FNormalButtons, 'M', 4, Ord('M'));
  AddKey(FNormalButtons, ',', 4, $BC);
  AddKey(FNormalButtons, '.', 4, VK_DECIMAL);
  AddKey(FNormalButtons, '/', 4, VK_DIVIDE);
  AddCtrlKey(FNormalButtons, 'Shift', ckShift, 10, VK_SHIFT);
  Inc(aRow);

  AddCtrlKey(FNormalButtons, 'Ctrl', ckCtrl, 6, VK_CONTROL);
  AddCtrlKey(FNormalButtons, 'Lang', ckLanguage, 5, 0);
  AddCtrlKey(FNormalButtons, 'Alt', ckAlt, 6, VK_MENU);
  AddCtrlKey(FNormalButtons, ' ',  ckSpace, 26, VK_SPACE);
  AddCtrlKey(FNormalButtons, 'Alt', ckAlt, 6, VK_MENU);
  AddCtrlKey(FNormalButtons, 'Lang', ckLanguage, 5, 0);
  AddCtrlKey(FNormalButtons, 'Ctrl', ckCtrl, 6, VK_CONTROL);

  if not (csLoading in ComponentState) then
    CalcButtons
end;

procedure TposKeyboard.ChangeScale(M, D: Integer);
begin
  inherited;
  CtrlFont.Size := MulDiv(CtrlFont.Size, M, D);
end;

constructor TposKeyboard.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse];
//  Style := Style - [fsBorder] + [fsLatedOpaque];
  Style := Style - [fsBorder] + [fsOpaque];
  FSizeFactor := cSizeFactor;
  FBorderColor := clDefault;
  FButtonColor := cKeyboardButtonColor;
  FPressedColor := clDefault;
  FMultiPAD := True;
  FLanguages := TposKeyLanguages.Create;
  FCtrlFont := TFont.Create;
  FCtrlFont.OnChange := CtrlFontChanged;
  FNormalButtons := TKeyboardButtonList.Create;
  FPADButtons := TKeyboardButtonList.Create;
  InitLanguages;
  CreatePads;
  FButtons := FNormalButtons;
end;

procedure TposKeyboard.PaintInner(vCanvas: TCanvas; var vRect: TRect; vColor: TColor);
begin
  inherited;
  FButtons.Paint(vCanvas, vRect);
end;

procedure TposKeyboard.CalcButtons;
var
  i: integer;
  aRect: TRect;
  aRow: Integer;
  aCol: Integer;
  aSizeUnit:Integer;
begin
  inherited;
  aSizeUnit := (InnerHeight - (Margin * 2)) div 5 div SizeFactor;
  i := 0;
  aRow := -1;
  aCol := 0;
  while i < FNormalButtons.Count do
  begin
    if aRow <> FNormalButtons[i].Row then
    begin
      aRow := FNormalButtons[i].Row;
      aCol := 0;
    end;
    aRect.Top := Padding + Margin + aRow * aSizeUnit * SizeFactor;
    aRect.Bottom := aRect.Top + aSizeUnit * SizeFactor - 1;
    aRect.Left := Padding + Margin + aCol * aSizeUnit;
    aRect.Right := aRect.Left + FNormalButtons[i].Size * aSizeUnit - 1;
    aCol := aCol + FNormalButtons[i].Size;
    FNormalButtons[i].BoundsRect := aRect;
    FNormalButtons[i].Visible := True;
    Inc(i);
  end;


  aSizeUnit := (InnerHeight - (Margin * 2)) div 4 div SizeFactor;
  i := 0;
  aRow := -1;
  aCol := 0;
  while i < FPADButtons.Count do
  begin
    if aRow <> FPADButtons[i].Row then
    begin
      aRow := FPADButtons[i].Row;
      aCol := 0;
    end;
    aRect.Top := Padding + Margin + aRow * aSizeUnit * SizeFactor;
    aRect.Bottom := aRect.Top + aSizeUnit * SizeFactor - 1;
    aRect.Left := Padding + Margin + aCol * aSizeUnit;
    aRect.Right := aRect.Left + FPADButtons[i].Size * aSizeUnit - 1;
    aCol := aCol + FPADButtons[i].Size;
    FPADButtons[i].BoundsRect := aRect;
    FPADButtons[i].Visible := True;
    Inc(i);
  end;
end;

procedure TposKeyboard.SetCtrlFont(const Value: TFont);
begin
  FCtrlFont.Assign(Value);
  Invalidate;
end;

procedure TposKeyboard.SetActiveButton(const Value: TposCustomKeyboardKey);
begin
  if FActiveButton <> Value then
  begin
    if FActiveButton <> nil then
      InvalidateRect(FActiveButton.BoundsRect);
    FActiveButton := Value;
    if (FActiveButton <> nil) then
      InvalidateRect(FActiveButton.BoundsRect);
  end;
end;

procedure TposKeyboard.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

destructor TposKeyboard.Destroy;
begin
  FreeAndNil(FNormalButtons);
  FreeAndNil(FPADButtons);
  FCtrlFont.Free;
  FLanguages.Free;
  inherited;
end;

procedure TposKeyboard.SendKey(Key: Word);
var
  KeyInfo: TposKeyInfo;
begin
  KeyInfo.VirtualKey := Key;
  DoSendKey(KeyInfo);
end;

procedure TposKeyboard.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ActiveButton := FButtons.InRect(X, Y);
end;

procedure TposKeyboard.SetButtonColor(const Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Invalidate;
  end;
end;

procedure TposKeyboard.InitLanguages;
var
  aLang: TposKeyLanguage;
begin
  aLang := TposKeyLanguage.Create('English' ,'EN', Languages);
  TposKeyLanguageItem.Create(aLang, '`', '`', '~');
  TposKeyLanguageItem.Create(aLang, '1', '1', '!');
  TposKeyLanguageItem.Create(aLang, '2', '2', '@');
  TposKeyLanguageItem.Create(aLang, '3', '3', '#');
  TposKeyLanguageItem.Create(aLang, '4', '4', '$');
  TposKeyLanguageItem.Create(aLang, '5', '5', '%');
  TposKeyLanguageItem.Create(aLang, '6', '6', '^');
  TposKeyLanguageItem.Create(aLang, '7', '7', '&');
  TposKeyLanguageItem.Create(aLang, '8', '8', '*');
  TposKeyLanguageItem.Create(aLang, '9', '9', '(');
  TposKeyLanguageItem.Create(aLang, '0', '0', ')');
  TposKeyLanguageItem.Create(aLang, '-', '-', '_');
  TposKeyLanguageItem.Create(aLang, '=', '=', '+');
  TposKeyLanguageItem.Create(aLang, '\', '\', '|');

  TposKeyLanguageItem.Create(aLang, 'Q', 'q', 'Q');
  TposKeyLanguageItem.Create(aLang, 'W', 'w', 'W');
  TposKeyLanguageItem.Create(aLang, 'E', 'e', 'E');
  TposKeyLanguageItem.Create(aLang, 'R', 'r', 'R');
  TposKeyLanguageItem.Create(aLang, 'T', 't', 'T');
  TposKeyLanguageItem.Create(aLang, 'Y', 'y', 'Y');
  TposKeyLanguageItem.Create(aLang, 'U', 'u', 'U');
  TposKeyLanguageItem.Create(aLang, 'I', 'i', 'I');
  TposKeyLanguageItem.Create(aLang, 'O', 'o', 'O');
  TposKeyLanguageItem.Create(aLang, 'P', 'p', 'P');
  TposKeyLanguageItem.Create(aLang, '[', '[', '{');
  TposKeyLanguageItem.Create(aLang, ']', ']', '}');

  TposKeyLanguageItem.Create(aLang, 'A', 'a', 'A');
  TposKeyLanguageItem.Create(aLang, 'S', 's', 'S');
  TposKeyLanguageItem.Create(aLang, 'D', 'd', 'D');
  TposKeyLanguageItem.Create(aLang, 'F', 'f', 'F');
  TposKeyLanguageItem.Create(aLang, 'G', 'g', 'G');
  TposKeyLanguageItem.Create(aLang, 'H', 'h', 'H');
  TposKeyLanguageItem.Create(aLang, 'J', 'j', 'J');
  TposKeyLanguageItem.Create(aLang, 'K', 'k', 'K');
  TposKeyLanguageItem.Create(aLang, 'L', 'l', 'L');
  TposKeyLanguageItem.Create(aLang, ';', ';', ':');
  TposKeyLanguageItem.Create(aLang, '''', '''', '"');

  TposKeyLanguageItem.Create(aLang, 'Z', 'z', 'Z');
  TposKeyLanguageItem.Create(aLang, 'X', 'x', 'X');
  TposKeyLanguageItem.Create(aLang, 'C', 'c', 'C');
  TposKeyLanguageItem.Create(aLang, 'V', 'v', 'V');
  TposKeyLanguageItem.Create(aLang, 'B', 'b', 'B');
  TposKeyLanguageItem.Create(aLang, 'N', 'n', 'N');
  TposKeyLanguageItem.Create(aLang, 'M', 'm', 'M');
  TposKeyLanguageItem.Create(aLang, ',', ',', '<');
  TposKeyLanguageItem.Create(aLang, '.', '.', '>');
  TposKeyLanguageItem.Create(aLang, '/', '/', '?');

  aLang := TposKeyLanguage.Create('Arabic', 'AR', Languages);
  TposKeyLanguageItem.Create(aLang, '`', 'Ð', 'ø');
  TposKeyLanguageItem.Create(aLang, '1', '1', '!');
  TposKeyLanguageItem.Create(aLang, '2', '2', '@');
  TposKeyLanguageItem.Create(aLang, '3', '3', '#');
  TposKeyLanguageItem.Create(aLang, '4', '4', '$');
  TposKeyLanguageItem.Create(aLang, '5', '5', '%');
  TposKeyLanguageItem.Create(aLang, '6', '6', '^');
  TposKeyLanguageItem.Create(aLang, '7', '7', '&');
  TposKeyLanguageItem.Create(aLang, '8', '8', '*');
  TposKeyLanguageItem.Create(aLang, '9', '9', '(');
  TposKeyLanguageItem.Create(aLang, '0', '0', ')');
  TposKeyLanguageItem.Create(aLang, '-', '-', '_');
  TposKeyLanguageItem.Create(aLang, '=', '=', '+');
  TposKeyLanguageItem.Create(aLang, '\', '\', '|');

  TposKeyLanguageItem.Create(aLang, 'Q', 'Ö', 'ó');
  TposKeyLanguageItem.Create(aLang, 'W', 'Õ', 'ð');
  TposKeyLanguageItem.Create(aLang, 'E', 'Ë', 'õ');
  TposKeyLanguageItem.Create(aLang, 'R', 'Þ', 'ñ');
  TposKeyLanguageItem.Create(aLang, 'T', 'Ý', 'áÅ');
  TposKeyLanguageItem.Create(aLang, 'Y', 'Û', 'Å');
  TposKeyLanguageItem.Create(aLang, 'U', 'Ú', '‘');
  TposKeyLanguageItem.Create(aLang, 'I', 'å', '÷');
  TposKeyLanguageItem.Create(aLang, 'O', 'Î', '×');
  TposKeyLanguageItem.Create(aLang, 'P', 'Í', 'º');
  TposKeyLanguageItem.Create(aLang, '[', 'Ì', '<');
  TposKeyLanguageItem.Create(aLang, ']', 'Ï', '>');

  TposKeyLanguageItem.Create(aLang, 'A', 'Ô', 'ö');
  TposKeyLanguageItem.Create(aLang, 'S', 'Ó', 'ò');
  TposKeyLanguageItem.Create(aLang, 'D', 'í', ']');
  TposKeyLanguageItem.Create(aLang, 'F', 'È', '[');
  TposKeyLanguageItem.Create(aLang, 'G', 'á', 'áÃ');
  TposKeyLanguageItem.Create(aLang, 'H', 'Ç', 'Ã');
  TposKeyLanguageItem.Create(aLang, 'J', 'Ê', 'Ü');
  TposKeyLanguageItem.Create(aLang, 'K', 'ä', '¡');
  TposKeyLanguageItem.Create(aLang, 'L', 'ã', '/');
  TposKeyLanguageItem.Create(aLang, ';', 'ß', ':');
  TposKeyLanguageItem.Create(aLang, '''', 'Ø', '"');

  TposKeyLanguageItem.Create(aLang, 'Z', 'Æ', '~');
  TposKeyLanguageItem.Create(aLang, 'X', 'Á', 'ú');
  TposKeyLanguageItem.Create(aLang, 'C', 'Ä', '}');
  TposKeyLanguageItem.Create(aLang, 'V', 'Ñ', '{');
  TposKeyLanguageItem.Create(aLang, 'B', 'áÇ', 'áÂ');
  TposKeyLanguageItem.Create(aLang, 'N', 'ì', 'Â');
  TposKeyLanguageItem.Create(aLang, 'M', 'É', '’');
  TposKeyLanguageItem.Create(aLang, ',', 'æ', ',');
  TposKeyLanguageItem.Create(aLang, '.', 'Ò', '.');
  TposKeyLanguageItem.Create(aLang, '/', 'Ù', '¿');

  CurrentLanguage := 'EN';
end;

procedure TposKeyboard.CtrlFontChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TposKeyboard.SetCurrentLanguage(const Value: string);
begin
  if FCurrentLanguage <> Value then
  begin
    FCurrentLanguage := Value;
    Inc(FLangTR);
    Invalidate;
  end;
end;

procedure TposKeyboard.SetMultiPAD(const AValue: Boolean);
begin
  if FMultiPAD <> AValue then
  begin
    FMultiPAD := AValue;
    CreatePads;
  end;
end;

procedure TposKeyboard.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if ActiveButton <> nil then
    ActiveButton.Click;
  ActiveButton := nil;
end;

procedure TposKeyboard.DoButtonPress(Value: string);
begin
  if Interactive then
    posEngine.ProcessKeys(Value);
  if Assigned(FOnButtonPress) then
    FOnButtonPress(Self, Value);
end;

procedure TposKeyboard.DoSendKey(Key: TposKeyInfo);
begin
  if Assigned(FOnSendKey) then
    FOnSendKey(Self, Key);
end;

procedure TposKeyboard.FlipCurrentLanguage;
begin
  CurrentLanguage := GetNextLanguage;
end;

procedure TposKeyboard.ChooseLanguageByName(Name: string);
var
  i:Integer;
begin
  i := Languages.IndexOfName(Name);
  if i>=0 then
    ChooseLanguageByShortName(Languages[i].ShortName);
end;

procedure TposKeyboard.ChooseLanguageByShortName(Name: string);
begin
  CurrentLanguage := Name;
  Invalidate;
end;

function TposKeyboard.GetNextLanguage: string;
var
  i: Integer;
begin
  i := Languages.IndexOfShortName(CurrentLanguage);
  if i >= 0 then
  begin
    Inc(i);
    if i >= Languages.Count then
      i := 0;
    Result := Languages[i].ShortName;
  end
  else
    Result := CurrentLanguage;
end;

procedure TposKeyboard.Resized;
begin
  inherited;
  CalcButtons;
end;

procedure TposKeyboard.SetShiftState(const Value: TposShiftStates);
begin
  if FShiftState <> Value then
  begin
    FShiftState := Value;
    Invalidate;
  end;
end;

procedure TposKeyboard.SetKind(const Value: TposKeyboardKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    case FKind of
      keyNormal: Buttons := FNormalButtons;
      keyNumberPAD: Buttons := FPADButtons;
    end;
  end;
end;

procedure TposKeyboard.SetPressedColor(const Value: TColor);
begin
  if FPressedColor <> Value then
  begin
    FPressedColor := Value;
    Invalidate;
  end;
end;

procedure TposKeyboard.SetButtons(const Value: TKeyboardButtonList);
begin
  if FButtons <> Value then
  begin
    FButtons := Value;
    Invalidate;
  end;
end;

{ TposCustomKeyboardKey }

procedure TposKeyboard.Loaded;
begin
  inherited;
  CalcButtons;
end;

{ TposCustomKeyboardKey }

procedure TposCustomKeyboardKey.Click;
begin
  Keyboard.SendKey(VirtualKey);
end;

constructor TposCustomKeyboardKey.Create(AKeyboard: TposKeyboard);
begin
  inherited Create;
  FKeyboard := AKeyboard;
end;

destructor TposCustomKeyboardKey.Destroy;
begin
  inherited;
end;

function TposCustomKeyboardKey.GetButtonColor: TColor;
begin
  Result := Keyboard.ButtonColor;
end;

procedure TposCustomKeyboardKey.Paint(Canvas: TCanvas; Rect: TRect);
begin
end;

procedure TposCustomKeyboardKey.Pressed;
begin
  FState := True;
end;

procedure TposCustomKeyboardKey.Release;
begin
  FState := False;
end;

procedure TposCustomKeyboardKey.SetBoundsRect(const Value: TRect);
begin
  FBoundsRect := Value;
end;

procedure TposCustomKeyboardKey.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TposCustomKeyboardKey.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

{ TKeyboardButtonList }

function TKeyboardButtonList.GetItem(Index: Integer): TposCustomKeyboardKey;
begin
  Result := inherited Items[Index] as TposCustomKeyboardKey;
end;

function TKeyboardButtonList.InRect(X,
  Y: Integer): TposCustomKeyboardKey;
var
  i: Integer;
  Pt: TPoint;
begin
  Result := nil;
  Pt.X := X;
  Pt.Y := Y;
  for i := 0 to Count - 1 do
  begin
    if PtInRect(Items[i].BoundsRect, Pt) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TKeyboardButtonList.Paint(Canvas: TCanvas; vRect: TRect);
var
  i: Integer;
  aUpdateRect: TRect;
begin
  aUpdateRect := Canvas.ClipRect;
  for i := 0 to Count - 1 do
  begin
    if CollideRect(Items[i].BoundsRect, aUpdateRect) then
    begin
      Items[i].Paint(Canvas, vRect);
      ExcludeClipRect(Canvas, Items[i].BoundsRect);
    end;
  end;
end;

procedure TKeyboardButtonList.SetItem(Index: Integer;
  const Value: TposCustomKeyboardKey);
begin
  inherited Items[Index] := Value;
end;

{ TposKeyLanguages }

function TposKeyLanguages.FindByName(vName: string): TposKeyLanguage;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, vName) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TposKeyLanguages.FindByShortName(vShortName: string): TposKeyLanguage;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].ShortName, vShortName) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TposKeyLanguages.GetItem(Index: Integer): TposKeyLanguage;
begin
  Result := inherited Items[Index] as TposKeyLanguage;
end;

function TposKeyLanguages.IndexOfName(vName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, vName) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TposKeyLanguages.IndexOfShortName(vShortName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].ShortName, vShortName) then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TposKeyLanguages.SetItem(Index: Integer;
  const Value: TposKeyLanguage);
begin
  inherited Items[Index] := Value;
end;

{ TposKeyLanguage }

constructor TposKeyLanguage.Create(Name, ShortName: string; Languages: TposKeyLanguages);
begin
  inherited Create;
  Languages.Add(Self);
  FLanguages := Languages;
  FName := Name;
  FShortName := ShortName;
end;

destructor TposKeyLanguage.Destroy;
begin

  inherited;
end;

function TposKeyLanguage.Find(vName: string): TposKeyLanguageItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, vName) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TposKeyLanguage.GetItem(Index: Integer): TposKeyLanguageItem;
begin
  Result := inherited Items[Index] as TposKeyLanguageItem;
end;

procedure TposKeyLanguage.SetItem(Index: Integer;
  const Value: TposKeyLanguageItem);
begin
  inherited Items[Index] := Value;
end;

{ TposKeyLanguageItem }

constructor TposKeyLanguageItem.Create(Language: TposKeyLanguage; Name, Normal, Shift: string; VirtualKey: Word);
begin
  inherited Create;
  Language.Add(Self);
  FLanguage := Language;
{$ifdef FPC}//must checkunicode
  FName := CP1256ToUTF8(Name);
  FNormalCaption := CP1256ToUTF8(Normal);
{$else}
  FName := Name;
  FNormalCaption := Normal;
{$endif}
  FShiftCaption := Shift;
end;

destructor TposKeyLanguageItem.Destroy;
begin
  inherited;
end;

{ TposCtrlKeyboardKey }

procedure TposCtrlKeyboardKey.Click;
begin
  inherited;
  case FCtrlKey of
    ckEscape:
    begin
      Keyboard.DoButtonPress(#27);
    end;
    ckBackspace:
    begin
      Keyboard.DoButtonPress(#8);
    end;
    ckTab:
    begin
      Keyboard.DoButtonPress(#9);
    end;
    ckClear:
    begin
      Keyboard.DoButtonPress(#12);
    end;
    ckEnter:
    begin
      Keyboard.DoButtonPress(#13);
    end;
    ckSpace:
    begin
      Keyboard.DoButtonPress(' ');
    end;
    ckPAD:
    begin
      if Keyboard.Kind = keyNumberPAD then
        Keyboard.Kind := keyNormal
      else
        Keyboard.Kind := keyNumberPAD
    end;
    ckCapsLock:
      begin
        if pssCapsLock in Keyboard.FShiftState then
          Keyboard.ShiftState := Keyboard.ShiftState - [pssCapsLock]
        else
          Keyboard.ShiftState := Keyboard.ShiftState + [pssCapsLock];
      end;
    ckShift:
      begin
        if pssShift in Keyboard.FShiftState then
          Keyboard.ShiftState := Keyboard.ShiftState - [pssShift]
        else
          Keyboard.ShiftState := Keyboard.ShiftState + [pssShift];
      end;
    ckCtrl:
      begin
        if pssCtrl in Keyboard.ShiftState then
          Keyboard.ShiftState := Keyboard.ShiftState - [pssCtrl]
        else
          Keyboard.ShiftState := Keyboard.ShiftState + [pssCtrl];
      end;
    ckAlt:
      begin
        if pssAlt in Keyboard.ShiftState then
          Keyboard.ShiftState := Keyboard.ShiftState - [pssAlt]
        else
          Keyboard.ShiftState := Keyboard.ShiftState + [pssAlt];
      end;
    ckLanguage:
      begin
        Keyboard.FlipCurrentLanguage;
        Keyboard.Invalidate;
      end;
  end;
end;

procedure TposCtrlKeyboardKey.Paint(Canvas: TCanvas; Rect: TRect);
var
  aButtonColor: TColor;
  s: string;
  aDown: Boolean;
  aShape: TposShapeKind;
  aState : TposDrawStates;
  aRect: TRect;
begin
  inherited;
  if Visible then
  begin
    aRect := BoundsRect;
    Canvas.Font := Keyboard.CtrlFont;
    aState := [pdsBorder];
    aDown := FState or (Keyboard.FActiveButton = self);
    aShape := shpNone;
    s := Name;
    case FCtrlKey of
      ckBackspace:
      begin
        aShape := shpLeft;
        s := '';
      end;
      ckCapsLock:
        begin
          aDown := aDown or (pssCapsLock in Keyboard.ShiftState);
        end;
      ckAlt:
        begin
          aDown := aDown or (pssAlt in Keyboard.ShiftState);
        end;
      ckCtrl:
        begin
          aDown := aDown or (pssCtrl in Keyboard.ShiftState);
        end;
      ckShift:
        begin
          aDown := aDown or (pssShift in Keyboard.ShiftState);
        end;
      ckLanguage:
        begin
          s := Keyboard.GetNextLanguage;
        end;
    end;
    if aDown then
    begin
      aButtonColor := Keyboard.PressedColor;
      if aButtonColor = clDefault then
        aButtonColor := Lighten(Keyboard.ButtonColor, cDownDarker);
      aState := aState + [pdsDown];
    end
    else
    begin
      aButtonColor := Keyboard.ButtonColor;
    end;
    PaintButton(Canvas, s, aShape, aRect, aButtonColor, Keyboard.BorderColor, aState);
  end;
end;

{ TposKeyboardKey }

procedure TposKeyboardKey.Click;
var
  aLangKey: TposKeyLanguageItem;
  b: Boolean;
  aCaption: string;
begin
  inherited;
  aLangKey := GetLangKey;
  if aLangKey <> nil then
  begin
    b := False;
    if pssShift in Keyboard.ShiftState then
      b := not b;
    if pssCapsLock in Keyboard.ShiftState then
      b := not b;
    if not b then
      aCaption := aLangKey.NormalCaption
    else
      aCaption := aLangKey.ShiftCaption;
    Keyboard.DoButtonPress(aCaption);
  end;
  Keyboard.ShiftState := Keyboard.FShiftState - [pssShift, pssCtrl, pssAlt];
end;

function TposKeyboardKey.GetLangKey: TposKeyLanguageItem;
var
  aLang: TposKeyLanguage;
begin
  if (FLangKey = nil) or (FLangTR <> Keyboard.FLangTR) then
  begin
    aLang := Keyboard.Languages.FindByShortName(Keyboard.CurrentLanguage);
    if aLang <> nil then
      FLangKey := aLang.Find(Name)
    else
      FLangKey := nil;
    FLangTR := Keyboard.FLangTR;
  end;
  Result := FLangKey;
end;

procedure TposKeyboardKey.Paint(Canvas: TCanvas; Rect: TRect);
var
  aButtonColor: TColor;
  aCaption: string;
  aLangKey: TposKeyLanguageItem;
  b:Boolean;
  aState : TposDrawStates;
begin
  inherited;
  if Visible then
  begin
    Canvas.Font := Keyboard.Font;
    aState := [pdsBorder];
    if FState or (Keyboard.FActiveButton = self) then
    begin
      aButtonColor := Keyboard.PressedColor;
      if aButtonColor = clDefault then
        aButtonColor := Lighten(Keyboard.ButtonColor, cDownDarker);
      aState := aState + [pdsDown];
    end
    else
    begin
      aButtonColor := Keyboard.ButtonColor;
    end;
    aLangKey := GetLangKey;
    if aLangKey <> nil then
    begin
      b := False;
      if pssShift in Keyboard.ShiftState then
        b := not b;
      if pssCapsLock in Keyboard.ShiftState then
        b := not b;
      if not b then
        aCaption := aLangKey.NormalCaption
      else
        aCaption := aLangKey.ShiftCaption;
    end;
    PaintButton(Canvas, aCaption, shpNone, BoundsRect, aButtonColor, Keyboard.BorderColor, aState);
  end;
end;

{ TposPADKeyboardKey }

procedure TposPADKeyboardKey.Click;
begin
  inherited;
  Keyboard.DoButtonPress(Name);
end;

procedure TposPADKeyboardKey.Paint(Canvas: TCanvas; Rect: TRect);
var
  aButtonColor: TColor;
  aState: TposDrawStates;
begin
  inherited;
  if Visible then
  begin
    Canvas.Font := Keyboard.Font;
    aState := [pdsBorder];
    if FState or (Keyboard.FActiveButton = self) then
    begin
      aButtonColor := Keyboard.PressedColor;
      if aButtonColor = clDefault then
        aButtonColor := Lighten(Keyboard.ButtonColor, cDownDarker);
      aState := aState + [pdsDown];
    end
    else
    begin
      aButtonColor := Keyboard.ButtonColor;
    end;
    PaintButton(Canvas, Name, shpNone, BoundsRect, aButtonColor, Keyboard.BorderColor, aState);
  end;
end;

end.

