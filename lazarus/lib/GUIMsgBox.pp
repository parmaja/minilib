unit GUIMsgBox;
{$mode objfpc}{$H+}
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MsgBox, Clipbrd, Buttons,
  LCLType, LCLProc, LCLIntf,
  Contnrs,  ExtCtrls;

type

  { TMsgForm }

  TMsgForm = class(TForm)
  private
    FMsgKind: TMsgKind;
    FSenderObject: TObject;
    FOwnerControls: TComponent;
  protected
    TextBox: TEdit;
    LabelMsg: TLabel;
    ChoiceIndex: Integer;
    IsStatus: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    function ShowNow(DefaultChoiceIndex: Integer): Integer;
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ButtonClick(Sender: TObject);
  public
    constructor CreateNew(vMsgKind: TMsgKind; AOwner: TComponent); reintroduce;
    constructor Create(AOwner: TComponent); override;
  end;

  { TGUIMsgBox }

  TGUIMsgBox = class(TMsgPrompt)
  private
    FMinButtonWidth: Integer;
    FStatusForms: TObjectList;
  protected
    function ShowMessage(const vText: string; Choices: array of TMsgSelect; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer; override;
    function ShowMessage(var vResult: string; const vText: string; Choices: array of TMsgSelect; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer; override;
    procedure ShowStatus(vText: string; Sender: TObject = nil); override;
    procedure UpdateStatus(vText: string; Sender: TObject = nil); override;
    procedure HideStatus(Sender: TObject); override;

    function CreateButton(AOwner: TComponent; vSelect: TMsgSelect): TCustomButton; virtual;
    function CreateForm(Kind: TMsgKind): TMsgForm; virtual;
    function FindSender(Sender: TObject): Integer;

    procedure CreateFormObjects(vForm: TMsgForm; const vMsg, vTitle: string; Choices: array of TMsgSelect; DefaultChoice, CancelChoice: Integer); virtual;
    procedure Created; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MinButtonWidth: Integer read FMinButtonWidth write FMinButtonWidth;
  end;


const
  ChoiceGlyphKind: array[TmsgChoice] of TBitBtnKind = (
    bkCustom, bkYes, bkNo, bkOK, bkCancel, bkAbort, bkRetry, bkIgnore, bkCustom, bkCustom, bkCustom,
    bkAll, bkNo, bkYes, bkHelp);

implementation

uses
  Types, StrUtils;

function Max(I, J: Integer): Integer;
begin
  if I > J then
    Result := I
  else
    Result := J;
end;

constructor TMsgForm.Create(AOwner: TComponent);
begin
  inherited;
  Position := poMainFormCenter;
end;

constructor TMsgForm.CreateNew(vMsgKind: TMsgKind; AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  FMsgKind := vMsgKind;
  KeyPreview := True;
  Position := poMainFormCenter;
end;

function TGUIMsgBox.CreateForm(Kind: TMsgKind): TMsgForm;
begin
  Result := TMsgForm.CreateNew(Kind, Application.MainForm);
end;

const
  IconIDs: array[TMsgKind] of Integer = (idDialogConfirm, idDialogWarning, idDialogError, idDialogInfo, idDialogConfirm, idDialogInfo, idDialogInfo, idDialogInfo);

procedure TGUIMsgBox.CreateFormObjects(vForm: TMsgForm; const vMsg, vTitle: string; Choices: array of TMsgSelect; DefaultChoice, CancelChoice: Integer);
const
  cHorzMargin = 5;
  cVertMargin = 5;

  cHorzSpacing = 5;
  cVertSpacing = 5;
  cButtonSpacing = 5;
  {$ifdef LINUX}
  cButtonHeight = 32;
  cButtonMargin = 5;
  cIconSize = 48;
  {$else}
  cButtonMargin = 0;
  cButtonHeight = 28;
  cIconSize = 32;
  {$endif}
  procedure FlipRect(var Rect: TRect; Width: Integer);
  begin
    OffsetRect(Rect, Width - Rect.Right - Rect.Left, 0);
  end;
var
  ButtonWidth, ButtonCount, ButtonsWidth,
    i, X: Integer;
  B: TMsgSelect;
  IconID: Integer;
  aRect: TRect;
  TextRect: TRect;
  aButton: TCustomButton;
  aClientWidth: Integer;
  aClientHeight: Integer;
  aIcon: TCustomBitmap;
begin
  with vForm do
  begin
    FOwnerControls.Free;
    FOwnerControls := TComponent.Create(vForm);
    Font := Screen.MenuFont;
    if Application.MainForm <> nil then
      BiDiMode := Application.MainForm.BiDiMode
    else
      BidiMode := Application.BidiMode;
    BorderStyle := bsDialog;
    Canvas.Font := Font;
    if FMsgKind <> msgkStatus then
    begin
      if vTitle = '' then
        Caption := Application.Title
      else
        Caption := vTitle;
    end;

    TextRect := Rect(0, 0, Screen.Width div 2, 0);

    DrawText(Canvas.Handle, PChar(vMsg), Length(vMsg), TextRect, DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK{ or DrawTextBiDiModeFlagsReadingOnly});
    aClientWidth := TextRect.Right;
    aClientHeight := TextRect.Bottom;

    IconID := IconIDs[FMsgKind];
    aClientWidth := aClientWidth + cIconSize + cHorzSpacing; //add icon width

    if aClientWidth < cIconSize then
      aClientWidth := cIconSize;
    if aClientHeight < cIconSize then
      aClientHeight := cIconSize;

    if FMsgKind = msgkStatus then
      aClientWidth := aClientWidth + cHorzSpacing;

    ButtonWidth := MinButtonWidth;
    ButtonCount := Length(Choices);

    if ButtonCount <> 0 then
      ButtonsWidth := (ButtonWidth + cButtonMargin * 2) * ButtonCount + cButtonSpacing * (ButtonCount - 1)
    else
      ButtonsWidth := 0;

    aClientWidth := Max(aClientWidth, ButtonsWidth);

    aClientWidth := aClientWidth + cHorzMargin * 2;

    with TImage.Create(FOwnerControls) do
    begin
      Name := 'Image';
      Parent := vForm;
      aIcon := GetDialogIcon(IconID);
      Picture.Graphic := aIcon;
      FreeAndNil(aIcon);
      aRect.Left := cHorzMargin;
      aRect.Top := cVertMargin;
      aRect.Right := aRect.Left + cIconSize;
      aRect.Bottom := aRect.Top + cIconSize;
      if vForm.UseRightToLeftAlignment then
        FlipRect(aRect, aClientWidth);
      BoundsRect := aRect;
    end;

    LabelMsg := TLabel.Create(FOwnerControls);
    with LabelMsg do
    begin
      Name := 'Message';
      Parent := vForm;
      WordWrap := True;
      Caption := vMsg;
      AutoSize := False;
      Layout := tlCenter;
      if (vMsg <> '') and (UpperCase(vMsg[1]) >= 'A') and (UpperCase(vMsg[1]) <= 'Z') then
        BiDiMode := bdLeftToRight
      else
        BiDiMode := vForm.Bidimode;
      aRect.Left := cHorzMargin + cIconSize + cVertSpacing;
      aRect.Top := cVertMargin;
      aRect.Right := aRect.Left + TextRect.Right;
      if FMsgKind = msgkStatus then
      begin
        Layout := tlCenter;
        Alignment := taCenter;
        aClientHeight := aClientHeight + cVertMargin;
        aRect.Bottom := aRect.Top + aClientHeight - cVertMargin - cVertMargin;
      end
      else
        aRect.Bottom := aRect.Top + aClientHeight;
        //aRect.Bottom := aRect.Top + TextRect.Bottom;//if not layout = center
      if vForm.UseRightToLeftAlignment then
        FlipRect(aRect, aClientWidth);
      BoundsRect := aRect;
    end;

    if FMsgKind = msgkStatus then
    begin
    end
    else
    begin
      aClientHeight := aClientHeight + cVertMargin;

      with TBevel.Create(FOwnerControls) do
      begin
        Parent := vForm;
        Shape := bsBottomLine;
        BiDiMode := vForm.BiDiMode;
        aRect.Left := cHorzMargin;
        aRect.Right := aClientWidth - cHorzMargin * 2;
        aRect.Top := aClientHeight + cVertMargin;
        aRect.Bottom := aRect.Top + 2;
        aClientHeight := aRect.Bottom;
        BoundsRect := aRect;
      end;

      if FMsgKind = msgkInput then
      begin
        //aClientHeight := aClientHeight + cHorzSpacing;

        TextBox := TEdit.Create(FOwnerControls);
        with TextBox do
        begin
          Parent := vForm;
          aRect.Left := cHorzMargin;
          aRect.Right := aClientWidth - cHorzMargin * 2;
          aRect.Top := aClientHeight + cVertMargin;
          aRect.Bottom := aRect.Top + 22;
          aClientHeight := aRect.Bottom;
          BoundsRect := aRect;
          BiDiMode := vForm.BiDiMode;
          TabOrder := 0;
        end;
        {$ifdef LINUX}
        if ButtonCount <> 0 then
          aClientHeight := aClientHeight + cHorzSpacing;
        {$endif}
      end;

      if ButtonCount <> 0 then
        aClientHeight := aClientHeight + cHorzSpacing;

      X := (aClientWidth - ButtonsWidth) div 2;
      for i := 0 to Length(Choices) -1 do
      begin
        B := Choices[i];
        aButton := CreateButton(FOwnerControls, B);
        with aButton do
        begin
          Tag := i;//need for result
          OnClick := @ButtonClick;
          TabStop := True;
          Font := Canvas.Font;
          BidiMode := vForm.BidiMode;
          Name := 'Button' + IntToStr(i);
          Parent := vForm;
          if (i = DefaultChoice) then
          begin
            if (FMsgKind = msgkError) then
              TabStop := False
            else
            begin
              Default := True;
              if FMsgKind <> msgkInput then
                TabOrder := 0;
            end;
          end;
          {if (i = CancelChoice) then //we use KeyDown
            Cancel := True;}
          aRect.Left := X;
          aRect.Top := aClientHeight;
          aRect.Right := aRect.Left + ButtonWidth + cButtonMargin * 2;
          aRect.Bottom := aRect.Top + cButtonHeight;
          if aButton.UseRightToLeftReading then
            FlipRect(aRect, aClientWidth);
          BoundsRect := aRect;
          Inc(X, ButtonWidth + cButtonSpacing + cButtonMargin * 2);
        end;
      end;
      aClientHeight := aClientHeight + cButtonHeight;
    end;

    aClientHeight := aClientHeight + cVertMargin;
    ClientWidth := aClientWidth;
    ClientHeight := aClientHeight;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
  end;
end;

constructor TGUIMsgBox.Create;
begin
  inherited;
  FStatusForms := TObjectList.Create(true);
  FMinButtonWidth := 80;
end;

destructor TGUIMsgBox.Destroy;
begin
  FreeAndNil(FStatusForms);
  inherited;
end;

procedure TMsgForm.DoClose(var CloseAction: TCloseAction);
begin
  if IsStatus then
    CloseAction := caNone
  else
    CloseAction := caHide;
end;

function TMsgForm.ShowNow(DefaultChoiceIndex: Integer): Integer;
begin
  Position := poMainFormCenter;
  ChoiceIndex := DefaultChoiceIndex;
  ShowModal;
  Result := ChoiceIndex;
end;

function TGUIMsgBox.CreateButton(AOwner: TComponent; vSelect: TMsgSelect): TCustomButton;
begin
  Result := TBitBtn.Create(AOwner);
  (Result as TBitBtn).Kind := ChoiceGlyphKind[vSelect.Choice];
  Result.Caption := vSelect.Caption;
end;

procedure TMsgForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if ((Shift = [ssCtrl]) and (Key = Word('C'))) or (((Shift = [ssCtrl]) and (Key = VK_INSERT))) then
  begin
    Clipboard.AsText := LabelMsg.Caption;
  end
  else if not IsStatus and (Shift = []) and (Key = VK_ESCAPE) then
    Close;
end;

procedure TMsgForm.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
end;

procedure TGUIMsgBox.HideStatus(Sender: TObject);
var
  i: Integer;
begin
{  if Sender is TComponent then
  begin
    (Sender as TComponent).RemoveFreeNotification(Self);
  end;}
  i := FindSender(Sender);
  if i >= 0 then
  begin
    FStatusForms.Delete(i);
  end;
end;

procedure TGUIMsgBox.ShowStatus(vText: string; Sender: TObject);
var
  aMsgForm: TMsgForm;
  i: Integer;
begin
  try
    i := FindSender(Sender);
    if i >= 0 then
      aMsgForm := TMsgForm(FStatusForms.Items[i])
    else
    begin
      aMsgForm := CreateForm(msgkStatus);
      aMsgForm.FSenderObject := Sender;
      aMsgForm.Caption := '';
      aMsgForm.BorderIcons := [];
      aMsgForm.BorderStyle := bsNone;
      aMsgForm.Position := poMainFormCenter;
      aMsgForm.FormStyle := fsStayOnTop;
      aMsgForm.IsStatus := True;
      FStatusForms.Add(aMsgForm);
    end;
    CreateFormObjects(aMsgForm, vText, Application.Title, [], 0, 0);
    if not aMsgForm.Visible then
      aMsgForm.Show; //need to make it kind of modal
      //ShowWindow(aMsgForm.Handle, SW_SHOWNOACTIVATE);
    Application.ProcessMessages;
  finally
  end;
end;

procedure TGUIMsgBox.UpdateStatus(vText: string; Sender: TObject);
begin
end;

procedure TMsgForm.DoHide;
begin
  inherited;
end;

procedure TMsgForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    if FMsgKind = msgkStatus then
      Style := WS_POPUP or WS_BORDER;
    ExStyle := ExStyle or WS_EX_DLGMODALFRAME;

    if Screen.ActiveForm <> nil then
      WndParent := Screen.ActiveForm.Handle
    else if Application.MainForm <> nil then
      WndParent := Application.MainForm.Handle;
  end;
end;

procedure TMsgForm.ButtonClick(Sender: TObject);
begin
  ChoiceIndex := (Sender as TCustomButton).Tag;
  ModalResult := 1;
end;

function TGUIMsgBox.ShowMessage(const vText: string; Choices: array of TMsgSelect; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer;
var
  aMsgForm: TMsgForm;
begin
  aMsgForm := CreateForm(Kind);
  CreateFormObjects(aMsgForm, vText, Application.Title, Choices, DefaultChoice, CancelChoice);
  with aMsgForm do
  try
    Position := poScreenCenter;
    Result := ShowNow(CancelChoice);
  finally
    Free;
  end;
end;

function TGUIMsgBox.ShowMessage(var vResult: string; const vText: string; Choices: array of TMsgSelect; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer;
var
  aMsgForm: TMsgForm;
begin
  aMsgForm := CreateForm(msgkInput);
  CreateFormObjects(aMsgForm, vText, Application.Title, Choices, DefaultChoice, CancelChoice);
  with aMsgForm do
  try
    TextBox.Text := vResult;
    if Kind = msgkPassword then
      TextBox.PasswordChar := '*';
    Result := ShowNow(CancelChoice);
    vResult := TextBox.Text;
  finally
    Free;
  end;
end;

function TGUIMsgBox.FindSender(Sender: TObject): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FStatusForms.Count - 1 do
  begin
    if TMsgForm(FStatusForms[i]).FSenderObject = Sender then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TGUIMsgBox.Created;
begin
  FName := 'GUI';
  FTitle := 'GUI Messages';
end;

procedure TMsgForm.DoShow;
begin
  inherited; 
  Position := poMainFormCenter;
  BringToFront;
end;

initialization
  Msg.Register(TGUIMsgBox, True);
finalization
end.

