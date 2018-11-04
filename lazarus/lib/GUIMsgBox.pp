
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
    FDefaultChoice: Integer;
  protected
    MsgPanel: TPanel;
    InputPanel: TPanel;
    ButtonsPanel: TPanel;
    TextBox: TEdit;
    ListBox: TListBox;
    MsgLabel: TLabel;
    MsgImage: TImage;
    ChoiceIndex: Integer;
    IsStatus: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure OnListBoxDblClick(Sender: TObject);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    function ShowNow(vCancelChoiceIndex: Integer): Integer;
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
    function ShowInput(var Answer: string; const vText: string; Choices: array of TMsgSelect; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer; override;
    function ShowList(var Answer: Integer; const vText: string; vStrings:TStrings; Choices: array of TmsgSelect; DefaultChoice: Integer; CancelChoice: Integer; Kind: TmsgKind): Integer; override;
    procedure ShowStatus(vText: string; Sender: TObject = nil); override;
    procedure UpdateStatus(vText: string; Sender: TObject = nil); override;
    procedure HideStatus(Sender: TObject); override;

    function CreateButton(AOwner: TComponent; vSelect: TMsgSelect): TCustomButton; virtual;
    function CreateForm(Kind: TMsgKind): TMsgForm; virtual;
    function FindSender(Sender: TObject): Integer;

    procedure CreateFormObjects(vForm: TMsgForm; const vMsg, vTitle: string; Choices: array of TMsgSelect; DefaultChoice, CancelChoice: Integer; vKind: TmsgKind = msgkNormal); virtual;
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
  Types;

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
  IconIDs: array[TMsgKind] of Integer = (idDialogConfirm, idDialogWarning, idDialogError, idDialogInfo, idDialogConfirm, idDialogInfo, idDialogInfo, idDialogInfo, idDialogInfo);

procedure TGUIMsgBox.CreateFormObjects(vForm: TMsgForm; const vMsg, vTitle: string; Choices: array of TMsgSelect; DefaultChoice, CancelChoice: Integer; vKind: TmsgKind);
const
  cMargin = 5;
  cSpacing = 2;

var
  i: Integer;
  B: TMsgSelect;
  IconID: Integer;
  aButtonHeight: Integer;
  aButton: TCustomButton;
  aIcon: TCustomBitmap;
begin
  with vForm do
  begin
    FDefaultChoice := DefaultChoice;
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

    IconID := IconIDs[FMsgKind];

    ChildSizing.HorizontalSpacing := cMargin * 2;
    ChildSizing.VerticalSpacing := cMargin;

    MsgPanel := TPanel.Create(FOwnerControls);
    with MsgPanel do
    begin
      Parent := vForm;
      Font := vForm.Font;
      Caption := '';
      BiDiMode := vForm.BiDiMode;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderWidth := cMargin;
      ChildSizing.HorizontalSpacing := cSpacing;
      ChildSizing.VerticalSpacing := cSpacing;
      Width := Canvas.TextWidth(vMsg) + cMargin + cSpacing;
      aButtonHeight := Canvas.TextHeight('WOK') + cSpacing;
    end;

    MsgImage := TImage.Create(FOwnerControls);
    with MsgImage do
    begin
      Name := 'Image';
      Parent := MsgPanel;
      Center := true;

      aIcon := GetDialogIcon(IconID);
      Picture.Graphic := aIcon;
      Width := Picture.Width;

      FreeAndNil(aIcon);

      if vForm.UseRightToLeftAlignment then
        Align := alRight
      else
        Align := alLeft;
    end;

    MsgLabel := TLabel.Create(FOwnerControls);
    with MsgLabel do
    begin
      Name := 'Message';
      Parent := MsgPanel;
      //WordWrap := True;
      Left := 0;
      Top := 0;
      BorderSpacing.Around := cMargin;
      AutoSize := true;
      Caption := vMsg;
      Layout := tlCenter;
      if (vMsg <> '') and (UpperCase(vMsg[1]) >= 'A') and (UpperCase(vMsg[1]) <= 'Z') then
        BiDiMode := bdLeftToRight
      else
        BiDiMode := vForm.Bidimode;
      if FMsgKind = msgkStatus then
      begin
        Alignment := taCenter;
      end;
      Align := alClient;
    end;

    MsgPanel.Align := alClient;
    MsgPanel.AutoSize := true;
    AutoSize := true;

    if FMsgKind = msgkStatus then
    begin
    end
    else
    begin
      if FMsgKind in [msgkInput, msgkPassword, msgkList] then
      begin
        InputPanel := TPanel.Create(FOwnerControls);
        with InputPanel do
        begin
          Parent := vForm;
          TabStop := False;
          Align := alBottom;
          Caption := '';
          BevelInner := bvNone;
          BevelOuter := bvNone;
          BorderWidth := cMargin;
          AutoSize := True;
          //ChildSizing.VerticalSpacing := cSpacing;
          //ChildSizing.HorizontalSpacing := cSpacing;
        end;

        if (FMsgKind in [msgkInput, msgkPassword]) or (vKind in [msgkInput, msgkPassword]) then
        begin
          TextBox := TEdit.Create(FOwnerControls);
          with TextBox do
          begin
            Parent := InputPanel;
            Align := alBottom;
            BiDiMode := vForm.BiDiMode;
            TabOrder := 0;
          end;
        end;

        if FMsgKind = msgkList then
        begin
          ListBox := TListBox.Create(FOwnerControls);
          with ListBox do
          begin
            Parent := InputPanel;
            Align := alClient;
            BiDiMode := vForm.BiDiMode;
            OnDblClick := @OnListBoxDblClick;
            TabOrder := 0;
          end;
        end;
      end;

      with TBevel.Create(FOwnerControls) do
      begin
        Parent := vForm;
        Align := alBottom;
        Height := 2;
      end;

      ButtonsPanel := TPanel.Create(FOwnerControls);
      with ButtonsPanel do
      begin
        Parent := vForm;
        TabStop := False;
        Align := alBottom;
        Caption := '';
        BevelInner := bvNone;
        BevelOuter := bvNone;
        BorderWidth := cMargin;
        Height := BorderWidth + aButtonHeight * 2;
        Top := vForm.ClientHeight;
        ChildSizing.VerticalSpacing := cSpacing;
        ChildSizing.HorizontalSpacing := cSpacing;
      end;

      for i := 0 to Length(Choices) -1 do
      begin
        B := Choices[i];
        aButton := CreateButton(FOwnerControls, B);
        with aButton do
        begin
          Parent := ButtonsPanel;
          Tag := i; //need for result
          TabStop := True;
          Font := Canvas.Font;
          Constraints.MinWidth := MinButtonWidth;
          AutoSize := true;
          BidiMode := vForm.BidiMode;
          Name := 'Button' + IntToStr(i);
          OnClick := @ButtonClick;

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
          if UseRightToLeftReading then
            Align := alLeft
          else
            Align := alRight;
        end;
      end;
    end;
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

function TMsgForm.ShowNow(vCancelChoiceIndex: Integer): Integer;
var
  mr: Integer;
begin
  Position := poMainFormCenter;
  ChoiceIndex := vCancelChoiceIndex;
  mr := ShowModal;
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
    Clipboard.AsText := MsgLabel.Caption;
  end
  else if not IsStatus and (Shift = []) and (Key = VK_ESCAPE) then
    Close;
end;

procedure TMsgForm.OnListBoxDblClick(Sender: TObject);
begin
  ChoiceIndex := FDefaultChoice;
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

function TGUIMsgBox.ShowInput(var Answer: string; const vText: string; Choices: array of TMsgSelect; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer;
var
  aMsgForm: TMsgForm;
begin
  aMsgForm := CreateForm(msgkInput);
  CreateFormObjects(aMsgForm, vText, Application.Title, Choices, DefaultChoice, CancelChoice);
  with aMsgForm do
  try
    TextBox.Text := Answer;
    if Kind = msgkPassword then
      TextBox.PasswordChar := '*';
    Result := ShowNow(CancelChoice);
    Answer := TextBox.Text;
  finally
    Free;
  end;
end;

function TGUIMsgBox.ShowList(var Answer: Integer; const vText: string; vStrings: TStrings; Choices: array of TmsgSelect; DefaultChoice: Integer; CancelChoice: Integer; Kind: TmsgKind): Integer;
var
  aMsgForm: TMsgForm;
begin
  aMsgForm := CreateForm(msgkList);
  CreateFormObjects(aMsgForm, vText, Application.Title, Choices, DefaultChoice, CancelChoice);
  with aMsgForm do
  try
    ListBox.Items.Assign(vStrings);
    if Answer < ListBox.Items.Count then
      ListBox.ItemIndex := Answer;
    Result := ShowNow(CancelChoice);
    Answer := ListBox.ItemIndex;
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

