unit MsgBox;
{$mode objfpc}{$H+}
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}
{
 TODO;
 Check if not MsgBox installed
}
interface

uses
  SysUtils, Variants, Classes, Contnrs;

type
  TMsgKind = (msgkNormal, msgkWarning, msgkError, msgkInformation, msgkConfirmation, msgkInput, msgkPassword, msgkStatus);
  TChoiceKind = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbDiscard, mbNone, mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TChoices = set of TChoiceKind;

  TChoiceRec = record
    Caption: string;
    Kind: TChoiceKind;
  end;

  { TMsgPrompt }

  TMsgPrompt = class(TObject)
  private
  protected
    FName: String;
    FTitle: String;
    function OutMsg(const Msg: string; Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer; virtual; abstract;
    function OutMsg2(const Msg: string; Choices: array of TChoiceRec; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer; virtual; abstract;
    function InputMsg(var vResult: string; const Msg: string; Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer; virtual; abstract;
    function InputMsg2(var vResult: string; const Text: string; Choices: array of TChoiceRec; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer; virtual; abstract;
    procedure ShowStatus(Msg: string; Sender: TObject = nil); virtual; abstract;
    procedure UpdateStatus(Msg: string; Sender: TObject = nil); virtual; abstract;
    procedure HideStatus(Sender: TObject); virtual; abstract;
    procedure Created; virtual; abstract;
    property Name: String read FName;
    property Title: String read FTitle write FTitle;
  public
    constructor Create; virtual;
  end;

  TMsgPromptClass = class of TMsgPrompt;

  { TMsgBox }

  TMsgBox = class(TObjectList)
  private
    FCurrent: TMsgPrompt;
    FLockCount: Integer;
    function GetItem(Index: Integer): TMsgPrompt;
    function GetLocked: Boolean;
    procedure SetCurrent(AValue: TMsgPrompt);
    procedure SetLocked(const Value: Boolean);
  protected
    function OutMsg(const Msg: string; Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer;
    function OutMsg2(const Msg: string; Choices: array of TChoiceRec; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer;
    function InputMsg(var vResult: string; const Msg: string; Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer;
    property Current: TMsgPrompt read FCurrent write SetCurrent;
    function Find(vName: String): TMsgPrompt;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Register(MsgPromptClass: TMsgPromptClass; SwitchToCurrent: Boolean = False);
    function Switch(vName: String): TMsgPrompt;
    procedure EnumItems(vItems: TStrings);
    property Items[Index: Integer]: TMsgPrompt read GetItem;

    function Input(var vResult: string; const vMsg: string): boolean;
    function Password(var vResult: string; const vMsg: string): boolean;
    function Ask(const Msg: string; Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind = msgkNormal): Integer;
    function Ask(const Msg: string; Choices: array of string; DefaultChoice: Integer; CancelChoice: Integer = -1; Kind: TMsgKind = msgkNormal): Integer;
    function Ask(const Msg: string; Choices: array of TChoiceRec; DefaultChoice: Integer; CancelChoice: Integer = -1; Kind: TMsgKind = msgkNormal): Integer;

    //OK/Cancel the default OK
    function Ok(const vStr: string): boolean;
    //OK/Cancel the default Cancel
    function Cancel(const vStr: string): boolean;
    //Yes/No the default Yes
    function Yes(const vStr: string): boolean;
    //Yes/No the default No
    function No(const vStr: string): boolean;
    function YesNoCancel(const vStr: string): Integer;

    function Error(const vStr: string): boolean;
    function Warning(const vStr: string): boolean;
    function Hint(const vStr: string): boolean;

    procedure Show(vVar: Variant);
    procedure List(Strings: TStringList; Kind: TMsgKind);

    procedure ShowStatus(Sender: TObject; const vMsg: string);
    procedure HideStatus(Sender: TObject);

    property Locked: Boolean read GetLocked write SetLocked;
  end;

const //copy from Controls.pas
  mrUnkown = 0;
  mrOK = mrUnkown + 1;
  mrCancel = mrUnkown + 2;
  mrAbort = mrUnkown + 3;
  mrRetry = mrUnkown + 4;
  mrIgnore = mrUnkown + 5;
  mrYes = mrUnkown + 6;
  mrNo = mrUnkown + 7;
  mrAll = mrUnkown + 8;
  mrNoToAll = mrUnkown + 9;
  mrYesToAll = mrUnkown + 10;
  mrClose = mrUnkown + 11;
  mrNone = mrUnkown + 12;
  mrDiscard = mrUnkown + 12;
  mrLast = mrClose;

var
  ChoiceNames: array[TChoiceKind] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'Discard', 'None',  'All', 'NoToAll',
    'YesToAll', 'Help');

  ChoiceCaptions: array[TChoiceKind] of string = (
    '&Yes', '&No', '&OK', '&Cancel', '&Abort', '&Retry', 'Di&scard', 'N&one', '&Ignore', '&All', 'No &To All',
    'Yes To A&ll', '&Help');

  ModalResults: array[TChoiceKind] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrNone, mrNone, mrAll, mrNoToAll,
    mrYesToAll, mrNone);

{* TMsgConsole

}
type

  { TMsgConsole }

  TMsgConsole = class(TMsgPrompt)
  private
  protected
    function OutMsg(const Msg: string; Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer; override;
    function InputMsg(var vResult: string; const Msg: string; Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer; override;
    procedure ShowStatus(Msg: string; Sender: TObject = nil); override;
    procedure HideStatus(Sender: TObject = nil); override;
    procedure Created; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

function Msg: TMsgBox;
function MsgB(Caption: string; Kind: TChoiceKind = mbNone): TChoiceRec;

implementation

var
  FMsgBox: TMsgBox = nil;

function Msg: TMsgBox;
begin
  if FMsgBox = nil then
    FMsgBox := TMsgBox.Create;
  Result := FMsgBox;
end;

function MsgB(Caption: string; Kind: TChoiceKind): TChoiceRec;
begin
  Result.Caption := Caption;
  Result.Kind := Kind;
end;

{ TMsgPrompt }

constructor TMsgPrompt.Create;
begin
  inherited Create;
end;

constructor TMsgBox.Create;
begin
  inherited;
end;

destructor TMsgBox.Destroy;
begin
  inherited;
  FMsgBox := nil;
end;

procedure TMsgBox.Register(MsgPromptClass: TMsgPromptClass; SwitchToCurrent: Boolean);
var
  lMsgPrompt: TMsgPrompt;
begin
  lMsgPrompt := MsgPromptClass.Create;
  inherited Add(lMsgPrompt);
  if SwitchToCurrent or ((Count = 1) and (FCurrent = nil)) then
    Current := lMsgPrompt;
end;

function TMsgBox.Switch(vName: String): TMsgPrompt;
var
  aItem: TMsgPrompt;
  aCurrent: TMsgPrompt;
begin
  aCurrent := Current;

  aItem := Find(vName);
  if aItem = nil then
    Exception.Create(vName + ' not found!')
  else
    aCurrent := aItem;
  Result := aItem;
  Current := aCurrent;
end;

procedure TMsgBox.EnumItems(vItems: TStrings);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    vItems.AddObject(Items[i].Title, Items[i]);
  end;
end;

function TMsgBox.Cancel(const vStr: string): boolean;
begin
  Result := OutMsg(vStr, [mbOK, mbCancel], mbCancel, mbOk, msgkWarning) = mrCancel
end;

function TMsgBox.Ok(const vStr: string): boolean;
begin
  Result := OutMsg2(vStr, [MsgB(ChoiceCaptions[mbOK], mbOK), MsgB(ChoiceCaptions[mbCancel], mbCancel)], 0, 1, msgkWarning) = mrOK;
end;

function TMsgBox.Input(var vResult: string; const vMsg: string): boolean;
begin
  Result := InputMsg(vResult, vMsg, [mbOK, mbCancel], mbOk, mbCancel, msgkConfirmation) = mrOK
end;

function TMsgBox.Password(var vResult: string; const vMsg: string): boolean;
begin
  Result := InputMsg(vResult, vMsg, [mbOK, mbCancel], mbOk, mbCancel, msgkPassword) = mrOK
end;

function TMsgBox.Yes(const vStr: string): boolean;
begin
  Result := OutMsg(vStr, [mbYes, mbNo], mbYes, mbNo, msgkConfirmation) = mrYes;
end;

function TMsgBox.No(const vStr: string): boolean;
begin
  Result := OutMsg(vStr, [mbYes, mbNo], mbNo, mbNo, msgkConfirmation) in [mrCancel, mrNo];
end;

function TMsgBox.YesNoCancel(const vStr: string): Integer;
begin
  Result := OutMsg(vStr, [mbYes, mbNo, mbCancel], mbYes, mbCancel, msgkConfirmation);
end;

function TMsgBox.Error(const vStr: string): boolean;
begin
  Result := OutMsg(vStr, [mbOK], mbOK, mbOk, msgkError) = mrOK
end;

function TMsgBox.Hint(const vStr: string): boolean;
begin
  Result := OutMsg(vStr, [mbOK], mbOK, mbOK, msgkError) = mrOK
end;

function TMsgBox.Warning(const vStr: string): boolean;
begin
  Result := OutMsg(vStr, [mbYes], mbOK, mbOK, msgkWarning) = mrOK
end;

procedure TMsgBox.Show(vVar: Variant);
begin
  OutMsg(VarToStr(vVar), [mbOK], mbOK, mbOK, msgkInformation)
end;

procedure TMsgBox.List(Strings: TStringList; Kind: TMsgKind);
var
  s: string;
  i, c: Integer;
begin
  s := '';
  c := Strings.Count;
  if c > 30 then
    c := 30;
  for i := 0 to c - 1 do
  begin
    if s <> '' then
      s := s + #13;
    s := s + Strings[i];
  end;
  if c < Strings.Count then
    s := s + #13 + '...';
  OutMsg(s, [mbOK], mbOK, mbOK, Kind);
end;

procedure TMsgBox.HideStatus(Sender: TObject);
begin
  if not Locked and (FCurrent <> nil) then
    FCurrent.HideStatus(Sender);
end;

procedure TMsgBox.ShowStatus(Sender: TObject; const vMsg: string);
begin
  if not Locked and (FCurrent <> nil) then
    FCurrent.ShowStatus(vMsg, Sender);
end;

function TMsgBox.InputMsg(var vResult: string; const Msg: string;
  Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind;
  Kind: TMsgKind): Integer;
begin
  if not Locked and (FCurrent <> nil) then
    Result := FCurrent.InputMsg(vResult, Msg, Choices, DefaultChoice, CancelChoice, Kind)
  else
    Result := ModalResults[DefaultChoice];
end;

function TMsgBox.OutMsg(const Msg: string; Choices: TChoices;
  DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer;
begin
  if not Locked and (FCurrent <> nil) then
    Result := FCurrent.OutMsg(Msg, Choices, DefaultChoice, CancelChoice, Kind)
  else
    Result := ModalResults[DefaultChoice];
end;

function TMsgBox.OutMsg2(const Msg: string; Choices: array of TChoiceRec; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind): Integer;
begin
  if not Locked and (FCurrent <> nil) then
    Result := FCurrent.OutMsg2(Msg, Choices, DefaultChoice, CancelChoice, Kind)
  {else
    Result := ModalResults[DefaultChoice];}//TODO
end;

function TMsgBox.Find(vName: String): TMsgPrompt;
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

function TMsgBox.GetLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

function TMsgBox.GetItem(Index: Integer): TMsgPrompt;
begin
  Result := inherited Items[Index] as TMsgPrompt;
end;

procedure TMsgBox.SetCurrent(AValue: TMsgPrompt);
begin
  if FCurrent =AValue then Exit;
  FCurrent :=AValue;
end;

procedure TMsgBox.SetLocked(const Value: Boolean);
begin
  if Value then
    Inc(FLockCount)
  else
    Dec(FLockCount);
end;

function TMsgBox.Ask(const Msg: string; Choices: TChoices;
  DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer;
begin
  Result := OutMsg(Msg, Choices, DefaultChoice, CancelChoice, Kind);
end;

function TMsgBox.Ask(const Msg: string; Choices: array of string; DefaultChoice: Integer; CancelChoice: Integer; Kind: TMsgKind = msgkNormal): Integer;
begin
  //Result := OutMsg(Msg, Choices, DefaultChoice, CancelChoice, Kind);
end;

function TMsgBox.Ask(const Msg: string; Choices: array of TChoiceRec; DefaultChoice: Integer; CancelChoice: Integer = -1; Kind: TMsgKind = msgkNormal): Integer;
begin
end;

constructor TMsgConsole.Create;
begin
  inherited;
end;

destructor TMsgConsole.Destroy;
begin
  inherited;
end;

procedure TMsgConsole.HideStatus(Sender: TObject);
begin
  //TODO
end;

procedure TMsgConsole.Created;
begin
  FName := 'CONSOLE';
  FTitle := 'Console Messages';
end;

procedure TMsgConsole.ShowStatus(Msg: string; Sender: TObject);
begin
  WriteLn(Msg);
end;

function TMsgConsole.OutMsg(const Msg: string; Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer;
var
  B: TChoiceKind;
  i, p: Integer;
  s: string;
  ch: Char;
begin
  //SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
  Write(Msg + ' [');
  i := 0;
  for B := Low(TChoiceKind) to High(TChoiceKind) do
    if B in Choices then
    begin
      if i > 0 then
        write(',');
      s := ChoiceCaptions[B];
      p := Pos('&', s);
      if p > 0 then
      begin
        write(Copy(s, 1, p - 1));
        //SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY);
        write(Copy(s, p + 1, 1));
        //SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
        write(Copy(s, p + 2, MaxInt));
      end
      else
      begin
        write(s);
      end;
      Inc(i);
    end;
  write('] : ');
  ReadLn(ch);
  if ch = '' then
    Result := ModalResults[DefaultChoice]
  else
  begin
    Result := mrNone;
    for B := Low(TChoiceKind) to High(TChoiceKind) do
      if B in Choices then
      begin
        s := ChoiceCaptions[B];
        p := Pos('&', s);
        if p > 0 then
        begin
          if UpperCase(ch) = UpperCase(s[p + 1]) then
          begin
            Result := ModalResults[B];
            break;
          end;
        end;
      end;
  end;
end;

function TMsgConsole.InputMsg(var vResult: string; const Msg: string; Choices: TChoices; DefaultChoice: TChoiceKind; CancelChoice: TChoiceKind; Kind: TMsgKind): Integer;
var
  OldMode: Cardinal;
begin
  if Kind = msgkPassword then
  begin
//    GetConsoleMode(GetStdHandle(STD_Input_HANDLE), OldMode);
//    SetConsoleMode(GetStdHandle(STD_Input_HANDLE), OldMode and not ENABLE_ECHO_INPUT);
  end;
  Write(Msg + ': ');
  ReadLn(vResult);
  if Kind = msgkPassword then
  begin
    WriteLn('');
//    SetConsoleMode(GetStdHandle(STD_Input_HANDLE), OldMode);
  end;
  Result := mrOK;
end;

initialization
  Msg.Register(TMsgConsole);
finalization
  FreeAndNil(FMsgBox);
end.

