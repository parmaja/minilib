unit MainForm;

{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, StdCtrls, ExtCtrls, mnSockets, mnServers, mnHttpServer,
  LResources, Buttons, Menus;

type

  { TMain }

  TMain = class(TForm)
    Bevel2: TBevel;
    KeepAliveChk: TCheckBox;
    MainMenu1: TMainMenu;
    Memo: TMemo;
    MaxOfThreads: TLabel;
    MenuItem1: TMenuItem;
    StartBtn: TButton;
    RootEdit: TEdit;
    Label1: TLabel;
    StopBtn: TButton;
    Label2: TLabel;
    PortEdit: TEdit;
    NumberOfThreadsLbl: TLabel;
    NumberOfThreads: TLabel;
    Bevel1: TBevel;
    ExitBtn: TButton;
    procedure ExitBtnClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure StayOnTopChkChange(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    WebServer: TmnHttpServer;
    FMax: Integer;
    procedure WebServerBeforeOpen(Sender: TObject);
    procedure WebServerAfterClose(Sender: TObject);
    procedure WebServerChanged(Listener: TmnListener);
    procedure WebServerLog(const S: String);
  public
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

procedure TMain.StartBtnClick(Sender: TObject);
begin
  WebServer.Start;
end;

procedure TMain.FormHide(Sender: TObject);
begin
end;

procedure TMain.MenuItem1Click(Sender: TObject);
begin
  Close;
end;

procedure TMain.ExitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.StayOnTopChkChange(Sender: TObject);
begin

end;

procedure TMain.StopBtnClick(Sender: TObject);
begin
  WebServer.Stop;
  StartBtn.Enabled := True;
  FMax := 0;
end;

procedure TMain.WebServerBeforeOpen(Sender: TObject);
var
  aRoot: String;
begin
  StartBtn.Enabled := False;
  StopBtn.Enabled := True;
  aRoot := RootEdit.Text;
  if (LeftStr(aRoot, 2) = '.\') or (LeftStr(aRoot, 2) = './') then
    aRoot := ExtractFilePath(Application.ExeName) + Copy(aRoot, 3, MaxInt);
  WebServer.DocumentRoot := aRoot;
  WebServer.AllowKeepAlive := KeepAliveChk.Checked;
  WebServer.Port := PortEdit.Text;
end;

function FindCmdLineValue(Switch: String; var Value: String; const Chars: TSysCharSet = ['/', '-']; Seprator: Char = '='): Boolean;
var
  i, l: Integer;
  s, c, w: String;
begin
  Result := False;
  l := Length(Switch);
  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    c := Copy(s, l + 2, 1);
    w := Copy(s, 2, l);
    if (Chars = []) or ((s <> '') and (s[1] in Chars)) then
      if (w = Switch) and ((c = '') or (c = Seprator)) then
      begin
        Value := Copy(S, l + 3, Maxint);
        Result := True;
        break;
      end;
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
var
  aReg: TRegistry;

  function GetOption(AName, ADefault: String): String; overload;
  var
    s: String;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result := AnsiDequotedStr(s, '"')
    else if aReg.ValueExists(AName) then
      Result := aReg.ReadString(AName)
    else
      Result := ADefault;
  end;

  function GetOption(AName: String; ADefault: Boolean): Boolean; overload;
  var
    s: String;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result := SameText(AnsiDequotedStr(s, '"'), 'true')
    else if aReg.ValueExists(AName) then
      Result := aReg.ReadBool(AName)
    else
      Result := ADefault;
  end;

  function GetSwitch(AName, ADefault: String): String;//if found in cmd mean it is true
  var
    s: String;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result := 'True'
    else if aReg.ValueExists(AName) then
      Result := aReg.ReadString(AName)
    else
      Result := ADefault;
  end;

var
  aAutoRun: Boolean;
begin
  WebServer := TmnHttpServer.Create();
  WebServer.OnBeforeOpen := WebServerBeforeOpen;
  WebServer.OnAfterClose := WebServerAfterClose;
  WebServer.OnChanged := WebServerChanged;
  WebServer.OnLog := WebServerLog;

  aReg := TRegistry.Create;
  try
    aReg.OpenKey('software\miniWebServer\Options', True);
    RootEdit.Text := GetOption('root', '.\html');
    PortEdit.Text := GetOption('port', '80');
    KeepAliveChk.Checked := GetOption('keepalive', True);
    aAutoRun := StrToBoolDef(GetSwitch('run', ''), False);
  finally
    aReg.Free;
  end;
  if aAutoRun then
    WebServer.Start;
end;

procedure TMain.FormDestroy(Sender: TObject);
var
  aReg: TRegistry;
begin
  WebServer.Stop;
  FreeAndNil(WebServer);
  if ParamCount = 0 then
  begin
    aReg := TRegistry.Create;
    try
      aReg.OpenKey('software\miniWebServer\Options', True);
      aReg.WriteString('root', RootEdit.Text);
      aReg.WriteString('port', PortEdit.Text);
      aReg.WriteBool('keepalive', KeepAliveChk.Checked);
    finally
      aReg.Free;
    end;
  end;
end;

procedure TMain.WebServerAfterClose(Sender: TObject);
begin
  StartBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMain.WebServerChanged(Listener: TmnListener);
begin
  if FMax < Listener.Count then
    FMax := Listener.Count;
  NumberOfThreads.Caption := IntToStr(Listener.Count);
  MaxOfThreads.Caption := IntToStr(FMax);
end;

procedure TMain.WebServerLog(const S: String);
begin
  Memo.Lines.Add(s);
end;

end.
