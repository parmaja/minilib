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
  IniFiles, StdCtrls, ExtCtrls, mnSockets, mnServers, ScatCommands,
  LResources, Buttons, Menus;

type

  { TMain }

  TMain = class(TForm)
    Bevel2: TBevel;
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
    ScatServer: TScatServer;
    FMax:Integer;
    procedure ScatServerBeforeOpen(Sender: TObject);
    procedure ScatServerAfterClose(Sender: TObject);
    procedure ScatServerChanged(Listener: TmnListener);
    procedure ScatServerLog(const S: String);
  public
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

procedure TMain.StartBtnClick(Sender: TObject);
begin
  ScatServer.Start;
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
  ScatServer.Stop;
  StartBtn.Enabled:=true;
end;

procedure TMain.ScatServerBeforeOpen(Sender: TObject);
var
  aRoot:string;
begin
  StartBtn.Enabled := False;
  StopBtn.Enabled := True;
  aRoot := RootEdit.Text;
  if (LeftStr(aRoot, 2)='.\') or (LeftStr(aRoot, 2)='./') then
    aRoot := ExtractFilePath(Application.ExeName) + Copy(aRoot, 3, MaxInt);
  ScatServer.DocumentRoot := aRoot;
  ScatServer.Port := PortEdit.Text;
end;

function FindCmdLineValue(Switch: string; var Value: string; const Chars: TSysCharSet = ['/','-']; Seprator: Char = '='): Boolean;
var
  i, l: Integer;
  s, c, w: string;
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
  aIni:TIniFile;
  function GetOption(AName, ADefault:string):string;
  var
    s:string;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result :=AnsiDequotedStr(s, '"')
    else
      Result := aIni.ReadString('options', AName, ADefault);
  end;
  
  function GetSwitch(AName, ADefault:string):string;//if found in cmd mean it is true
  var
    s:string;
  begin
    s := '';
    if FindCmdLineValue(AName, s) then
      Result := 'True'
    else
      Result := aIni.ReadString('options',AName, ADefault);
  end;

var
  aAutoRun:Boolean;
begin
  ScatServer := TScatServer.Create;
  ScatServer.OnBeforeOpen := ScatServerBeforeOpen;
  ScatServer.OnAfterClose := ScatServerAfterClose;
  ScatServer.OnChanged :=  ScatServerChanged;
  ScatServer.OnLog := ScatServerLog;

  aIni := TIniFile.Create(Application.Location + 'config.ini');
  try
    RootEdit.Text := GetOption('root', '.\html');
    PortEdit.Text := GetOption('port', '81');
    aAutoRun := StrToBoolDef(GetSwitch('run', ''), False);
  finally
    aIni.Free;
  end;
  if aAutoRun then
     ScatServer.Start;
end;

procedure TMain.FormDestroy(Sender: TObject);
var
  aIni:TIniFile;
begin
  if ParamCount = 0 then
  begin
    aIni := TIniFile.Create(Application.Location+'config.ini');
    try
      aIni.WriteString('options', 'root', RootEdit.Text);
      aIni.WriteString('options', 'port', PortEdit.Text);
    finally
      aIni.Free;
    end;
  end
end;

procedure TMain.ScatServerAfterClose(Sender: TObject);
begin
	StartBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMain.ScatServerChanged(Listener: TmnListener);
begin
  if FMax < Listener.Count then
    FMax := Listener.Count;
  NumberOfThreads.Caption:=IntToStr(Listener.Count);
  MaxOfThreads.Caption:=IntToStr(FMax);
end;

procedure TMain.ScatServerLog(const S: String);
begin
  Memo.Lines.Add(s);
end;

end.
