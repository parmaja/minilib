unit MainForm;

{$MODE Delphi}

{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, StdCtrls, ExtCtrls, mnWin32Sockets, mnConnections, mnSockets, mnServers, mnHttpServer,
  LResources, Buttons;

type

  { TMain }

  TMain = class(TForm)
    Memo: TMemo;
    StartBtn: TButton;
    RootEdit: TEdit;
    Label1: TLabel;
    StopBtn: TButton;
    VirtualDomainsChk: TCheckBox;
    Label2: TLabel;
    PortEdit: TEdit;                          
    StayOnTopChk: TCheckBox;             
    NumberOfThreadsLbl: TLabel;
    NumberOfThreads: TLabel;
    Bevel1: TBevel;
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure VirtualDomainsChkChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StayOnTopChkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    WebServer: TmnHttpServer;
    procedure WebServerBeforeOpen(Sender: TObject);
    procedure WebServerAfterClose(Sender: TObject);
    procedure WebServerChanged(Listener: TmnListener);
    procedure WebServerLog(Connection: TmnConnection; const S: String);
  public
  end;

var
  Main: TMain;

implementation


procedure TMain.StartBtnClick(Sender: TObject);
begin
  WebServer.Start;
end;

procedure TMain.StopBtnClick(Sender: TObject);
begin
  WebServer.Stop;
  StartBtn.Enabled:=true;
end;

procedure TMain.VirtualDomainsChkChange(Sender: TObject);
begin
end;

procedure TMain.WebServerBeforeOpen(Sender: TObject);
var
  aRoot:string;
begin
  Memo.Clear;
  StartBtn.Enabled:=false;
  StopBtn.Enabled:=True;
  aRoot := RootEdit.Text;
  if LeftStr(aRoot, 2)='.\' then
     aRoot := ExtractFilePath(Application.ExeName) + Copy(aRoot, 3, MaxInt);
  WebServer.DocumentRoot := aRoot;
  WebServer.Port:=PortEdit.Text;
  WebServer.VirtualDomains:= VirtualDomainsChk.Checked;
end;

function FindCmdLineValue(Switch: string; var Value: string; const Chars: TSysCharSet = ['/','-']; Seprator: Char = ' '; IgnoreCase: Boolean = true): Boolean;
var
  I: Integer;
  S: string;
begin
  Switch := Switch + Seprator;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if (Chars = []) or (S[1] in Chars) then
      if IgnoreCase then
      begin
        if (AnsiCompareText(Copy(S, 2, Length(Switch)), Switch) = 0) then
        begin
          Result := True;
          Value := Copy(S, Length(Switch) + 2, Maxint);
          Exit;
        end;
      end
      else
      begin
        if (AnsiCompareStr(Copy(S, 2, Length(Switch)), Switch) = 0) then
        begin
          Result := True;
          Value := Copy(S, Length(Switch) + 2, Maxint);
          Exit;
        end;
      end;
  end;
  Result := False;
end;

procedure TMain.FormCreate(Sender: TObject);
var
  s:string;
  aReg:TRegistry;
begin
  WebServer := TmnHttpServer.Create(Self);
  WebServer.OnBeforeOpen := WebServerBeforeOpen;
  WebServer.OnAfterClose := WebServerAfterClose;
  WebServer.OnChanged :=  WebServerChanged;
  WebServer.OnLog := WebServerLog;

  if ParamCount=0 then
  begin
    aReg:=TRegistry.Create;
    aReg.OpenKey('software\miniWebServer\Options', True);
    if aReg.ValueExists('DocumentRoot') then
      RootEdit.Text:=aReg.ReadString('DocumentRoot');
    if aReg.ValueExists('Port') then
      PortEdit.Text:=aReg.ReadString('Port');
    aReg.Free;
  end
  else
  begin
    if FindCmdLineValue('root',s) then
      RootEdit.Text:=AnsiDequotedStr(s,'"');
    if FindCmdLineValue('port',s) then
      PortEdit.Text:=s;
    if FindCmdLineValue('multi',s) then
      VirtualDomainsChk.Checked:=StrToBoolDef(s,true);
    if FindCmdLineSwitch('run',true) then
      WebServer.Start;
  end;
end;

procedure TMain.StayOnTopChkClick(Sender: TObject);
begin
{	if StayOnTopChk.Checked then
  	SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
          SWP_NOSIZE or SWP_NOACTIVATE)
  else
  	SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
          SWP_NOSIZE or SWP_NOACTIVATE);}
end;

procedure TMain.FormDestroy(Sender: TObject);
var
  aReg:TRegistry;
begin
  if ParamCount=0 then
  begin
    aReg := TRegistry.Create;
    aReg.OpenKey('software\miniWebServer\Options', True);
    aReg.WriteString('DocumentRoot', RootEdit.Text);
    aReg.WriteString('Port', PortEdit.Text);
    aReg.Free;
  end
end;

procedure TMain.WebServerAfterClose(Sender: TObject);
begin
	StartBtn.Enabled:=True;
  StopBtn.Enabled:=False;
end;

procedure TMain.WebServerChanged(Listener: TmnListener);
begin
  NumberOfThreads.Caption:=IntToStr(Listener.Count);
end;

procedure TMain.WebServerLog(Connection: TmnConnection; const S: String);
begin
  Memo.Lines.Add(Connection.Stream.Socket.GetRemoteAddress +': '+ s);
end;

initialization
  {$i MainForm.lrs}
end.

