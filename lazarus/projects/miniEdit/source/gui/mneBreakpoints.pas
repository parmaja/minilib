unit mneBreakpoints;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TBreakpointsForm = class(TForm)
    BreakpointList: TListView;
    CloseBtn: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BreakpointListDblClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
  private
    procedure Reload;
    procedure DoGoto;
  public
  end;

procedure ShowBreakpointsForm;

implementation

uses
  EditorEngine, mneResources;

{$R *.lfm}

procedure ShowBreakpointsForm;
begin
  with TBreakpointsForm.Create(Application) do
  begin
    Show;
  end;
end;

procedure TBreakpointsForm.FormCreate(Sender: TObject);
begin
  Engine.Debug.Lock;
  try
    Reload;
  finally
    Engine.Debug.Unlock;
  end;
end;

procedure TBreakpointsForm.Reload;
var
  i: Integer;
  aItem: TListItem;
begin
  BreakpointList.Clear;
  for i := 0 to Engine.Debug.Breakpoints.Count - 1 do
  begin
    aItem := BreakpointList.Items.Add;
    aItem.ImageIndex := 40;
    aItem.Data := Pointer(Engine.Debug.Breakpoints[i].Handle);
    aItem.Caption := Engine.Debug.Breakpoints[i].FileName;
    aItem.SubItems.Add(IntToStr(Engine.Debug.Breakpoints[i].Line));
  end;
end;

procedure TBreakpointsForm.Button2Click(Sender: TObject);
begin
  if BreakpointList.Selected <> nil then
  begin
    Engine.Debug.Lock;
    try
      Engine.Debug.Breakpoints.Remove(Integer(BreakpointList.Selected.Data));
      Reload;
      Engine.UpdateState([ecsDebug]);
    finally
      Engine.Debug.Unlock;
    end;
  end;
end;

procedure TBreakpointsForm.Button1Click(Sender: TObject);
begin
  Engine.Debug.Lock;
  try
    Engine.Debug.Breakpoints.Clear;
    Reload;
    Engine.UpdateState([ecsDebug]);
  finally
    Engine.Debug.Unlock;
  end;
end;

procedure TBreakpointsForm.Button3Click(Sender: TObject);
begin
  DoGoto;
end;

procedure TBreakpointsForm.DoGoto;
begin
  if BreakpointList.Selected <> nil then
  begin
    Engine.Files.ShowFile(BreakpointList.Selected.Caption, StrToIntDef(BreakpointList.Selected.SubItems[0], 0));
  end;
end;

procedure TBreakpointsForm.BreakpointListDblClick(Sender: TObject);
begin
  DoGoto;
end;

procedure TBreakpointsForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

end.

