unit ParamsForms;
{$mode objfpc}{$H+}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  LCLProc, LCLIntf, LCLType,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls,
  mncConnections, sqlvClasses, mncCSVExchanges;

type

  { TParamsForm }

  TParamsForm = class(TForm)
    CancelBtn: TButton;
    OkBtn: TButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FEditList: TList;
  public
  end;

function ShowSQLParams(Command: TmncCommand):Boolean;

implementation

type
  TsqlvParamEdit = class(TCustomEdit)
  protected
    procedure Change; override;
  public
    CheckBox: TCheckBox;
    Param: TmncParamItem;
  end;

function ShowSQLParams(Command: TmncCommand):Boolean;
var
  x: Integer;
  i: Integer;
  aForm: TParamsForm;
  aEdit: TsqlvParamEdit;
  aCheckBox: TCheckBox;
  aMaxWidth, w:Integer;
const
  xStart = 4;
  yStart = 6;
begin
  if Command.Params.Count = 0 then
    Result := True
  else
  begin
    aForm := TParamsForm.Create(Application);
    with aForm do
    begin
      aMaxWidth := 10;
      for i := 0 to Command.Params.Count - 1 do
      begin
        w := Canvas.TextWidth(Command.Params.Items[i].Name);
        if aMaxWidth < w  then
          aMaxWidth := w;
      end;
      aMaxWidth := aMaxWidth + GetSystemMetrics(SM_CXMENUCHECK) + 10;

      FEditList := TList.Create;
      x := xStart;
      for i := 0 to Command.Params.Count - 1 do
      begin
        aCheckBox:=TCheckBox.Create(aForm);
        aCheckBox.Parent := aForm;
        aCheckBox.Caption := Command.Params.Items[i].Name;
        aCheckBox.Hint := Command.Params.Items[i].Name;
        aCheckBox.SetBounds(yStart, x, aMaxWidth, 21);
        //aCheckBox.Alignment := taLeftJustify;

        aEdit := TsqlvParamEdit.Create(aForm);
        aEdit.Parent := aForm;
        aEdit.CheckBox := aCheckBox; 
        aEdit.Param := Command.Params.Items[i];
        w := yStart + aMaxWidth + 4;
        aEdit.SetBounds(w, x, ClientWidth - w - 4, 21);
        aEdit.Anchors := [akTop, akLeft, akRight];
        Inc(x, 23);
        FEditList.Add(aEdit);
      end;
      ClientHeight := x + OkBtn.Height + 8;
      ActiveControl := TsqlvParamEdit(FEditList[0]);

      Result := ShowModal = mrOK;
      if Result then
      begin
        for i := 0 to FEditList.Count-1 do
        begin
          if TsqlvParamEdit(FEditList[i]).CheckBox.Checked then
            TsqlvParamEdit(FEditList[i]).Param.AsString := TsqlvParamEdit(FEditList[i]).Text
          else
            TsqlvParamEdit(FEditList[i]).Param.Clear;
        end;
      end;
      Free;
    end;
  end;
end;

procedure TParamsForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TParamsForm.FormCreate(Sender: TObject);
begin
end;

{ TsqlvParamEdit }

procedure TsqlvParamEdit.Change;
begin
  inherited;
  CheckBox.Checked := True;
end;

initialization
  {$i ParamsForms.lrs}
end.

