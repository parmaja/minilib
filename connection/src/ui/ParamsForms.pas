unit ParamsForms;
{$mode objfpc}{$H+}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

interface

uses
  LCLProc, LCLIntf, LCLType,
  Classes, SysUtils, FileUtil, StdCtrls, LResources, Forms, Controls, Graphics, Dialogs,
  mncConnections;

type

  { TParamsForm }

  TParamsForm = class(TForm)
    CancelBtn: TButton;
    OkBtn: TButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

function ShowSQLParams(Command: TmncCommand): Boolean;

implementation

{$R *.lfm}

type
  TsqlvParamEdit = class(TCustomEdit)
  protected
    procedure Change; override;
  public
    CheckBox: TCheckBox;
    Param: TmncCustomField;
  end;

function ShowSQLParams(Command: TmncCommand): Boolean;
var
  y: Integer;
  r: TRect;
  i: Integer;
  aForm: TParamsForm;
  aEdit: TsqlvParamEdit;
  aCheckList: TList;
  aEditList: TList;
  aCheckBox: TCheckBox;
const
  xStart = 6;
  yStart = 4;
begin
  if Command.Params.Count = 0 then
    Result := True
  else
  begin
    aForm := TParamsForm.Create(Application);
    with aForm do
    begin
      aEditList := TList.Create;
      aCheckList := TList.Create;
      try
        y := yStart;
        for i := 0 to Command.Params.Count - 1 do
        begin
          aCheckBox := TCheckBox.Create(aForm);
          aCheckBox.Parent := aForm;
          aCheckBox.AutoSize := True;
          aCheckBox.Caption := Command.Params.Items[i].Name;
          aCheckBox.Hint := Command.Params.Items[i].Name;
          //aCheckBox.Alignment := taLeftJustify;
          aCheckBox.Top := y;
          aCheckBox.Left := xStart;
          y := y + aCheckBox.Height + 3;
          aCheckList.Add(aCheckBox);
        end;

        for i := 0 to Command.Params.Count - 1 do
        begin
          aEdit := TsqlvParamEdit.Create(aForm);
          aEdit.Parent := aForm;
          aEdit.Param := Command.Params.Items[i];
          aEdit.CheckBox := TCheckBox(aCheckList[i]);
          r := aEdit.CheckBox.BoundsRect;
          r.Left := r.Right + 3;
          r.Right := ClientWidth - 3;
          aEdit.BoundsRect := r;
          aEdit.Anchors := [akTop, akLeft, akRight];
          aEditList.Add(aEdit);
        end;

        ClientHeight := y + OkBtn.Height + 3;
        ActiveControl := TsqlvParamEdit(aEditList[0]);

        Result := ShowModal = mrOK;
        if Result then
        begin
          for i := 0 to aEditList.Count-1 do
          begin
            if TsqlvParamEdit(aEditList[i]).CheckBox.Checked then
              TsqlvParamEdit(aEditList[i]).Param.AsString := TsqlvParamEdit(aEditList[i]).Text
            else
              TsqlvParamEdit(aEditList[i]).Param.Clear;
          end;
        end;
      finally
        FreeAndNil(aEditList);
        FreeAndNil(aCheckList);
        Free;
      end;
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

end.

