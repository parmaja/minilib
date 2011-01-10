unit ntvRegCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, LResources, TypInfo, LCLProc, Forms, Controls,
  ntvDotMatrix, ntvCtrls, ntvProgressBars, ntvTabSets, ntvPageControls,
  ComponentEditors, PropEdits;

type

  { TntvTabSetEditor }

  TntvTabSetEditor = class(TDefaultComponentEditor)
  protected
  public
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Native', [TDotMatrix, TTextDotMatrix, TntvProgressBar, TntvTabSet, TntvPageControl]);
  RegisterComponentEditor(TntvTabSet, TntvTabSetEditor);
end;

{ TntvTabSetEditor }

constructor TntvTabSetEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  BestEditEvent := 'Items';
end;

procedure TntvTabSetEditor.Edit;
begin
  EditCollection(Component, (Component as TntvTabSet).Items, 'Items');
end;

procedure TntvTabSetEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
    1: (Component as TntvTabSet).Next;
    2: (Component as TntvTabSet).Prior;
  end;
end;

function TntvTabSetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Tabs';
    1: Result := 'Next';
    2: Result := 'Prior';
  end;
end;

function TntvTabSetEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

procedure TntvTabSetEditor.PrepareItem(Index: Integer; const AnItem: TMenuItem);
begin
  inherited;
end;

initialization
end.

