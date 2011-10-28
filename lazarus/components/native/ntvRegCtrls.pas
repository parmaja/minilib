unit ntvRegCtrls;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

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
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner);  override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
    procedure PrepareItem(Index: integer; const AnItem: TMenuItem); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Native', [TntvDotMatrix, TntvTextDotMatrix, TntvProgressBar,
    TntvTabSet, TntvPageControl]);
  RegisterComponentEditor(TntvCustomTabSet, TntvTabSetEditor);
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
  EditCollection(Component, (Component as TntvCustomTabSet).Items, 'Items');
end;

procedure TntvTabSetEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: Edit;
    1: (Component as TntvCustomTabSet).Next;
    2: (Component as TntvCustomTabSet).Prior;
  end;
end;

function TntvTabSetEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Tabs';
    1: Result := 'Next';
    2: Result := 'Prior';
  end;
end;

function TntvTabSetEditor.GetVerbCount: integer;
begin
  Result := 3;
end;

procedure TntvTabSetEditor.PrepareItem(Index: integer; const AnItem: TMenuItem);
begin
  inherited;
end;

initialization
  {$i ntvCtrlsLib.lrs}
end.

