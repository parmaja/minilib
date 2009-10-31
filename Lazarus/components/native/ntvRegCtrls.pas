unit ntvRegCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, LResources, TypInfo, LCLProc, Forms, Controls,
  ntvDotMatrix, ntvCtrls, ntvProgressBars, ntvPageControls,
  ComponentEditors, PropEdits{, OIFavouriteProperties};

type

  { TntvPageControlEditor }

  TntvPageControlEditor = class(TDefaultComponentEditor)
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
  procedure AddFav(ABaseClass: TPersistentClass; const APropertyName: string);
  begin
  //  DefaultOIFavouriteProperties.Add(TOIFavouriteProperty.Create(ABaseClass,APropertyName,true));
  end;
begin
  RegisterComponents('Native', [TDotMatrix, TTextDotMatrix, TntvProgressBar, TntvGauge, TntvPageControl, TntvPage]);
  RegisterComponentEditor(TntvPageControl, TntvPageControlEditor);
  AddFav(TntvPageControl, 'Items');
  //RegisterNoIcon([TntvPage]);
end;

{ TntvPageControlEditor }

constructor TntvPageControlEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  BestEditEvent := 'Items';
end;

procedure TntvPageControlEditor.Edit;
begin
  EditCollection(Component, (Component as TntvPageControl).Items, 'Items');
end;

procedure TntvPageControlEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
    1: (Component as TntvPageControl).NextPage;
    2: (Component as TntvPageControl).PriorPage;
  end;
end;

function TntvPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Pages';
    1: Result := 'Next';
    2: Result := 'Prior';
  end;
end;

function TntvPageControlEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

procedure TntvPageControlEditor.PrepareItem(Index: Integer; const AnItem: TMenuItem);
begin
  inherited;
end;

initialization
end.

