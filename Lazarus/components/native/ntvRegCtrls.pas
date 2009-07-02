unit ntvRegCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, TypInfo, LCLProc, Forms, Controls, 
  DotMatrix, ntvCtrls{, CmponentsEditors};


//type

  { TntvPageControlEditor }

  {TntvPageControlEditor = class(TDefaultComponentEditor)
  protected
    procedure AddNewPageToDesigner(Index: integer); virtual;
    procedure DoAddPage; virtual;
    procedure DoInsertPage; virtual;
    procedure DoDeletePage; virtual;
    procedure DoMoveActivePageLeft; virtual;
    procedure DoMoveActivePageRight; virtual;
    procedure DoMovePage(CurIndex, NewIndex: Integer); virtual;
    procedure AddMenuItemsForPages(ParentMenuItem: TMenuItem); virtual;
    procedure ShowPageMenuItemClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function Notebook: TCustomNotebook; virtual;
  end;}


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Native', [TDotMatrix, TntvProgressBar, TntvGauge, TntvPageControl, TntvPage]);
  //RegisterComponentEditor(TntvPageControl, TntvPageControlEditor);
  //RegisterNoIcon([TntvPage]);
end;

{ TntvPageControlEditor }

{procedure TntvPageControlEditor.AddNewPageToDesigner(Index: integer);
begin

end;

procedure TntvPageControlEditor.DoAddPage;
begin

end;

procedure TntvPageControlEditor.DoInsertPage;
begin

end;

procedure TntvPageControlEditor.DoDeletePage;
begin

end;

procedure TntvPageControlEditor.DoMoveActivePageLeft;
begin

end;

procedure TntvPageControlEditor.DoMoveActivePageRight;
begin

end;

procedure TntvPageControlEditor.DoMovePage(CurIndex, NewIndex: Integer);
begin

end;

procedure TntvPageControlEditor.AddMenuItemsForPages(ParentMenuItem: TMenuItem);
begin

end;

procedure TntvPageControlEditor.ShowPageMenuItemClick(Sender: TObject);
begin

end;

procedure TntvPageControlEditor.ExecuteVerb(Index: Integer);
begin
  inherited ExecuteVerb(Index);
end;

function TntvPageControlEditor.GetVerb(Index: Integer): string;
begin
  Result:=inherited GetVerb(Index);
end;

function TntvPageControlEditor.GetVerbCount: Integer;
begin
  Result:=inherited GetVerbCount;
end;

procedure TntvPageControlEditor.PrepareItem(Index: Integer; const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
end;

function TntvPageControlEditor.Notebook: TCustomNotebook;
begin

end;}

initialization
end.

