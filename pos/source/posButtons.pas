unit posButtons;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, Types,
  Forms,
  posControls, posTypes, posStuffs;

type
  TposOnGetText = procedure(Sender:TObject; var S:string) of object;

  TposButton = class(TposLabeledFrame)
  private
    FCaption: TCaption;
    FOnGetText: TposOnGetText;
    procedure SetCaption(Value: TCaption);
  protected
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure GetCaption(var vCaption: string); virtual;
    procedure PaintInner(vCanvas: TCanvas; const Rect: TRect; vColor: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Caption: TCaption read FCaption write SetCaption;
    property BidiMode;
    property ParentBidiMode;
    property ParentFont;
    property ParentColor;
    property Font;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnGetText:TposOnGetText read FOnGetText write FOnGetText;
  end;
  
  { TposLabeledButton }

  TposLabeledButton = class(TposButton)
  private
  protected
  public
  published
  end;

  TposButtonStuff = class(TInterfacedObject, IposStuff)
  private
    FSize: Integer;
    FCaption: string;
    FStates: TposDrawStates;
    FColor: TColor;
    FID: Integer;
  protected
    function GetObject:TObject;
    procedure SetStates(vStates:TposDrawStates); virtual;
    function GetDrawSize:Integer;
    function Draw(vCanvas:TCanvas; vRect:TRect; vColor:TColor; vStates:TposDrawStates):Boolean; virtual;
    procedure Click; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Size:Integer read FSize write FSize;
    property Color:TColor read FColor write FColor; 
    property Caption:string read FCaption write FCaption;
    property ID:Integer read FID write FID;
    property States:TposDrawStates read FStates write FStates;
  end;

implementation

uses
  posUtils;

procedure TposButton.SetCaption(Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TposButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  try
    inherited;
    Down := True;
  except
    Down := False; 
    raise;
  end;
end;

procedure TposButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  PT: TPoint;
begin
  inherited;
  if MouseCapture then
  begin
    PT.X := X;
    PT.Y := Y;
    if PtInRect(ClientRect, PT) then
      Down := True
    else
      Down := False;
  end;
end;

procedure TposButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  try
    inherited;
  finally
    Down := False;
  end;
end;

constructor TposButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption];
  Style := Style - [fsOpaque, fsBorder];
  Width := 60;
  Height := 22;
end;

destructor TposButton.Destroy;
begin
  inherited Destroy;
end;

procedure TposButton.PaintInner(vCanvas: TCanvas; const Rect: TRect; vColor: TColor);
var
  aCaption: string;
  aRect :TRect;
begin
  inherited;
  vColor := Color;
  if Active then
    vColor := Lighten(vColor, 30)
  else if Down then
    vColor := Lighten(vColor, -15);
  aRect := Rect;
  vCanvas.Font := Self.Font;
  aCaption := '';
  GetCaption(aCaption);
  PaintButton(vCanvas, aCaption, aRect, vColor, clDefault, States + [pdsBorder] + cRightToLeftStates[UseRightToLeftAlignment]);
end;

procedure TposButton.GetCaption(var vCaption: string);
begin
  vCaption := FCaption;
  if Assigned(FOnGetText) then
    FOnGetText(Self, vCaption);
end;

procedure TposButton.DblClick;
begin
  try
    inherited;
    Click;
  finally
  end;
end;

procedure TposButton.Click;
begin
  try
    inherited;
    if AutoActive then
      Active := not Active; 
  finally
    PlayEffect('CLICK', True, True);
  end;
end;

{ TposButtonStuff }

procedure TposButtonStuff.Click;
begin
  inherited;
end;

constructor TposButtonStuff.Create;
begin
  inherited Create;
  FSize := 100;
  FColor := clDEfault;
end;

destructor TposButtonStuff.Destroy;
begin
  inherited;
end;

function TposButtonStuff.Draw(vCanvas:TCanvas; vRect:TRect; vColor:TColor; vStates:TposDrawStates): Boolean;
begin
  if FColor <> clDefault then
    vColor := FColor;
  PaintButton(vCanvas, Caption, vRect, vColor, clDefault, vStates + FStates + [pdsBorder]);
  Result := True;
end;

function TposButtonStuff.GetObject: TObject;
begin
  Result := Self;
end;

procedure TposButtonStuff.SetStates(vStates: TposDrawStates);
begin
  FStates := vStates;
end;

function TposButtonStuff.GetDrawSize: Integer;
begin
  Result := FSize;
end;

end.
