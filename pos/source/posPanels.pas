unit posPanels;
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
  {$IFDEF FPC}
  LCLIntf,
  LCLType,
  {$ELSE}
  Windows,
  Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Types,
  Forms,
  posControls, posTypes, posStuffs;

type

  { TposPanel }

  TposPanel = class(TCustomControl)
  private
  protected
    {$IFNDEF FPC}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$ENDIF}
  public
    {$IFDEF FPC}
    procedure EraseBackground(DC: HDC); override;
    {$endif}
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  posUtils;



{ TposPanel }

constructor TposPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, {csOpaque, }csDoubleClicks];//, csPannable
end;

procedure TposPanel.Paint;
begin
  //inherited;
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Color := Font.Color;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := Color;
    Canvas.Rectangle(ClientRect);
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);
  end;
end;

{$IFDEF FPC}
procedure TposPanel.EraseBackground(DC: HDC);
begin
end;
{$ELSE}
procedure TposPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
//  inherited;
  Message.Result := 1;
end;
{$ENDIF}

end.
