unit mnTailwinds; //* BETA do not use it
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of mod://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    AI(Kimi K2.6)
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *
 *  Tailwind CSS renderer for mnWebElements
 *  https://tailwindcss.com/docs
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, syncobjs, StrUtils,
  DateUtils,
  mnTypes, mnUtils, mnDON, mnSockets, mnServers, mnStreams, mnStreamUtils,
  mnFields, mnParams, mnMultipartData, mnModules, mnWebModules, mnWebElements;

type

  { TTWRenderer }

  TTWRenderer = class(TmnwRenderer)
  protected
    class var TW_ElementRenderers: TmnwElementRenderers;
    procedure Created; override;
  public
    class function ElementRenderers: TmnwElementRenderers; override;
    class constructor Register;
    class destructor Destroy;
  public
    type

      { TElement }

      THTMLElement = class abstract(TmnwElementRenderer)
      protected
        procedure DoEnterRender(Scope: TmnwScope; const Context: TmnwContext); override;
      end;

      THTMLLayout = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
      public
      end;

      { THTMLComponent }

      THTMLComponent = class(THTMLLayout)
      protected
        procedure RenderImageLocation(const Context: TmnwContext; const Image: TImageLocation);
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { THTMLControl }

      THTMLControl = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TComment }

      TComment = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TDocument }

      TDocument = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TBody }

      TBody = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TFile }

      TFile = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TJSFile }

      TJSFile = class(TFile)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TCSSFile }

      TCSSFile = class(TFile)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TDynamicCompose }

      TDynamicCompose = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TIntervalCompose }

      TIntervalCompose = class(TDynamicCompose)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { THeader }

      THeader = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      TNavTools = class(THTMLComponent)
      private
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      public
      end;

      TNavDropdown = class(THTMLComponent)
      private
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      public
      end;

      { TNavBar }

      TNavBar = class(THTMLComponent)
      private
      protected
        procedure DoRenderBrand(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); virtual;
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      public
      end;

      { TMenuBar }

      TMenuBar = class(THTMLComponent)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { THTMLItem }

      THTMLItem = class(THTMLControl)
      protected
        procedure DoEnterRender(Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      public
      end;

      { TLink }

      TLink = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TSpan }

      TSpan = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TFooter }

      TFooter = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TToast }

      TToast = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TContent }

      TContent = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TSideBar }

      TSideBar = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TMain }

      TMain = class(THTMLLayout)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TRow }

      TRow = class(THTMLLayout)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TColumn }

      TColumn = class(THTMLLayout)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TBar }

      TBar = class(THTMLLayout)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TAccordion }

      TAccordion = class(THTMLElement)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TAccordionSection }

      TAccordionSection = class(THTMLElement)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TAccordionItem }

      TAccordionItem = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TCard }

      TCard = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      TPanel = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TCollapseCaption }

      TCollapseCaption = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TThemeModeButton }

      TThemeModeButton = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TDropdown }

      TDropdown = class(THTMLControl)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoEnterRender(Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
        procedure DoLeaveRender(Scope: TmnwScope; const Context: TmnwContext); override;
      end;

      { TDropdownItem }

      TDropdownItem = class(TLink)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TGroupButtons }

      TGroupButtons = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TToolbar }

      TToolbar = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TForm }

      TForm = class(THTMLElement)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TParagraph }

      TParagraph = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      TCode = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      TMultilineCode = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TBreak }

      TBreak = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TButton }

      TButton = class(THTMLItem)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TZoomButtons }

      TZoomButtons = class(TGroupButtons)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TNavItem }

      TNavItem = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TMenuItem }

      TMenuItem = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TSubbMenu }

      TSubMenu = class(THTMLControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TInput }

      TInput = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      TInputPassword = class(TInput)
      end;

      { TImage }

      TImage = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TImageFile }

      TImageFile = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TImageMemory }

      TImageMemory = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

    public
      procedure AddHead(const Context: TmnwContext); override;
  end;

  { TTailwind_Library }

  TTailwind_Library = class(TmnwLibrary)
  protected
    procedure AddHead(const Context: TmnwContext); override;
    procedure Created; override;
  public
  end;

  TmnwTWBoundingHelper = record helper for TmnwBounding
  public
    function IsUniform: Boolean; inline;
    function IsUniformSides: Boolean; inline;
    function ToTWString(prefix: string): string; {$ifndef DEBUG}inline;{$endif}
  end;

function TWAlignToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;
function TWContentJustifyToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;
function TWAlignItemsToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;

function TWFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = True): string;
function TWSizeToStr(Size: TSize; WithSpace: Boolean = True): string;
function TWItemStyleToStr(const Prefix: string; Style: TItemStyle; WithSpace: Boolean = True): string;

implementation

function TWCustomAlignToStr(const s: string; Align: TmnwAlign; WithSpace: Boolean): string; inline;
const
  AlignSuffixes: array[TmnwAlign] of string = ('', 'start', 'center', 'streach', 'baseline', 'end');
begin
  if (Align >= alignStart) and (Align <= alignEnd) then
    Result := s + '-' + AlignSuffixes[Align]
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function TWAlignToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  Result := TWCustomAlignToStr('self', Align, WithSpace);
end;

function TWContentJustifyToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  Result := TWCustomAlignToStr('justify', Align, WithSpace);
end;

function TWAlignItemsToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  Result := TWCustomAlignToStr('items', Align, WithSpace);
end;

function TWFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = True): string;
begin
  case Fixed of
    fixedTop: Result := 'fixed top-0 left-0 right-0 z-50';
    fixedBottom: Result := 'fixed bottom-0 left-0 right-0 z-50';
    fixedStart: Result := 'fixed top-0 bottom-0 left-0 z-50';
    fixedEnd: Result := 'fixed top-0 bottom-0 right-0 z-50';
    stickyTop: Result := 'sticky top-0 z-40';
    stickyBottom: Result := 'sticky bottom-0 z-40';
    stickyStart: Result := 'sticky left-0 z-40';
    stickyEnd: Result := 'sticky right-0 z-40';
  else
    Result := '';
  end;
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function TWSizeToStr(Size: TSize; WithSpace: Boolean = True): string;
const
  SizeStrs: array[TSize] of string = ('', 'xs', 'sm', 'md', 'lg', 'xl', 'parent', 'content');
begin
  Result := SizeStrs[Size];
  if WithSpace and (Result <> '') then
    Result := ' ' + Result
  else if not WithSpace then
    Result := Result;
end;

function TWItemStyleToStr(const Prefix: string; Style: TItemStyle; WithSpace: Boolean): string;
const
  StyleNames: array[TItemStyle] of string = ('', 'primary', 'secondary', 'success', 'danger',
    'warning', 'info', 'light', 'dark', 'link', 'bg-transparent');
begin
  if Style = styleNone then
    Result := StyleNames[Style]
  else if Style > styleUndefined then
    Result := Prefix + StyleNames[Style]
  else
    Result := '';
  if WithSpace and (Result <> '') then
    Result := ' ' + Result;
end;

function TmnwTWBoundingHelper.IsUniform: Boolean; 
begin
  Result := (Top = Left) and (Top = Bottom) and (Top = Right);
end;

function TmnwTWBoundingHelper.IsUniformSides: Boolean; 
begin
  Result := (Top = Bottom) and (Left = Right);
end;

function TmnwTWBoundingHelper.ToTWString(prefix: string): string;
var
  sb: TStringBuilder;
begin
  Result := '';

  if IsUniform then
  begin
    if Top >= 0 then
      Result := prefix + '-' + Top.ToString;
    Exit;
  end;

  sb := TStringBuilder.Create;
  try
    // Handle Y-axis (Top/Bottom)
    if Top >= 0 then
    begin
      if Top = Bottom then
        sb.Append(prefix).Append('y-').Append(Top.ToString)
      else
      begin
        sb.Append(prefix).Append('t-').Append(Top.ToString);
        if Bottom >= 0 then
        begin
          sb.Append(' ');
          sb.Append(prefix).Append('b-').Append(Bottom.ToString);
        end;
      end;
    end
    else if Bottom >= 0 then
      sb.Append(prefix).Append('b-').Append(Bottom.ToString);

    // Handle X-axis (Left/Right)
    if Left >= 0 then
    begin
      if sb.Length > 0 then
        sb.Append(' ');
      if Left = Right then
        sb.Append(prefix).Append('x-').Append(Left.ToString)
      else
      begin
        sb.Append(prefix).Append('l-').Append(Left.ToString);
        if Right >= 0 then
        begin
          sb.Append(' ');
          sb.Append(prefix).Append('r-').Append(Right.ToString);
        end;
      end;
    end
    else if Right >= 0 then
    begin
      if sb.Length > 0 then
        sb.Append(' ');
      sb.Append(prefix).Append('r-').Append(Right.ToString);
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TTWRenderer.Created;
begin
  inherited;
  Libraries.RegisterLibrary(TJQuery_Library, 1000);
  Libraries.RegisterLibrary(TWebElements_Library, 2000, True);
  Libraries.RegisterLibrary(TTailwind_Library, True);
end;

class destructor TTWRenderer.Destroy;
begin
  FreeAndNil(TW_ElementRenderers);
end;

class function TTWRenderer.ElementRenderers: TmnwElementRenderers;
begin
  if TW_ElementRenderers = nil then
    TW_ElementRenderers := TmnwElementRenderers.Create;
  Result := TW_ElementRenderers;
end;

procedure TTWRenderer.AddHead(const Context: TmnwContext);
begin
  // Tailwind-specific head additions if needed
end;

class constructor TTWRenderer.Register;
begin
  inherited;
  with ElementRenderers do
  begin
    RegisterRenderer(THTML.TDynamicCompose, TDynamicCompose);
    RegisterRenderer(THTML.TIntervalCompose, TIntervalCompose);
    RegisterRenderer(THTML.TFile, TFile);
    RegisterRenderer(THTML.TJSFile, TJSFile);
    RegisterRenderer(THTML.TCSSFile, TCSSFile);

    RegisterRenderer(THTML.TComment, TComment);
    RegisterRenderer(THTML.TDocument, TDocument);
    RegisterRenderer(THTML.TBody, TBody);
    RegisterRenderer(THTML.TParagraph, TParagraph);
    RegisterRenderer(THTML.TBreak, TBreak);
    RegisterRenderer(THTML.TNavTools, TNavTools);
    RegisterRenderer(THTML.TNavDropdown, TNavDropdown);
    RegisterRenderer(THTML.TNavBar, TNavBar);
    RegisterRenderer(THTML.TMenuBar, TMenuBar);
    RegisterRenderer(THTML.THeader, THeader);
    RegisterRenderer(THTML.TContent, TContent);
    RegisterRenderer(THTML.TSideBar, TSideBar);
    RegisterRenderer(THTML.TAccordion, TAccordion);
    RegisterRenderer(THTML.TAccordionSection, TAccordionSection);
    RegisterRenderer(THTML.TAccordionItem, TAccordionItem);
    RegisterRenderer(THTML.TMain, TMain);
    RegisterRenderer(THTML.TFooter, TFooter);
    RegisterRenderer(THTML.TToast, TToast);
    RegisterRenderer(THTML.TLink, TLink);
    RegisterRenderer(THTML.TSpan, TSpan);
    RegisterRenderer(THTML.TButton, TButton);
    RegisterRenderer(THTML.TNavItem, TNavItem);
    RegisterRenderer(THTML.TMenuItem, TMenuItem);
    RegisterRenderer(THTML.TDropdownItem, TDropdownItem);
    RegisterRenderer(THTML.TInput, TInput);
    RegisterRenderer(THTML.TInputPassword, TInputPassword);
    RegisterRenderer(THTML.TImage, TImage);
    RegisterRenderer(THTML.TImageFile, TImageFile);
    RegisterRenderer(THTML.TImageMemory, TImageMemory);
    RegisterRenderer(THTML.TCard, TCard);
    RegisterRenderer(THTML.TDropdown, TDropdown);
    RegisterRenderer(THTML.TGroupButtons, TGroupButtons);
    RegisterRenderer(THTML.TToolbar, TToolbar);
    RegisterRenderer(THTML.TZoomButtons, TZoomButtons);
    RegisterRenderer(THTML.TCollapseCaption, TCollapseCaption);
    RegisterRenderer(THTML.TForm, TForm);
    RegisterRenderer(THTML.TRow, TRow);
    RegisterRenderer(THTML.TColumn, TColumn);
    RegisterRenderer(THTML.TPanel, TPanel);
    RegisterRenderer(THTML.TCode, TCode);
    RegisterRenderer(THTML.TMultilineCode, TMultilineCode);
    RegisterRenderer(THTML.TBar, TBar);

    RegisterRenderer(THTML.THTMLElement, THTMLElement);
    RegisterRenderer(THTML.THTMLComponent, THTMLComponent);
    RegisterRenderer(THTML.THTMLControl, THTMLControl);

    RegisterRenderer(THTML.TThemeModeButton, TThemeModeButton);
  end;
end;

{ TTWRenderer.THTMLElement }

procedure TTWRenderer.THTMLElement.DoEnterRender(Scope: TmnwScope; const Context: TmnwContext);
begin
  if Scope.Element.Comment <> '' then
    Context.Writer.WriteLn('<!-- ' + Scope.Element.Comment + ' -->');
  inherited;
end;

{ TTWRenderer.THTMLComponent }

procedure TTWRenderer.THTMLComponent.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.THTMLComponent;
begin
  e := Scope.Element as THTML.THTMLComponent;
  inherited;
end;

procedure TTWRenderer.THTMLComponent.RenderImageLocation(const Context: TmnwContext; const Image: TImageLocation);
begin
  if Image.IconClass <> '' then
    Context.Writer.AddTag('span', 'class=' + DQ(Image.IconClass))
  else if Image.Path <> '' then
    Context.Writer.AddShortTag('img', 'src=' + DQ(Image.Path) + ' alt=""');
end;

{ TTWRenderer.THTMLControl }

procedure TTWRenderer.THTMLControl.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.THTMLControl;
begin
  e := Scope.Element as THTML.THTMLControl;
  if e.Hint <> '' then
  begin
    Scope.Attributes['title'] := e.Hint;
  end;
  if e.Size > szUndefined then
    Scope.Classes.Add('max-w-' + TWSizeToStr(e.Size));
  case e.Shadow of
    shadowLight: Scope.Classes.Add('shadow-sm');
    ShadowHeavy: Scope.Classes.Add('shadow-lg');
    else;
  end;
  inherited;
end;

{ TTWRenderer.TComment }

procedure TTWRenderer.TComment.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TComment;
begin
  inherited;
  e := Scope.Element as THTML.TComment;
  Context.Writer.AddComment(e.Comment);
end;

{ TTWRenderer.TDocument }

procedure TTWRenderer.TDocument.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TDocument;
begin
  e := Scope.Element as THTML.TDocument;
  if e.Schema.Direction = dirRightToLeft then
    Scope.Attributes['dir'] := 'rtl'
  else if e.Schema.Direction = dirLeftToRight then
    Scope.Attributes['dir'] := 'ltr';
  Scope.Attributes['lang'] := 'en';
end;

procedure TTWRenderer.TDocument.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TDocument;
  aLibrary: TmnwLibrary;
begin
  e := Scope.Element as THTML.TDocument;
  Scope.Attributes.Delete('Name'); //* Not for HTML tag
  Context.Writer.WriteLn('<!DOCTYPE html>');
  Context.Writer.OpenTag('html', Scope.ToString);
  Context.Writer.OpenTag('head');
  Context.Writer.AddTag('title', '', e.Title);
  Context.Writer.AddShortTag('link', 'rel="icon" href="data:,"'); //disable call favicon.ico
  Context.Writer.AddShortTag('meta', 'charset="UTF-8"');
  Context.Writer.AddShortTag('meta', 'name="viewport" content="width=device-width, initial-scale=1"');
  if e.Parent <> nil then // Only root have head
  begin
    AddHead(Scope, Context);
    //* Library Head
    for aLibrary in Renderer.Libraries do
    begin
      if (aLibrary.Usage > 0) and not aLibrary.EndOfBody then
        aLibrary.AddHead(Context);
    end;
    //* Renderer Head
    (Renderer as TTWRenderer).AddHead(Context);
  end;
  Context.Writer.CloseTag('head');
  e.Body.Render(Context, AResponse);
  Context.Writer.CloseTag('html');
end;

{ TTWRenderer.TBody }

procedure TTWRenderer.TBody.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TBody;
begin
  e := Scope.Element as THTML.TBody;
  inherited;
  if e.Schema.RefreshInterval <> 1 then //* not default, 0 Disable it
    Scope.Attributes['data-mnw-refresh-interval'] := e.Schema.RefreshInterval.ToString;
end;

procedure TTWRenderer.TBody.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TBody;
  s: string;
  function GetAttach: string;
  begin
    if Context.Schema.Interactive then
    begin
      Result := ' data-mnw-interactive="true"';
    end;
  end;
var
  aLibrary: TmnwLibrary;
begin
  e := Scope.Element as THTML.TBody;
  Scope.Attributes.Delete('Name'); //* Not for HTML tag

  if e.FontName <> '' then
    s := ' style="font-family: ' + SQ(e.FontName) + '!important;"'
  else
    s := '';

  // Tailwind: dark mode via class strategy if needed
  if e.Theme = themeDark then
    Scope.Classes.Add('dark')
  else if e.Theme = themeLight then
    Scope.Classes.Add('');

  Scope.Classes.Add('bg-white text-gray-900 dark:bg-gray-900 dark:text-gray-100');

  Context.Writer.OpenTag('body', Scope.ToString + GetAttach + s);
  inherited;

  for aLibrary in Renderer.Libraries do
  begin
    if (aLibrary.Usage > 0) and aLibrary.EndOfBody then
      aLibrary.AddHead(Context);
  end;

  Context.Writer.CloseTag('body');
end;

{ TTWRenderer.TFile }

procedure TTWRenderer.TFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TFile;
begin
  e := Scope.Element as THTML.TFile;
  if ftEmbed in e.Options then
    Scope.Element.Respond(Context, AResponse);
  inherited;
end;

{ TTWRenderer.TJSFile }

procedure TTWRenderer.TJSFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TJSFile;
  src: string;
begin
  e := Scope.Element as THTML.TJSFile;
  if ftEmbed in e.Options then
  begin
    Context.Writer.OpenTag('script', 'type="text/javascript"' + Scope.GetText);
    inherited;
    Context.Writer.WriteLn('');
    Context.Writer.CloseTag('script');
  end
  else
  begin
    src := Context.GetPath(e);
    Context.Writer.AddTag('script', 'type="text/javascript"' + When(e.Defer, ' defer') + ' src=' + DQ(src + '?v=' + IntToStr(Context.Schema.Web.TimeStamp)));
    inherited;
  end;
end;

{ TTWRenderer.TCSSFile }

procedure TTWRenderer.TCSSFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCSSFile;
  src: string;
begin
  e := Scope.Element as THTML.TCSSFile;
  if ftEmbed in e.Options then
  begin
    Context.Writer.OpenTag('style', 'type="text/css"' + Scope.GetText);
    inherited;
    Context.Writer.WriteLn();
    Context.Writer.CloseTag('style');
  end
  else
  begin
    src := Context.GetPath(e);
    Context.Writer.AddTag('link', 'rel="stylesheet" href=' + DQ(src + '?v=' + IntToStr(Context.Schema.Web.TimeStamp)));
    inherited;
  end;
end;

{ TTWRenderer.TDynamicCompose }

procedure TTWRenderer.TDynamicCompose.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Context.Writer.OpenTag('div', Scope.Attributes.ToString);
  inherited;
  Scope.Element.Respond(Context, AResponse);
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TIntervalCompose }

procedure TTWRenderer.TIntervalCompose.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
  Scope.Attributes['data-mnw-refresh-url'] := Context.GetPath(Scope.Element);
end;

{ TTWRenderer.THeader }

procedure TTWRenderer.THeader.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Scope.Classes.AddClasses('sticky top-0 z-40 flex items-center bg-gray-900 text-white py-2 px-4 shadow');
  Context.Writer.OpenTag('header', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('header');
end;

{ TTWRenderer.TFooter }

procedure TTWRenderer.TFooter.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TFooter;
begin
  e := Scope.Element as THTML.TFooter;
  Scope.Classes.Add('text-center py-4 text-gray-600 dark:text-gray-400');
  Context.Writer.OpenTag('footer', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('footer');
end;

{ TTWRenderer.TToast }

procedure TTWRenderer.TToast.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TToast;
begin
  e := Scope.Element as THTML.TToast;
  Context.Writer.OpenTag('div', 'aria-live="polite" aria-atomic="true"');
  Context.Writer.OpenTag('div', 'id="toast-container" class="fixed top-4 right-4 z-50 space-y-2"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TContent }

procedure TTWRenderer.TContent.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TContent;
begin
  e := Scope.Element as THTML.TContent;
  if e.Wide then
    Scope.Classes.Add('w-full px-4')
  else
    Scope.Classes.Add('container mx-auto px-4');
  Context.Writer.OpenTag('div', Scope.ToString);
  Context.Writer.OpenTag('div', 'id="content" class="flex flex-wrap"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TMain }

procedure TTWRenderer.TMain.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TMain;
  classes: TElementClasses;
begin
  e := Scope.Element as THTML.TMain;
  classes.Init('flex-1');
  if (e.Schema as THTML).Document.Body.Header.CanRender then
    classes.Add('min-h-[calc(100vh-4rem)]');
  if (e.Parent.Parent as THTML.TBody).SideBar.CanRender then
    classes.Add('md:flex-1');
  classes.Add('p-0');
  classes.Add('m-0');
  Context.Writer.OpenTag('main', classes.ToString);

  Scope.Classes.Add('main-content');
  if e.Gap > 0 then
    Scope.Classes.Add('space-y-' + e.Gap.ToString);

  Scope.Classes.Add('flex');
  Scope.Classes.Add('flex-col');
  Scope.Classes.Add('justify-center');

  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');

  Context.Writer.CloseTag('main');
end;

{ TTWRenderer.TCard }

procedure TTWRenderer.TCard.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCard;
begin
  e := Scope.Element as THTML.TCard;
  Scope.Classes.Add('bg-white dark:bg-gray-800 shadow rounded-lg overflow-hidden border border-gray-200 dark:border-gray-700');

  Context.Writer.OpenTag('div', Scope.ToString);
  if e.Caption <> '' then
  begin
    Context.Writer.OpenTag('div', 'id="' + e.id + '-header" class="px-4 py-3 border-b border-gray-200 dark:border-gray-700 flex items-center justify-between bg-gray-50 dark:bg-gray-900"');
    Context.Writer.WriteLn(e.Caption);
    if e.Collapse then
    begin
      Context.Writer.Write('<button class="text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-200 focus:outline-none"');
      Context.Writer.Write(' onclick="const el=document.getElementById(''' + e.id + '-body''); el.classList.toggle(''hidden'');"');
      Context.Writer.WriteLn('>');
      Context.Writer.WriteLn('<svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"></path></svg>');
      Context.Writer.WriteLn('</button>');
    end;
    Context.Writer.CloseTag('div');
  end;

  Context.Writer.OpenTag('div', 'id="' + e.id + '-body" class="px-4 py-4"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TPanel }

procedure TTWRenderer.TPanel.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TPanel;
begin
  e := Scope.Element as THTML.TPanel;
  Scope.Classes.Add('bg-white dark:bg-gray-800 shadow rounded-lg border border-gray-200 dark:border-gray-700');
  Context.Writer.OpenTag('div', Scope.ToString);
  if e.Caption <> '' then
    Context.Writer.AddTag('div', 'class="px-4 py-2 border-b border-gray-200 dark:border-gray-700 bg-gray-50 dark:bg-gray-900 font-medium"', e.Caption);

  Scope.Classes.Add('p-4');
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TCollapseCaption }

procedure TTWRenderer.TCollapseCaption.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCollapseCaption;
begin
  e := Scope.Element as THTML.TCollapseCaption;
  Context.Writer.OpenTag('div', 'class="cursor-pointer" onclick="const el=document.getElementById(''' + e.ID + '-text''); el.classList.toggle(''hidden'');"');
  if e.Caption <> '' then
    Context.Writer.WriteLn(e.Caption);
  Context.Writer.CloseTag('div');
  Context.Writer.OpenTag('div', 'id="' + e.ID + '-text" class="hidden"');
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TThemeModeButton }

procedure TTWRenderer.TThemeModeButton.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TThemeModeButton;
begin
  e := Scope.Element as THTML.TThemeModeButton;
  Context.Writer.OpenTag('button', 'class="bg-transparent mx-0 py-0 px-1 border-0 cursor-pointer" type="button" aria-label="Toggle navigation" onclick="mnw.switch_theme(this, event)"');
  Context.Writer.AddTag('span', 'class="icon mw-theme"');
  inherited;
  Context.Writer.CloseTag('button');
end;

{ TTWRenderer.TForm }

procedure TTWRenderer.TForm.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  Scope.Classes.Add('block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 sm:text-sm');
  inherited;
end;

procedure TTWRenderer.TForm.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TForm.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TForm;
  aPostTo: string;
begin
  e := Scope.Element as THTML.TForm;
  Context.Writer.OpenTag('form', 'method="post"' + NV('action', Context.GetLocationPath(e.PostTo)) + ' enctype="multipart/form-data"' + Scope.GetText);
  inherited;
  if e.RedirectTo <> '' then
    Context.Writer.AddShortTag('input', 'type="hidden" name="redirect" value="' + e.RedirectTo + '"');
  Context.Writer.AddShortTag('input', 'type="hidden" name="execute" value="true"');
  Context.Writer.CloseTag('form');

  if e.Submit.Caption <> '' then
    Context.Writer.AddTag('button', 'class="inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-green-600 hover:bg-green-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-green-500" type="submit" form="' + e.ID + '" value="Submit"', e.Submit.Caption);
  if e.Cancel.Caption <> '' then
    Context.Writer.AddTag('button', 'class="inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500" type="cancel" form="' + e.ID + '" value="Cancel"', e.Cancel.Caption);
  if e.Reset.Caption <> '' then
    Context.Writer.AddTag('button', 'class="inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500" type="reset" form="' + e.ID + '" value="Reset"', e.Reset.Caption);
end;

{ TTWRenderer.TParagraph }

procedure TTWRenderer.TParagraph.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TParagraph;
begin
  e := Scope.Element as THTML.TParagraph;
  Context.Writer.OpenInlineTag('p', Scope.ToString);
  if e.Text <> '' then
    Context.Writer.Write(e.Text);
  inherited;
  Context.Writer.CloseTag('p');
end;

{ TTWRenderer.TBreak }

procedure TTWRenderer.TBreak.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Context.Writer.AddShortTag('br');
end;

{ TTWRenderer.TButton }

procedure TTWRenderer.TButton.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TButton;
  event: string;
  StyleClasses: string;
begin
  e := Scope.Element as THTML.TButton;
  StyleClasses := 'inline-flex items-center justify-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm focus:outline-none focus:ring-2 focus:ring-offset-2';

  case e.ControlStyle of
    stylePrimary: StyleClasses := StyleClasses + ' text-white bg-blue-600 hover:bg-blue-700 focus:ring-blue-500';
    styleSecondary: StyleClasses := StyleClasses + ' text-white bg-gray-600 hover:bg-gray-700 focus:ring-gray-500';
    styleSuccess: StyleClasses := StyleClasses + ' text-white bg-green-600 hover:bg-green-700 focus:ring-green-500';
    styleDanger: StyleClasses := StyleClasses + ' text-white bg-red-600 hover:bg-red-700 focus:ring-red-500';
    styleWarning: StyleClasses := StyleClasses + ' text-white bg-yellow-600 hover:bg-yellow-700 focus:ring-yellow-500';
    styleInfo: StyleClasses := StyleClasses + ' text-white bg-cyan-600 hover:bg-cyan-700 focus:ring-cyan-500';
    styleLight: StyleClasses := StyleClasses + ' text-gray-700 bg-gray-100 hover:bg-gray-200 focus:ring-gray-500';
    styleDark: StyleClasses := StyleClasses + ' text-white bg-gray-800 hover:bg-gray-900 focus:ring-gray-500';
    styleLink: StyleClasses := StyleClasses + ' text-blue-600 bg-transparent hover:bg-blue-50 focus:ring-blue-500';
  else
    StyleClasses := StyleClasses + ' text-white bg-blue-600 hover:bg-blue-700 focus:ring-blue-500';
  end;

  Scope.Classes.Add(StyleClasses);

  if e.JSFunction <> '' then
    event := ' onclick="' + e.JSFunction + '(this, event)"'
  else if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', ' + SQ('click') + ')"';
  Context.Writer.OpenTag('button', 'type="button"' + event + Scope.GetText);
  inherited;
  Context.Writer.CloseTag('button');
end;

{ TTWRenderer.TNavItem }

procedure TTWRenderer.TNavItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavItem;
  event: string;
begin
  e := Scope.Element as THTML.TNavItem;
  if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', ' + SQ('click') + ')"';
  Scope.Classes.Add('text-gray-300 hover:text-white hover:bg-gray-700 px-3 py-2 rounded-md text-sm font-medium transition-colors');
  Context.Writer.AddTag('a', 'href="' + When(e.LinkTo, '#') + '"' + event + Scope.GetText, e.Caption);
  inherited;
end;

{ TTWRenderer.TMenuItem }

procedure TTWRenderer.TMenuItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TMenuItem;
  event: string;
begin
  e := Scope.Element as THTML.TMenuItem;
  if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', ' + SQ('click') + ')"';
  Scope.Classes.Add('block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 dark:text-gray-200 dark:hover:bg-gray-700');
  Context.Writer.AddTag('button', 'role="menuitem" type="button"' + event + Scope.GetText, e.Caption);
  inherited;
end;

{ TTWRenderer.TSubMenu }

procedure TTWRenderer.TSubMenu.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TSubMenu.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
end;

{ TTWRenderer.TInput }

procedure TTWRenderer.TInput.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  Scope.Attributes['placeholder'] := (Scope.Element as THTML.TInput).PlaceHolder;
  Scope.Attributes['type'] := (Scope.Element as THTML.TInput).EditType;
  inherited;
end;

procedure TTWRenderer.TInput.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TInput;
  event: string;
  isFormChild: Boolean;
begin
  e := Scope.Element as THTML.TInput;
  isFormChild := True;
  if isFormChild then
    Scope.Classes.Add('block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 sm:text-sm dark:bg-gray-800 dark:border-gray-600 dark:text-white');

  if e.Caption <> '' then
    Context.Writer.AddTag('label', When(isFormChild, 'class="block text-sm font-medium text-gray-700 dark:text-gray-300"') + ' for="' + e.ID + '"', e.Caption);

  if Context.Schema.Interactive then
    event := ' onchange="mnw.send(' + SQ(e.ID) + ', ' + SQ('change') + ', ' + 'this.value' + ')"';

  Context.Writer.AddShortTag('input', event + When(e.Required, ' required') + Scope.GetText);
  if e.HelpText <> '' then
    Context.Writer.AddTag('p', 'class="mt-1 text-sm text-gray-500 dark:text-gray-400"', e.HelpText);
  inherited;
end;

{ TTWRenderer.TImage }

procedure TTWRenderer.TImage.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  Scope.Attributes['src'] := (Scope.Element as THTML.TImage).Source;
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImage).AltText; //* always set
  inherited;
end;

procedure TTWRenderer.TImage.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Context.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TTWRenderer.TImageMemory }

procedure TTWRenderer.TImageMemory.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  Scope.Attributes['src'] := Context.GetPath(Scope.Element);
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImageMemory).AltText;
  inherited;
end;

procedure TTWRenderer.TImageMemory.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TImageMemory;
begin
  e := Scope.Element as THTML.TImageMemory;
  Context.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TTWRenderer.TImageFile }

procedure TTWRenderer.TImageFile.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  Scope.Attributes['src'] := Context.GetPath(Scope.Element);
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImageFile).AltText;
  inherited;
end;

procedure TTWRenderer.TImageFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TImageFile;
begin
  e := Scope.Element as THTML.TImageFile;
  Context.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TTWRenderer.TRow }

procedure TTWRenderer.TRow.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TRow;
begin
  e := Scope.Element as THTML.TRow;
  Scope.Classes.Add('flex flex-wrap');
  Scope.Classes.Add(TWContentJustifyToStr(e.ContentAlign, False));
  if e.Fixed <> fixedDefault then
    Scope.Classes.Add(TWFixedToStr(e.Fixed, False));
  if e.Align <> alignDefault then
    Scope.Classes.Add(TWAlignToStr(e.Align, False));
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TColumn }

procedure TTWRenderer.TColumn.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  if e.Size > 0 then
    Scope.Classes.Add('w-full md:w-' + IntToStr(e.Size) + '/12')
  else
    Scope.Classes.Add('flex-1');
  if e.Fixed <> fixedDefault then
    Scope.Classes.Add(TWFixedToStr(e.Fixed, False));
  if e.Align <> alignDefault then
    Scope.Classes.Add(TWAlignToStr(e.Align, False));
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TBar }

procedure TTWRenderer.TBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TBar;
begin
  e := Scope.Element as THTML.TBar;
  Scope.Classes.Add('flex items-center bg-gray-50 dark:bg-gray-800 border-b border-gray-200 dark:border-gray-700');
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TAccordion }

procedure TTWRenderer.TAccordion.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TAccordion.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TAccordion.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Scope.Classes.Add('border border-gray-200 dark:border-gray-700 rounded-md divide-y divide-gray-200 dark:divide-gray-700');
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TAccordionSection }

procedure TTWRenderer.TAccordionSection.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TAccordionSection.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TAccordionSection.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TAccordionSection;
begin
  e := Scope.Element as THTML.TAccordionSection;

  // Use native details/summary for accordion behavior without JS
  Context.Writer.OpenTag('details', 'class="group"' + When(e.SaveState, ' data-mnw-save-state="1"'));

  Context.Writer.OpenTag('summary', 'class="flex items-center justify-between px-4 py-3 cursor-pointer bg-gray-50 dark:bg-gray-800 hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors list-none"');

  Context.Writer.OpenTag('span', 'class="flex items-center gap-2 font-medium text-gray-900 dark:text-white"');
  if e.Image.IconClass <> '' then
    Context.Writer.AddTag('span', 'class=' + DQ(e.Image.IconClass))
  else if e.Image.Path <> '' then
    Context.Writer.AddShortTag('img', 'src=' + DQ(e.Image.Path) + ' alt="" class="w-5 h-5"');
  if e.Caption <> '' then
    Context.Writer.WriteLn(e.Caption);
  Context.Writer.CloseTag('span');

  // Chevron icon
  Context.Writer.WriteLn('<svg class="w-5 h-5 text-gray-500 transition-transform group-open:rotate-180" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"></path></svg>');

  Context.Writer.CloseTag('summary');

  Scope.Classes.Add('px-4 py-3 bg-white dark:bg-gray-900');
  if e.SaveState then
    Scope.Attributes.Add('data-mnw-section', e.ID);
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');

  Context.Writer.CloseTag('details');
end;

{ TTWRenderer.TAccordionItem }

procedure TTWRenderer.TAccordionItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
end;

{ TTWRenderer.TDropdown }

procedure TTWRenderer.TDropdown.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 dark:text-gray-200 dark:hover:bg-gray-700');
end;

procedure TTWRenderer.TDropdown.DoEnterRender(Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TDropdown.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TDropdown;
  classes: string;
begin
  e := Scope.Element as THTML.TDropdown;

  Scope.Classes.Add('inline-flex items-center justify-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm focus:outline-none focus:ring-2 focus:ring-offset-2 text-white bg-blue-600 hover:bg-blue-700 focus:ring-blue-500 cursor-pointer');
  if dropArraw in e.Options then
    Scope.Classes.Add('gap-2');

  Context.Writer.OpenTag('div', 'class="relative inline-block text-left group"');

  // Button
  Context.Writer.OpenTag('button', 'type="button" id="' + e.ID + '-button" aria-expanded="true" aria-haspopup="true"' + Scope.ToString);
  RenderImageLocation(Context, e.Image);
  if e.Caption <> '' then
    Context.Writer.WriteLn(e.Caption);
  if dropArraw in e.Options then
    Context.Writer.WriteLn('<svg class="-mr-1 ml-2 h-5 w-5" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z" clip-rule="evenodd" /></svg>');
  Context.Writer.CloseTag('button');

  // Dropdown menu - using group-hover for simple CSS-only dropdown
  classes := 'absolute right-0 z-10 mt-2 w-56 origin-top-right rounded-md bg-white dark:bg-gray-800 shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none';
  if dropEnd in e.Options then
    classes := classes + ' right-0'
  else
    classes := classes + ' left-0';

  Context.Writer.OpenTag('div', 'class="' + classes + ' hidden group-hover:block" role="menu" aria-orientation="vertical" aria-labelledby="' + e.ID + '-button"');
  Context.Writer.OpenTag('div', 'class="py-1" role="none"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');

  Context.Writer.CloseTag('div');
end;

procedure TTWRenderer.TDropdown.DoLeaveRender(Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

{ TTWRenderer.TDropdownItem }

procedure TTWRenderer.TDropdownItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TDropdownItem;
begin
  e := Scope.Element as THTML.TDropdownItem;
  if e.Caption = '-' then
  begin
    Scope.Classes.Remove('block w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 dark:text-gray-200 dark:hover:bg-gray-700');
    Scope.Classes.Add('border-t border-gray-100 dark:border-gray-700 my-1');
    Context.Writer.AddTag('div', Scope.ToString);
  end
  else
  begin
    inherited;
  end;
end;

{ TTWRenderer.TGroupButtons }

procedure TTWRenderer.TGroupButtons.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TGroupButtons;
begin
  e := Scope.Element as THTML.TGroupButtons;
  Scope.Classes.Add('inline-flex rounded-md shadow-sm');
  Scope.Attributes.Add('role', 'group');
  Scope.Attributes.Add('aria-label', e.ID);
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TToolbar }

procedure TTWRenderer.TToolbar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TToolbar;
begin
  e := Scope.Element as THTML.TToolbar;
  Scope.Classes.Add('flex items-center gap-2');
  Scope.Attributes.Add('role', 'toolbar');
  Scope.Attributes.Add('aria-label', e.ID);
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TZoomButtons }

procedure TTWRenderer.TZoomButtons.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
end;

{ TTWRenderer.TLink }

procedure TTWRenderer.TLink.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TLink;
  s: string;
begin
  e := Scope.Element as THTML.TLink;
  if e.ClickType = clickAction then
    s := ' onclick="mnw.click(this, event)"'
  else if e.ClickType = clickNewWindow then
    s := ' target="_blank"';
  if not e.NoDecoration then
    Scope.Classes.Add('text-blue-600 hover:text-blue-800 hover:underline');
  Context.Writer.OpenInlineTag('a', 'href="' + When(e.Location, '#') + '"' + s + Scope.GetText, e.Caption);
  inherited;
  Context.Writer.CloseTag('a');
end;

{ TTWRenderer.TSpan }

procedure TTWRenderer.TSpan.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TSpan;
begin
  e := Scope.Element as THTML.TSpan;
  Context.Writer.OpenInlineTag('span', Scope.ToString, e.Text);
  inherited;
  Context.Writer.CloseTag('span');
end;

{ TTWRenderer.THTMLLayout }

procedure TTWRenderer.THTMLLayout.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.THTMLLayout;
  MarginPrefix, PaddingPrefix: string;
begin
  e := Scope.Element as THTML.THTMLLayout;
  inherited;
  Scope.Classes.Add(TWFixedToStr(e.Fixed, False));
  Scope.Classes.Add(TWAlignToStr(e.Align, False));
  Scope.Classes.Add(TWAlignItemsToStr(e.AlignItems, False));
  Scope.Classes.Add(TWContentJustifyToStr(e.JustifyItems, False));
  if e.Solitary then
    Scope.Classes.Add('mx-auto');

  if e.Medium then
  begin
    MarginPrefix := 'md:m';
    PaddingPrefix := 'md:p';
  end
  else
  begin
    MarginPrefix := 'm';
    PaddingPrefix := 'p';
  end;

  Scope.Classes.Add(e.Margin.ToTWString(MarginPrefix));
  Scope.Classes.Add(e.Padding.ToTWString(PaddingPrefix));
end;

{ TTWRenderer.THTMLItem }

procedure TTWRenderer.THTMLItem.DoEnterRender(Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.THTMLItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.THTMLItem;
begin
  e := Scope.Element as THTML.THTMLItem;
  RenderImageLocation(Context, e.Image);
  inherited;
  if e.Caption <> '' then
  begin
    if e.AutoHideText then
      Context.Writer.AddInlineTag('span', 'class="hidden md:inline"', e.Caption)
    else
      Context.Writer.WriteLn(e.Caption);
  end;
end;

{ TTWRenderer.TNavBar }

procedure TTWRenderer.TNavBar.DoRenderBrand(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavBar;
begin
  e := Scope.Element as THTML.TNavBar;
  Context.Writer.OpenTag('a', 'class="flex items-center gap-2 text-xl font-bold text-white hover:text-gray-200" href="' + Context.GetPath(e) + '"');
  e.Image.Render(Context, AResponse);
  if e.Title <> '' then
    Context.Writer.WriteLn(e.Title);
  Context.Writer.CloseTag('a');
end;

procedure TTWRenderer.TNavBar.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TNavBar.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TNavBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavBar;
  sb: THTML.TSideBar;
begin
  e := Scope.Element as THTML.TNavBar;
  Scope.Classes.Add('bg-gray-800');
  if e.Fixed = fixedTop then
    Scope.Classes.Add('fixed top-0 left-0 right-0 z-50');
  Scope.Classes.AddClasses('flex items-center justify-between py-2 px-4');

  Context.Writer.OpenTag('nav', Scope.ToString);

  if (e.Schema as THTML).Document.Body.SideBar.CanRender then
  begin
    sb := (e.Schema as THTML).Document.Body.SideBar;
    Context.Writer.OpenTag('button', 'class="md:hidden text-gray-300 hover:text-white p-1 rounded-md" type="button" onclick="document.getElementById(''' + sb.id + '-body'').classList.toggle(''hidden'');"');
    Context.Writer.WriteLn('<svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"/></svg>');
    Context.Writer.CloseTag('button');
  end;

  DoRenderBrand(Scope, Context, AResponse);

  Context.Writer.OpenTag('div', 'class="hidden md:flex items-center space-x-1" id="' + e.id + '-items' + '"');
  inherited;
  Context.Writer.CloseTag('div');

  if e.Tools.Count > 0 then
    e.Tools.Render(Context, AResponse);

  Context.Writer.CloseTag('nav');
end;

{ TTWRenderer.TMenuBar }

procedure TTWRenderer.TMenuBar.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TMenuBar.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TMenuBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
end;

{ TTWRenderer.TNavTools }

procedure TTWRenderer.TNavTools.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TNavTools.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TTWRenderer.TNavTools.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavTools;
begin
  e := Scope.Element as THTML.TNavTools;
  Scope.Classes.Add('flex items-center gap-2 ml-auto');
  inherited;
end;

procedure TTWRenderer.TNavTools.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

{ TTWRenderer.TNavDropdown }

procedure TTWRenderer.TNavDropdown.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
  Scope.WrapClasses.Add('relative');
end;

procedure TTWRenderer.TNavDropdown.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  Context.Writer.OpenTag('div', 'class="block px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 dark:text-gray-200 dark:hover:bg-gray-700"');
  inherited;
end;

procedure TTWRenderer.TNavDropdown.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavDropdown;
  classes: TElementClasses;
begin
  e := Scope.Element as THTML.TNavDropdown;
  Scope.Classes.Add('text-gray-300 hover:text-white hover:bg-gray-700 px-3 py-2 rounded-md text-sm font-medium transition-colors inline-flex items-center gap-1');
  if dropArraw in e.Options then
    Scope.Classes.Add('gap-2');

  Context.Writer.OpenTag('div', 'class="relative group"');

  Context.Writer.OpenTag('button', 'type="button" class="' + Scope.Classes.ToString + '"');
  RenderImageLocation(Context, e.Image);
  if e.Caption <> '' then
    Context.Writer.WriteLn(e.Caption);
  if dropArraw in e.Options then
    Context.Writer.WriteLn('<svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"></path></svg>');
  Context.Writer.CloseTag('button');

  classes.Init('absolute z-10 mt-1 w-48 rounded-md shadow-lg bg-white dark:bg-gray-800 ring-1 ring-black ring-opacity-5 hidden group-hover:block');
  if dropEnd in e.Options then
    classes.Add('right-0')
  else
    classes.Add('left-0');

  Context.Writer.OpenTag('div', classes.ToString);
  Context.Writer.OpenTag('div', 'class="py-1" role="menu"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');

  Context.Writer.CloseTag('div');
end;

procedure TTWRenderer.TNavDropdown.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TTWRenderer.TSideBar }

procedure TTWRenderer.TSideBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TSideBar;
begin
  e := Scope.Element as THTML.TSideBar;
  Scope.Classes.Add('w-64 bg-gray-50 dark:bg-gray-900 border-r border-gray-200 dark:border-gray-700 flex-shrink-0');
  if (e.Schema as THTML).Document.Body.Header.CanRender then
    Scope.Classes.Add('min-h-[calc(100vh-4rem)]');
  Scope.Classes.Add('p-0');

  Context.Writer.OpenTag('aside', Scope.ToString);
  Context.Writer.OpenTag('div', 'id="' + e.ID + '-body" class="sidebar-body"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('aside');
end;

{ TTWRenderer.TCode }

procedure TTWRenderer.TCode.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCode;
begin
  e := Scope.Element as THTML.TCode;
  Scope.Classes.Add('font-mono text-sm bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded');
  Context.Writer.OpenTag('code', Scope.ToString, e.Text);
  inherited;
  Context.Writer.CloseTag('code');
end;

{ TTWRenderer.TMultilineCode }

procedure TTWRenderer.TMultilineCode.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCode;
begin
  e := Scope.Element as THTML.TCode;
  Scope.Classes.Add('block font-mono text-sm bg-gray-100 dark:bg-gray-800 p-4 rounded overflow-auto');
  Context.Writer.OpenTag('pre', Scope.ToString);
  Context.Writer.OpenTag('code', '', e.Text);
  Context.Writer.CloseTag('code');
  Context.Writer.CloseTag('pre');
end;

{ TTailwind_Library }

procedure TTailwind_Library.AddHead(const Context: TmnwContext);
begin
  inherited;
  //Context.Writer.AddHTMLScript('https://cdn.tailwindcss.com', '');
end;

procedure TTailwind_Library.Created;
begin
  inherited;
  //Sources.Add('https://cdn.tailwindcss.com/3.4.17', '');
  Sources.Add(stScript, 'https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4', '');
end;

initialization
  Renderers.RegisterRenderer('Tailwind', TTWRenderer);
finalization
end.
