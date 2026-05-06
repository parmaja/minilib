unit mnBootstraps;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of mod://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>

  https://fastbootstrap.com/components/accordion/
  https://fastbootstrap.com/components/layout/
  https://freefrontend.com/bootstrap-sidebars/

  https://dev.to/codeply/bootstrap-5-sidebar-examples-38pb

  https://bootswatch.com/darkly/
 *}

{$M+}
{$H+}                                                                                                            

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, syncobjs, StrUtils, //NetEncoding, Hash,
  DateUtils,
  mnTypes, mnUtils, mnDON, mnSockets, mnServers, mnStreams, mnStreamUtils,
  mnFields, mnParams, mnMultipartData, mnModules, mnWebModules, mnWebElements;

type
  { TBSRenderer }

  TBSRenderer = class(TmnwHTMLRenderer)
  protected
    class var BS_ElementRenderers: TmnwElementRenderers;
    procedure Created; override;
  public
    class function ElementRenderers: TmnwElementRenderers; override;
    class procedure RegisterElements; override;    
    class constructor Register; 
    class destructor Destroy;      
  public
  type
  
      THTMLLayout = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
      public
      end;

      { THTMLComponent }

      THTMLComponent = class(THTMLLayout)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { THTMLControl }

      THTMLControl = class(THTMLComponent)
      protected
        procedure RenderImageLocation(const Context: TmnwContext; const Image: TImageLocation);
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
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
      
      { TDocument }

      TDocument = class(TmnwHTMLRenderer.TDocument)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TBody }

      TBody = class(TmnwHTMLRenderer.TBody)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
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
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
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
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
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
  public
    procedure AddHead(const Context: TmnwContext); override;
  end;

  TBootstrap_Library = class(TmnwLibrary)
  protected
    procedure Created; override;     
  public  
  end;

  TBootstrapIcons_Library = class(TmnwLibrary)
  protected
    procedure Created; override;     
  public
  end;

  TmnwBSBoundingHelper = record helper for TmnwBounding
  public
    function IsUniform: Boolean; inline;
    function IsUniformSides: Boolean; inline;
    function ToBSString(prefix: string): string; {$ifndef DEBUG}inline;{$endif}
  end;

function BSAlignToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;
function BSContentJustifyToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;
function BSAlignItemsToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;

function BSFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = True): string;
function BSSizeToStr(Size: TSize; WithSpace: Boolean = True): string;
function BSItemStyleToStr(const Prefix: string; Style: TItemStyle; WithSpace: Boolean = True): string;

implementation

function BSCustomAlignToStr(const s: string; Align: TmnwAlign; WithSpace: Boolean): string; inline;
const
  AlignSuffixes: array[TmnwAlign] of string = ('', 'start', 'center', 'streach', 'baseline', 'end');
begin
  if (Align >= alignStart) and (Align <= alignEnd) then
    Result := s + AlignSuffixes[Align]
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSAlignToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  Result := BSCustomAlignToStr('align-self', Align, WithSpace);
end;

function BSContentJustifyToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  Result := BSCustomAlignToStr('justify-content', Align, WithSpace);
end;

function BSAlignItemsToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  Result := BSCustomAlignToStr('align-items-', Align, WithSpace);
end;

function BSFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = True): string;
const
  FixedStrs: array[TmnwFixed] of string = ('', 'fixed-top', 'fixed-bottom', 'fixed-start', 'fixed-end',
    'sticky-top', 'sticky-bottom', 'sticky-start', 'sticky-end');
begin
  Result := FixedStrs[Fixed];
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSSizeToStr(Size: TSize; WithSpace: Boolean = True): string;
const
  SizeStrs: array[TSize] of string = ('', 'xs', 'sm', 'md', 'lg', 'xl', 'parent', 'content');
begin
  Result := SizeStrs[Size];
  if WithSpace and (Result <> '') then
    Result := ' ' + Result
  else if not WithSpace then
    Result := Result;
end;

function BSItemStyleToStr(const Prefix: string; Style: TItemStyle; WithSpace: Boolean): string;
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

function TmnwBSBoundingHelper.IsUniform: Boolean; 
begin
  Result := (Top = Left) and (Top = Bottom) and (Top = Right);
end;

function TmnwBSBoundingHelper.IsUniformSides: Boolean; 
begin
  Result := (Top = Bottom) and (Left = Right);
end;

function TmnwBSBoundingHelper.ToBSString(prefix: string): string;
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
        sb.Append(prefix).Append('s-').Append(Left.ToString);
        if Right >= 0 then
        begin
          sb.Append(' ');
          sb.Append(prefix).Append('e-').Append(Right.ToString);
        end;
      end;
    end
    else if Right >= 0 then
    begin
      if sb.Length > 0 then
        sb.Append(' ');
      sb.Append(prefix).Append('e-').Append(Right.ToString);
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TBSRenderer.Created;
begin
  inherited;  
  Libraries.Use(TBootstrap_Library);
  Libraries.Use(TBootstrapIcons_Library);
end;

class destructor TBSRenderer.Destroy;
begin
  FreeAndNil(BS_ElementRenderers);
end;

class function TBSRenderer.ElementRenderers: TmnwElementRenderers;
begin
  if BS_ElementRenderers = nil then
    BS_ElementRenderers:= TmnwElementRenderers.Create;
  Result := BS_ElementRenderers;
end;

procedure TBSRenderer.AddHead(const Context: TmnwContext);
begin
(*  Context.Writer.WriteLn('<style type="text/css">', [woOpenIndent]);
  Context.Writer.WriteLn('.small-card {');
  Context.Writer.WriteLn('    max-width: 22rem;');
  Context.Writer.WriteLn('}');
  Context.Writer.WriteLn('</style>', [woCloseIndent]); *)
end;

class constructor TBSRenderer.Register;
begin
  inherited;  
  RegisterElements;    
end;

class procedure TBSRenderer.RegisterElements;
begin
  inherited;
  with ElementRenderers do
  begin 
    RegisterRenderer(THTML.TDocument ,TDocument, True);
    RegisterRenderer(THTML.TBody ,TBody, True);
    
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

    RegisterRenderer(THTML.THTMLComponent, THTMLComponent);
    RegisterRenderer(THTML.THTMLControl, THTMLControl);

    RegisterRenderer(THTML.TThemeModeButton, TThemeModeButton);
  end;
end;

{ TBSRenderer.THTMLComponent }

procedure TBSRenderer.THTMLComponent.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.THTMLComponent;
begin
  e := Scope.Element as THTML.THTMLComponent;
  inherited;
end;

{ TBSRenderer.THTMLControl }

procedure TBSRenderer.THTMLControl.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.THTMLControl;
begin
  e := Scope.Element as THTML.THTMLControl;
  if e.Hint <> '' then
  begin
    Scope.Attributes['data-bs-toggle'] := 'tooltip';
    Scope.Attributes['data-bs-placement'] := 'top';
    Scope.Attributes['title'] := e.Hint;
  end;
  if e.Size > szUndefined then
    Scope.Classes.Add('max-w-'+BSSizeToStr(e.Size));
  case e.Shadow of
    shadowLight: Scope.Classes.Add('shadow-sm');
    ShadowHeavy: Scope.Classes.Add('shadow-thin');
    else ;
  end;
  inherited;
end;

procedure TBSRenderer.THTMLControl.RenderImageLocation(const Context: TmnwContext; const Image: TImageLocation);
begin
  if Image.Location = imgSymbol then
    Context.Writer.AddTag('span', 'class='+ DQ(Image.Symbol))
  else if Image.Location = imgPath then
    Context.Writer.AddShortTag('img', 'src='+ DQ(Image.Path) + ' alt=""')
  else if Image.Location = imgMemory then
  begin
{    if Route <> '' then    
      Context.Writer.AddShortTag('img', 'src='+ DQ(Image.Path) + ' alt=""');}
  end;
end;

{ TBSRenderer.TDocumentHTML }

procedure TBSRenderer.TDocument.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
end;

{ TBSRenderer.THeaderHTML }

procedure TBSRenderer.THeader.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Scope.Classes.Append('header sticky-top d-flex align-items-center navbar-dark bg-black py-0 px-1');
  Scope.Attributes.Add('data-bs-theme', 'dark'); //Needed because Header is always darktheme some items/icons not detected it
  Context.Writer.OpenTag('header', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('header');
end;

{ TBSRenderer.TFooterHTML }

procedure TBSRenderer.TFooter.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TFooter;
begin
  e := Scope.Element as THTML.TFooter;
  Context.Writer.OpenTag('footer', 'class="text-center"');
  inherited;
  Context.Writer.CloseTag('footer');
end;

{ TBSRenderer.TToast }

procedure TBSRenderer.TToast.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TToast;
begin
  e := Scope.Element as THTML.TToast;
  Context.Writer.OpenTag('div', 'aria-live="polite" aria-atomic="true"');
  Context.Writer.OpenTag('div', 'id="toast-container" class="toast-container position-fixed bottom-0 end-0 p-2" style="z-index:1056;"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TContent }

procedure TBSRenderer.TContent.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TContent;
begin
  e := Scope.Element as THTML.TContent;
  if e.Wide then
    Scope.Classes.Add('container-fluid')
  else
    Scope.Classes.Add('container');
  Context.Writer.OpenTag('div', Scope.ToString);
  Context.Writer.OpenTag('div', 'id="content" class="content row"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TMainHTML }

procedure TBSRenderer.TMain.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TMain;
  classes: TElementClasses;
begin
  e := Scope.Element as THTML.TMain;
  //Context.Writer.OpenTag('div', 'class="row"');
  classes.Init('main');
  if (e.Schema as THTML).Document.Body.Header.CanRender  then
    classes.Add('max-content-height');
  if (e.Parent.Parent as THTML.TBody).SideBar.CanRender then
    classes.Add('col-md');
  classes.Add('p-0');
  classes.Add('m-0');
  Context.Writer.OpenTag('main', classes.ToString);

  Scope.Classes.Add('main-content');
  if e.Gap > 0 then
    //Scope.Classes.Add('gap-' + e.Gap.ToString);
    Scope.Classes.Add('m-childs-' + e.Gap.ToString);

  //Scope.Classes.Add('d-flex');
  //Scope.Classes.Add('flex-column');

  //Scope.Classes.Add('flex-wrap');
  Scope.Classes.Add('justify-content-center');
//container-fluid for full width, container not full width
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');

  Context.Writer.CloseTag('main');
end;

{ TBSRenderer.TCardHTML }

//https://disjfa.github.io/bootstrap-tricks/card-collapse-tricks/
//https://bootstrapbrain.com/tutorial/bootstrap-accordion-with-plus-minus-icon/

procedure TBSRenderer.TCard.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCard;
begin
  e := Scope.Element as THTML.TCard;
  Scope.Classes.Add('card');

  Context.Writer.OpenTag('div', Scope.ToString);
  if e.Caption <> '' then
  begin
    Context.Writer.OpenTag('h5', 'id="' + e.id + '-header" class="card-header d-flex"');
    Context.Writer.WriteLn(e.Caption);
    if e.Collapse then
    begin
      Context.Writer.Write('<span class="ms-auto my-auto icon-animate icon mnw-chevron-up"');
      Context.Writer.Write(' role="button" data-bs-toggle="collapse" data-bs-target="#'+e.id+'-body" aria-labelledby="' + e.id + '-header" aria-expanded="true" aria-controls="'+e.id+'-body"');
      Context.Writer.WriteLn('></span>');
    end;
    Context.Writer.CloseTag('h5');
  end;

  Context.Writer.OpenTag('div', 'id="'+e.id+'-body" class="card-body overflow-hidden collapse show" aria-labelledby="'+e.id+'-header"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TFormHTML }

procedure TBSRenderer.TForm.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  Scope.Classes.Add('form-control');
  inherited;
end;

procedure TBSRenderer.TForm.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TBSRenderer.TForm.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TForm;
begin
  e := Scope.Element as THTML.TForm;
  Context.Writer.OpenTag('form', 'method="post"'+ NV('action', Context.GetLocationPath(e.PostTo)) + ' enctype="multipart/form-data"' + Scope.GetText);
  inherited;
  if e.RedirectTo <> '' then
    Context.Writer.AddShortTag('input', 'type="hidden" name="redirect" value="' + e.RedirectTo + '"');
  Context.Writer.AddShortTag('input', 'type="hidden" name="execute" value="true"');
  Context.Writer.CloseTag('form');

  if e.Submit.Caption <> '' then
    Context.Writer.AddTag('button', 'class="btn btn-success" type="submit" form="'+e.ID+'" value="Submit"', e.Submit.Caption);
  if e.Cancel.Caption <> '' then
    Context.Writer.AddTag('button', 'class="btn btn-primary" type="cancel" form="'+e.ID+'" value="Cancel"', e.Cancel.Caption);
  if e.Reset.Caption <> '' then
    Context.Writer.AddTag('button', 'class="btn btn-primary" type="reset" form="'+e.ID+'" value="Reset"', e.Reset.Caption);
end;

{ TBSRenderer.TParagraphHTML }

procedure TBSRenderer.TParagraph.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
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

{ TBSRenderer.TBreakHTML }

procedure TBSRenderer.TBreak.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Context.Writer.AddShortTag('br');
end;

{ TBSRenderer.TTButton }

procedure TBSRenderer.TButton.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TButton;
  event: string;
begin
  e := Scope.Element as THTML.TButton;
  Scope.Classes.Add('btn');
  if e.ControlStyle <> styleUndefined then
    Scope.Classes.Add(BSItemStyleToStr('btn-', e.ControlStyle));
  if e.JSFunction <> '' then
    event := ' onclick="'+e.JSFunction+'(this, event)"'
  else if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Context.Writer.OpenTag('button', 'type="button"' + event + Scope.GetText);
  inherited;
  Context.Writer.CloseTag('button');
end;

{ TBSRenderer.TNavItem }

procedure TBSRenderer.TNavItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavItem;
  event: string;
begin
  e := Scope.Element as THTML.TNavItem;
  if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Scope.Classes.Add('nav-link');
  Context.Writer.AddTag('a', 'href="'+When(e.LinkTo, '#') + '"' + event + Scope.GetText, e.Caption);
  inherited;
end;

{ TBSRenderer.TMenuItem }

procedure TBSRenderer.TMenuItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TMenuItem;
  event: string;
begin
  e := Scope.Element as THTML.TMenuItem;
  if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Context.Writer.AddTag('button', 'role="menu" type="button"' + event + Scope.GetText, e.Caption);
  inherited;
end;

{ TBSRenderer.TSubbMenu }

procedure TBSRenderer.TSubMenu.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
end;

procedure TBSRenderer.TSubMenu.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
end;

{ TBSRenderer.TInputHTML }

procedure TBSRenderer.TInput.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  Scope.Attributes['placeholder'] := (Scope.Element as THTML.TInput).PlaceHolder;
  Scope.Attributes['type'] := (Scope.Element as THTML.TInput).EditType;
  inherited;
end;

procedure TBSRenderer.TInput.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TInput;
  event: string;
  isFormChild: Boolean;
begin
  e := Scope.Element as THTML.TInput;
  isFormChild := True;
  if isFormChild then
    Scope.Classes.Add('form-control');

  if e.Caption <> '' then
    Context.Writer.AddTag('label', When(isFormChild, 'class="form-label"') + ' for="' + e.ID + '"', e.Caption);

  if Context.Schema.Interactive then
    event := ' onchange="mnw.send(' + SQ(e.ID) + ', '+ SQ('change') + ',' + 'this.value' + ')"';

  Context.Writer.AddShortTag('input', event + When(e.Required, ' required') + Scope.GetText); //TODO need to generate less spaces
  if e.HelpText <> '' then
    Context.Writer.AddTag('div', 'class="form-text"', e.HelpText);
  inherited;
end;

{ TBSRenderer.TImageHTML }

procedure TBSRenderer.TImage.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  Scope.Attributes['src'] := (Scope.Element as THTML.TImage).Source;
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImage).AltText; //* always set
  inherited;
end;

procedure TBSRenderer.TImage.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Context.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TBSRenderer.TMemoryImageHTML }

procedure TBSRenderer.TImageMemory.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  Scope.Attributes['src'] := Context.GetPath(Scope.Element);
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImageMemory).AltText;
  inherited;
end;

procedure TBSRenderer.TImageMemory.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TImageMemory;
begin
  e := Scope.Element as THTML.TImageMemory;
  Context.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TBSRenderer.TBody }

procedure TBSRenderer.TBody.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TBody;
begin
  e := Scope.Element as THTML.TBody;
  inherited;
  if e.Theme = themeDark then
    Scope.Attributes['data-bs-theme'] := 'dark'
  else if e.Theme = themeLight then
    Scope.Attributes['data-bs-theme'] := 'light';
end;

procedure TBSRenderer.TBody.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TBody;
  aTheme: string;  
begin
  e := Scope.Element as THTML.TBody;
  Context.Writer.OpenTag('body', Scope.ToString);  

  aTheme := 'light';
  if e.Theme = themeDark then
    aTheme := 'dark';   
  Context.Writer.OpenTag('script');
  Context.Writer.Writeln('const theme = localStorage.getItem("mnw-theme") || "'+aTheme+'";');
  Context.Writer.Writeln('document.body.setAttribute("data-bs-theme", theme);');
  Context.Writer.CloseTag('script');
  
  inherited;  
  
  Context.Writer.CloseTag('body');
end;

{ TBSRenderer.TPanel }

procedure TBSRenderer.TPanel.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TPanel;
begin
  e := Scope.Element as THTML.TPanel;
  Context.Writer.OpenTag('div', 'class="panel fit-content"');
  if e.Caption <> '' then
    Context.Writer.AddTag('div', 'class="panel-header"', e.Caption);

  Scope.Classes.Add('panel-body');
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TCollapseCaption }

procedure TBSRenderer.TCollapseCaption.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCollapseCaption;
begin
  e := Scope.Element as THTML.TCollapseCaption;
  Context.Writer.OpenTag('p', 'class="panel d-flex m-0" data-bs-toggle="collapse" role="button" data-bs-target="#'+e.ID+'-text" aria-expanded="false" aria-controls="'+e.ID+'-text"');
  if e.Caption <> '' then
    Context.Writer.WriteLn(e.Caption);
  Context.Writer.AddTag('span', 'class="ms-auto p-0 align-bottom icon mnw-three-dots"');
  Context.Writer.CloseTag('p');
  Context.Writer.OpenTag('div', 'id="'+e.ID+'-text" class="panel-body m-0 collapse"');
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TThemeModeButton }

procedure TBSRenderer.TThemeModeButton.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TThemeModeButton;
begin
  e := Scope.Element as THTML.TThemeModeButton;
  Context.Writer.OpenTag('button', 'class="bg-transparent mx-0 py-0 px-1 border-0" type="button" aria-label="Toggle navigation" onclick="mnw.switch_theme(this, event)"');
  Context.Writer.AddTag('span', 'class="icon mnw-theme"');
  inherited;
  Context.Writer.CloseTag('button');
end;

{ TBSRenderer.TDropdown }

procedure TBSRenderer.TDropdown.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('dropdown-item');
end;

procedure TBSRenderer.TDropdown.DoEnterRender(Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TBSRenderer.TDropdown.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TDropdown;
  classes: string;
begin
  e := Scope.Element as THTML.TDropdown;

  Scope.Classes.Add('btn');
  if dropArraw in e.Options then
    Scope.Classes.Add('dropdown-toggle');
  if dropSplit in e.Options then
    Scope.Classes.Add('dropdown-toggle-split');
  if e.ControlStyle <> styleUndefined then
    Scope.Classes.Add(BSItemStyleToStr('btn-', e.ControlStyle));
	Scope.Attributes.Add('data-bs-toggle', 'dropdown');
  Scope.Attributes.Add('aria-expanded', 'false');
  Scope.Attributes.Add('type', 'button');

  Context.Writer.OpenTag('div', 'class="dropdown"');

  //Button
  Context.Writer.OpenTag('button', Scope.ToString);
  RenderImageLocation(Context, e.Image);
  if e.Caption <> '' then
    Context.Writer.WriteLn(e.Caption);
  Context.Writer.CloseTag('button');

  classes := 'dropdown-menu';
  if dropEnd in e.Options then
    classes := classes + ' dropdown-menu-end';
  // Body of dropdown menu
  Context.Writer.OpenTag('div', 'class="' + classes + '" aria-labelledby="' + e.ID + '"');
  inherited;
  Context.Writer.CloseTag('div');

  Context.Writer.CloseTag('div');
end;

procedure TBSRenderer.TDropdown.DoLeaveRender(Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

{ TBSRenderer.TDropdownItem }

procedure TBSRenderer.TDropdownItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TDropdownItem;
begin
  e := Scope.Element as THTML.TDropdownItem;
  if e.Caption = '-' then
  begin
    Scope.Classes.Remove('dropdown-item');
    Scope.Classes.Add('dropdown-divider');
    Context.Writer.AddTag('div', Scope.ToString);
  end
  else
  begin
//    Scope.Classes.Add('dropdown-item');
//    Context.Writer.AddTag('a', Scope.ToString, e.Caption);
    inherited;
  end;
end;

{ TBSRenderer.TGroupButtons }

procedure TBSRenderer.TGroupButtons.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TGroupButtons;
begin
  e := Scope.Element as THTML.TGroupButtons;
  Scope.Classes.Add('btn-group');
  Scope.Attributes.Add('role', 'group');
  Scope.Attributes.Add('aria-label', e.ID);
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TToolbar }

procedure TBSRenderer.TToolbar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TToolbar;
begin
  e := Scope.Element as THTML.TToolbar;
  Scope.Classes.Add('btn-toolbar');
  Scope.Attributes.Add('role', 'toolbar');
  Scope.Attributes.Add('aria-label', e.ID);
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TZoomButtons }

procedure TBSRenderer.TZoomButtons.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

{ TBSRenderer.TRow }

procedure TBSRenderer.TRow.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TRow;
begin
  e := Scope.Element as THTML.TRow;
  Scope.Classes.Add('row');
  Scope.Classes.Add('flex-md-nowrap');
  Scope.Classes.Add(BSContentJustifyToStr(e.ContentAlign, False));
  if e.Fixed <> fixedDefault then
    Scope.Classes.Add(BSFixedToStr(e.Fixed, False));
  if e.Align <> alignDefault then
    Scope.Classes.Add(BSAlignToStr(e.Align, False));
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TColumn }

procedure TBSRenderer.TColumn.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  if e.Size > 0 then
    Scope.Classes.Add('col-'+e.Size.ToString)
  else
    Scope.Classes.Add('col');
  if e.Fixed <> fixedDefault then
    Scope.Classes.Add(BSFixedToStr(e.Fixed, False));
  if e.Align <> alignDefault then
    Scope.Classes.Add(BSAlignToStr(e.Align, False));
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TBar }

procedure TBSRenderer.TBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TBar;
begin
  e := Scope.Element as THTML.TBar;
  Scope.Classes.Add('bar');
  //Scope.Classes.Add('bg-body');
  Scope.Classes.Add('d-flex');
  Scope.Classes.Add('p-1');
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TAccordion }

procedure TBSRenderer.TAccordion.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
//  Context.Writer.OpenTag('div', 'class="accordion"');
  inherited;
end;

procedure TBSRenderer.TAccordion.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  //Context.Writer.CloseTag('div');
end;

procedure TBSRenderer.TAccordion.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Scope.Classes.Add('accordion');
  //Scope.Classes.Add('col');
  Scope.Classes.Add('accordion-flush');
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TAccordionSection }

procedure TBSRenderer.TAccordionSection.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
var 
  classes: TElementClasses;
begin
  classes.init('list-group-item');
  classes.Add('bg-transparent');
  classes.Append(Scope.WrapClasses);
  Context.Writer.OpenTag('li',classes.ToString);
  inherited;
end;

procedure TBSRenderer.TAccordionSection.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  Context.Writer.CloseTag('li');
end;

procedure TBSRenderer.TAccordionSection.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TAccordionSection;
  aScope: TmnwScope;  
begin
  e := Scope.Element as THTML.TAccordionSection;

  Context.Writer.OpenTag('div', 'class="accordion-item bg-transparent"');

  aScope := TmnwScope.Create(nil);
  try
    // Build header button attributes with TStringBuilder for efficiency
    aScope.Attributes.Add('id', e.ID + '-button');
    aScope.Classes.Append('accordion-button p-1');
    if not e.Expanded then
      aScope.Classes.Add('collapsed');
    aScope.Attributes.Add('type','button');
    aScope.Attributes.Add('data-bs-toggle', 'collapse'); 
    aScope.Attributes.Add('data-bs-target','#'+e.ID);
    aScope.Attributes.Add('aria-expanded', When(e.Expanded));

    aScope.Attributes.Add('aria-controls', e.ID);  
  
    if e.SaveState then
      aScope.Attributes.Add('data-mnw-savestate', 'true');

    Context.Writer.OpenTag('h2', 'id="'+e.id+'-header" class="accordion-header"');
    Context.Writer.OpenTag('button', aScope.ToString);
  finally
    aScope.Free;
  end;

  if e.Image.Location = imgSymbol then
    Context.Writer.AddTag('span', 'class='+ DQ(e.Image.Symbol + '  p-1'))
  else if e.Image.Location = imgPath then
    Context.Writer.AddShortTag('img', 'class="p-1" src='+ DQ(e.Image.Path) + ' alt=""');
{  else if e.Image.Location = imgMemory then
    Context.Writer.AddShortTag('img', 'src='+ DQ(e.Image.Path) + ' alt=""');}

  if e.Caption <> '' then
    Context.Writer.WriteLn(e.Caption);
  Context.Writer.CloseTag('button');
  Context.Writer.CloseTag('h2');

  Scope.Classes.Add('accordion-collapse collapse');
  if e.Expanded then
    Scope.Classes.Add('show');
  if (e.Parent is THTML.TAccordion) and
     not (e.Parent as THTML.TAccordion).AlwaysOpen then
    Scope.Attributes.Add('data-bs-parent', '#'+e.Parent.ID);
  Context.Writer.OpenTag('div', Scope.ToString + ' aria-labelledby="' + e.ID + '-header"');
  Context.Writer.OpenTag('ul', 'class="accordion-body list-group list-group-flush p-1"');
  inherited;
  Context.Writer.CloseTag('ul');
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TBSRenderer.TAccordionItem }

procedure TBSRenderer.TAccordionItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
{var
  e: THTML.TAccordionItem;}
begin
  //e := Scope.Element as THTML.TAccordionItem;
  //Scope.Classes.Add('');
  inherited;
end;

{ TBSRenderer.TNavBar }

procedure TBSRenderer.TNavBar.DoRenderBrand(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavBar;
begin
  e := Scope.Element as THTML.TNavBar;
  Context.Writer.OpenTag('a', 'class="logo navbar-brand align-items-center me-auto" href="' + Context.GetPath(e)+'"');

//  if e.Schema.Web.Assets.Logo.Data.Size > 0 then
//    Context.Writer.AddShortTag('img', 'src="' + Context.GetPath(e.Schema.Web.Assets.Logo)+ '" alt=""');
  e.Logo.Render(Context, AResponse); // Render Image

  if e.Title <> '' then
    Context.Writer.AddTag('span', '', e.Title);
  Context.Writer.CloseTag('a');
end;

procedure TBSRenderer.TNavBar.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
var
  classes: TElementClasses;
begin
  classes.Init('nav-item');
  classes.Append(Scope.WrapClasses);
  Context.Writer.OpenTag('li', classes.ToString);
  inherited;
end;

procedure TBSRenderer.TNavBar.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  Context.Writer.CloseTag('li');
end;

procedure TBSRenderer.TNavBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavBar;
  sb: THTML.TSideBar;
begin
  e := Scope.Element as THTML.TNavBar;
  Scope.Classes.Add('navbar');
  if e.Fixed = fixedTop then
    Scope.Classes.Add('fixed-top');
  Scope.Classes.Add('navbar-expand-md');
  Scope.Classes.Add('navbar-dark');
//  Scope.Classes.Add('bg-black');
  Scope.Classes.Append('flex-nowrap w-100 py-0 px-1');

  Context.Writer.OpenTag('nav', Scope.ToString);

  if (e.Schema as THTML).Document.Body.SideBar.CanRender then
  begin
    sb := (e.Schema as THTML).Document.Body.SideBar;
    Context.Writer.OpenTag('button', 'class="navbar-toggler my-0 py-0 px-1 border-0" type="button" data-bs-toggle="offcanvas" data-bs-target="#' + sb.id + '-body' + '" aria-controls="' + sb.id + '-items' + '" aria-expanded="false" aria-label="Toggle Sidebar"');
    Context.Writer.AddTag('span', 'class="icon mnw-chevron-right"');
    Context.Writer.CloseTag('button');
  end;

	DoRenderBrand(Scope, Context, AResponse);

  Context.Writer.OpenTag('div', 'id="'+e.id+'-items'+'" class="offcanvas offcanvas-top'+When((e.Schema as THTML).Document.Body.Header.CanRender, ' content-top') + ' navbar-dark bg-black" data-bs-scroll="true" data-bs-backdrop="true" data-bs-keyboard="false" tabindex="-1"');
  //Context.Writer.WriteLn('<div class="offcanvas-body">', [woOpenIndent]);
  Context.Writer.OpenTag('ul', 'class="navbar-nav me-auto m-2 m-md-0"');
  inherited;
  Context.Writer.CloseTag('ul');
  Context.Writer.CloseTag('div');
  //Context.Writer.WriteLn('</div>', [woCloseIndent]);

  if e.Tools.Count>0 then
    e.Tools.Render(Context, AResponse); // Render buttons

  if e.Count > 0 then
  begin
    Context.Writer.OpenTag('button', 'class="navbar-toggler p-0 border-0" type="button" data-bs-toggle="offcanvas" data-bs-target="#'+e.ID+'-items'+'" aria-controls="'+e.ID+'-items'+'" aria-expanded="false" aria-label="Toggle navigation"');
    Context.Writer.AddTag('span', 'class="icon mnw-list"');
    Context.Writer.CloseTag('button');
  end;
  Context.Writer.CloseTag('nav');
end;

{ TBSRenderer.TMenuBar }

procedure TBSRenderer.TMenuBar.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TBSRenderer.TMenuBar.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TBSRenderer.TMenuBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
end;

{ TBSRenderer.THTMLItem }

procedure TBSRenderer.THTMLItem.DoEnterRender(Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TBSRenderer.THTMLItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.THTMLItem;
begin
  e := Scope.Element as THTML.THTMLItem;
  RenderImageLocation(Context, e.Image);
  inherited;
  if e.Caption <> '' then
  begin
    if e.AutoHideText then
      Context.Writer.AddInlineTag('span', 'class="autohide"', e.Caption)
    else
      Context.Writer.WriteLn(e.Caption);
  end;
end;

{ TBSRenderer.TLink }

procedure TBSRenderer.TLink.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TLink;
  s: string;
begin
  e := Scope.Element as THTML.TLink;
  if e.ClickType = clickAction then
    s :=' onclick="mnw.click(this, event)"'
  else if e.ClickType = clickNewWindow then
    s :=' target="_blank"';
  if e.NoDecoration then
    Scope.Classes.Add('text-decoration-none');
  Context.Writer.OpenInlineTag('a', 'href="'+When(e.Location, '#') + '"'+ s + Scope.GetText, e.Caption);
  inherited;
  Context.Writer.CloseTag('a');
end;

{ TBSRenderer.TSideBar }

procedure TBSRenderer.TSideBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TSideBar;
begin
  e := Scope.Element as THTML.TSideBar;
  Scope.Classes.Add('sidebar');
  //Scope.Classes.Add('navbar-expand-md');
  if (e.Schema as THTML).Document.Body.Header.CanRender then
    Scope.Classes.Add('min-content-height');
  Scope.Classes.Add('p-0');
  Scope.Classes.Add('m-0');
  if e.Theme = themeDark then
  begin
    Scope.Classes.Add('bg-dark');
    Scope.Attributes.Add('data-bs-theme', 'dark');
  end
  else if e.Theme = themeLight then
  begin
    Scope.Classes.Add('bg-light');
    Scope.Attributes.Add('data-bs-theme', 'light');
  end;
  Context.Writer.OpenTag('aside', Scope.ToString);
  Context.Writer.OpenTag('div id="' + e.ID + '-content' + '" class="sidebar-content' + When((e.Schema as THTML).Document.Body.Header.CanRender, ' min-content-height') + ' fixed"');
  Context.Writer.OpenTag('div id="' + e.ID + '-body" class="sidebar-body offcanvas-md offcanvas-start" data-bs-scroll="true" data-bs-backdrop="false" data-bs-keyboard="false" aria-controls="header"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('aside');
end;

{ TBSRenderer.TSpan }

procedure TBSRenderer.TSpan.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TSpan;
begin
  e := Scope.Element as THTML.TSpan;
  Context.Writer.OpenInlineTag('span', Scope.ToString, e.Text);
  inherited;
  Context.Writer.CloseTag('span');
end;

{ TBSRenderer.THTMLLayout }

procedure TBSRenderer.THTMLLayout.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.THTMLLayout;
  MarginPrefix, PaddingPrefix: string;
begin
  e := Scope.Element as THTML.THTMLLayout;
  inherited;
  Scope.Classes.Add(BSFixedToStr(e.Fixed, False));
  Scope.Classes.Add(BSAlignToStr(e.Align, False));
  Scope.Classes.Add(BSAlignItemsToStr(e.AlignItems, False));
  Scope.Classes.Add(BSContentJustifyToStr(e.JustifyItems, False));
  if e.Solitary then
    Scope.Classes.Add('mx-auto');

  // Optimize margin/padding prefix calculation
  if e.Medium then
  begin
    MarginPrefix := 'm-md';
    PaddingPrefix := 'p-md';
  end
  else
  begin
    MarginPrefix := 'm';
    PaddingPrefix := 'p';
  end;

  Scope.Classes.Add(e.Margin.ToBSString(MarginPrefix));
  Scope.Classes.Add(e.Padding.ToBSString(PaddingPrefix));
end;

{ TBSRenderer.TImageFile }

procedure TBSRenderer.TImageFile.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TImageFile;
begin
  e := Scope.Element as THTML.TImageFile;
  Scope.Attributes['src'] := Context.GetPath(Scope.Element);
  Scope.Attributes['alt'] := When(e.AltText, e.Name);
  inherited;
end;

procedure TBSRenderer.TImageFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TImageFile;
begin
  e := Scope.Element as THTML.TImageFile;
  Context.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TBSRenderer.TNavTools }

procedure TBSRenderer.TNavTools.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
end;

procedure TBSRenderer.TNavTools.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
{var
  classes: TElementClasses;}
begin
  //classes.Init('nav-item');
  //classes.Append('align-items-center');
  //classes.Append(Scope.WrapClasses);
  //Context.Writer.OpenTag('li', classes.ToString);
  inherited;
end;

procedure TBSRenderer.TNavTools.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavTools;
//  event: string;
begin
  e := Scope.Element as THTML.TNavTools;
  Scope.Classes.Add('navbar-nav ms-auto');
//  Context.Writer.OpenTag('ul', 'class="navbar-nav"');
  inherited;
//  Context.Writer.CloseTag('ul');
end;

procedure TBSRenderer.TNavTools.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
//  Context.Writer.CloseTag('li');
end;

{ TBSRenderer.TNavDropdown }

procedure TBSRenderer.TNavDropdown.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
  Scope.WrapClasses.Add('dropdown');
end;

procedure TBSRenderer.TNavDropdown.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  Context.Writer.OpenTag('li', 'class="dropdown-item"');
  inherited;
end;

procedure TBSRenderer.TNavDropdown.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavDropdown;
  event: string;
  classes: TElementClasses;
begin
  e := Scope.Element as THTML.TNavDropdown;
  Scope.Classes.Add('nav-link');
  if dropArraw in e.Options then
    Scope.Classes.Add('dropdown-toggle');
  if dropSplit in e.Options then
    Scope.Classes.Add('dropdown-toggle-split');
  Scope.Attributes.Add('data-bs-toggle', 'dropdown');
  Scope.Attributes.Add('aria-expanded', 'false');
  Context.Writer.AddTag('a', 'href="#" ' + event + Scope.GetText, e.Caption);

  classes.Init('dropdown-menu');
  if dropEnd in e.Options then
    classes.Add ('dropdown-menu-end');
  Context.Writer.OpenTag('ul', classes.ToString);
  inherited;
  Context.Writer.CloseTag('ul');
end;

procedure TBSRenderer.TNavDropdown.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  Context.Writer.CloseTag('li');
end;

{ TBSRenderer.TCoded }

procedure TBSRenderer.TCode.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCode;
begin
  e := Scope.Element as THTML.TCode;
//  Scope.Classes.Add('language-'+e.Language);
  Context.Writer.OpenTag('code', Scope.ToString, e.Text);
  inherited;
  Context.Writer.CloseTag('code');
end;

{ TBSRenderer.TMultilineCode }

procedure TBSRenderer.TMultilineCode.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCode;
begin
  e := Scope.Element as THTML.TCode;
  Context.Writer.OpenTag('pre');
  inherited;
  Context.Writer.CloseTag('pre');
end;

{ TBootstrap_Library }

procedure TBootstrap_Library.Created;
const
  cssBaseURL = 'https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/';
  jsBaseURL = 'https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/js/';
begin
  inherited;
  Sources.Add(stStyle, stOnline, cssBaseURL, 'bootstrap.rtl.min.css', dirRightToLeft, 'sha384-CfCrinSRH2IR6a4e6fy2q6ioOX7O6Mtm1L9vRvFZ1trBncWmMePhzvafv7oIcWiW', [libCross]);
  Sources.Add(stStyle, stOnline, cssBaseURL, 'bootstrap.min.css', dirLeftToRight, 'sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB', [libCross]);
  Sources.Add(stScript, stOnline, jsBaseURL, 'bootstrap.bundle.min.js', dirUndefined, 'sha384-FKyoEForCGlyvwx9Hj09JcYn3nv7wiPVlz7YYwJrWVcXK/BmnVDxM+D2scQbITxI', [libDefer, libCross]);
end;

{ TBootstrapIcons_Library }

procedure TBootstrapIcons_Library.Created;
begin
  inherited;
  Sources.Add(stStyle, stOnline, 'cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/', 'bootstrap-icons.min.css', dirUndefined, '', [libCross]);
end;

initialization
  Renderers.RegisterRenderer('Bootstrap', TBSRenderer);
finalization
end.

