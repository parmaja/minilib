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
  
  Gap
  https://stackoverflow.com/questions/6507014/how-to-space-the-children-of-a-div-with-css


  //https://disjfa.github.io/bootstrap-tricks/card-collapse-tricks/
  //https://bootstrapbrain.com/tutorial/bootstrap-accordion-with-plus-minus-icon/

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
  mnFields, mnParams, mnMultipartData, mnModules, mnWebModules, mnClasses, mnWebElements;

const
  ForceGap: Boolean = False;
  GapChilds = 'm-childs';

  WideSize = 'md';
  ControlPadding = 1;

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
{      THTMLContainer = class(THTMLElement)
      private
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;}

      { THTMLComponent }

      THTMLComponent = class abstract(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { THTMLControl }

      THTMLControl = class abstract(THTMLComponent)
      protected
        procedure RenderImageLocation(const Ctx: TmnwContext; const Image: TImageLocation);
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      THTMLFormControl = class abstract(THTMLControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoEnterRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoLeaveRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      public      
      end;

      { TImage }

      TImage = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TImageFile }

      TImageFile = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TImageMemory }

      TImageMemory = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;
      
      { TDocument }

      TDocument = class(TmnwHTMLRenderer.TDocument)
      end;

      { TBody }

      TBody = class(TmnwHTMLRenderer.TBody)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { THeader }

      THeader = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      TNavTools = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      public
      end;

      TNavDropdown = class(THTMLComponent)
      private
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      public
      end;

      { TNavBar }

      TNavBar = class(THTMLComponent)
      private
      protected
        procedure DoRenderBrand(Scope: TmnwScope; Ctx: TmnwContext); virtual;
        procedure DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      public
      end;

      { TMenuBar }

      TMenuBar = class(THTMLComponent)
      end;

      { THTMLItem }

      THTMLItem = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      public
      end;

      { TLink }

      TLink = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TSpan }

      TSpan = class(THTMLElement)
      protected        
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TSpanButton }

      TSpanButton = class(TSpan)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TFooter }

      TFooter = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TToast }

      TToast = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TContent }

      TContent = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TSideBar }

      TSideBar = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TMain }

      TMain = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      THTMLLayout = class abstract(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TBox }

      TBox = class(THTMLLayout)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TBar }

      TBar = class(THTMLLayout)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TRow }

      TRow = class(THTMLLayout)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TColumn }

      TColumn = class(THTMLLayout)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TPanel }

      TCustomPanel = class(THTMLControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      TPanel = class(TCustomPanel)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TCard }

      TCardHeader = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      TCardFooter = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      TCard = class(TCustomPanel)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TAccordion }

      TAccordion = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TAccordionSection }

      TAccordionSection = class(THTMLElement)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TAccordionItem }

      TAccordionItem = class(THTMLControl)
      end;

      { TCollapseCaption }

      TCollapseCaption = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TDropdown }

      TDropdown = class(THTMLControl)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TPopupMenu }

      TPopupMenu = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      TGroup = class(THTMLControl)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;
      
      { TDropdownItem }

      TDropdownItem = class(TLink)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TGroupButtons }

      TGroupButtons = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TToolbar }

      TToolbar = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TForm }

      TForm = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TParagraph }

      TParagraph = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { THeading }

      THeading = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      TCode = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      TMultilineCode = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;      

      { TBreak }

      TBreak = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { THorzLine }

      THorzLine = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TButton }

      TButton = class(THTMLItem)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      TToolButton = class(TButton)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TThemeButton }

      TThemeButton = class(TToolButton)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      TSubmitForm = class(TButton)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      TLinkButton = class(THTMLItem)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      TResetForm = class(TButton)
      protected        
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      TActionForm = class(TButton)
      protected        
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TNavItem }

      TNavItem = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TMenuItem }

      TMenuItem = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TSubMenu }

      TSubMenu = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TInput }

      TInput = class(THTMLFormControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      TUsername = class(TInput)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      TPassword = class(TInput)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;
      
      TNewPassword = class(TPassword)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TIntegerInput }

      TIntegerInput = class(TInput)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TCountInput }

      TCountInput = class(TIntegerInput)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TDateInput }

      TDateInput = class(TInput)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TTimeInput }

      TTimeInput = class(TInput)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TDateTimeInput }

      TDateTimeInput = class(TInput)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TMaskInput }

      TMaskInput = class(TInput)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
      end;

      { TSelect }

      TSelect = class(THTMLFormControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TTextArea }

      TTextArea = class(THTMLFormControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TCheckbox }

      TCheckbox = class(THTMLControl) //Yes THTMLControl
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoEnterRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoLeaveRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      THiddenInput = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;      
  public
    procedure AddHead(const Ctx: TmnwContext); override;
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

  TColSizeHelper = record helper for TColSize
    function ToString: string;
  end;

function BSJustifyToStr(const s: string; Align: TmnwJustify; WithSpace: Boolean = False): string; 
function BSRowAlignToStr(const s: string; Align: TmnwAlign; WithSpace: Boolean = False): string;
function BSColumnAlignToStr(const s: string; Align: TmnwAlign; WithSpace: Boolean = False): string;

function BSFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = False): string;
function BSSizeToStr(const Prefix: string; Width: TWidthSize; WithSpace: Boolean = False): string;
function BSControlStyleToStr(const Prefix: string; Style: TItemStyle; WithSpace: Boolean = False): string;

procedure AddRowClasses(var Classes: TElementClasses; Wrap: Boolean = True);
procedure AddColumnClasses(var Classes: TElementClasses);

implementation

function BSRowAlignToStr(const s: string; Align: TmnwAlign; WithSpace: Boolean): string;
const
  sSuffixes: array[TmnwAlign] of string = ('', 'start', 'center', 'stretch', 'end'); // 'baseline',
begin
  if (Align >= alFirst) and (Align <= alLast) then
    Result := s + sSuffixes[Align]
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSJustifyToStr(const s: string; Align: TmnwJustify; WithSpace: Boolean): string;
const
  sSuffixes: array[TmnwJustify] of string = ('', 'start', 'center', 'between', 'around', 'evenly', 'end');
begin
  if (Align >= jstStart) and (Align <= jstEnd) then
    Result := s + sSuffixes[Align]
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSColumnAlignToStr(const s: string; Align: TmnwAlign; WithSpace: Boolean = False): string;
const
  sSuffixes: array[TmnwAlign] of string = ('', 'top', 'center', 'stretch', 'bottom');
begin
  if (Align >= alFirst) and (Align <= alLast) then
    Result := s + sSuffixes[Align]
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean): string;
const
  FixedStrs: array[TmnwFixed] of string = ('', 'fixed-top', 'fixed-bottom', 'fixed-start', 'fixed-end');
//    'sticky-top', 'sticky-bottom', 'sticky-start', 'sticky-end');
begin
  Result := FixedStrs[Fixed];
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSSizeToStr(const Prefix: string; Width: TWidthSize; WithSpace: Boolean): string;
const
  SizeStrs: array[TWidthSize] of string = ('', 'xxs', 'xs', 'sm', 'md', 'lg', 'xl', 'xxl');
begin
  Result := SizeStrs[Width];
  if (Result <> '') then
    Result := Prefix + Result;  
  if WithSpace and (Result <> '') then
    Result := ' ' + Result;
end;

function BSControlStyleToStr(const Prefix: string; Style: TItemStyle; WithSpace: Boolean): string;
const
  StyleNames: array[TItemStyle] of string = ('', 'primary', 'secondary', 'success', 'danger', 'warning', 'info', 'light', 'dark', 'link', 'bg-transparent');
begin
  Result := StyleNames[Style];
  if Result <> '' then
    Result := Prefix + Result;
  if WithSpace and (Result <> '') then
    Result := ' ' + Result;
end;

procedure AddRowClasses(var Classes: TElementClasses; Wrap: Boolean);
begin
  Classes.Add('d-flex', ssInner);
  if Wrap then
    Classes.Add('flex-wrap', ssInner)
  else
    Classes.Add('flex-nowrap', ssInner);
end;

procedure AddColumnClasses(var Classes: TElementClasses);
begin
  Classes.Add('d-flex', ssInner);
  Classes.Add('flex-column', ssInner);
end;

function TmnwBSBoundingHelper.IsUniform: Boolean; 
begin
  Result := (Top = Left) and (Top = Bottom) and (Top = Right);
end;

function TmnwBSBoundingHelper.IsUniformSides: Boolean; 
begin
  Result := (Top = Bottom) and (Left = Right);
end;

{ TColSizeHelper }

function TColSizeHelper.ToString: string;
begin
  case Kind of
    cskUndefined: Result := '';
    cskNumber: Result := 'col-' + WideSize + '-' + IntToStr(Self);
    cskAuto: Result := 'col-' + WideSize;
    cskFit: Result := 'col-' + WideSize+ '-auto';
    cskMax:
    begin
      Result := 'col-' + WideSize+ '-12';
      Result := Result + BSSizeToStr(' max-w-' , Max);
    end;
  end;
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
  Require(TBootstrap_Library);
  Require(TBootstrapIcons_Library);
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

procedure TBSRenderer.AddHead(const Ctx: TmnwContext);
begin
(*  Ctx.Writer.WriteLn('<style type="text/css">', [woOpenIndent]);
  Ctx.Writer.WriteLn('.small-card {');
  Ctx.Writer.WriteLn('    max-width: 22rem;');
  Ctx.Writer.WriteLn('}');
  Ctx.Writer.WriteLn('</style>', [woCloseIndent]); *)
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
    RegisterRenderer(THTML.THeading, THeading);
    RegisterRenderer(THTML.TBreak, TBreak);
    RegisterRenderer(THTML.THorzLine, THorzLine);
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
    RegisterRenderer(THTML.TToolButton, TToolButton);
    RegisterRenderer(THTML.TSubmitForm, TSubmitForm);
    RegisterRenderer(THTML.TLinkButton, TLinkButton);
    RegisterRenderer(THTML.TResetForm, TResetForm);
    RegisterRenderer(THTML.TActionForm, TActionForm);
    RegisterRenderer(THTML.TNavItem, TNavItem);
    RegisterRenderer(THTML.TMenuItem, TMenuItem);
    RegisterRenderer(THTML.TDropdownItem, TDropdownItem);
    RegisterRenderer(THTML.TInput, TInput); //Yes not TCustomInput
    RegisterRenderer(THTML.TUsername, TUsername);
    RegisterRenderer(THTML.TPassword, TPassword);
    RegisterRenderer(THTML.TNewPassword, TNewPassword);
    RegisterRenderer(THTML.TIntegerInput, TIntegerInput);
    RegisterRenderer(THTML.TCountInput, TCountInput);
    RegisterRenderer(THTML.TDateInput, TDateInput);
    RegisterRenderer(THTML.TTimeInput, TTimeInput);
    RegisterRenderer(THTML.TDateTimeInput, TDateTimeInput);
    RegisterRenderer(THTML.TMaskInput, TMaskInput);
    RegisterRenderer(THTML.TSelect, TSelect);
    RegisterRenderer(THTML.TTextArea, TTextArea);
    RegisterRenderer(THTML.TCheckbox, TCheckbox);
    RegisterRenderer(THTML.THiddenInput, THiddenInput);
    
    RegisterRenderer(THTML.TImage, TImage);
    RegisterRenderer(THTML.TImageFile, TImageFile);
    RegisterRenderer(THTML.TImageMemory, TImageMemory);
    
    RegisterRenderer(THTML.TCardHeader, TCardHeader);
    RegisterRenderer(THTML.TCard, TCard);
    RegisterRenderer(THTML.TCardFooter, TCardFooter);
    RegisterRenderer(THTML.TDropdown, TDropdown);
    RegisterRenderer(THTML.TPopupMenu, TPopupMenu);
    RegisterRenderer(THTML.TGroup, TGroup);
    RegisterRenderer(THTML.TGroupButtons, TGroupButtons);
    RegisterRenderer(THTML.TToolbar, TToolbar);
    RegisterRenderer(THTML.TCollapseCaption, TCollapseCaption);
    RegisterRenderer(THTML.TForm, TForm);
    RegisterRenderer(THTML.TBox, TBox);
    RegisterRenderer(THTML.TBar, TBar);
    RegisterRenderer(THTML.TRow, TRow);
    RegisterRenderer(THTML.TColumn, TColumn);
    RegisterRenderer(THTML.TPanel, TPanel);
    RegisterRenderer(THTML.TCode, TCode);
    RegisterRenderer(THTML.TMultilineCode, TMultilineCode);

    RegisterRenderer(THTML.THTMLComponent, THTMLComponent);
    RegisterRenderer(THTML.THTMLFormControl, THTMLFormControl);
    RegisterRenderer(THTML.THTMLControl, THTMLControl);

    RegisterRenderer(THTML.TThemeButton, TThemeButton);
  end;
end;

{ TBSRenderer.THTMLControl }

procedure TBSRenderer.THTMLControl.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.THTMLControl;
begin
  e := Scope.Element as THTML.THTMLControl;
  if e.Hint <> '' then
  begin
    Scope.Attributes.Add('data-bs-toggle', 'tooltip', ssInner);
    Scope.Attributes.Add('data-bs-placement', 'top', ssInner);
    Scope.Attributes.Add('title', e.Hint, ssInner);
  end;

  if not (sstSize in Scope.State)  then
  begin
    //Scope.Classes.AddIf(e.Width > 0, 'max-w-' + e.Width.ToString, ssOuter);
    //Maybe need to check e.responsible
    Scope.Classes.AddIf(e.Size, e.Size.ToString, ssOuter); //Yes when it is big take the size, if small screen get full width
  end;

  case e.Shadow of
    shadowHairline: Scope.Classes.Add('shadow-hairline', ssOuter);
    shadowThin: Scope.Classes.Add('shadow-thin', ssOuter);
    ShadowThick: Scope.Classes.Add('shadow-thick', ssOuter);
    ShadowEnd: Scope.Classes.Add('shadow-end', ssOuter);
    ShadowBottom: Scope.Classes.Add('shadow-bottom', ssOuter);
    else ;
  end;

  if e.Bind.Name <> '' then
    Scope.Attributes.Add('data-bind-name', e.Bind.Name, ssInner);

  if e.Bind.Group <> '' then
  begin
    Scope.Attributes.Add('data-bind-group', e.Bind.Group, ssInner);
    Scope.Attributes.Add('data-bind-action', BindActionToStr(e.Bind.Action), ssInner);
  end;
  inherited;
end;

procedure TBSRenderer.THTMLControl.RenderImageLocation(const Ctx: TmnwContext; const Image: TImageLocation);
begin
  if Image.Location = imgSymbol then
  begin
    if Image.Symbol <> '' then    
      Ctx.Writer.AddTag('span', 'class='+ DQ(Image.Symbol))//TODO check d-block?
  end
  else if Image.Location = imgPath then
  begin
    if Image.Path <> '' then    
      Ctx.Writer.AddShortTag('img', 'src='+ DQ(Image.Path) + ' alt=""')
  end
  else if Image.Location = imgMemory then
  begin
{    if Route <> '' then    
      Ctx.Writer.AddShortTag('img', 'src='+ DQ(Image.Path) + ' alt=""');}
  end;
end;

{ TBSRenderer.THeaderHTML }

procedure TBSRenderer.THeader.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin  
  Scope.Classes.Append('header sticky-top d-flex align-items-center py-0 px-1');
  Scope.Classes.Append('navbar-dark bg-black'); //dark theme header
  Scope.Attributes.Add('data-bs-theme', 'dark'); //Needed because our Header is always darktheme some items/icons not detected it
  Ctx.Writer.OpenTag('header', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('header');
end;

{ TBSRenderer.TFooterHTML }

procedure TBSRenderer.TFooter.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Scope.Classes.Add('text-center');
  Ctx.Writer.OpenTag('footer', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('footer');
end;

{ TBSRenderer.TToast }

procedure TBSRenderer.TToast.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.OpenTag('div', 'aria-live="polite" aria-atomic="true"');
  Ctx.Writer.OpenTag('div', 'id="toast-container" class="toast-container position-fixed bottom-0 end-0 p-2" style="z-index:1056;"');
  inherited;
  Ctx.Writer.CloseTag('div');
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TContent }

procedure TBSRenderer.TContent.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TContent;
begin
  e := Scope.Element as THTML.TContent;
  if e.Wide then
    Scope.Classes.Add('container-fluid')
  else
    Scope.Classes.Add('container');
  Ctx.Writer.OpenTag('div', Scope.ToString);
  Ctx.Writer.OpenTag('div', 'id="content" class="content row"');
  inherited;
  Ctx.Writer.CloseTag('div');
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TMainHTML }

procedure TBSRenderer.TMain.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TMain;
begin
  e := Scope.Element as THTML.TMain;
  Scope.Classes.Add('main');
//  Scope.Classes.Add('d-flex', ssInner);
//  Scope.Classes.Add('align-items-start', ssInner);
  //Scope.Classes.Add('d-column', ssInner);

  if (e.Schema as THTML).Document.Body.Header.CanRender  then
    Scope.Classes.Add('max-content-height');
  if (e.Parent.Parent as THTML.TBody).SideBar.CanRender then
    Scope.Classes.Add('col-md');
  Scope.Classes.Add('p-1');
  Scope.Classes.Add('p-sm-2'); //???
  Scope.Classes.Add('m-0'); //do not change it, keep it 0

  Ctx.Writer.OpenTag('main', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('main');
end;

procedure TBSRenderer.TCard.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TCard;
begin
  e := Scope.Element as THTML.TCard;
  inherited;
end;

{ TBSRenderer.TCardHTML }

procedure TBSRenderer.TCard.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TCard;
begin
  e := Scope.Element as THTML.TCard;
  Scope.Classes.Add('card');
  Scope.Classes.Add('p-0');

  Ctx.Writer.OpenTag('div', Scope.ToString([ssOuter]));
  if (e.Caption <> '') or (e.Header.Count > 0) then
  begin
    Ctx.Writer.OpenTag('h5', 'id="' + e.id + '-header" class="card-header align-items-center d-flex'+ BSControlStyleToStr('text-bg-', e.ControlStyle, True) + BSControlStyleToStr('bg-', e.ControlStyle, True) + '"');
    Ctx.Writer.WriteLn(e.Caption);
    Ctx.Writer.OpenTag('div', 'class="d-flex ms-auto"');
    e.Header.Render(Ctx);
    Ctx.Writer.CloseTag('div');

    if e.Collapse then
    begin
      Ctx.Writer.Write('<span class="ms-auto my-auto icon-animate icon mnw-chevron-up"');
      Ctx.Writer.Write(' role="button" data-bs-toggle="collapse" data-bs-target="#'+e.id+'-body" aria-labelledby="' + e.id + '-header" aria-expanded="true" aria-controls="'+e.id+'-body"');
      Ctx.Writer.WriteLn('></span>');
    end;
    Ctx.Writer.CloseTag('h5');
  end;

  Ctx.Writer.OpenTag('div', 'id="'+e.id+'-body" class="card-body p-0 collapse show" aria-labelledby="'+e.id+'-header"');  //removed `overflow-hidden`

  // InnerClasses (d-flex, flex-column, etc.) use !important which overrides
  // Bootstrap's .collapse:not(.show) { display: none; }. Wrap children in a
  // flex container so the collapse target div can be hidden properly.

  Ctx.Writer.OpenTag('div', 'id="'+e.id+'-panel" class="overflow-hidden p-1' //p-1 needed for highlights inputs
//    + When((e.Gap > 0) or ForceGap, ' ' + GapChilds)
    + SpaceIf(Scope.Classes.ToString([ssInner]))
    + '"'
    );
  inherited;
  Ctx.Writer.CloseTag('div'); //Panel
  Ctx.Writer.CloseTag('div'); //Body
  if e.Footer <> nil then
    e.Footer.Render(Ctx);
  Ctx.Writer.CloseTag('div');
end;

procedure TBSRenderer.TForm.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TForm;
begin
  e := Scope.Element as THTML.TForm;
  if (e.Gap > 0) or ForceGap then
    Scope.Classes.Add(GapChilds, ssInner);
  Scope.Attributes.Add('method', 'post');
  Scope.Attributes.AddIf('action', Ctx.GetLocationPath(e, e.Endpoint));
  Scope.Attributes.AddIf('onsubmit', e.CallScript);
  Scope.Attributes.AddIf('enctype', 'multipart/form-data');
  inherited;
end;

{ TBSRenderer.TFormHTML }

procedure TBSRenderer.TForm.DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;
end;

procedure TBSRenderer.TForm.DoLeaveChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;
end;

procedure TBSRenderer.TForm.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TForm;
begin
  e := Scope.Element as THTML.TForm;
  Ctx.Writer.OpenTag('form', Scope.ToString);
  inherited;
  if e.RedirectTo <> '' then
    Ctx.Writer.AddShortTag('input', 'type="hidden" name="redirect" value="' + e.RedirectTo + '"');

  if e.CallScript = '' then
    Ctx.Writer.AddShortTag('input', 'type="hidden" name="execute" value="true"');

  if (e.Submit.Caption <> '') or (e.Cancel.Caption <> '') or (e.Reset.Caption <> '') then
    Ctx.Writer.AddShortTag('hr');

  if e.Submit.Caption <> '' then
    Ctx.Writer.AddTag('button', 'class="btn btn-success" type="submit" form="'+e.ID+'" value="Submit"', e.Submit.Caption);
  if e.Reset.Caption <> '' then
    Ctx.Writer.AddTag('button', 'class="btn btn-secondary" type="reset" form="'+e.ID+'" value="Reset"', e.Reset.Caption);
  if e.Cancel.Caption <> '' then
      if e.CancelTo.Where <> toNone then
        Ctx.Writer.AddTag('a', 'class="btn btn-primary" href="' + Ctx.GetLocationPath(e, e.CancelTo) + '"', e.Cancel.Caption);
        //Ctx.Writer.AddTag('button', 'class="btn btn-primary" type="cancel" onclick="location.href=''' + Ctx.GetLocationPath(e.CancelTo) + '''"', e.Cancel.Caption);
  Ctx.Writer.CloseTag('form');
end;

{ TBSRenderer.TParagraphHTML }

procedure TBSRenderer.TParagraph.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TParagraph;
begin
  e := Scope.Element as THTML.TParagraph;
  Ctx.Writer.OpenInlineTag('p', Scope.ToString);
  if e.Text <> '' then
    Ctx.Writer.Write(e.Text);
  inherited;
  Ctx.Writer.CloseTag('p');
end;

{ TBSRenderer.THeading }

procedure TBSRenderer.THeading.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.THeading;
  Tag: string;
begin
  e := Scope.Element as THTML.THeading;
  Tag := 'h' + IntToStr(e.Level);
  Ctx.Writer.OpenInlineTag(Tag, Scope.ToString);
  if e.Text <> '' then
    Ctx.Writer.Write(e.Text);
  inherited;
  Ctx.Writer.CloseTag(Tag);
end;

{ TBSRenderer.TBreakHTML }

procedure TBSRenderer.TBreak.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.AddShortTag('br');
  //Ctx.Writer.AddSpace;
end;

procedure TBSRenderer.TButton.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TCustomButton;
begin
  e := Scope.Element as THTML.TCustomButton;
  Scope.Classes.Add('btn');
  Scope.Attributes.Add('type', 'button', ssInner);
  if e.ConfirmMessage <> '' then
    Scope.Attributes.Add('data-mnw-confirm', e.ConfirmMessage, ssInner);
  inherited;
end;

{ TBSRenderer.TTButton }

procedure TBSRenderer.TButton.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TCustomButton;
  event: string;
begin
  e := Scope.Element as THTML.TCustomButton;
  if e.ControlStyle <> styleUndefined then
  begin
    if e.Outline then
      Scope.Classes.Add(BSControlStyleToStr('btn-outline-', e.ControlStyle))
    else
      Scope.Classes.Add(BSControlStyleToStr('btn-', e.ControlStyle));
  end;
  if e.CallScript <> '' then
    event := ' onclick='''+e.CallScript+''''
  else if Ctx.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"'
  else
    event := '';
  Ctx.Writer.OpenTag('button', Scope.ToString + event);
  inherited;
  Ctx.Writer.CloseTag('button');
end;

{ TBSRenderer.TNavItem }

procedure TBSRenderer.TNavItem.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TNavItem;
  event: string;
begin
  e := Scope.Element as THTML.TNavItem;
  if Ctx.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Scope.Classes.Add('nav-link');
  Ctx.Writer.AddTag('a', 'href="'+When(e.LinkTo, '#') + '"' + event + Scope.ToString(True), e.Caption);
  inherited;
end;

{ TBSRenderer.TMenuItem }

procedure TBSRenderer.TMenuItem.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TMenuItem;
  event: string;
begin
  e := Scope.Element as THTML.TMenuItem;
  if Ctx.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Ctx.Writer.AddTag('button', 'role="menu" type="button"' + event + Scope.ToString(True), e.Caption);
  inherited;
end;

{ TBSRenderer.TInputHTML }

procedure TBSRenderer.TInput.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TInput;
begin
  e := Scope.Element as THTML.TInput;
  Scope.Attributes.AddIf(e.EditType<> '', 'type', e.EditType, ssInner);
  Scope.Attributes.Add('placeholder', e.PlaceHolder, ssInner);
  if e.AutoFocus then
    Scope.Attributes.AddProp('autofocus', ssInner);
  if not e.AutoComplete then
  begin
    Scope.Attributes.Add('autocomplete', 'off', ssInner);
    //Scope.Attributes.Add('aria-autocomplete', 'none', ssInner);
  end;
  Scope.Attributes.Add('value', e.Value, ssInner);
  inherited;
end;

procedure TBSRenderer.TInput.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TInput;
begin
  e := Scope.Element as THTML.TInput;

  if Ctx.Schema.Interactive then
    Scope.Attributes.Add('onchange', 'mnw.send(' + SQ(e.ID) + ', '+ SQ('change') + ',' + 'this.value' + ')', ssInner);

  Ctx.Writer.AddShortTag('input', Scope.ToString([ssInner])); //TODO [ssInner]
  inherited;
end;

{ TBSRenderer.TImageHTML }

procedure TBSRenderer.TImage.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  Scope.Attributes.Add('src' , (Scope.Element as THTML.TImage).Source, ssInner);
  Scope.Attributes.Add('alt', (Scope.Element as THTML.TImage).AltText, ssInner); //* always set
  inherited;
end;

procedure TBSRenderer.TImage.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TBSRenderer.TImageMemory }

procedure TBSRenderer.TImageMemory.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  Scope.Attributes.Add('src', Ctx.GetPath(Scope.Element), ssInner);
  Scope.Attributes.Add('alt', (Scope.Element as THTML.TImageMemory).AltText, ssInner);
  inherited;
end;

procedure TBSRenderer.TImageMemory.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TBSRenderer.TBody }

procedure TBSRenderer.TBody.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TBody;
begin
  e := Scope.Element as THTML.TBody;
  inherited;
  Scope.Classes.Add('body-bg');
  if e.Theme = themeDark then
  begin
    Scope.Attributes.Add('data-bs-theme', 'dark', ssInner);
    Scope.Attributes.Add('data-theme', 'dark', ssInner);
  end
  else if e.Theme = themeLight then
  begin
    Scope.Attributes.Add('data-bs-theme', 'light', ssInner);
    Scope.Attributes.Add('data-theme', 'light', ssInner);
  end;
end;

procedure TBSRenderer.TBody.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TBody;
begin
  e := Scope.Element as THTML.TBody;
  Ctx.Writer.OpenTag('body', Scope.ToString);

  Ctx.Writer.OpenTag('script');
  Ctx.Writer.Writeln('const theme = localStorage.getItem("mnw-theme") || "'+When(e.Theme = themeDark, ThemeToStr(e.Theme), 'light')+'";');
  Ctx.Writer.Writeln('document.body.setAttribute("data-bs-theme", theme);');
  Ctx.Writer.Writeln('document.body.setAttribute("data-theme", theme);');

  Ctx.Writer.Writeln('let mnw_zoom = localStorage.getItem("mnw-zoom");');
  Ctx.Writer.Writeln('if (mnw_zoom) document.documentElement.setAttribute("data-mnw-zoom", mnw_zoom);');

  Ctx.Writer.CloseTag('script');
  
  inherited;  
  
  if Ctx.Web.ShowVersion then
    Ctx.Writer.WriteLn('<div class="version">' + Ctx.Web.Version + ' mnw: v'+ cVersion +'</div>');
  Ctx.Writer.CloseTag('body');
end;

{ TBSRenderer.TCustomPanel }

procedure TBSRenderer.TCustomPanel.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TCustomPanel;
begin
  e := Scope.Element as THTML.TCustomPanel;
  inherited;
  if (e.Gap > 0) or ForceGap then
    Scope.Classes.Add(GapChilds, ssInner);
  if e.Direction <> dirUndefined then
    Scope.Attributes.Add('dir', DirectionToStr(e.Direction));
  if e.Solitary then
  begin
//    Scope.Classes.Add('mx-auto');
//    Scope.Classes.Add('my-auto');
    Scope.Classes.Append('top-50 start-50 translate-middle', ssOuter);
  end;

  if e.Mode = emdUndefined then
  begin
    //Scope.Classes.Add('d-inline-block', ssInner);
  end
  else if e.Mode = emdRow then
  begin
    AddRowClasses(Scope.Classes);
  end
  else if e.Mode = emdColumn then
  begin
    AddColumnClasses(Scope.Classes);
    if e.AlignItems > alDefault then
      Scope.Classes.Add(BSRowAlignToStr('align-items-', e.AlignItems), ssInner);
  end;
//  Scope.InnerClasses.Add(BSJustifyToStr('justify-content-', e.JustifyItems));
end;


procedure TBSRenderer.TPanel.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TPanel;
begin
  e := Scope.Element as THTML.TPanel;
  Scope.Classes.Add('panel');
  Scope.Classes.Add('p-1');
  Scope.Classes.Add('bg-body');
  Scope.Classes.Add('border');
  Scope.Classes.Add('rounded');
  Scope.Classes.Add('overflow-hidden');
  case e.Sticky of
    stickyTop:
    begin
      Scope.Classes.Add('position-sticky');
      Scope.Classes.Add('top-0');
    end;
    stickyBottom:
    begin
      Scope.Classes.Add('position-sticky');
      Scope.Classes.Add('bottom-0');
    end;
  end;

  Ctx.Writer.OpenTag('div', Scope.ToString); //fit-content
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TCollapseCaption }

procedure TBSRenderer.TCollapseCaption.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TCollapseCaption;
begin
  e := Scope.Element as THTML.TCollapseCaption;
  Ctx.Writer.OpenTag('p', 'class="panel d-flex m-0" data-bs-toggle="collapse" role="button" data-bs-target="#'+e.ID+'-text" aria-expanded="false" aria-controls="'+e.ID+'-text"');
  if e.Caption <> '' then
    Ctx.Writer.WriteLn(e.Caption);
  Ctx.Writer.AddTag('span', 'class="ms-auto p-0 align-bottom icon mnw-three-dots"');
  Ctx.Writer.CloseTag('p');
  Ctx.Writer.OpenTag('div', 'id="'+e.ID+'-text" class="panel-body m-0 collapse"');
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TThemeButton }

procedure TBSRenderer.TThemeButton.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Attributes.Add('aria-label', 'Toggle theme', ssInner);
end;

{ TBSRenderer.TDropdown }

procedure TBSRenderer.TDropdown.DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('dropdown-item');
end;

procedure TBSRenderer.TDropdown.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TDropdown;
  classes: string;
begin
  e := Scope.Element as THTML.TDropdown;

  Scope.Classes.Add('btn');
  if dropArrow in e.Options then
    Scope.Classes.Add('dropdown-toggle');
  if dropSplit in e.Options then
    Scope.Classes.Add('dropdown-toggle-split');
  if e.ControlStyle <> styleUndefined then
    Scope.Classes.Add(BSControlStyleToStr('btn-', e.ControlStyle));
  Scope.Attributes.Add('data-bs-toggle', 'dropdown');
  Scope.Attributes.Add('aria-expanded', 'false');
  Scope.Attributes.Add('type', 'button');

  Ctx.Writer.OpenTag('div', 'class="dropdown"');

  //Button
  Ctx.Writer.OpenTag('button', Scope.ToString);
  RenderImageLocation(Ctx, e.Image);
  if e.Caption <> '' then
    Ctx.Writer.WriteLn(e.Caption);
  Ctx.Writer.CloseTag('button');

  classes := 'dropdown-menu';
  if dropEnd in e.Options then
    classes := classes + ' dropdown-menu-end';
  // Body of dropdown menu
  Ctx.Writer.OpenTag('div', 'class="' + classes + '" aria-labelledby="' + e.ID + '"');
  inherited;
  Ctx.Writer.CloseTag('div');

  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TPopupMenu }

procedure TBSRenderer.TPopupMenu.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TPopupMenu;
  Item: string;
  Classes: string;
  Event: string;
  i: Integer;
begin
  e := Scope.Element as THTML.TPopupMenu;

  Scope.Classes.Add('btn');
  Scope.Classes.Add('dropdown-toggle');
  if e.ControlStyle <> styleUndefined then
    Scope.Classes.Add(BSControlStyleToStr('btn-', e.ControlStyle));
  Scope.Attributes.Add('data-bs-toggle', 'dropdown');
  Scope.Attributes.Add('aria-expanded', 'false');
  Scope.Attributes.Add('type', 'button');

  Ctx.Writer.OpenTag('div', 'class="dropdown"');

  //Button
  Ctx.Writer.OpenTag('button', Scope.ToString);
  RenderImageLocation(Ctx, e.Image);
  if e.Caption <> '' then
    Ctx.Writer.WriteLn(e.Caption);
  Ctx.Writer.CloseTag('button');

  Classes := 'dropdown-menu';
  Ctx.Writer.OpenTag('ul', 'class="' + Classes + '" aria-labelledby="' + e.ID + '"');

  for i := 0 to e.Items.Count - 1 do
  begin
    Item := e.Items[i];
    if Item = '-' then
      Ctx.Writer.WriteLn('<li><hr class="dropdown-divider"></li>')
    else
    begin
      if Ctx.Schema.Interactive then
        Event := ' onclick="event.preventDefault(); mnw.send(' + SQ(e.ID) + ', ' + SQ('click') + ', ' + SQ(IntToStr(i)) + ')"'
      else
        Event := '';
      Ctx.Writer.WriteLn('<li><a class="dropdown-item" href="#"' + Event + '>' + EscapeAttr(Item) + '</a></li>');
    end;
  end;

  Ctx.Writer.CloseTag('ul');
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TDropdownItem }

procedure TBSRenderer.TDropdownItem.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TDropdownItem;
begin
  e := Scope.Element as THTML.TDropdownItem;
  if e.Caption = '-' then
  begin
    Scope.Classes.Remove('dropdown-item');
    Scope.Classes.Add('dropdown-divider');
    Ctx.Writer.AddTag('div', Scope.ToString);
  end
  else
  begin
//    Scope.Classes.Add('dropdown-item');
//    Ctx.Writer.AddTag('a', Scope.ToString, e.Caption);
    inherited;
  end;
end;

{ TBSRenderer.TGroupButtons }

procedure TBSRenderer.TGroupButtons.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TGroupButtons;
begin
  e := Scope.Element as THTML.TGroupButtons;
  Scope.Classes.Add('btn-group');
  Scope.Attributes.Add('role', 'group');
  Scope.Attributes.Add('aria-label', e.ID);
  Ctx.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TToolbar }

procedure TBSRenderer.TToolbar.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TToolbar;
begin
  e := Scope.Element as THTML.TToolbar;
  Scope.Classes.Add('btn-toolbar');
  Scope.Attributes.Add('role', 'toolbar');
  Scope.Attributes.Add('aria-label', e.ID);
  Ctx.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TRow }

procedure TBSRenderer.TRow.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TRow;
begin
  e := Scope.Element as THTML.TRow;
  Scope.Classes.Add('row', ssInner);
  Scope.Classes.Add('m-0');
  Scope.Classes.Add('p-0');
//  AddRowClasses(Scope.Classes, e.Wrap);
//  Scope.Classes.Add('row');
//  Scope.Classes.Add('flex-lg-nowrap');
//  Scope.Classes.Add('d-block');
  Scope.Classes.Add(BSRowAlignToStr('align-items-', e.AlignItems), ssInner);
  Scope.Classes.Add(BSJustifyToStr('justify-content-', e.JustifyItems), ssInner);
  inherited;
end;

procedure TBSRenderer.TRow.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TRow;
begin
  e := Scope.Element as THTML.TRow;
  Scope.Classes.Add(BSFixedToStr(e.Fixed));
{  if e.Align <> alignDefault then
    Scope.Classes.Add(BSAlignToStr(e.Align));}
  Ctx.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TColumn }

procedure TBSRenderer.TColumn.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  Scope.Classes.Add('d-flex');
  Scope.Classes.Add('m-0');
  Scope.Classes.Add('p-0');
  if e.Reverse then
    Scope.Classes.Add('flex-column-reverse')
  else
    Scope.Classes.Add('flex-column');
  inherited;
end;

procedure TBSRenderer.TColumn.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  //Scope.Classes.Add(BSColumnAlignToStr('', e.ContentAlign));
  if e.Fixed <> fixedNone then
    Scope.Classes.Add(BSFixedToStr(e.Fixed));
{  if e.Align <> alignDefault then
    Scope.Classes.Add(BSAlignToStr(e.Align));}
  Ctx.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TBar }

procedure TBSRenderer.TBar.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TBar;
begin
  e := Scope.Element as THTML.TBar;
  AddRowClasses(Scope.Classes, e.Wrap);
  Scope.Classes.Add('align-items-center');
  Scope.Classes.Add('p-0');
  Ctx.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TAccordion }

procedure TBSRenderer.TAccordion.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Scope.Classes.Add('accordion');
  //Scope.Classes.Add('col');
  Scope.Classes.Add('accordion-flush');
  Ctx.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TAccordionSection }

procedure TBSRenderer.TAccordionSection.DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
var 
  classes: TElementClasses;
begin
  classes.Init('list-group-item');
  classes.Add('bg-transparent');
  classes.Append(Scope.WrapClasses);
  Ctx.Writer.OpenTag('li', classes.ToFullString);
  inherited;
end;

procedure TBSRenderer.TAccordionSection.DoLeaveChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;
  Ctx.Writer.CloseTag('li');
end;

procedure TBSRenderer.TAccordionSection.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TAccordionSection;
  aScope: TmnwScope;  
begin
  e := Scope.Element as THTML.TAccordionSection;

  Ctx.Writer.OpenTag('div', 'class="accordion-item bg-transparent"');

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

    Ctx.Writer.OpenTag('h2', 'id="'+e.id+'-header" class="accordion-header p-1"'); //p-1 for full show box-shadow when focused
    Ctx.Writer.OpenTag('button', aScope.ToString);
  finally
    aScope.Free;
  end;

  if e.Image.Location = imgSymbol then
  begin
    if e.Image.Symbol <> '' then    
      Ctx.Writer.AddTag('span', 'class='+ DQ(e.Image.Symbol + ' px-1'));
  end
  else if e.Image.Location = imgPath then
  begin
    if e.Image.Path <> '' then    
      Ctx.Writer.AddShortTag('img', 'class="p-1" src='+ DQ(e.Image.Path) + ' alt=""');
  end;
{  else if e.Image.Location = imgMemory then
    Ctx.Writer.AddShortTag('img', 'src='+ DQ(e.Image.Path) + ' alt=""');}

  if e.Caption <> '' then
    Ctx.Writer.WriteLn(e.Caption);
  Ctx.Writer.CloseTag('button');
  Ctx.Writer.CloseTag('h2');

  Scope.Classes.Add('accordion-collapse collapse');
  if e.Expanded then
    Scope.Classes.Add('show');
  if (e.Parent is THTML.TAccordion) and
     not (e.Parent as THTML.TAccordion).AlwaysOpen then
    Scope.Attributes.Add('data-bs-parent', '#'+e.Parent.ID);
  Ctx.Writer.OpenTag('div', Scope.ToString + ' aria-labelledby="' + e.ID + '-header"');
  Ctx.Writer.OpenTag('ul', 'class="accordion-body list-group list-group-flush p-1"');
  inherited;
  Ctx.Writer.CloseTag('ul');
  Ctx.Writer.CloseTag('div');
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TNavBar }

procedure TBSRenderer.TNavBar.DoRenderBrand(Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TNavBar;
begin
  e := Scope.Element as THTML.TNavBar;
  Ctx.Writer.OpenTag('a', 'class="logo d-flex navbar-brand align-items-center p-0 me-auto" href="' + EndURL(Ctx.GetDefaultPath) + '"');

//  if e.Schema.Web.Assets.Logo.Data.Size > 0 then
//    Ctx.Writer.AddShortTag('img', 'src="' + Ctx.GetPath(e.Schema.Web.Assets.Logo)+ '" alt=""');
  e.Logo.Render(Ctx); // Render Image

  if e.Title <> '' then
    Ctx.Writer.AddTag('span', '', e.Title);
  Ctx.Writer.CloseTag('a');
end;

procedure TBSRenderer.TNavBar.DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
var
  classes: TElementClasses;
begin
  classes.Init('nav-item');
  classes.Append(Scope.WrapClasses);
  Ctx.Writer.OpenTag('li', classes.ToFullString);
  inherited;
end;

procedure TBSRenderer.TNavBar.DoLeaveChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;
  Ctx.Writer.CloseTag('li');
end;

procedure TBSRenderer.TNavBar.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TNavBar;
  sb: THTML.TSideBar;
begin
  e := Scope.Element as THTML.TNavBar;
  Scope.Classes.Add('navbar');
  if e.Fixed = fixedTop then
    Scope.Classes.Add('fixed-top');
  Scope.Classes.Add('navbar-expand-' + WideSize);
  Scope.Classes.Add('navbar-dark');
//  Scope.Classes.Add('bg-black');
  Scope.Classes.Add(GapChilds);
  Scope.Classes.Append('flex-nowrap w-100 py-0 px-1');

  Ctx.Writer.OpenTag('nav', Scope.ToString);

  if (e.Schema as THTML).Document.Body.SideBar.CanRender then
  begin
    sb := (e.Schema as THTML).Document.Body.SideBar;
    Ctx.Writer.OpenTag('button', 'class="navbar-toggler my-0 py-0 px-1 border-0" type="button" data-bs-toggle="offcanvas" data-bs-target="#' + sb.id + '-body' + '" aria-controls="' + sb.id + '-body' + '" aria-expanded="false" aria-label="Toggle Sidebar"');
    Ctx.Writer.AddTag('span', 'class="icon mnw-list"'); //mnw-chevron-right
    Ctx.Writer.CloseTag('button');
  end;

  DoRenderBrand(Scope, Ctx);

  Ctx.Writer.OpenTag('div', 'id="'+e.id+'-items'+'" class="offcanvas offcanvas-top'+When((e.Schema as THTML).Document.Body.Header.CanRender, ' content-top') + ' navbar-dark bg-black" data-bs-scroll="true" data-bs-backdrop="true" data-bs-keyboard="false" tabindex="-1"');
  //Ctx.Writer.WriteLn('<div class="offcanvas-body">', [woOpenIndent]);
  Ctx.Writer.OpenTag('ul', 'class="navbar-nav me-auto m-2 m-' + WideSize+'-0"');
  inherited;
  Ctx.Writer.CloseTag('ul');
  Ctx.Writer.CloseTag('div');
  //Ctx.Writer.WriteLn('</div>', [woCloseIndent]);

  if e.Tools.Count > 0 then
    e.Tools.Render(Ctx); // Render buttons

  if e.CountComposed > 0 then
  begin
    Ctx.Writer.OpenTag('button', 'class="navbar-toggler p-0 border-0" type="button" data-bs-toggle="offcanvas" data-bs-target="#'+e.ID+'-items'+'" aria-controls="'+e.ID+'-items'+'" aria-expanded="false" aria-label="Toggle navigation"');
    Ctx.Writer.AddTag('span', 'class="bi bi-chevron-down"');
    Ctx.Writer.CloseTag('button');
  end;
  Ctx.Writer.CloseTag('nav');
end;

{ TBSRenderer.THTMLItem }

procedure TBSRenderer.THTMLItem.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.THTMLItem;
begin
  e := Scope.Element as THTML.THTMLItem;
  RenderImageLocation(Ctx, e.Image);
  inherited;
  if e.Caption <> '' then
  begin
    if e.AutoHideText then
      Ctx.Writer.AddInlineTag('span', 'class="autohide"', e.Caption)
    else
      Ctx.Writer.WriteLn(e.Caption);
  end;
end;

{ TBSRenderer.TLink }

procedure TBSRenderer.TLink.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TLink;
  s: string;
begin
  e := Scope.Element as THTML.TLink;
  if e.ClickType = clickAction then
    s :=' onclick="mnw.click(event)"'
  else if e.ClickType = clickNewWindow then
    s :=' target="_blank"';
  if e.NoDecoration then
    Scope.Classes.Add('text-decoration-none');
  Ctx.Writer.OpenTag('a', 'href="'+When(e.Location, '#') + '"'+ s + Scope.ToString(True));
  RenderImageLocation(Ctx, e.Image);
  Ctx.Writer.Write(e.Caption);
  inherited;
  Ctx.Writer.CloseTag('a');
end;

{ TBSRenderer.TSideBar }

procedure TBSRenderer.TSideBar.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TSideBar;
begin
  e := Scope.Element as THTML.TSideBar;
  Scope.Classes.Add('sidebar');
  //Scope.Classes.Add('visible-');
  //Scope.Classes.Add('navbar-expand-' + WideSize);
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

  Ctx.Writer.OpenTag('aside', Scope.ToString);
  Ctx.Writer.OpenTag('div', 'id="' + e.ID + '-content' + '" class="sidebar-content' + When((e.Schema as THTML).Document.Body.Header.CanRender, ' min-content-height') + ' fixed"');
  Ctx.Writer.OpenTag('div', 'id="' + e.ID + '-body" class="sidebar-body offcanvas-' + WideSize+' offcanvas-start px-0" data-bs-scroll="true" data-bs-backdrop="false" data-bs-keyboard="false" aria-controls="header"');
  inherited;
  Ctx.Writer.CloseTag('div');
  Ctx.Writer.CloseTag('div');
  Ctx.Writer.CloseTag('aside');
end;

{ TBSRenderer.TSpan }

procedure TBSRenderer.TSpan.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TSpan;
  s: string;
begin
  e := Scope.Element as THTML.TSpan;
  s := Scope.ToString;
  if (s <> '') or (e.Text <> '') then  
  begin
    Ctx.Writer.OpenInlineTag('span', Scope.ToString, e.Text);
    inherited;
    Ctx.Writer.CloseTag('span');
  end
  else
    inherited;
end;

{ TBSRenderer.TImageFile }

procedure TBSRenderer.TImageFile.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TImageFile;
begin
  e := Scope.Element as THTML.TImageFile;
  Scope.Attributes.Add('src', Ctx.GetPath(Scope.Element), ssInner);
  Scope.Attributes.Add('alt', When(e.AltText, e.Name), ssInner);
  inherited;
end;

procedure TBSRenderer.TImageFile.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TBSRenderer.TNavTools }

procedure TBSRenderer.TNavTools.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Scope.Classes.Add('navbar-nav ms-auto');
  inherited;
end;

{ TBSRenderer.TNavDropdown }

procedure TBSRenderer.TNavDropdown.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.WrapClasses.Add('dropdown');
end;

procedure TBSRenderer.TNavDropdown.DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.OpenTag('li', 'class="dropdown-item"');
  inherited;
end;

procedure TBSRenderer.TNavDropdown.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TNavDropdown;
  classes: TElementClasses;
begin
  e := Scope.Element as THTML.TNavDropdown;
  Scope.Classes.Add('nav-link');

  if dropArrow in e.Options then
    Scope.Classes.Add('dropdown-toggle');
  if dropSplit in e.Options then
    Scope.Classes.Add('dropdown-toggle-split');
  Scope.Attributes.Add('data-bs-toggle', 'dropdown');
  Scope.Attributes.Add('aria-expanded', 'false');
  Ctx.Writer.AddTag('a', 'href="#"' + Scope.ToString(True), e.Caption);

  classes.Init('dropdown-menu');
  if dropEnd in e.Options then
    classes.Add ('dropdown-menu-end');
  Ctx.Writer.OpenTag('ul', classes.ToFullString);
  inherited;
  Ctx.Writer.CloseTag('ul');
end;

procedure TBSRenderer.TNavDropdown.DoLeaveChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;
  Ctx.Writer.CloseTag('li');
end;

{ TBSRenderer.TSubMenu }

procedure TBSRenderer.TSubMenu.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Scope.Classes.Add('dropdown');
  Ctx.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TCoded }

procedure TBSRenderer.TCode.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TCode;
begin
  e := Scope.Element as THTML.TCode;
//  Scope.Classes.Add('language-'+e.Language);
  Ctx.Writer.OpenTag('code', Scope.ToString);
  Ctx.Writer.Write(EscapeAttr(e.Text));
  inherited;
  Ctx.Writer.CloseTag('code');
end;

{ TBSRenderer.TMultilineCode }

procedure TBSRenderer.TMultilineCode.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.OpenTag('pre');
  inherited;
  Ctx.Writer.CloseTag('pre');
end;

{ TBootstrap_Library }

procedure TBootstrap_Library.Created;
const
  cssBaseURL = 'https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/';
  jsBaseURL = 'https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/js/';
begin
  inherited;
  with Sources.Add(stStyle, stOnline, cssBaseURL, 'bootstrap.rtl.min.css', 'sha384-CfCrinSRH2IR6a4e6fy2q6ioOX7O6Mtm1L9vRvFZ1trBncWmMePhzvafv7oIcWiW', [libCross]) do
    Direction := dirRightToLeft;
  with Sources.Add(stStyle, stOnline, cssBaseURL, 'bootstrap.min.css', 'sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB', [libCross]) do
    Direction := dirLeftToRight;
  Sources.Add(stScript, stOnline, jsBaseURL, 'bootstrap.bundle.min.js', 'sha384-FKyoEForCGlyvwx9Hj09JcYn3nv7wiPVlz7YYwJrWVcXK/BmnVDxM+D2scQbITxI', [libDefer, libCross]);
end;

{ TBootstrapIcons_Library }

procedure TBootstrapIcons_Library.Created;
begin
  inherited;
  Sources.Add(stStyle, stOnline, 'https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/', 'bootstrap-icons.min.css', '', [libCross]);
end;

{ TBSRenderer.TGroup }

procedure TBSRenderer.TGroup.DoEnterChildRender(var Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.THTMLComponent;
begin
  if Scope.Element is THTML.THTMLComponent then  
    e := Scope.Element as THTML.THTMLComponent
  else 
    e := nil;
  inherited;
  if (e <> nil) then    
  begin
    Scope.Classes.Add('list-group-item');
    if e.Active then    
    begin
      Scope.Attributes.Add('aria-current', 'true');
    end;
  end;
end;

procedure TBSRenderer.TGroup.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Scope.Classes.Add('list-group');
  Ctx.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.THTMLContainer }
{
procedure TBSRenderer.THTMLContainer.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.THTMLContainer;
  PaddingPrefix: string;
begin
  e := Scope.Element as THTML.THTMLContainer;
  inherited;
  if (e.AlignItems1 <> alignDefault) or (e.JustifyItems1 <> alignDefault) then
    Scope.InnerClasses.Add('d-flex');
  
  Scope.InnerClasses.Add(BSAlignItemsToStr(e.AlignItems1));
  Scope.InnerClasses.Add(BSContentJustifyToStr(e.JustifyItems1));

  if e.Medium then
    PaddingPrefix := 'p-md'
  else
    PaddingPrefix := 'p';

  Scope.Classes.Add(e.Padding.ToBSString(PaddingPrefix));
end;
}
{ TBSRenderer.THTMLComponent }

procedure TBSRenderer.THTMLComponent.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.THTMLComponent;
begin
  e := Scope.Element as THTML.THTMLComponent;
  if e.Active then    
  begin
    Scope.Classes.Add('active');
  end;
  inherited;
end;

{ TBSRenderer.TCardFooter }

procedure TBSRenderer.TCardFooter.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TCardFooter;
begin
  e := Scope.Element as THTML.TCardFooter;
  if e.Count > 0 then
  begin
    Scope.Classes.Add('card-footer');
    if e.Sticky then
    begin
      Scope.Classes.Add('position-sticky');
      Scope.Classes.Add('bottom-0');
    end;
    Ctx.Writer.OpenTag('div', Scope.ToString);
  end;
  inherited;
  if e.Count > 0 then  
    Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.THorzLine }

procedure TBSRenderer.THorzLine.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;
  Ctx.Writer.AddShortTag('hr');
end;

{ TBSRenderer.TSpanButton }

procedure TBSRenderer.TSpanButton.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  Scope.Classes.Add('btn');
  inherited;  
end;

{ TBSRenderer.THTMLFormControl }

procedure TBSRenderer.THTMLFormControl.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.THTMLFormControl;
begin
  e := Scope.Element as THTML.THTMLFormControl;
  if e.Caption <> '' then
    Scope.State := Scope.State + [sstSize];
  if ControlPadding > 0 then
    Scope.Classes.Add('p-'+ControlPadding.ToString, ssOuter);
  inherited;
  Scope.Classes.Add('form-control', ssInner);
  if e.Required then
    Scope.Attributes.AddProp('required', ssInner);
end;

procedure TBSRenderer.THTMLFormControl.DoEnterRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.THTMLFormControl;
  labelClasses: string;
begin
  e := Scope.Element as THTML.THTMLFormControl;
  if e.Caption <> '' then
  begin
    //Scope.Classes.AddIf(e.Width > 0, 'max-w-' + e.Width.ToString);
    Scope.Classes.AddIf(e.Size, ' ' + e.Size.ToString);
    if e.LabelLayout = lfFloating then
      Scope.Classes.Add('form-floating')
    else if e.LabelLayout = lfAbove then
    begin
      Scope.Classes.Add('col');
      Scope.Classes.Add('align-items-center');
      Scope.Classes.Add('px-0');
      labelClasses := ' mb-1';
    end
    else
    begin
      Scope.Classes.Add('d-flex');
      Scope.Classes.Add('align-items-center');
      Scope.Classes.Add('px-0');
      labelClasses := ' mx-2';
    end;
    Ctx.Writer.OpenTag('div', Scope.ToString([ssOuter]));
    if e.LabelLayout <> lfFloating then
      Ctx.Writer.AddTag('label', 'id=' + DQ(e.ID+'_label') + ' class="form-label p-0 my-auto text-nowrap' + labelClasses + '" for="' + e.ID + '"', e.Caption);
  end;
  inherited;
end;

procedure TBSRenderer.THTMLFormControl.DoLeaveRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.THTMLFormControl;
begin
  inherited;
  e := Scope.Element as THTML.THTMLFormControl;
  if (e.Caption <> '') then
  begin
    if e.LabelLayout = lfFloating then
      Ctx.Writer.AddTag('label', 'id=' + DQ(e.ID+'_label') + ' class="form-label" for="' + e.ID + '"', e.Caption);
    Ctx.Writer.CloseTag('div');
  end;
end;

{ TBSRenderer.TSubmitForm }

procedure TBSRenderer.TSubmitForm.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TSubmitForm;
begin
  e := Scope.Element as THTML.TSubmitForm;
  inherited;
  Scope.Classes.Add('btn-success');
  Scope.Attributes.Add('type', 'submit', ssInner);
  if e.FormID <> '' then
    Scope.Attributes.Add('form', e.FormID, ssInner);
end;

{ TBSRenderer.TResetForm }

procedure TBSRenderer.TResetForm.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TResetForm;
begin
  e := Scope.Element as THTML.TResetForm;
  inherited;
//  Scope.Classes.Add('btn-success');
  Scope.Attributes.Add('type', 'reset', ssInner);
  if e.FormID <> '' then
    Scope.Attributes.Add('form', e.FormID, ssInner);
end;

{ TBSRenderer.TPassword }

procedure TBSRenderer.TPassword.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TPassword;
begin
  e := Scope.Element as THTML.TPassword;
  inherited;
  Scope.Attributes.Add('type', 'password', ssInner);
  if e.Token <> '' then
    Scope.Attributes.Add('data-token', e.Token, ssInner);
end;

{ TBSRenderer.TUsername }

procedure TBSRenderer.TUsername.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
end;

{ TBSRenderer.TNewPassword }

procedure TBSRenderer.TNewPassword.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Attributes.Add('autocomplete', 'new-password', ssInner);
end;

{ TBSRenderer.TIntegerInput }

procedure TBSRenderer.TIntegerInput.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Attributes.Add('type', 'number', ssInner);
  Scope.Attributes.Add('step', '1', ssInner); //Whole numbers only
end;

{ TBSRenderer.TCountInput }

procedure TBSRenderer.TCountInput.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TCountInput;
begin
  inherited;
  e := Scope.Element as THTML.TCountInput;
  Scope.Attributes.Add('type', 'number', ssInner);
  Scope.Attributes.Add('min', e.Min.ToString, ssInner);
  Scope.Attributes.Add('max', e.Max.ToString, ssInner);
end;

{ TBSRenderer.TDateInput }

procedure TBSRenderer.TDateInput.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Attributes.Add('type', 'date', ssInner);
end;

{ TBSRenderer.TActionForm }

procedure TBSRenderer.TActionForm.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TActionForm;
begin
  e := Scope.Element as THTML.TActionForm;
  inherited;
  Scope.Attributes.Add('type', 'submit', ssInner);
  Scope.Attributes.Add('data-action', e.Action, ssInner);
  if e.FormID <> '' then
    Scope.Attributes.Add('form', e.FormID, ssInner);
end;

{ TBSRenderer.THiddenInput }

procedure TBSRenderer.THiddenInput.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.THiddenInput;
begin
  e := Scope.Element as THTML.THiddenInput;
  inherited;
  Scope.Attributes.Add('type', 'hidden', ssInner);
  Scope.Attributes.Add('value', e.Value, ssInner);
end;

procedure TBSRenderer.THiddenInput.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.AddShortTag('input', Scope.ToString);
  inherited;
end;

{ TBSRenderer.TToolButton }

procedure TBSRenderer.TToolButton.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('d-block');
  Scope.Classes.Add('p-1');
  Scope.Attributes.Add('aria-label', 'Toggle navigation');
end;

procedure TBSRenderer.TToolButton.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;

end;

{ TBSRenderer.TBox }

procedure TBSRenderer.TBox.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TBox;
begin
  inherited;
  e := Scope.Element as THTML.TBox;
  Scope.Classes.Add('m-0');
  Scope.Classes.Add('p-0');
  Scope.Classes.Remove('d-flex'); //HMMMM
  Scope.Classes.Add('d-grid');

  if e.Columns > 0 then
    Scope.Classes.Add('g-cols-'+e.Columns.ToString)
end;

procedure TBSRenderer.TBox.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.TDateTimeInput }

procedure TBSRenderer.TDateTimeInput.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Attributes.Add('type', 'datetime-local', ssInner);
end;

{ TBSRenderer.TTimeInput }

procedure TBSRenderer.TTimeInput.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Attributes.Add('type', 'time', ssInner);
end;

{ TBSRenderer.TMaskInput }

procedure TBSRenderer.TMaskInput.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TMaskInput;
begin
  inherited;
  e := Scope.Element as THTML.TMaskInput;
  if e.Format <> '' then
    Scope.Attributes.Add('data-mask', e.Format, ssInner);
end;

{ TBSRenderer.TSelect }

procedure TBSRenderer.TSelect.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TSelect;
begin
  e := Scope.Element as THTML.TSelect;
  inherited;
  Scope.Classes.Remove('form-control');
  Scope.Classes.Add('form-select', ssInner);
  if e.Multiple then
    Scope.Attributes.AddProp('multiple', ssInner);
  if e.ChangeScript <> '' then
    Scope.Attributes.Add('onchange', e.ChangeScript, ssInner);
end;

procedure TBSRenderer.TSelect.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TSelect;
  o: TmnNameValueObject;
  s, Selected: string;
begin
  e := Scope.Element as THTML.TSelect;
  Ctx.Writer.OpenTag('select', Scope.ToString([ssInner]));
  for o in e.Items do
  begin
    s := o.Value;
    if s = '' then
      s := o.Name;
    Selected := '';
    if SameText(s, e.SelectedValue) then
      Selected := ' selected';
    Ctx.Writer.AddTag('option', 'value=' + DQ(EscapeAttr(s)) + Selected, o.Name);
  end;
  inherited;
  Ctx.Writer.CloseTag('select');
end;

{ TBSRenderer.TTextArea }

procedure TBSRenderer.TTextArea.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TTextArea;
begin
  e := Scope.Element as THTML.TTextArea;
  inherited;
  if e.Rows > 0 then
    Scope.Attributes.Add('rows', e.Rows.ToString, ssInner);
end;

procedure TBSRenderer.TTextArea.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TTextArea;
begin
  e := Scope.Element as THTML.TTextArea;
  Ctx.Writer.OpenInlineTag('textarea', Scope.ToString);
  if e.Text <> '' then
    Ctx.Writer.Write(EscapeAttr(e.Text));
  Ctx.Writer.CloseTag('textarea');
  inherited;
end;

{ TBSRenderer.TCheckbox }

procedure TBSRenderer.TCheckbox.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TCheckbox;
begin
  e := Scope.Element as THTML.TCheckbox;
  inherited;
  Scope.Classes.Remove('form-control');
  Scope.Classes.Add('align-items-center', ssOuter);
  Scope.Classes.Add('p-1', ssOuter);
  Scope.Classes.Add('form-check-input', ssInner);
  Scope.Attributes.Add('type', 'checkbox', ssInner);
  if e.Value <> '' then
    Scope.Attributes.Add('value', e.Value, ssInner);
  if e.Checked then
    Scope.Attributes.AddProp('checked', ssInner);
end;

procedure TBSRenderer.TCheckbox.DoEnterRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TCheckbox;
begin
  e := Scope.Element as THTML.TCheckbox;
  Ctx.Writer.OpenTag('div', 'id=' + DQ(e.id + '-outter') + Scope.ToString([ssOuter], True));
  inherited;
end;

procedure TBSRenderer.TCheckbox.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TCheckbox;
begin
  e := Scope.Element as THTML.TCheckbox;
  Ctx.Writer.AddShortTag('input', Scope.ToString([ssInner]));
  if e.Caption <> '' then
    Ctx.Writer.AddTag('label', 'id=' + DQ(e.id + '-label') + ' class="form-check-label p-0 my-auto text-nowrap" for="' + e.ID + '"', e.Caption);
  inherited;
end;

procedure TBSRenderer.TCheckbox.DoLeaveRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.CloseTag('div');
end;

{ TBSRenderer.THTMLLayout }

procedure TBSRenderer.THTMLLayout.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.THTMLLayout;
begin
  e := Scope.Element as THTML.THTMLLayout;
  if (e.Gap > 0) or ForceGap then
    Scope.Classes.Add(GapChilds);
  if e.Size then
    Scope.Classes.Add(e.Size.ToString);
  inherited;
end;

{ TBSRenderer.TCardHeader }

procedure TBSRenderer.TCardHeader.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;
end;

{ TBSRenderer.TBackForm }

procedure TBSRenderer.TLinkButton.DoCollectAttributes(var Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('btn-secondary');
  Scope.Classes.Add('btn');
//  Scope.Attributes.Add('type', 'link', ssInner);
end;

procedure TBSRenderer.TLinkButton.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TLinkButton;
begin
  e := Scope.Element as THTML.TLinkButton;
  Ctx.Writer.OpenTag('a', 'href="'+When(e.Location, '#') + '"'+ Scope.ToString(True));
  inherited;
  Ctx.Writer.CloseTag('a');
end;

initialization
  Libraries.RegisterLibrary(TBootstrap_Library);
  Libraries.RegisterLibrary(TBootstrapIcons_Library);
  Renderers.RegisterRenderer('Bootstrap', TBSRenderer);
finalization
end.

