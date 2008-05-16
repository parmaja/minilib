unit mnPrinters;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  {$ifdef FPC}
  LCLType,
  {$else}
  Windows,
  {$endif}
  Classes,
  Graphics,
  SysUtils;

type
  TmnPrintStyle = (mnpsLines, mnpsCanvas, mnpsStreamCanvas);
  TmnDensity = (mndHi, mndMedium, mndLow);
  
  TmnCustomPrinter = class;
  
  { TmnCustomPage }

  TmnCustomPage = class(TObject)
  private
    FHeight: Integer;
    FWidth: Integer;
    function GetBoundsRect: TRect;
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
  protected
    FPrinter: TmnCustomPrinter;
    function GetCanvas: TCanvas; virtual;
    procedure ResizePage; virtual;
  public
    constructor Create(Printer: TmnCustomPrinter); virtual;
    destructor Destroy; override;
    procedure SaveToFile(FileName:string); virtual;
    property Canvas: TCanvas read GetCanvas;
    procedure PrintCanvas(Canvas: TCanvas); virtual;
    property Width:Integer read FWidth write SetWidth;
    property Height:Integer read FHeight write SetHeight;
    property BoundsRect:TRect read GetBoundsRect;
  end;
  
  { TmnCustomPrinter }

  TmnCustomPrinter = class(TObject)
  private
    FBitmap:TBitmap;
    FPage: TmnCustomPage;
    FWidth: Integer;
    FHeight: Integer;
    FStyle: TmnPrintStyle;
    FDensity: TmnDensity;
    procedure SetCurrentPage(const AValue: TmnCustomPage);
  protected
    FStream:TStream;
    function GetName: string; virtual;
    function DoCreatePage: TmnCustomPage; virtual;
    procedure PrintPage(vPage:TmnCustomPage); virtual;
  public
    constructor Create(Style:TmnPrintStyle; Stream:TStream); virtual;
    destructor Destroy; override;
    function CreatePage: TmnCustomPage;
    procedure BeginDocument; virtual;
    procedure EndDocument; virtual;
    procedure NewPage; virtual;
    procedure EndPage; virtual;
    procedure CancelPage; virtual;
    procedure AbortDocument; virtual;
    procedure Print(S: AnsiString);
    procedure PrintLn(S: AnsiString);
    property Bitmap:TBitmap read FBitmap;
    property Page:TmnCustomPage read FPage write SetCurrentPage;
    property Style:TmnPrintStyle read FStyle;
    property Density:TmnDensity read FDensity write FDensity;
    property Name:string read GetName;
  end;

implementation

{ TmnCustomPrinter }

constructor TmnCustomPrinter.Create(Style:TmnPrintStyle; Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FStyle := Style;
end;

function TmnCustomPrinter.GetName: string;
begin
  Result := '';
end;

procedure TmnCustomPrinter.SetCurrentPage(const AValue: TmnCustomPage);
begin
  if Page <> AValue then
  begin
    if Page <> nil then
      EndPage;
    Page:=AValue;
  end;
end;

function TmnCustomPrinter.DoCreatePage: TmnCustomPage;
begin
  Result := nil;
end;

procedure TmnCustomPrinter.PrintPage(vPage: TmnCustomPage);
begin

end;

destructor TmnCustomPrinter.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TmnCustomPrinter.CreatePage: TmnCustomPage;
begin
  Result := DoCreatePage;
end;

procedure TmnCustomPrinter.BeginDocument;
begin

end;

procedure TmnCustomPrinter.EndDocument;
begin

end;

procedure TmnCustomPrinter.NewPage;
begin
  FPage := CreatePage;
end;

procedure TmnCustomPrinter.EndPage;
begin
  PrintPage(FPage);
  FPage := nil;
end;

procedure TmnCustomPrinter.CancelPage;
begin
  FreeAndNil(FPage);
end;

procedure TmnCustomPrinter.AbortDocument;
begin

end;

procedure TmnCustomPrinter.Print(S: AnsiString);
begin
  if FStream <> nil then
    FStream.Write(PChar(S)^, Length(s));
end;

procedure TmnCustomPrinter.PrintLn(S: AnsiString);
begin
  PrintLn(s + #13#10);
end;

{ TmnCustomPage }

function TmnCustomPage.GetCanvas: TCanvas;
begin
  Result := nil;
end;

procedure TmnCustomPage.ResizePage;
begin
end;

function TmnCustomPage.GetBoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

procedure TmnCustomPage.SetHeight(const AValue: Integer);
begin
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    ResizePage;
  end;
end;

procedure TmnCustomPage.SetWidth(const AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    ResizePage;
  end;
end;

constructor TmnCustomPage.Create(Printer: TmnCustomPrinter);
begin
  inherited Create;
  FPrinter := Printer;
end;

destructor TmnCustomPage.Destroy;
begin
  inherited Destroy;
end;

procedure TmnCustomPage.SaveToFile(FileName: string);
begin

end;

procedure TmnCustomPage.PrintCanvas(Canvas: TCanvas);
begin
end;

end.

