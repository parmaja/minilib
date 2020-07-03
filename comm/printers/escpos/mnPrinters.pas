unit mnPrinters;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
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
  mnDrivers,
  contnrs,
  Classes,
  Graphics,
  SysUtils;

type
  EmnPrinter = class(Exception);

  TmnPrintStyle = (mnpsLines, mnpsCanvas, mnpsStreamCanvas);
  TmnDensity = (mndHi, mndMedium, mndLow);
  
  TmnPrinter = class;
  
  { TmnCustomPage }

  TmnCustomPage = class(TObject)
  private
    FHeight: Integer;
    FWidth: Integer;
    function GetBoundsRect: TRect;
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
  protected
    FPrinter: TmnPrinter;
    function GetCanvas: TCanvas; virtual;
    procedure ResizePage; virtual;
  public
    constructor Create(Printer: TmnPrinter); virtual;
    destructor Destroy; override;
    procedure SaveToFile(FileName:string); virtual;
    property Canvas: TCanvas read GetCanvas;
    procedure PrintCanvas(Canvas: TCanvas); virtual;
    property Width:Integer read FWidth write SetWidth;
    property Height:Integer read FHeight write SetHeight;
    property BoundsRect:TRect read GetBoundsRect;
  end;
  
  { TmnPrinter }

  TmnPrinter = class(TObject)
  private
    FBitmap:TBitmap;
    FPage: TmnCustomPage;
    FStyle: TmnPrintStyle;
    FDensity: TmnDensity;
    FDefaultHeight: Integer;
    FDefaultWidth: Integer;
    procedure SetCurrentPage(const AValue: TmnCustomPage);
  protected
    FStream:TStream;
    function GetName: string; virtual;
    function DoCreatePage: TmnCustomPage; virtual;
    procedure PrintPage(vPage:TmnCustomPage); virtual;
  public
    constructor Create(Style: TmnPrintStyle; Stream:TStream); virtual;
    destructor Destroy; override;
    class function PrinterTitle: string; virtual;
    class function PrinterName: string; virtual;
    function CreatePage: TmnCustomPage;
    procedure BeginDocument; virtual;
    procedure EndDocument; virtual;
    procedure NewPage; virtual;
    procedure EndPage; virtual;
    //Print line feed until paper ejected
    procedure Eject; virtual;
    procedure Cut; virtual;
    procedure OpenDraw; virtual;
    procedure CloseDraw; virtual;
    procedure CancelPage; virtual;
    procedure AbortDocument; virtual;
    procedure Print(S: AnsiString);
    procedure PrintLn(S: AnsiString);
    property Bitmap:TBitmap read FBitmap;
    property Page:TmnCustomPage read FPage write SetCurrentPage;
    property Style:TmnPrintStyle read FStyle;
    property Density:TmnDensity read FDensity write FDensity;
    property Name:string read GetName;
    property DefaultWidth: Integer read FDefaultWidth write FDefaultWidth default 380;
    property DefaultHeight: Integer read FDefaultHeight write FDefaultHeight default 380;
  end;

  TmnPrinterClass = class of TmnPrinter;

  TmnRegisteredPrinter = class(TObject)
  public
    Name: string;
    Title: string;
    PrinterClass: TmnPrinterClass;
  end;

  { TmnRegisteredPrinters }

  TmnRegisteredPrinters = class(TObjectList)
  private
    function GetItems(Index: Integer): TmnRegisteredPrinter;
  public
    function Add(vPrinterClass: TmnPrinterClass): TmnRegisteredPrinter;
    function Find(vName: string): TmnRegisteredPrinter;
    function IndexOf(vName: string): Integer;
    function CreateByName(vName: string; vStyle: TmnPrintStyle; vStream: TStream): TmnPrinter;
    property Items[Index:Integer]: TmnRegisteredPrinter read GetItems; default;
  end;

function mnRegisteredPrinters: TmnRegisteredPrinters;

implementation

var
  FmnRegisteredPrinters: TmnRegisteredPrinters = nil;

function mnRegisteredPrinters: TmnRegisteredPrinters;
begin
  if FmnRegisteredPrinters = nil then
    FmnRegisteredPrinters := TmnRegisteredPrinters.Create;
  Result := FmnRegisteredPrinters;
end;

{ TmnPrinter }

constructor TmnPrinter.Create(Style:TmnPrintStyle; Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FStyle := Style;
  FDefaultWidth := 380;
  FDefaultHeight := 380;
end;

function TmnPrinter.GetName: string;
begin
  Result := '';
end;

procedure TmnPrinter.SetCurrentPage(const AValue: TmnCustomPage);
begin
  if Page <> AValue then
  begin
    if Page <> nil then
      EndPage;
    Page:=AValue;
  end;
end;

function TmnPrinter.DoCreatePage: TmnCustomPage;
begin
  Result := nil;
end;

procedure TmnPrinter.PrintPage(vPage: TmnCustomPage);
begin

end;

destructor TmnPrinter.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

class function TmnPrinter.PrinterTitle: string;
begin
  Result := '';
end;

class function TmnPrinter.PrinterName: string;
begin
  Result := '';
end;

function TmnPrinter.CreatePage: TmnCustomPage;
begin
  Result := DoCreatePage;
end;

procedure TmnPrinter.BeginDocument;
begin

end;

procedure TmnPrinter.EndDocument;
begin

end;

procedure TmnPrinter.NewPage;
begin
  FPage := CreatePage;
end;

procedure TmnPrinter.EndPage;
begin
  PrintPage(FPage);
  FPage := nil;
end;

procedure TmnPrinter.Eject;
begin
end;

procedure TmnPrinter.Cut;
begin
end;

procedure TmnPrinter.OpenDraw;
begin
end;

procedure TmnPrinter.CloseDraw;
begin
end;

procedure TmnPrinter.CancelPage;
begin
  FreeAndNil(FPage);
end;

procedure TmnPrinter.AbortDocument;
begin

end;

procedure TmnPrinter.Print(S: AnsiString);
begin
  if (FStream <> nil) and (S <> '') then
    FStream.Write(PChar(S)^, Length(s));
end;

procedure TmnPrinter.PrintLn(S: AnsiString);
begin
  Print(s + #13#10);
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

constructor TmnCustomPage.Create(Printer: TmnPrinter);
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


{ TmnRegisteredPrinters }

function TmnRegisteredPrinters.GetItems(Index: Integer): TmnRegisteredPrinter;
begin
  Result := inherited Items[Index] as TmnRegisteredPrinter;
end;

function TmnRegisteredPrinters.Add(vPrinterClass: TmnPrinterClass): TmnRegisteredPrinter;
begin
  Result := TmnRegisteredPrinter.Create;
  Result.Name := vPrinterClass.PrinterName;
  Result.Title := vPrinterClass.PrinterTitle;
  Result.PrinterClass := vPrinterClass;
  inherited Add(Result);
  mnDriversClasses.Add('Printers', 'SerialPrinters', 'Serial Printer', vPrinterClass);
end;

function TmnRegisteredPrinters.Find(vName: string): TmnRegisteredPrinter;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmnRegisteredPrinters.IndexOf(vName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count -1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TmnRegisteredPrinters.CreateByName(vName: string; vStyle: TmnPrintStyle; vStream: TStream): TmnPrinter;
var
  P: TmnRegisteredPrinter;
begin
  Result := nil;
  P := Find(vName);
  if P <> nil then
    Result := P.PrinterClass.Create(vStyle, vStream)
  else
    raise EmnPrinter.Create('Printer ' + vName + ' not found');
end;

end.

