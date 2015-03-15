unit mnWinCEPrinters;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef win32}
  Forms,
  {$endif}
  Windows, Classes, Graphics, SysUtils, registry, Forms;

type
  { TmnWincePrinter }

  TmnWincePrinter = class
  private
    dm: PDEVMODE;
    dc: HDC;
    {$ifdef win32}
    {$else}
    di: DOCINFO;
    {$endif}

  protected
    procedure DirectToPrinter(S: AnsiString);
    procedure DrawString(Text: widestring; Alignment: TAlignment);
    function GetWidth: DWORD;
  public
    PosY: Integer;
    Title: widestring;
    PrinterName: widestring;
    FontName: widestring;
    Width: DWORD;
    PageHeight: DWord;
    FontHeight: Integer;
    FontBold: Boolean;
    LineMargin: Integer;
    procedure Start;
    procedure Stop;
    procedure Escape(Text: AnsiString);
    procedure Print(Text: widestring; Alignment: TAlignment = taCenter);
    procedure Print(x, y: Integer; Bitmap: TBitmap);
    constructor Create(AName, ATitle: string); virtual;
    destructor Destroy; override;
  end;

{$ifdef win32}
var
  PrinterOut: TForm = nil;
{$endif}

{*
   For COM Printers
*}
const
  sPrinterName = 'APS_UC05';

  APS_ID			= $1001;
  APS_STATUS	=	$1002;
  APS_NEOP		= $1003;

{$ifdef WINCE}
const
  printer_dll = 'prnport.dll';

function PrinterOpen(sPort: PWideChar): HANDLE; stdcall; external printer_dll;
function PrinterSend(hPrint: DWORD; lpBuffer: LPCVOID; dwBytes: DWORD): BOOL; stdcall; external printer_dll;
procedure PrinterClose(hPrint: DWORD); stdcall; external printer_dll;
function GetPrinterInfo(hPrint: HANDLE; Opt: integer;  pBuf: Pointer; var pBufLen: Integer): DWORD; stdcall; external printer_dll;
{$else}
function PrinterOpen(sPort: PWideChar): HANDLE;
function PrinterSend(hPrint: DWORD; lpBuffer: LPCVOID; dwBytes: DWORD): BOOL;
procedure PrinterClose(hPrint: DWORD);
function GetPrinterInfo(hPrint: HANDLE; Opt: integer;  pBuf: Pointer; pBufLen: PInteger): DWORD;
{$endif}

implementation

function TmnWincePrinter.GetWidth: DWORD;
var
  reg: TRegistry;
begin
  {$ifdef win32}
  Result := 370;
  {$else}
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('Printers\\' + PrinterName, False) then
    begin
      result := reg.ReadInteger('APS_Dots');
    end
    else
      result := 200;
    reg.CloseKey;
  finally
    reg.free;
  end;
  {$endif}
end;

procedure TmnWincePrinter.Start;
var
  WhitePen: HPEN;
  OldPen: HGDIOBJ;
begin
  PosY := 0;
  {$ifdef win32}
  dc := PrinterOut.Canvas.Handle;
  {$else}
  GetMem(dm, sizeof(DEVMODE));
  dm^.dmSize := sizeof(DEVMODE);
  dm^.dmDeviceName := PrinterName;

  dc := CreateDC( 'APS_pcl.dll', nil, nil, dm);

  if (dc > 0) then
  begin
    di.cbSize := sizeof(DOCINFO);
    di.lpszDocName := PWideChar(Title);
    di.lpszOutput := nil;
    di.lpszDatatype := nil;
    di.fwType := 0;

    if (StartDoc(dc, di) > 0) then
    begin
      if StartPage(dc) > 0 then
      begin
        //Draw a white rectangle for the page.
        WhitePen := CreatePen(PS_SOLID, 1, RGB(255,255,255));
        OldPen := SelectObject(dc, WhitePen);
        Rectangle(dc, 0, 0, Width, PageHeight); //page heigh defined at 20cm
        SelectObject(dc, OldPen);
      end;
    end;
  end
  else
    RaiseLastOSError;
  {$endif}
end;

procedure TmnWincePrinter.Stop;
begin
  {$ifdef win32}
  dc := 0;
  {$else}
  EndPage(dc);
  EndDoc(dc);
  DeleteDC(dc);
  FreeMem(dm);
  dc := 0;
  dm := nil;
  {$endif}
end;


type
  TPassThrough = packed record
    Len: Word;
    Buff: array[0..255] of AnsiChar;
end;

//https://stackoverflow.com/questions/18605078/zebra-printer-direct-communication

procedure TmnWincePrinter.DirectToPrinter(S: AnsiString);
begin
  if ExtEscape(dc, PASSTHROUGH, Length(S), PChar(S), 0, nil) < 0 then
    raise Exception.Create('Error at printing to printer');
end;

procedure TmnWincePrinter.Escape(Text: AnsiString);
begin
  DirectToPrinter(Text);
end;

procedure TmnWincePrinter.DrawString(Text: widestring; Alignment: TAlignment);
var
  //WhitePen: HPEN;
  //OldPen: HGDIOBJ;
  lf: LOGFONT;
  oldfnt, fnt: HFONT;
  i: integer;
  Rect: TRect;
  DT: UInt;
begin
    //WhitePen := CreatePen(PS_SOLID, 1, RGB(255,255,255));
    //OldPen := SelectObject(dc, WhitePen);

    lf.lfHeight := FontHeight;
    lf.lfWidth := 0;
    lf.lfEscapement := 0;
    lf.lfOrientation := 0;
    if FontBold then
      lf.lfWeight := FW_BOLD
    else
      lf.lfWeight := FW_NORMAL;
    lf.lfItalic := 0;
    lf.lfUnderline:=0;
    lf.lfStrikeOut:=0;
    lf.lfCharSet := DEFAULT_CHARSET;
    lf.lfOutPrecision := OUT_DEFAULT_PRECIS;
    lf.lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lf.lfQuality := DEFAULT_QUALITY;
    lf.lfPitchAndFamily := DEFAULT_PITCH;
    lf.lfFaceName := FontName+#0;
                             //fre3of9x.ttf
    fnt := CreateFontIndirect(@lf);
    oldfnt := SelectObject(dc, fnt);

    Rect.left := 0;
    Rect.top := PosY;
    Rect.right := Width;
    Rect.bottom := PosY + 200;

    SetTextColor(dc, 0);
    DT :=  DT_SINGLELINE;
    if Alignment = taCenter then
      DT := DT or DT_CENTER
    else if Alignment = taRightJustify then
      DT := DT or DT_CENTER
    else
      DT := DT or DT_LEFT;
    PosY := LineMargin + PosY + DrawTextW(dc, PWideChar(Text), Length(Text), Rect, DT) ;

    //SelectObject(dc, OldPen);
    SelectObject(dc, OldFnt);

    //DeleteObject(WhitePen);
end;


procedure TmnWincePrinter.Print(Text: widestring; Alignment: TAlignment);
begin
  DrawString(Text, Alignment);
  Application.ProcessMessages;
end;

procedure TmnWincePrinter.Print(x, y: Integer; Bitmap: TBitmap);
begin
  BitBlt(dc, x, PosY + y, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  PosY := PosY + Bitmap.Height + LineMargin;
  Application.ProcessMessages;
end;

constructor TmnWincePrinter.Create(AName, ATitle: string);
begin
  inherited Create;
  FontName := 'Arial';
  PrinterName := AName;
  Title := ATitle;
  Width := GetWidth;
  PageHeight :=  8 * 8;//5cm
  FontHeight := 16;
  LineMargin := 2;
  {$ifdef win32}
  if PrinterOut = nil then
  begin
    PrinterOut := TForm.CreateNew(Application);
    PrinterOut.Show;
    PrinterOut.DoubleBuffered := true;
    PrinterOut.ClientWidth := Width;
    PrinterOut.ClientHeight := 800;
  end;
  {$endif}
end;

destructor TmnWincePrinter.Destroy;
begin
  inherited Destroy;
end;

{*

*}

{$ifdef WINCE}
{$else}
function PrinterOpen(sPort: PWideChar): HANDLE;
begin
  result := 10;
end;

function PrinterSend(hPrint: DWORD; lpBuffer: LPCVOID; dwBytes: DWORD): BOOL;
begin
end;

procedure PrinterClose(hPrint: DWORD);
begin
end;

function GetPrinterInfo(hPrint: HANDLE; Opt: integer;  pBuf: Pointer; pBufLen: PInteger): DWORD;
begin
end;
{$endif}

end.

