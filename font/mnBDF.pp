unit mnBDF;
{*
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$modeswitch arrayoperators}
{$modeswitch advancedrecords}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  fpimage, fpwritepng, zstream;

type
  { TCodePoint }

  TCodePoint = record
    Code: Integer;       // Unicode codepoint (from ENCODING)
    Width: Integer;      // Advance width in pixels (DWIDTH x)
    BBXWidth: Integer;   // Glyph bounding box width
    BBXHeight: Integer;  // Glyph bounding box height
    BBXOffX: Integer;    // Glyph bounding box x-offset
    BBXOffY: Integer;    // Glyph bounding box y-offset (from baseline, negative = above)
    Bits: TBytes;        // Raw bitmap bits packed: 1 bit per pixel, MSB first, row-major
  end;
  PCodePoint = ^TCodePoint;

  { TBDF }

  TBDF = class
  private
    FCodePoints: array of TCodePoint;
    FCount: Integer;
    FBaseLine: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FPngStream: TMemoryStream; // PNG font atlas, ready for RayLib LoadFontFromImage
    FLoaded: Boolean;
    function GetCodePoints(Index: Integer): TCodePoint;
  protected
    procedure Clear;
    function ParseHexRow(const HexLine: string; BitWidth: Integer): TBytes;
    procedure BuildAtlasToStream(Stream: TStream);
  public
    destructor Destroy; override;
    // Load a BDF from a local file (read as binary into memory first)
    procedure LoadFromFile(const FileName: string);
    // Load a BDF from an already loaded binary memory stream
    procedure LoadFromStream(Stream: TStream);
    // Load a BDF from raw binary memory (AnsiString/RawByteString)
    procedure LoadFromMemory(const Data: RawByteString);
    // Encode loaded glyphs into a PNG font atlas memory stream (for RayLib)
    procedure EncodeToPNG;
    property Count: Integer read FCount;
    property CodePoints[Index: Integer]: TCodePoint read GetCodePoints;
    // Font pixel height (from FONTBOUNDINGBOX height or SIZE)
    property Height: Integer read FHeight;
    // Font advance width (from FONTBOUNDINGBOX width)
    property Width: Integer read FWidth;
    // Baseline offset in pixels from the top of the font cell (FONTBOUNDINGBOX y negated)
    property BaseLine: Integer read FBaseLine;
    // PNG font atlas as a memory stream (call EncodeToPNG first, or it is done on load)
    property PngStream: TMemoryStream read FPngStream;
    property Loaded: Boolean read FLoaded;
  end;

// Split a whitespace separated line into tokens.
function BDFToken(const Line: string; Index: Integer): string;

implementation

function BDFToken(const Line: string; Index: Integer): string;
var
  p, n, StartPos: Integer;
  ch: Char;
begin
  Result := '';
  p := 1;
  n := 0;
  // skip leading whitespace
  while (p <= Length(Line)) and (Line[p] in [' ', #9]) do
    Inc(p);
  StartPos := p;
  while p <= Length(Line) do
  begin
    ch := Line[p];
    if ch in [' ', #9] then
    begin
      if n = Index then
      begin
        Result := Copy(Line, StartPos, p - StartPos);
        Exit;
      end;
      Inc(n);
      // skip whitespace
      while (p <= Length(Line)) and (Line[p] in [' ', #9]) do
        Inc(p);
      StartPos := p;
    end
    else
      Inc(p);
  end;
  if n = Index then
    Result := Copy(Line, StartPos, p - StartPos);
end;

{ TBDF }

destructor TBDF.Destroy;
begin
  Clear;
  if Assigned(FPngStream) then
    FreeAndNil(FPngStream);
  inherited;
end;

procedure TBDF.Clear;
begin
  FCount := 0;
  FHeight := 0;
  FWidth := 0;
  FBaseLine := 0;
  FLoaded := False;
  SetLength(FCodePoints, 0);
end;

function TBDF.GetCodePoints(Index: Integer): TCodePoint;
begin
  Result := FCodePoints[Index];
end;

function TBDF.ParseHexRow(const HexLine: string; BitWidth: Integer): TBytes;
var
  i, ByteCount, MaxBytes: Integer;
  b: Integer;
  vHex: string;
begin
  // Each hex char pair = 1 byte. Bytes are MSB-first; bits within a byte are MSB-first.
  // The row is BitWidth bits wide -> ceil(BitWidth/8) bytes.
  ByteCount := (BitWidth + 7) div 8;
  SetLength(Result, ByteCount);
  FillChar(Result[0], ByteCount, 0);
  vHex := Trim(HexLine);
  MaxBytes := Length(vHex) div 2;
  if MaxBytes > ByteCount then
    MaxBytes := ByteCount;
  for i := 0 to MaxBytes - 1 do
  begin
    b := StrToInt('$' + Copy(vHex, i * 2 + 1, 2));
    Result[i] := Byte(b);
  end;
end;

procedure TBDF.LoadFromMemory(const Data: RawByteString);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    if Length(Data) > 0 then
      ms.Write(Data[1], Length(Data));
    ms.Position := 0;
    LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TBDF.LoadFromFile(const FileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(FileName);
    LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TBDF.LoadFromStream(Stream: TStream);
var
  Lines: TStringList;
  LineIndex: Integer;
  Line, Keyword: string;
  InChar: Boolean;
  InBitmap: Boolean;
  Glyph: TCodePoint;
  BitmapRows: TStringList;
  FontBBXWidth, FontBBXHeight, FontBBXOffX, FontBBXOffY: Integer;
  GlyphRowBytes: Integer;
  i: Integer;
  Row: TBytes;
begin
  Clear;
  Lines := TStringList.Create;
  BitmapRows := TStringList.Create;
  try
    Lines.LoadFromStream(Stream);

    FontBBXWidth := 0;
    FontBBXHeight := 0;
    FontBBXOffX := 0;
    FontBBXOffY := 0;

    InChar := False;
    InBitmap := False;
    FillChar(Glyph, SizeOf(Glyph), 0);

    for LineIndex := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[LineIndex]);
      if Line <> '' then
      begin
        Keyword := BDFToken(Line, 0);
        if not InChar then
        begin
          if Keyword = 'STARTFONT' then
            Continue
          else if Keyword = 'SIZE' then
            FHeight := StrToIntDef(BDFToken(Line, 1), 0)
          else if Keyword = 'FONTBOUNDINGBOX' then
          begin
            FontBBXWidth := StrToIntDef(BDFToken(Line, 1), 0);
            FontBBXHeight := StrToIntDef(BDFToken(Line, 2), 0);
            FontBBXOffX := StrToIntDef(BDFToken(Line, 3), 0);
            FontBBXOffY := StrToIntDef(BDFToken(Line, 4), 0);
          end
          else if Keyword = 'CHARS' then
            Continue
          else if Keyword = 'ENDFONT' then
            Break;
        end
        else
        begin
          // inside STARTCHAR .. ENDCHAR
          if Keyword = 'ENCODING' then
            Glyph.Code := StrToIntDef(BDFToken(Line, 1), 0)
          else if Keyword = 'DWIDTH' then
            Glyph.Width := StrToIntDef(BDFToken(Line, 1), 0)
          else if Keyword = 'BBX' then
          begin
            Glyph.BBXWidth := StrToIntDef(BDFToken(Line, 1), 0);
            Glyph.BBXHeight := StrToIntDef(BDFToken(Line, 2), 0);
            Glyph.BBXOffX := StrToIntDef(BDFToken(Line, 3), 0);
            Glyph.BBXOffY := StrToIntDef(BDFToken(Line, 4), 0);
          end
          else if Keyword = 'BITMAP' then
          begin
            InBitmap := True;
            BitmapRows.Clear;
          end
          else if Keyword = 'ENDCHAR' then
          begin
            InBitmap := False;
            // Pack bitmap rows into a contiguous byte array (MSB-first per row).
            if (Glyph.BBXWidth > 0) and (Glyph.BBXHeight > 0) and (BitmapRows.Count > 0) then
            begin
              GlyphRowBytes := (Glyph.BBXWidth + 7) div 8;
              SetLength(Glyph.Bits, GlyphRowBytes * BitmapRows.Count);
              for i := 0 to BitmapRows.Count - 1 do
              begin
                Row := ParseHexRow(BitmapRows[i], Glyph.BBXWidth);
                Move(Row[0], Glyph.Bits[i * GlyphRowBytes], Length(Row));
              end;
            end;
            FCount := FCount + 1;
            SetLength(FCodePoints, FCount);
            FCodePoints[FCount - 1] := Glyph;
            FillChar(Glyph, SizeOf(Glyph), 0);
            InChar := False;
          end
          else if InBitmap then
            BitmapRows.Add(Line);
        end;
        if Keyword = 'STARTCHAR' then
        begin
          InChar := True;
          InBitmap := False;
          FillChar(Glyph, SizeOf(Glyph), 0);
          BitmapRows.Clear;
        end;
      end;
    end;

    if FontBBXHeight > 0 then
      FHeight := FontBBXHeight;
    FWidth := FontBBXWidth;
    if FontBBXOffY < 0 then
      FBaseLine := FontBBXHeight + FontBBXOffY
    else
      FBaseLine := FontBBXHeight;

    FLoaded := True;
    EncodeToPNG;
  finally
    BitmapRows.Free;
    Lines.Free;
  end;
end;

procedure TBDF.EncodeToPNG;
begin
  if not FLoaded then
    Exit;
  if not Assigned(FPngStream) then
    FPngStream := TMemoryStream.Create;
  FPngStream.Clear;
  BuildAtlasToStream(FPngStream);
  FPngStream.Position := 0;
end;

procedure TBDF.BuildAtlasToStream(Stream: TStream);
var
  img: TFPMemoryImage;
  writer: TFPWriterPNG;
  x, y, i, cx, RowBytes, ByteIndex, BitInByte, BitValue, Mask, px, py: Integer;
  Glyph: TCodePoint;
  CellWidth, CellHeight, TotalWidth, RowStart, OriginY: Integer;
  FgColor, BgColor: TFPColor;
begin
  if FCount = 0 then
    Exit;

  // Atlas cell: each glyph in a cell of width = max(BBXWidth) and height = FHeight.
  CellHeight := FHeight;
  if CellHeight < 1 then
    CellHeight := 1;
  CellWidth := 0;
  for i := 0 to FCount - 1 do
    if FCodePoints[i].BBXWidth > CellWidth then
      CellWidth := FCodePoints[i].BBXWidth;
  if CellWidth = 0 then
    CellWidth := FWidth;
  if CellWidth < 1 then
    CellWidth := 1;
  TotalWidth := CellWidth * FCount;

  // RayLib font atlas: set pixels are black, unset are the magenta transparent key.
  FgColor.Red := $0000; FgColor.Green := $0000; FgColor.Blue := $0000; FgColor.Alpha := $FFFF;   // black
  BgColor.Red := $FFFF; BgColor.Green := $0000; BgColor.Blue := $FFFF; BgColor.Alpha := $FFFF;   // magenta key (transparent)

  img := TFPMemoryImage.Create(TotalWidth, CellHeight);
  try
    // background = transparent key (magenta)
    for y := 0 to CellHeight - 1 do
      for x := 0 to TotalWidth - 1 do
        img.Colors[x, y] := BgColor;
    for i := 0 to FCount - 1 do
    begin
      Glyph := FCodePoints[i];
      cx := i * CellWidth;
      if (Glyph.BBXWidth > 0) and (Glyph.BBXHeight > 0) and (Length(Glyph.Bits) > 0) then
      begin
        RowBytes := (Glyph.BBXWidth + 7) div 8;
        // BBXOffY is offset from the baseline; negative means the glyph extends above it.
        OriginY := CellHeight + Glyph.BBXOffY;
        for y := 0 to Glyph.BBXHeight - 1 do
        begin
          RowStart := y * RowBytes;
          for x := 0 to Glyph.BBXWidth - 1 do
          begin
            ByteIndex := RowStart + (x shr 3);
            if ByteIndex < Length(Glyph.Bits) then
            begin
              BitInByte := 7 - (x and 7);
              BitValue := Glyph.Bits[ByteIndex];
              Mask := (1 shl BitInByte);
              if (BitValue and Mask) <> 0 then
              begin
                px := cx + Glyph.BBXOffX + x;
                py := OriginY - y; // BDF bitmap rows run top-down from the top origin
                if (px >= 0) and (px < TotalWidth) and (py >= 0) and (py < CellHeight) then
                  img.Colors[px, py] := FgColor;
              end;
            end;
          end;
        end;
      end;
    end;

    writer := TFPWriterPNG.Create;
    try
      writer.CompressionLevel := cldefault;
      writer.UseAlpha := False;
      writer.GrayScale := False;
      img.SaveToStream(Stream, writer);
    finally
      writer.Free;
    end;
  finally
    img.Free;
  end;
end;

end.
