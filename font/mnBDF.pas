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
  {$IFDEF FPC}
  fpimage, fpwritepng, zstream
  {$ELSE}
  Graphics, PNGImage
  {$ENDIF};

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
    //FPngStream: TMemoryStream; // PNG font atlas, ready for RayLib LoadFontFromImage
    FLoaded: Boolean;
    function GetCodePoints(Index: Integer): TCodePoint;
  protected
    procedure Clear;
    function ParseHexRow(const HexLine: string; BitWidth: Integer): TBytes;
  public
    destructor Destroy; override;
    procedure BuildAtlasToStream(Stream: TStream);
    procedure BuildXNAToStream(Stream: TStream);
    // Load a BDF from a local file (read as binary into memory first)
    procedure LoadFromFile(const FileName: string);
    // Load a BDF from an already loaded binary memory stream
    procedure LoadFromStream(Stream: TStream);
    // Load a BDF from raw binary memory (AnsiString/RawByteString)
    procedure LoadFromMemory(const Data: RawByteString);
    // Encode loaded glyphs into a PNG font atlas memory stream (for RayLib)
    // PNG font atlas as a memory stream
    function EncodeToPNG: TMemoryStream;
    property Count: Integer read FCount;
    property CodePoints[Index: Integer]: TCodePoint read GetCodePoints;
    // Font pixel height (from FONTBOUNDINGBOX height or SIZE)
    property Height: Integer read FHeight;
    // Font advance width (from FONTBOUNDINGBOX width)
    property Width: Integer read FWidth;
    // Baseline offset in pixels from the top of the font cell (FONTBOUNDINGBOX y negated)
    property BaseLine: Integer read FBaseLine;
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
  finally
    BitmapRows.Free;
    Lines.Free;
  end;
end;

function TBDF.EncodeToPNG: TMemoryStream;
begin
  if not FLoaded then
    Exit;
  Result := TMemoryStream.Create;
  BuildXNAToStream(Result);
  Result.Position := 0;
end;

procedure TBDF.BuildAtlasToStream(Stream: TStream);
const
  cCharsPerRow = 32;
var
  {$IFDEF FPC}
  img: TFPMemoryImage;
  writer: TFPWriterPNG;
  FgColor, BgColor: TFPColor;
  {$ELSE}
  bmp: TBitmap;
  png: TPngImage;
  {$ENDIF}
  x, y, i, cx, cy, RowBytes, ByteIndex, BitInByte, BitValue, Mask, px, py: Integer;
  Glyph: TCodePoint;
  CellWidth, CellHeight, TotalWidth, TotalHeight, RowStart, BaseLineY, Rows: Integer;
begin
  if FCount = 0 then
    Exit;

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

  Rows := (FCount + cCharsPerRow - 1) div cCharsPerRow;
  if Rows < 1 then
    Rows := 1;
  TotalWidth := cCharsPerRow * CellWidth;
  TotalHeight := Rows * CellHeight;

  BaseLineY := 0;
  for i := 0 to FCount - 1 do
  begin
    y := FCodePoints[i].BBXOffY + FCodePoints[i].BBXHeight - 1;
    if y > BaseLineY then
      BaseLineY := y;
  end;

  {$IFDEF FPC}
  FgColor.Red := $0000; FgColor.Green := $0000; FgColor.Blue := $0000; FgColor.Alpha := $FFFF;
  BgColor.Red := $FFFF; BgColor.Green := $0000; BgColor.Blue := $FFFF; BgColor.Alpha := $FFFF;

  img := TFPMemoryImage.Create(TotalWidth, TotalHeight);
  try
    for py := 0 to TotalHeight - 1 do
      for px := 0 to TotalWidth - 1 do
        img.Colors[px, py] := BgColor;
    for i := 0 to FCount - 1 do
    begin
      Glyph := FCodePoints[i];
      cx := (i mod cCharsPerRow) * CellWidth;
      cy := (i div cCharsPerRow) * CellHeight;
      if (Glyph.BBXWidth > 0) and (Glyph.BBXHeight > 0) and (Length(Glyph.Bits) > 0) then
      begin
        RowBytes := (Glyph.BBXWidth + 7) div 8;
        for y := 0 to Glyph.BBXHeight - 1 do
        begin
          RowStart := y * RowBytes;
          py := cy + BaseLineY - (Glyph.BBXOffY + Glyph.BBXHeight - 1) + y;
          if (py >= cy) and (py < cy + CellHeight) then
          begin
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
                  if (px >= cx) and (px < cx + CellWidth) then
                    img.Colors[px, py] := FgColor;
                end;
              end;
            end;
          end;
        end;
      end;
    end;

    writer := TFPWriterPNG.Create;
    try
      writer.CompressionLevel := clDefault;
      writer.UseAlpha := False;
      writer.WordSized := False;
      writer.GrayScale := False;
      img.SaveToStream(Stream, writer);
    finally
      writer.Free;
    end;
  finally
    img.Free;
  end;
  {$ELSE}
  bmp := TBitmap.Create;
  try
    bmp.SetSize(TotalWidth, TotalHeight);
    bmp.PixelFormat := pf24bit;
    bmp.Canvas.Brush.Color := clFuchsia;
    bmp.Canvas.FillRect(Rect(0, 0, TotalWidth, TotalHeight));
    for i := 0 to FCount - 1 do
    begin
      Glyph := FCodePoints[i];
      cx := (i mod cCharsPerRow) * CellWidth;
      cy := (i div cCharsPerRow) * CellHeight;
      if (Glyph.BBXWidth > 0) and (Glyph.BBXHeight > 0) and (Length(Glyph.Bits) > 0) then
      begin
        RowBytes := (Glyph.BBXWidth + 7) div 8;
        for y := 0 to Glyph.BBXHeight - 1 do
        begin
          RowStart := y * RowBytes;
          py := cy + BaseLineY - (Glyph.BBXOffY + Glyph.BBXHeight - 1) + y;
          if (py >= cy) and (py < cy + CellHeight) then
          begin
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
                  if (px >= cx) and (px < cx + CellWidth) then
                    bmp.Canvas.Pixels[px, py] := clBlack;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    png := TPngImage.Create;
    try
      png.Assign(bmp);
      png.SaveToStream(Stream);
    finally
      png.Free;
    end;
  finally
    bmp.Free;
  end;
  {$ENDIF}
end;

procedure TBDF.BuildXNAToStream(Stream: TStream);
const
  cSpacing = 1;
  cFirstChar = 32;
  cGlyphCount = 95;
  cColumns = 21;
var
  {$IFDEF FPC}
  img: TFPMemoryImage;
  writer: TFPWriterPNG;
  KeyColor, CellColor, MarkColor: TFPColor;
  {$ELSE}
  bmp: TBitmap;
  png: TPngImage;
  {$ENDIF}
  x, y, i, px, py, RowBytes, ByteIndex, BitInByte, BitValue, Mask, Rows: Integer;
  Code, Index, CellWidth, CellHeight, TotalWidth, TotalHeight, BaseLineY, OffY: Integer;
  CodeIndex: array[0..255] of Integer;
  Glyph: TCodePoint;
  CellX, CellY: Integer;
begin
  if FCount = 0 then
    Exit;

  CellWidth := FWidth;
  for i := 0 to FCount - 1 do
    if FCodePoints[i].BBXWidth > CellWidth then
      CellWidth := FCodePoints[i].BBXWidth;
  if CellWidth < 1 then
    CellWidth := 1;

  CellHeight := FHeight;
  if CellHeight < 1 then
    CellHeight := 1;

  Rows := (cGlyphCount + cColumns - 1) div cColumns;
  if Rows < 1 then
    Rows := 1;

  TotalWidth := cSpacing + (CellWidth + cSpacing) * cColumns;
  TotalHeight := cSpacing + (CellHeight + cSpacing) * Rows;

  BaseLineY := 0;
  for i := 0 to FCount - 1 do
  begin
    y := FCodePoints[i].BBXOffY + FCodePoints[i].BBXHeight - 1;
    if y > BaseLineY then
      BaseLineY := y;
  end;

  for i := 0 to 255 do
    CodeIndex[i] := -1;
  for i := 0 to FCount - 1 do
    if (FCodePoints[i].Code >= 0) and (FCodePoints[i].Code <= 255) then
      CodeIndex[FCodePoints[i].Code] := i;

  {$IFDEF FPC}
  KeyColor.Red := $FFFF; KeyColor.Green := $0000; KeyColor.Blue := $FFFF; KeyColor.Alpha := $FFFF;
  CellColor.Red := $0000; CellColor.Green := $0000; CellColor.Blue := $0000; CellColor.Alpha := $0000;
  MarkColor.Red := $FFFF; MarkColor.Green := $FFFF; MarkColor.Blue := $FFFF; MarkColor.Alpha := $FFFF;

  img := TFPMemoryImage.Create(TotalWidth, TotalHeight);
  try
    img.UsePalette := True;
    for py := 0 to TotalHeight - 1 do
      for px := 0 to TotalWidth - 1 do
        img.Colors[px, py] := KeyColor;

    for Code := 0 to cGlyphCount - 1 do
    begin
      CellX := cSpacing + (Code mod cColumns) * (CellWidth + cSpacing);
      CellY := cSpacing + (Code div cColumns) * (CellHeight + cSpacing);

      for py := CellY to CellY + CellHeight - 1 do
        for px := CellX to CellX + CellWidth - 1 do
          img.Colors[px, py] := CellColor;

      Index := CodeIndex[cFirstChar + Code];
      if Index < 0 then
        Continue;
      Glyph := FCodePoints[Index];
      if (Glyph.BBXWidth <= 0) or (Glyph.BBXHeight <= 0) or (Length(Glyph.Bits) = 0) then
        Continue;

      RowBytes := (Glyph.BBXWidth + 7) div 8;
      OffY := BaseLineY - (Glyph.BBXOffY + Glyph.BBXHeight - 1);
      for y := 0 to Glyph.BBXHeight - 1 do
      begin
        py := CellY + OffY + y;
        if (py < CellY) or (py >= CellY + CellHeight) then
          Continue;
        for x := 0 to Glyph.BBXWidth - 1 do
        begin
          ByteIndex := y * RowBytes + (x shr 3);
          if ByteIndex >= Length(Glyph.Bits) then
            Continue;
          BitInByte := 7 - (x and 7);
          BitValue := Glyph.Bits[ByteIndex];
          Mask := 1 shl BitInByte;
          if (BitValue and Mask) <> 0 then
          begin
            px := CellX + Glyph.BBXOffX + x;
            if (px >= CellX) and (px < CellX + CellWidth) then
              img.Colors[px, py] := MarkColor;
          end;
        end;
      end;
    end;

    writer := TFPWriterPNG.Create;
    try
      writer.CompressionLevel := clDefault;
      writer.Indexed := True;
      writer.UseAlpha := True;
      writer.WordSized := False;
      writer.GrayScale := False;
      img.SaveToStream(Stream, writer);
    finally
      writer.Free;
    end;
  finally
    img.Free;
  end;
  {$ELSE}
  bmp := TBitmap.Create;
  try
    bmp.SetSize(TotalWidth, TotalHeight);
    bmp.PixelFormat := pf24bit;
    bmp.Canvas.Brush.Color := clFuchsia;
    bmp.Canvas.FillRect(Rect(0, 0, TotalWidth, TotalHeight));

    for Code := 0 to cGlyphCount - 1 do
    begin
      CellX := cSpacing + (Code mod cColumns) * (CellWidth + cSpacing);
      CellY := cSpacing + (Code div cColumns) * (CellHeight + cSpacing);

      bmp.Canvas.Brush.Color := clBlack;
      bmp.Canvas.FillRect(Rect(CellX, CellY, CellX + CellWidth, CellY + CellHeight));

      Index := CodeIndex[cFirstChar + Code];
      if Index < 0 then
        Continue;
      Glyph := FCodePoints[Index];
      if (Glyph.BBXWidth <= 0) or (Glyph.BBXHeight <= 0) or (Length(Glyph.Bits) = 0) then
        Continue;

      RowBytes := (Glyph.BBXWidth + 7) div 8;
      OffY := BaseLineY - (Glyph.BBXOffY + Glyph.BBXHeight - 1);
      for y := 0 to Glyph.BBXHeight - 1 do
      begin
        py := CellY + OffY + y;
        if (py < CellY) or (py >= CellY + CellHeight) then
          Continue;
        for x := 0 to Glyph.BBXWidth - 1 do
        begin
          ByteIndex := y * RowBytes + (x shr 3);
          if ByteIndex >= Length(Glyph.Bits) then
            Continue;
          BitInByte := 7 - (x and 7);
          BitValue := Glyph.Bits[ByteIndex];
          Mask := 1 shl BitInByte;
          if (BitValue and Mask) <> 0 then
          begin
            px := CellX + Glyph.BBXOffX + x;
            if (px >= CellX) and (px < CellX + CellWidth) then
              bmp.Canvas.Pixels[px, py] := clWhite;
          end;
        end;
      end;
    end;

    png := TPngImage.Create;
    try
      png.Assign(bmp);
      png.CreateAlpha;
      // Set alpha: magenta key and white marks = opaque, cell black = transparent
      for py := 0 to TotalHeight - 1 do
      begin
        for px := 0 to TotalWidth - 1 do
        begin
          if bmp.Canvas.Pixels[px, py] = clBlack then
            png.AlphaScanline[py][px] := 0
          else
            png.AlphaScanline[py][px] := 255;
        end;
      end;
      png.SaveToStream(Stream);
    finally
      png.Free;
    end;
  finally
    bmp.Free;
  end;
  {$ENDIF}
end;

end.
