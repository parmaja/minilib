unit mncCSVExchanges;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface
{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

uses
  SysUtils, Variants, Classes, mnUtils,
  mncConnections, mncCsv, mncCommons;

type
  TFieldInfo = record
    Name: string;
    Param: TmncCustomField;
  end;

  TFieldsInfo = array of TFieldInfo;
  { TmncCSVIE }

  TmncCSVIE = class(TmncObject)
  private
    FCount: Integer;
    FCommand: TmncCommand;
    FStream: TStream;
    FActive: Boolean;
    FLimit: Integer;
    procedure SetCommand(const AValue: TmncCommand);
    procedure SetHeaderLine(const AValue: TmncCSVHeader);
    procedure SetStream(const AValue: TStream);
  protected
    procedure DoExecute; virtual; abstract;
  public
    CSVOptions: TmncCSVOptions;
    constructor Create;
    procedure Execute;

    property EndOfLine: string read CSVOptions.EndOfLine write CSVOptions.EndOfLine;
    property DelimiterChar: Char read CSVOptions.DelimiterChar write CSVOptions.DelimiterChar;
    property EscapeChar: Char read CSVOptions.EscapeChar write CSVOptions.EscapeChar default '\';
    property QuoteChar: Char read CSVOptions.QuoteChar write CSVOptions.QuoteChar default #0;// or " or '
    property HeaderLine: TmncCSVHeader read CSVOptions.HeaderLine write SetHeaderLine default hdrNormal;
    property ANSIContents: Boolean read CSVOptions.ANSIContents write CSVOptions.ANSIContents default False; //the stream is ANSI not UTF-8

    property Limit: Integer read FLimit write FLimit default 0; //Max count of rows to export or import

    property Count: Integer read FCount; //count of rows was exported or imported
    property Active: Boolean read FActive;
    property Command: TmncCommand read FCommand write SetCommand; //Requierd property
    property Stream: TStream read FStream write SetStream;
  end;

  { TmncCSVExport }

  //Commad must have result like 'select * from table'
  TmncCSVExport = class(TmncCSVIE)
  private
  protected
  public
    procedure DoExecute; override;
  end;

  { TmncCSVImport }

  TmncCSVImport = class(TmncCSVIE)
  private
    FBufferSize: Integer;
  protected
    procedure Apply(Fields: TFieldsInfo; Index:Integer; Value:string); virtual;
  public
    constructor Create;
    procedure DoExecute; override;
    property BufferSize: Integer read FBufferSize write FBufferSize;
  end;

implementation

{$ifdef FPC}
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: DWORD);
begin
  Move(Source^, Destination^, Length);
end;
{$endif}

{ TmncCSVImport }

procedure TmncCSVImport.Apply(Fields: TFieldsInfo; Index: Integer; Value: string);
begin
  if Value = '' then
    Fields[Index].Param.Clear
  else
  begin
    if ANSIContents then
      Fields[Index].Param.AsText := AnsiToUtf8(Value)
    else
      Fields[Index].Param.AsText := Value;
  end;
end;

constructor TmncCSVImport.Create;
begin
  inherited Create;
  FBufferSize := 4096;
end;

procedure TmncCSVImport.DoExecute;
var
  i: Integer;
  aEOL: Char;
  aBuffer: PChar;
  aPos: PChar;
  aEnd: PChar;
  function CheckEOF: Boolean;
  begin
    if not (aPos < aEnd) then
    begin
      aPos := aBuffer;
      aEnd := aPos + FStream.read(aBuffer^, BufferSize);
    end;
    Result := not (aPos < aEnd);
  end;

  procedure EatDelimiter(Delimiter: string);
  var
    i: Integer;
  begin
    i := 1;
    while (not CheckEOF) and (aPos^ = Delimiter[i]) do
    begin
      Inc(i);
      Inc(aPos);
    end;
  end;

  function GetColumn(var aColumn: string): Boolean;
  var
    P: PChar;
    i: Integer;
    InQuote: Boolean;
  begin
    aColumn := '';
    Result := False;
    InQuote := False;
    while not CheckEOF do
    begin
      P := aPos;
      while P < aEnd do
      begin
        if (QuoteChar <> #0) and (P^ = QuoteChar) then
         InQuote := not InQuote
        else if (not InQuote and (P^ = DelimiterChar)) or (P^ = aEOL) then
        begin
          InQuote := False;//if EOL
          i := Length(aColumn);
          SetLength(aColumn, i + (P - aPos));
          CopyMemory(@aColumn[i + 1], aPos, P - aPos);
          if QuoteChar <> #0 then
            aColumn := DequoteStr(aColumn, QuoteChar);
          if EscapeChar <> #0 then
            if QuoteChar <> #0 then
              aColumn := DescapeString(aColumn, EscapeChar, [#13, #10, #9 , #8, QuoteChar], ['r', 'n', 't', 'b', QuoteChar])
            else
              aColumn := DescapeString(aColumn, EscapeChar, [#13, #10, #9 , #8], ['r', 'n', 't', 'b']);
          aPos := P;
          if P^ = aEOL then
          begin
            EatDelimiter(EndOfLine);
            Result := False;
          end
          else
          begin
            EatDelimiter(DelimiterChar);
            Result := True;
          end;
          Exit;
        end;
        Inc(p);
      end;
      i := Length(aColumn);
      SetLength(aColumn, i + (P - aPos));
      CopyMemory(@aColumn[i + 1], aPos, P - aPos);
      aPos := P;
    end;
  end;

  procedure PassBOM;
  var
    P: PChar;
    i: Integer;
  begin
    P := aPos;
    i := 1;
    while (P < aEnd) and (i < 3) do
    begin
      if P^ <> sUTF8BOM[i] then
      begin
        break;
      end;
      Inc(P);
      Inc(i);
    end;
    if i = 3 then //BOM bytes founded
    begin
      Inc(P);
      aPos := P;
    end;
  end;

var
  s: string;
  EOL: Boolean;
  Fields: TFieldsInfo;
begin
  aEOL := EndOfLine[1];
  GetMem(aBuffer, BufferSize);
  try
    aPos := aBuffer;
    aEnd := aBuffer;
    EOL := False;
    i := 0;
    {$ifdef FPC}
    s := '';
    {$endif}
    if not Command.Prepared then
      Command.Prepare;

    if HeaderLine = hdrNormal then
    begin
      while not EOL do
      begin
        EOL := not GetColumn(s);
        SetLength(Fields, i + 1);
        Fields[i].Name := s;
        Fields[i].Param := Command.Params.FindParam(s);
        Inc(i);
      end;
    end
    else
    begin
      if HeaderLine = hdrIgnore then //eating the first line, Hummm
      begin
        while not EOL do
        begin
          EOL := not GetColumn(s);
          Inc(i);
        end;
      end;
      SetLength(Fields, Command.Params.Count);
      for i := 0 to Command.Params.Count - 1 do
      begin
        Fields[i].Name := Command.Params.Items[i].Name;
        Fields[i].Param := Command.Params.Items[i]
      end;
    end;

    try
      if not CheckEOF and not ANSIContents then
      begin
        PassBOM;
      end;

      while not CheckEOF do
      begin
        //seek to skip BOM for first time
        EOL := False;
        i := 0;
        while not EOL do
        begin
          EOL := not GetColumn(s);
          //Ignore extra columns found in CSV file
          if (i < Length(Fields)) and (Fields[i].Param <> nil) then
          begin
            Apply(Fields, i, s);
          end;
          Inc(i);
        end;
        FCommand.Execute;
        Inc(FCount);
      end;
    except
      raise;
    end;
  finally
    FreeMem(aBuffer);
  end;
end;

{ TmncCSVExport }

procedure TmncCSVExport.DoExecute;
var
  i: Integer;
  s, st: string;
begin
  FCount := 0;
  try
    Command.Execute;
    if not Command.NextOnExecute then
      Command.Next;
    st := '';
    if HeaderLine = hdrNormal then
    begin
      for i := 0 to Command.Columns.Count - 1 do
      begin
        if i > 0 then
          st := st + DelimiterChar;
        s := Command.Columns.Items[i].Name;
        if QuoteChar <> #0 then
          s := QuoteStr(s, QuoteChar);
        st := st + s;
      end;
      st := st + EndOfLine;
      FStream.Write(st[1], Length(st));
    end;

    while not Command.Done do
    begin
      Inc(FCount);
      st := '';
      for i := 0 to Command.Fields.Count - 1 do
      begin
        if i > 0 then
          st := st + DelimiterChar;
        s := Command.Fields.Items[i].AsText;
        if EscapeChar <> #0 then
          if QuoteChar <> #0 then
            s := EscapeString(s, EscapeChar, [#13, #10, #9 , #8, QuoteChar], ['r', 'n', 't', 'b', QuoteChar])
          else
            s := EscapeString(s, EscapeChar, [#13, #10, #9 , #8], ['r', 'n', 't', 'b']);
        if QuoteChar <> #0 then
          s := QuoteStr(s, QuoteChar);
        st := st + s;
      end;
      st := st + EndOfLine;
      FStream.Write(st[1], Length(st));
      Command.Next;
    end;
  finally
  end;
end;

{ TmncCSVIE }

procedure TmncCSVIE.SetCommand(const AValue: TmncCommand);
begin
  if FCommand <> AValue then
  begin
    if (FCommand <> nil) and FCommand.Active then
      raise EmncException.Create('Import/Export: Command is active');
    FCommand := AValue;
  end;
end;

procedure TmncCSVIE.SetHeaderLine(const AValue: TmncCSVHeader);
begin
  if CSVOptions.HeaderLine <> AValue then
  begin
    if Active then
      raise EmncException.Create('Import/Export: Active, can not set Header property');
    CSVOptions.HeaderLine := AValue;
  end;
end;

procedure TmncCSVIE.SetStream(const AValue: TStream);
begin
  if FStream <> AValue then
  begin
    if Active then
      raise EmncException.Create('Import/Export: Active, can not set Stream property');
    FStream := AValue;
  end;
end;

constructor TmncCSVIE.Create;
begin
  inherited Create;
  CSVOptions.EndOfLine := #13;
  CSVOptions.DelimiterChar := ';';
  CSVOptions.EscapeChar := '/';
  CSVOptions.HeaderLine := hdrNormal;
end;

procedure TmncCSVIE.Execute;
begin
  if FStream = nil then
    raise EmncException.Create('Import/Export: Stream is not set');
  if FCommand = nil then
    raise EmncException.Create('Import/Export: Command is not');
  FActive := True;
  try
    DoExecute;
  finally
    FActive := True;
  end;
end;

initialization
end.

