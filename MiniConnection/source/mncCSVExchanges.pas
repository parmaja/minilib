unit mncCSVExchanges;
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}
interface

uses
  SysUtils, Variants, Classes, mncConnections, mnUtils;

type

  { TmncCSVIE }

  TmncCSVIE = class(TmncObject)
  private
    FEndOfLine: string;
    FSeparator: Char;
  public
    constructor Create;
    property EndOfLine: string read FEndOfLine write FEndOfLine;
    property Separator: Char read FSeparator write FSeparator;
  end;

  { TmncCSVExport }

  TmncCSVExport = class(TmncCSVIE)
  private
    FCount: Integer;
    FFileName: string;
    procedure SetCommand(const AValue: TmncCommand);
  protected
    FCommand: TmncCommand;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    //Commad must have result like 'select * from table'
    property Command: TmncCommand read FCommand write SetCommand;
    property FileName: string read FFileName write FFileName;
    property Count: Integer read FCount;
  end;

  TmncCSVImport = class(TmncCSVIE)
  private
    FFileName: string;
  protected
    FCommand: TmncCommand;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property Command: TmncCommand read FCommand;
    property FileName: string read FFileName write FFileName;
  end;

implementation

{$ifdef FPC}
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: DWORD);
begin
  Move(Source^, Destination^, Length);
end;
{$endif}

{ TmncCSVImport }

constructor TmncCSVImport.Create;
begin
  inherited;
end;

destructor TmncCSVImport.Destroy;
begin
  inherited;
end;

type
  TFieldInfo = record
    Name: string;
    Ignored: Boolean;
    Key: Boolean;
    Attributes: string;
    Field: TmncRecordField;
  end;

  TFieldsInfo = array of TFieldInfo;

procedure TmncCSVImport.Execute;
const
  sBufferSize = 2048;
var
  i: Integer;
  aEOL: Char;
  aFile: TFileStream;
  aBuffer: PChar;
  aPos: PChar;
  aEnd: PChar;
  function CheckEOF: Boolean;
  begin
    if not (aPos < aEnd) then
    begin
      aPos := aBuffer;
      aEnd := aPos + aFile.read(aBuffer^, sBufferSize);
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
  begin
    aColumn := '';
    Result := False;
    while not CheckEOF do
    begin
      P := aPos;
      while P < aEnd do
      begin
        if P^ in [Separator, aEOL] then
        begin
          i := Length(aColumn);
          SetLength(aColumn, i + (P - aPos));
          CopyMemory(@aColumn[i + 1], aPos, P - aPos);
          aPos := P;
          if P^ = aEOL then
          begin
            EatDelimiter(EndOfLine);
            Result := False;
          end
          else
          begin
            EatDelimiter(Separator);
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
var
  s: string;
  EOL: Boolean;
  Fields: TFieldsInfo;
begin
  aEOL := EndOfLine[1];
  aFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  GetMem(aBuffer, sBufferSize);
  try
    aPos := aBuffer;
    aEnd := aBuffer;
    EOL := False;
    i := 0;
    {$ifdef FPC}
    s := '';
    {$endif}
    while not EOL do
    begin
      EOL := not GetColumn(s);
      SetLength(Fields, i + 1);
      Fields[i].Name := s;
      Inc(i);
    end;

    Command.Prepare;

    for i := 0 to Length(Fields) - 1 do
    begin
      Fields[i].Field := Command.Params.ParamByName(Fields[i].Name);
    end;

    try
      while not CheckEOF do
      begin
        EOL := False;
        i := 0;
        while not EOL do
        begin
          EOL := not GetColumn(s);
          if s = '' then
            Fields[i].Field.Clear
          else
            Fields[i].Field.AsText := s;
          Inc(i);
        end;
        FCommand.Execute;
      end;
      //Command.Session.Commit
    except
      //Command.Session.Rollback;
      raise;
    end;
  finally
    FreeMem(aBuffer);
    aFile.Free;
  end;
end;

{ TmncCSVExport }

procedure TmncCSVExport.SetCommand(const AValue: TmncCommand);
begin
  if FCommand <> AValue then
  begin
    if (FCommand <> nil) and FCommand.Active then
      raise EmncException.Create('Export: Command is active');
    FCommand := AValue;
  end;
end;

constructor TmncCSVExport.Create;
begin
  inherited;
end;

destructor TmncCSVExport.Destroy;
begin

  inherited;
end;

procedure TmncCSVExport.Execute;
var
  i: Integer;
  aFile: TFileStream;
  st: string;
begin
  FCount := 0;
  aFile := TFileStream.Create(FFileName, fmCreate or fmShareExclusive);
  try
    Command.Execute;
    if not Command.NextOnExecute then
      Command.Next;
    st := '';
    for i := 0 to Command.Current.Count - 1 do
    begin
      if i > 0 then
        st := st + Separator;
      st := st + Command.Current.Fields[i].Name;
    end;
    st := st + EndOfLine;
    aFile.Write(st[1], Length(st));

    while not Command.Eof do
    begin
      Inc(FCount);
      st := '';
      for i := 0 to Command.Current.Count - 1 do
      begin
        if i > 0 then
          st := st + Separator;
        st := st + EscapeString(Command.Current.Items[i].AsText, '\', [#13, #10, #9 , #8, '"'], ['r', 'n', 't', 'b', '"']);
      end;
      st := st + EndOfLine;
      aFile.Write(st[1], Length(st));
      Command.Next;
    end;
  finally
    aFile.Free;
  end;
end;

{ TmncCSVIE }

constructor TmncCSVIE.Create;
begin
  inherited Create;
  FEndOfLine := #13;
  FSeparator := ';';
end;

initialization
end.

