unit mncCSV;
{$M+}{$H+}{$IFDEF FPC}{$MODE delphi}{$ENDIF}
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

interface

uses
  Classes, SysUtils, Variants, IniFiles,
  mnUtils, mncConnections, mnStreams;

type
  {
    hdrNone: There is no header in export and import files
    hdrNormal: Header is the first line contain field names
    hdrIgnore: Header found for import files but ignored, header not exported
  }
  TmncCSVHeader = (
    hdrNone,
    hdrSkip,
    hdrNormal
  );

  TmncEmptyLine = (
    elFetch, //Load it
    elSkip, //Ignore it
    elDone //End of the file
  );

  { TmncCSVOptions }

  TmncCSVOptions = record
    EndOfLine: string;
    DelimiterChar: Char;
    EscapeChar: Char; //TODO take it from mncCSVExchanges
    QuoteChar: Char;
    SkipColumn: Integer;
    HeaderLine: TmncCSVHeader;
    ANSIContents: Boolean; //TODO take it from mncCSVExchanges
    SkipEmptyLines: Boolean;
    procedure SaveToIni(Section: string; ini: TIniFile);
    procedure LoadFromIni(Section: string; ini: TIniFile);
  end;


  { TmncCSVConnection }

  TmncCSVConnection = class(TmncConnection)
  private
    FConnected: Boolean;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
  public
    class function Capabilities: TmncCapabilities; override;
    class function EngineName: string; override;
    constructor Create; override;
  end;

  { TmncCSVSession }

  TmncCSVSession = class(TmncSession)
  private
    FCSVOptions: TmncCSVOptions;
  protected
    procedure DoStart; override;
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); override;
  public
    constructor Create(vConnection: TmncConnection); override;
    property CSVOptions: TmncCSVOptions read FCSVOptions write FCSVOptions;
    property DelimiterChar: Char read FCSVOptions.DelimiterChar write FCSVOptions.DelimiterChar default #9;
    property QuoteChar: Char read FCSVOptions.QuoteChar write FCSVOptions.QuoteChar default '"';
    property EndOfLine: string read FCSVOptions.EndOfLine write FCSVOptions.EndOfLine;
    property HeaderLine: TmncCSVHeader read FCSVOptions.HeaderLine write FCSVOptions.HeaderLine default hdrNormal;
  end;

  TmncCSVMode = (csvmRead, csvmWrite);

  { TmncCSVCommand }

  TmncCSVCommand = class(TmncCommand)
  private
    FEmptyLine: TmncEmptyLine;
    FBeginOfStream: Boolean;
    FCSVStream: TmnWrapperStream;
    FStream: TStream;
    FMode: TmncCSVMode;
    function GetConnection: TmncCSVConnection;
    function GetSession: TmncCSVSession;
  protected
    procedure DoParse; override;
    function CreateFields(vColumns: TmncColumns): TmncFields; override;
    function CreateParams: TmncParams; override;
    procedure PrepareParams;
    procedure LoadHeader;
    procedure SaveHeader;
    procedure LoadRecord;
    procedure SaveRecord;
    function ReadLine(out Line: string): Boolean; overload;
    function ReadLine(out Strings: TStringList): Boolean; overload;
    procedure WriteLine(S: string); //Because i dont trust with Strings.Text
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetDone: Boolean; override;
    function GetActive: Boolean; override;
    procedure DoClose; override;
    property Session: TmncCSVSession read GetSession;
    property Connection: TmncCSVConnection read GetConnection;
  public
    constructor Create(vSession: TmncSession; vStream: TStream; vMode: TmncCSVMode); overload;
    destructor Destroy; override;
    property Mode: TmncCSVMode read FMode;
    //property Stream: TStream read FStream write SetStream;//TODO
    //DoneOnEmpty: when True make Done when read empty line  
    property EmptyLine: TmncEmptyLine read FEmptyLine write FEmptyLine;
  end;

implementation

{ TmncCSVOptions }

function EscapeStr(Str: string): string;
begin
  Result:= EscapeString(Str, '\', [#13, #10, #9 , #8, '"'], ['r', 'n', 't', 'b', '"']);
end;

function DescapeStr(Str: string): string;
begin
  Result:= DescapeString(Str, '\', [#13, #10, #9 , #8, '"'], ['r', 'n', 't', 'b', '"']);
end;

procedure TmncCSVOptions.SaveToIni(Section: string; ini: TiniFile);
begin
  Ini.WriteString(Section, 'EndOfLine', EscapeStr(EndOfLine));
  Ini.WriteInteger(Section, 'DelimiterChar', Ord(DelimiterChar));
  Ini.WriteInteger(Section, 'EscapeChar', Ord(EscapeChar));
  Ini.WriteInteger(Section, 'QuoteChar', Ord(QuoteChar));
  Ini.WriteInteger(Section, 'SkipColumn', SkipColumn);
  Ini.WriteInteger(Section, 'HeaderLine', Ord(HeaderLine));
  Ini.WriteBool(Section, 'SkipEmptyLines', SkipEmptyLines);
  Ini.WriteBool(Section, 'ANSIContents', ANSIContents);
end;

procedure TmncCSVOptions.LoadFromIni(Section: string; ini: TiniFile);
var
  s: string;
begin
  s := DescapeStr(Ini.ReadString(Section, 'EndOfLine', EndOfLine));
  if s = '' then
    s := EndOfLine;
  EndOfLine := s;
  DelimiterChar := Char(Ini.ReadInteger(Section, 'DelimiterChar', Ord(DelimiterChar)));
  EscapeChar := Char(Ini.ReadInteger(Section, 'EscapeChar', Ord(EscapeChar)));
  QuoteChar := Char(Ini.ReadInteger(Section, 'QuoteChar', Ord(QuoteChar)));
  SkipColumn := Ini.ReadInteger(Section, 'SkipColumn', SkipColumn);
  HeaderLine := TmncCSVHeader(Ini.ReadInteger(Section, 'HeaderLine', Ord(HeaderLine)));
  SkipEmptyLines := Ini.ReadBool(Section, 'SkipEmptyLines', SkipEmptyLines);
  ANSIContents := Ini.ReadBool(Section, 'ANSIContents', ANSIContents);
end;

{ TmncCSVConnection }

constructor TmncCSVConnection.Create;
begin
  inherited;
end;

procedure TmncCSVConnection.DoConnect;
begin
  FConnected := True;
end;

function TmncCSVConnection.GetConnected: Boolean;
begin
  Result := FConnected;
end;

class function TmncCSVConnection.Capabilities: TmncCapabilities;
begin
  Result := [];
end;

class function TmncCSVConnection.EngineName: string;
begin
  Result := 'CSV';
end;

procedure TmncCSVConnection.DoDisconnect;
begin
  FConnected := False;
end;

{ TmncCSVSession }

procedure TmncCSVSession.DoStart;
begin
end;

procedure TmncCSVSession.DoStop(How: TmncSessionAction; Retaining: Boolean);
begin
end;

constructor TmncCSVSession.Create(vConnection: TmncConnection);
begin
  inherited;
  HeaderLine := hdrNormal;
  DelimiterChar := ';';
  EndOfLine := sUnixEndOfLine;
  QuoteChar := '"';
end;

{ TmncCSVCommand }

constructor TmncCSVCommand.Create(vSession: TmncSession; vStream: TStream; vMode: TmncCSVMode);
begin
  CreateBy(vSession);
  FMode := vMode;
  FStream := vStream;
end;

destructor TmncCSVCommand.Destroy;
begin
  inherited;
end;

function TmncCSVCommand.GetDone: Boolean;
begin
  Result := (Mode = csvmWrite) or (FCSVStream = nil);// do not check (FCSVStream.Done) last line will not loaded;
end;

procedure TmncCSVCommand.DoExecute;
begin
  if (Mode = csvmWrite) then
  begin
    SaveRecord;
  end;
end;

procedure TmncCSVCommand.DoNext;
begin
  if Mode = csvmRead then
    LoadRecord;
end;

procedure TmncCSVCommand.DoPrepare;
begin
  FBeginOfStream := True;
  FCSVStream := TmnWrapperStream.Create(FStream, False);
  FCSVStream.EndOfLine := Session.EndOfLine;
  if Session.HeaderLine <> hdrNone then
  begin
    if (Mode = csvmWrite) then
    begin
      if Session.HeaderLine <> hdrSkip then //do not save when it
        SaveHeader;
    end
    else
      LoadHeader;
  end;
  PrepareParams;
end;

procedure TmncCSVCommand.DoClose;
begin
  FreeAndNil(FCSVStream);
end;

function TmncCSVCommand.GetActive: Boolean;
begin
  Result := FCSVStream <> nil;
end;

function TmncCSVCommand.GetConnection: TmncCSVConnection;
begin
  Result := Session.Connection as TmncCSVConnection;
end;

function TmncCSVCommand.GetSession: TmncCSVSession;
begin
  Result := (Inherited Session) as TmncCSVSession;
end;

procedure TmncCSVCommand.DoParse;
begin
  //Nothing to do
end;

function TmncCSVCommand.CreateFields(vColumns: TmncColumns): TmncFields;
begin
  Result := TmncVariantFields.Create(vColumns);
end;

function TmncCSVCommand.CreateParams: TmncParams;
begin
  Result := TmncVariantParams.Create;
end;

procedure TmncCSVCommand.LoadHeader;
var
  aStrings: TStringList;
  i: Integer;
begin
  Columns.Clear;
  if ReadLine(aStrings) then
  begin
    try
      for i := 0 to aStrings.Count - 1 do
      begin
        Columns.Add(i, trim(DequoteStr(aStrings[i])), dtString);
      end;
    finally
      aStrings.Free;
    end;
  end
  else
    FreeAndNil(FCSVStream);//close it to make EOF
end;

type
  TStrToFields = class(TObject)
  public
    Fields: TmncFields;
    Session: TmncCSVSession;
  end;

procedure StrToFieldsCallbackProc(Sender: Pointer; Index: Integer; S: string; var Resume: Boolean);
var
  Info: TStrToFields;
begin
  Info := TStrToFields(Sender);
//  if (i < aRecord.Count) {and (i < Columns.Count)} then //TODO check it
  if Info.Session.CSVOptions.QuoteChar <> #0 then
    Info.Fields.Add(index, DequoteStr(s, Info.Session.CSVOptions.QuoteChar))
  else
    Info.Fields.Add(index, s)
end;

procedure TmncCSVCommand.LoadRecord;
var
  Info: TStrToFields;
  Line: string;
begin
  if ReadLine(Line) then
  begin
    Info := TStrToFields.Create;
    try
      Info.Fields := CreateFields(Columns);
      Info.Session := Session;
      StrToStringsCallback(Line, Info, StrToFieldsCallbackProc, [Session.DelimiterChar], [#0, #13, #10], [Session.QuoteChar]);
      Fields := Info.Fields;
    finally
      Info.Free;
    end;
  end
  else
    FreeAndNil(FCSVStream);//close it to make EOF
end;

procedure TmncCSVCommand.PrepareParams;
var
  i: Integer;
  aParams: TmncParams;
begin
  aParams := CreateParams;
  for i := 0 to Columns.Count - 1 do
    aParams.Add(Columns[i].Name);
  Params := aParams;
end;

function TmncCSVCommand.ReadLine(out Strings: TStringList): Boolean;
var
  s: string;
begin
  Result := ReadLine(s);
  if Result then
  begin
    Strings := TStringList.Create;
    StrToStrings(s, Strings, [Session.DelimiterChar], [#0, #13, #10], [Session.QuoteChar])
  end
  else
    Strings := nil;
  FBeginOfStream := False;
end;

function TmncCSVCommand.ReadLine(out Line: string): Boolean;
var
  t: rawbytestring;
begin
  Result := (FCSVStream <> nil) and not (cloRead in FCSVStream.Done);
  if Result then
  begin
    Line := '';
    repeat
      if Session.CSVOptions.ANSIContents then
      begin
        Result := FCSVStream.ReadLineRawByte(t, False);
        {$ifdef fpc}
        SetCodePage(t, SystemAnsiCodePage, false);
        Line := AnsiToUtf8(t);
        {$else}
        Line := string(t);
        {$endif}
      end
      else
        Result := FCSVStream.ReadLine(Line, False);
      Line := Trim(Line);
    until not Result or not ((Line = '') and (EmptyLine = elSkip));

    Result := Result and not ((Line = '') and (EmptyLine = elDone));
  end
end;

procedure TmncCSVCommand.SaveHeader;
var
  i: Integer;
  s: string;
begin
  s := '';
  for i := 0 to Columns.Count - 1 do
  begin
    if s <> '' then
      s := s + Session.DelimiterChar;
    s := s + Columns[i].Name;
  end;
  WriteLine(s);
end;

procedure TmncCSVCommand.SaveRecord;
var
  i: Integer;
  s: string;
  v: Variant;
  First: Boolean;
begin
  s := '';
  First := True;
  for i := 0 to Params.Count - 1 do
  begin
    if not First then
      s := s + Session.DelimiterChar
    else
      First := False;
    v := Params.Items[i].Value;
    s := s + VarToStrDef(v, '');
  end;
  WriteLine(s);
end;

procedure TmncCSVCommand.WriteLine(S: string);
var
  raw: RawByteString;
begin
  if FBeginOfStream then
    FBeginOfStream := False
  else
    FCSVStream.WriteLine;
  if Session.CSVOptions.ANSIContents then
  begin
    {$ifdef fpc}
    FCSVStream.WriteAnsiString(s);

    {raw := '';
    SetCodePage(raw, SystemAnsiCodePage, false);
    raw := Utf8ToAnsi(s);}
    {$else}
    raw := AnsiString(s);//Here you can fix the bug
    FCSVStream.WriteLineRawByte(raw);
    {$endif}
  end
  else
  begin
    {$ifdef FPC}
    raw := s;
    FCSVStream.WriteRawByte(raw);
    {$else}
    FCSVStream.WriteString(s);
    {$endif}
  end
end;

end.
