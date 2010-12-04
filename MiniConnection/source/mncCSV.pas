unit mncCSV;
{**
 *  This file is part of the "Mini Connections"
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
  Classes, SysUtils, Variants,
  mnUtils, mncConnections, mnStreams;

type
  TmncCSVConnection = class(TmncConnection)
  private
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
  public
    constructor Create;
  end;

  { TmncCSVSession }

  TmncCSVSession = class(TmncSession)
  private
    FEndOfLine: string;
    FHaveHeader: Boolean;
    FSpliteChar: Char;
  protected
    procedure DoStart; override;
    procedure DoStop(How: TmncSessionAction; Retaining: Boolean); override;
  public
    constructor Create(vConnection: TmncConnection); override;
    property SpliteChar: Char read FSpliteChar write FSpliteChar default #9;
    property EndOfLine: string read FEndOfLine write FEndOfLine;
    property HaveHeader: Boolean read FHaveHeader write FHaveHeader default True;
  end;
  TmncCSVMode = (csvmRead, csvmWrite);

  { TmncCSVCommand }

  TmncCSVCommand = class(TmncCommand)
  private
    FCSVStream: TmnStream;
    FStream: TStream;
    FMode: TmncCSVMode;
    FEOFOnEmpty: Boolean;
    function GetConnection: TmncCSVConnection;
    function GetSession: TmncCSVSession;
  protected
    procedure PrepareParams;
    procedure LoadHeader;
    procedure SaveHeader;
    procedure LoadRecord;
    procedure SaveRecord;
    function ReadLine(var Strings: TStringList): Boolean;
    procedure WriteLine(S: string); //Because i am not trust with Strings.Text
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetEOF: Boolean; override;
    function GetActive: Boolean; override;
    procedure DoClose; override;
    property Session: TmncCSVSession read GetSession;
    property Connection: TmncCSVConnection read GetConnection;
  public
    constructor Create(vSession: TmncSession; vStream: TStream; vMode: TmncCSVMode); overload;
    destructor Destroy; override;
    property Mode: TmncCSVMode read FMode;
    //EOFOnEmpty: when True make EOF when read empty line  
    property EOFOnEmpty: Boolean read FEOFOnEmpty write FEOFOnEmpty;
  end;

implementation

{ TmncCSVConnection }

constructor TmncCSVConnection.Create;
begin
  inherited Create;
end;

procedure TmncCSVConnection.DoConnect;
begin
end;

function TmncCSVConnection.GetConnected: Boolean;
begin
  Result := True;
end;

procedure TmncCSVConnection.DoDisconnect;
begin
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
  FHaveHeader := True;
  SpliteChar := #9; //TAB
  EndOfLine := sEndOfLine;
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

function TmncCSVCommand.GetEOF: Boolean;
begin
  Result := (FCSVStream = nil) or (Mode = csvmWrite) or (FCSVStream.EOF);
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
  FCSVStream := TmnStream.Create(FStream, False);
  FCSVStream.EndOfLine := Session.EndOfLine;
  if Session.HaveHeader then
  begin
    if (Mode = csvmWrite) then
      SaveHeader
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
  Result := FCSVStream = nil;
end;

function TmncCSVCommand.GetConnection: TmncCSVConnection;
begin
  Result := Session.Connection as TmncCSVConnection;
end;

function TmncCSVCommand.GetSession: TmncCSVSession;
begin
  Result := (Inherited Session) as TmncCSVSession;
end;

procedure TmncCSVCommand.LoadHeader;
var
  aStrings: TStringList;
  i: Integer;
begin
  Fields.Clear;
  aStrings := nil;
  if ReadLine(aStrings) then
  begin
    try
      for i := 0 to aStrings.Count - 1 do
      begin
        Fields.Add(i, DequoteStr(aStrings[i]), ftString);
      end;
    finally
      aStrings.Free;
    end;
  end
  else
    FreeAndNil(FCSVStream);//close it for make EOF
end;

procedure TmncCSVCommand.LoadRecord;
var
  aStrings: TStringList;
  aRecord: TmncFields;
  i: Integer;
begin
  if ReadLine(aStrings) then
  begin
    aRecord := TmncFields.Create(Fields);
    i := 0;
    try
      while (i < aStrings.Count) and (i < Fields.Count) do
      begin
        aRecord.Add(i, DequoteStr(aStrings[i]));
        Inc(i); 
      end;
    finally
      aStrings.Free;
    end;
    Current := aRecord;
  end
  else
    FreeAndNil(FCSVStream);//close it for make EOF
end;

procedure TmncCSVCommand.PrepareParams;
var
  i: Integer;
  aParams: TmncParams;
begin
  aParams := TmncParams.Create;
  for i := 0 to Fields.Count - 1 do
    aParams.Add(Fields[i].Name);
  Params := aParams;
end;

function TmncCSVCommand.ReadLine(var Strings: TStringList): Boolean;
var
  s: string;
begin
  s := '';
  Strings := TStringList.Create;
  FCSVStream.ReadLn(s);
  s := Trim(s);
  Result := (s <> '') or not (EOFOnEmpty);
  if Result then
    StrToStrings(s, Strings, [Session.SpliteChar], [#0, #13, #10], True, ['"']);
end;

procedure TmncCSVCommand.SaveHeader;
var
  i: Integer;
  s: string;
begin
  s := '';
  for i := 0 to Fields.Count - 1 do
  begin
    if s <> '' then
      s := s + #9;
    s := s + Fields[i].Name;
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
      s := s + Session.SpliteChar
    else
      First := False;
    v := Params.Items[i].Value;
    s := s + VarToStrDef(v, '');
  end;
  WriteLine(s);
end;

procedure TmncCSVCommand.WriteLine(S: string);
begin
  FCSVStream.WriteLn(s);
end;

end.

