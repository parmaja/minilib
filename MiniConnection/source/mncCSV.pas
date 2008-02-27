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
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  mncConnections, mnStreams;

type
  TmncCSVConnection = class(TmncConnection)
  private
    FSpliteChar: Char;
    FHaveHeader: Boolean;
    FEndOfLine: string;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected:Boolean; override;
  public
    constructor Create;
    property SpliteChar:Char read FSpliteChar write FSpliteChar default #9;
    property EndOfLine:string read FEndOfLine write FEndOfLine;
    property HaveHeader: Boolean read FHaveHeader write FHaveHeader default True;
  end;

  { TmncCSVSession }

  TmncCSVSession = class(TmncSession)
  protected
    procedure DoStart; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
  public
  end;
  TmncCSVMode = (csvmRead, csvmWrite);

  TmncCSVCommand = class(TmncCommand)
  private
    FCSVStream: TmnStream;
    FStream: TStream;
    FMode: TmncCSVMode;
    function GetConnection: TmncCSVConnection;
  protected
    procedure LoadHeader;
    procedure PrepareParams;
    procedure SaveHeader;
    procedure LoadRecord;
    procedure SaveRecord;
    function ReadLine:TStringList;
    procedure WriteLine(S:string);//Because i am not trust with Strings.Text
    procedure DoPrepare; override;
    procedure DoExecute; override;
    procedure DoNext; override;
    function GetEOF:Boolean; override;
    function GetActive:Boolean; override;
    procedure DoClose; override;
    property Connection:TmncCSVConnection read GetConnection;
  public
    constructor Create(vSession:TmncSession; vStream: TStream; vMode: TmncCSVMode);
    destructor Destroy; override;
    property Mode: TmncCSVMode read FMode;
  end;

implementation

{ TmncCSVConnection }

constructor TmncCSVConnection.Create;
begin
  inherited Create;
  FHaveHeader := True;
  SpliteChar := #9; //TAB
  EndOfLine := #13;
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

procedure TmncCSVSession.DoCommit;
begin
end;

procedure TmncCSVSession.DoRollback;
begin
end;

{ TmncCSVCommand }

constructor TmncCSVCommand.Create(vSession:TmncSession; vStream: TStream; vMode: TmncCSVMode);
begin
  inherited Create(vSession);
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
  FCSVStream.EndOfLine := Connection.EndOfLine;
  if (Session.Connection as TmncCSVConnection).HaveHeader then
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

procedure TmncCSVCommand.LoadHeader;
var
  aStrings: TStringList;
  i: Integer;
begin
  Fields.Clear;
  aStrings := ReadLine;
  try
    for i := 0 to aStrings.Count -1 do
    begin
      Fields.Add(i, aStrings[i]);//TODO must Dequote
    end;
  finally
    aStrings.Free;
  end;
end;

procedure TmncCSVCommand.LoadRecord;
var
  aStrings: TStringList;
  aRecord: TmncRecord;
  i: Integer;
begin
  aRecord := TmncRecord.Create(Fields);
  aStrings := ReadLine;
  try
    for i := 0 to aStrings.Count -1 do
    begin
      aRecord.Add(i, aStrings[i]);//TODO must Dequote
    end;
  finally
    aStrings.Free;
  end;
  Current := aRecord;
end;

procedure TmncCSVCommand.PrepareParams;
var
  i: Integer;
  aParams: TmncParams;
begin
  aParams := TmncParams.Create;
  for i := 0 to Fields.Count -1 do
    aParams.Add(Fields[i].Name);
  Params := aParams;
end;

function TmncCSVCommand.ReadLine: TStringList;
var
  s: string;
begin
  s := '';
  Result := TStringList.Create;
  FCSVStream.ReadLn(s);
  ExtractStrings([Connection.SpliteChar], [], PChar(s), Result);
end;

procedure TmncCSVCommand.SaveHeader;
var
  i: Integer;
  s: string;
begin
  s := '';
  for i := 0 to Fields.Count -1 do
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
begin
  s := '';
  for i := 0 to Params.Count -1 do
  begin
    if s <> '' then
      s := s + #9;
    s := s + Params.Items[i].Value;
  end;
  WriteLine(s);
end;

procedure TmncCSVCommand.WriteLine(S: string);
begin
  FCSVStream.WriteLn(s);
end;

end.

