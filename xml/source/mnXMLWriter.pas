unit mnXMLWriter;
{**
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
{$ENDIF}

interface

uses
  Classes, SysUtils, mnXML, mnXMLUtils;

const
  cMaxLine = 40;

type

  { TmnCustomXMLWriter }

  TmnCustomXMLWriter = class(TmnXMLFiler)
  private
    FOpenedTags: TStringList;
    FSmart: Boolean;
    FTabs: string;
    FEncoding: Boolean;
    FTaging: Boolean;
    FTagStarted: Boolean; //Work with StartTag
    AttributesCount: Integer;
    procedure SetSmart(const Value: Boolean);
  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure WriteHeader;
    function CharsetEncode(const Value: string): string;

    procedure DoWriteStartTag(Name: string); virtual; abstract;
    procedure DoWriteAttribute(Name, Value: string); virtual; abstract;
    procedure DoWriteStopTag(Name: string); virtual; abstract;
    procedure DoWriteCloseTag(Name: string); virtual; abstract;
    procedure DoWriteText(Value: string); virtual; abstract;
    procedure DoWriteComment(Value: string); virtual; abstract;
    procedure DoWriteCDATA(Value: string); virtual; abstract;

  public
    constructor Create; override;
    destructor Destroy; override;
    //Return tabs/spaces to put before write line in smart mode
    function GetIndents: string; overload;
    function GetIndents(Count: Integer): string; overload;
    //WriteStartTag like <name wotk wit StopTag
    //without add attribute and te > sign
    procedure WriteStartTagNS(NameSpace, Name: string); overload;
    procedure WriteStartTag(Name: string); overload;
    procedure WriteAttribute(Name, Value: string);
    procedure WriteXMLNS(NameSpace, Value: string); overload;
    //StopTag add attribute and close it by > or /> if empty = true
    procedure WriteStopTagNS(NameSpace, Name: string); overload;
    procedure WriteStopTag(Name: string); overload;
    procedure WriteStopTag; overload;

    //OpenTag add Tag with attributes <name att1="val1">
    //procedure WriteOpenTag(const Name, NameSpace: string); overload;
    procedure WriteOpenTagNS(NameSpace, Name: string; AttNames, AttValues: TStringArray); overload;
    procedure WriteOpenTagNS(NameSpace, Name: string); overload;
    procedure WriteOpenTag(Name: string; AttNames, AttValues: TStringArray); overload;
    procedure WriteOpenTag(Name: string); overload;
    //CloseTag work with WriteStartTag and WriteOpenTag
    procedure WriteCloseTagNS(NameSpace, Name: string); overload;
    procedure WriteCloseTag(Name: string); overload;
    procedure WriteCloseTag; overload;
    procedure WriteEmptyTag(Name: string); deprecated;
    procedure WriteText(Value: string);
    procedure WriteTextTagNS(NameSpace, Name, Value: string); overload;
    procedure WriteTextTag(Name, Value: string); overload;
    //procedure Write(const Name, Value: string; const Attributes: string = ''); deprecated;
    procedure WriteComment(Value: string);
    procedure WriteCDATA(Value: string);
    //States
    property TagStarted: Boolean read FTagStarted;
    //Smart engine
    property Smart: Boolean read FSmart write SetSmart default False;
    property Encoding: Boolean read FEncoding write FEncoding default True;
    property Tabs: string read FTabs write FTabs;
    //End smart engine
  end;

  TmnCustomXMLWriterClass = class of TmnCustomXMLWriter;

  { TmnXMLWriter }

  TmnXMLWriter = class(TmnCustomXMLWriter)
  protected
    procedure DoWriteStartTag(Name: string); override;
    procedure DoWriteAttribute(Name, Value: string); override;
    procedure DoWriteStopTag(Name: string); override;
    procedure DoWriteCloseTag(Name: string); override;
    procedure DoWriteText(Value: string); override;
    procedure DoWriteComment(Value: string); override;
    procedure DoWriteCDATA(Value: string); override;
  end;

  { TmnJSOWriter }

  TmnJSONWriter = class(TmnCustomXMLWriter) //for fun
  protected
    procedure DoWriteStartTag(Name: string); override;
    procedure DoWriteAttribute(Name, Value: string); override;
    procedure DoWriteStopTag(Name: string); override;
    procedure DoWriteCloseTag(Name: string); override;
    procedure DoWriteText(Value: string); override;
    procedure DoWriteComment(Value: string); override;
    procedure DoWriteCDATA(Value: string); override;
  end;

implementation

uses
  mnUtils;

{ TmnJSONWriter }

procedure TmnJSONWriter.DoWriteStartTag(Name: string);
begin
  Stream.WriteString(QuoteStr(Name) + ' = {');
end;

procedure TmnJSONWriter.DoWriteAttribute(Name, Value: string);
begin
  Stream.WriteString(QuoteStr(Name) + ' = ' + QuoteStr(Value));
end;

procedure TmnJSONWriter.DoWriteStopTag(Name: string);
begin
  //Stream.WriteString(Name + '}');
end;

procedure TmnJSONWriter.DoWriteCloseTag(Name: string);
begin
  Stream.WriteString('}');
end;

procedure TmnJSONWriter.DoWriteText(Value: string);
begin
  DoWriteAttribute('Value', Value);
end;

procedure TmnJSONWriter.DoWriteComment(Value: string);
begin
end;

procedure TmnJSONWriter.DoWriteCDATA(Value: string);
begin
end;

{ TmnXMLWriter }

procedure TmnXMLWriter.DoWriteStartTag(Name: string);
begin
  Stream.WriteString('<' + Name);
end;

procedure TmnXMLWriter.DoWriteAttribute(Name, Value: string);
begin
  Stream.WriteString(Name+'=' + QuoteStr(EntityEncode(Value)));
end;

procedure TmnXMLWriter.DoWriteStopTag(Name: string);
begin
  Stream.WriteString('>');
end;

procedure TmnXMLWriter.DoWriteCloseTag(Name: string);
begin
  Stream.WriteString('</' + Name + '>');
end;

procedure TmnXMLWriter.DoWriteText(Value: string);
begin
  Stream.WriteString(EntityEncode(Value));
end;

procedure TmnXMLWriter.DoWriteComment(Value: string);
begin
  Stream.WriteString('<!--' + EntityEncode(Value) + '-->');
end;

procedure TmnXMLWriter.DoWriteCDATA(Value: string);
begin
  Stream.WriteString('<![CDATA[' + EntityEncode(Value) + ']]');
end;

{ TmnCustomXMLWriter }

procedure TmnCustomXMLWriter.WriteCloseTag(Name: string);
begin
  WriteCloseTagNS('', Name);
end;

procedure TmnCustomXMLWriter.WriteCloseTag;
begin
  WriteCloseTag('');
end;

constructor TmnCustomXMLWriter.Create;
begin
  inherited Create;
  FOpenedTags := TStringList.Create;
  FTabs := '  ';
  FEncoding := True;
end;

destructor TmnCustomXMLWriter.Destroy;
begin
  FOpenedTags.Free;
  inherited;
end;

procedure TmnCustomXMLWriter.DoStop;
begin
  inherited;
  if Smart then
  begin
    while FOpenedTags.Count > 0 do
      WriteCloseTag;
  end;
end;

function TmnCustomXMLWriter.GetIndents: string;
begin
  Result := GetIndents(FOpenedTags.Count);
end;

function TmnCustomXMLWriter.GetIndents(Count: Integer): string;
begin
  Result := RepeatString(Tabs, Count);
end;

procedure TmnCustomXMLWriter.WriteStartTagNS(NameSpace, Name: string);
begin
  if NameSpace <> '' then
    Name := NameSpace + sNameSpaceSeparator + Name;
  AttributesCount := 0;

  if Smart and FTaging and (Tabs <> '') then
    Stream.WriteString(Stream.EndOfLine + RepeatString(Tabs, FOpenedTags.Count));

  DoWriteStartTag(Name);

  FTaging := True;
  FTagStarted := True;
  if FSmart then
    FOpenedTags.Add(Name);
end;

procedure TmnCustomXMLWriter.DoStart;
begin
  inherited;
  if Version <> '' then
    Header.Values['version'] := '"' + Version + '"';
  if Charset <> '' then
    Header.Values['encoding'] := '"' + Charset + '"';
  if Standalone = Yes then
    Header.Values['standalone'] := '"yes"'
  else if Standalone = No then
    Header.Values['standalone'] := '"no"'; //must delete it
  WriteHeader;
end;

procedure TmnCustomXMLWriter.WriteOpenTag(Name: string; AttNames, AttValues: TStringArray);
begin
  WriteOpenTagNS('', Name, AttNames, AttValues);
end;

procedure TmnCustomXMLWriter.WriteOpenTag(Name: string);
begin
  WriteOpenTag(Name, [], []);
end;

procedure TmnCustomXMLWriter.WriteCloseTagNS(NameSpace, Name: string);
begin
  if NameSpace <> '' then
    Name := NameSpace + sNameSpaceSeparator + Name;

  if FSmart then
  begin
    if Name = '' then
    begin
      Name := FOpenedTags[FOpenedTags.Count - 1];
      FOpenedTags.Delete(FOpenedTags.Count - 1);
    end
    else
    begin
      if (FOpenedTags.Count = 0) then
        raise EmnXMLException.Create('Tag name not opened')
      else if (FOpenedTags[FOpenedTags.Count - 1] <> Name) then //not the last tag
        raise EmnXMLException.Create('Tag name "' + Name + '" is not "' + FOpenedTags[FOpenedTags.Count - 1] + '"');
      FOpenedTags.Delete(FOpenedTags.Count - 1);
    end;
  end;

  if Smart and FTaging and (Tabs <> '') then
    Stream.WriteString(Stream.EndOfLine + GetIndents);

  DoWriteCloseTag(Name);

  FTaging := True;
end;

procedure TmnCustomXMLWriter.SetSmart(const Value: Boolean);
begin
  if FSmart <> Value then
  begin
    if Active then
      raise EmnXMLException.Create('Can not change smart value white opened');
    FSmart := Value;
  end;
end;

procedure TmnCustomXMLWriter.WriteHeader;
var
  i: Integer;
  s: string;
begin
  s := '';
  for i := 0 to Header.Count - 1 do
  begin
    if i > 0 then
      s := s + ' ';
    s := s + Header[i];
  end;
  s := '<?xml ' + s + ' ?>';
  Stream.WriteLine(s);
end;

procedure TmnCustomXMLWriter.WriteEmptyTag(Name: string);
begin
  Stream.WriteString('<' + Name + '/>');
  FTaging := True;
end;

procedure TmnCustomXMLWriter.WriteText(Value: string);
begin
  if Value <> '' then
  begin
    DoWriteText(Value);
    FTaging := False;
  end;
end;

procedure TmnCustomXMLWriter.WriteTextTagNS(NameSpace, Name, Value: string);
begin
  if Value <> '' then
  begin
    WriteOpenTagNS(NameSpace, Name, [], []);
    WriteText(Value);
    WriteCloseTagNS(NameSpace, Name);
  end;
end;

procedure TmnCustomXMLWriter.WriteTextTag(Name, Value: string);
begin
  WriteTextTagNS('', Name, Value);
end;

procedure TmnCustomXMLWriter.WriteComment(Value: string);
begin
  if Smart and (Tabs <> '') then
    Stream.WriteString(GetIndents);
  DoWriteComment(Value);
end;

procedure TmnCustomXMLWriter.WriteCDATA(Value: string);
begin
  if Smart and (Tabs <> '') then
    Stream.WriteString(GetIndents);
  DoWriteCDATA(Value);
end;

function TmnCustomXMLWriter.CharsetEncode(const Value: string): string;
begin
  Result := Value;
end;

procedure TmnCustomXMLWriter.WriteStartTag(Name: string);
begin
  WriteStartTagNS('', Name);
end;

procedure TmnCustomXMLWriter.WriteAttribute(Name, Value: string);
begin
  if Smart then
  begin
    if (AttributesCount > 0) then
    begin
      if (Length(Value) > cMaxLine) then
      begin
        Stream.WriteLine;
        Stream.WriteString(GetIndents(FOpenedTags.Count));
      end
      else
        Stream.WriteString(' ');
    end
    else
      Stream.WriteString(' ');
    DoWriteAttribute(Name, Value);
  end
  else
  begin
    Stream.WriteString(' ');
    DoWriteAttribute(Name, Value);
  end;
  Inc(AttributesCount);
end;

procedure TmnCustomXMLWriter.WriteStopTag(Name: string);
begin
  WriteStopTagNS('', Name);
end;

procedure TmnCustomXMLWriter.WriteXMLNS(NameSpace, Value: string);
begin
  NameSpace := 'xmlns' + sNameSpaceSeparator + NameSpace;
  WriteAttribute(NameSpace, Value);
end;

procedure TmnCustomXMLWriter.WriteStopTagNS(NameSpace, Name: string);
begin
  if NameSpace <> '' then
    Name := NameSpace + sNameSpaceSeparator + Name;

  if not FTagStarted then
    raise EmnXMLException.Create('Tag not started');

  DoWriteStopTag(Name);

  FTagStarted := False;
  AttributesCount := 0;
end;

procedure TmnCustomXMLWriter.WriteStopTag;
begin
  WriteStopTag('');
end;

procedure TmnCustomXMLWriter.WriteOpenTagNS(NameSpace, Name: string; AttNames, AttValues: TStringArray);
var
  i: Integer;
begin
  WriteStartTagNS(NameSpace, Name);
  for i := 0 to Length(AttNames) -1 do
  begin
    if i < Length(AttValues) then
      WriteAttribute(AttNames[i], AttValues[i]);
  end;
  WriteStopTagNS(NameSpace, Name);
end;

procedure TmnCustomXMLWriter.WriteOpenTagNS(NameSpace, Name: string);
begin
  WriteOpenTagNS(NameSpace, Name, [], []);
end;

end.

