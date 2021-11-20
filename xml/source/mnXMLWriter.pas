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

  { TmnXMLWriter }

  TmnXMLWriter = class(TmnXMLFiler)
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
    procedure WriteAttributes(Attributes: string); overload;
    procedure WriteAttribute(Name, Value: string); overload;
    procedure WriteXMLNS(NameSpace, Value: string); overload;
    //StopTag add attribute and close it by > or /> if empty = true
    procedure WriteStopTag(Empty: Boolean = False; const Attributes: string = ''); overload;
    procedure WriteStopTag(Name: string; Empty: Boolean = False; const Attributes: string = ''); overload;

    //OpenTag add Tag with attributes <name att1="val1">
    //procedure WriteOpenTag(const Name, NameSpace: string); overload;
    procedure WriteOpenTagNS(NameSpace, Name: string; const Attributes: string = ''); overload;
    procedure WriteOpenTag(Name: string; const Attributes: string); overload;
    procedure WriteOpenTag(Name: string); overload;
    //CloseTag work with WriteStartTag and WriteOpenTag
    procedure WriteCloseTag(NameSpace, Name: string); overload;
    procedure WriteCloseTag(Name: string); overload;
    procedure WriteCloseTag; overload;
    procedure WriteEmptyTag(Name: string);
    procedure WriteText(Value: string);
    procedure WriteTextTagNS(NameSpace, Name, Value: string); overload;
    procedure WriteTextTag(Name, Value: string); overload;
    procedure Write(const Name, Value: string; const Attributes: string = '');
    procedure WriteComment(Value: string);
    //States
    property TagStarted: Boolean read FTagStarted;
    //Smart engine
    property Smart: Boolean read FSmart write SetSmart default False;
    property Encoding: Boolean read FEncoding write FEncoding default True;
    property Tabs: string read FTabs write FTabs;
    //End smart engine
  end;

implementation

uses
  mnUtils;

{ TmnXMLWriter }

procedure TmnXMLWriter.WriteCloseTag(NameSpace, Name: string);
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
        raise EmnXMLException.Create('Tag name not opened "')
      else if (FOpenedTags[FOpenedTags.Count - 1] <> Name) then //not the last tag
        raise EmnXMLException.Create('Tag name "' + Name + '" is not "' + FOpenedTags[FOpenedTags.Count - 1] + '"');
      FOpenedTags.Delete(FOpenedTags.Count - 1);
    end;
  end;
  if Smart and FTaging and (Tabs <> '') then
    Stream.WriteString(Stream.EndOfLine + GetIndents + '</' + Name + '>')
  else
    Stream.WriteString('</' + Name + '>');
  FTaging := True;
end;

procedure TmnXMLWriter.WriteCloseTag(Name: string);
begin
  WriteCloseTag('', Name);
end;

procedure TmnXMLWriter.WriteCloseTag;
begin
  WriteCloseTag('');
end;

constructor TmnXMLWriter.Create;
begin
  inherited Create;
  FOpenedTags := TStringList.Create;
  FTabs := '  ';
  FEncoding := True;
end;

destructor TmnXMLWriter.Destroy;
begin
  FOpenedTags.Free;
  inherited;
end;

procedure TmnXMLWriter.DoStop;
begin
  inherited;
  if Smart then
  begin
    while FOpenedTags.Count > 0 do
      WriteCloseTag;
  end;
end;

function TmnXMLWriter.GetIndents: string;
begin
  Result := GetIndents(FOpenedTags.Count);
end;

function TmnXMLWriter.GetIndents(Count: Integer): string;
begin
  Result := RepeatString(Tabs, Count);
end;

procedure TmnXMLWriter.DoStart;
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

procedure TmnXMLWriter.WriteOpenTagNS(NameSpace, Name: string; const Attributes: string);
var
  s: string;
begin
  if NameSpace <> '' then
    Name := NameSpace + sNameSpaceSeparator + Name;

  if Smart and FTaging and (Tabs <> '') then
    s := Stream.EndOfLine + RepeatString(Tabs, FOpenedTags.Count);
  s := s + '<' + Name + Enclose(Attributes, ' ') + '>';
  Stream.WriteString(S);
  FTaging := True;
  if FSmart then
    FOpenedTags.Add(Name);
end;

procedure TmnXMLWriter.WriteOpenTag(Name: string; const Attributes: string);
begin
  WriteOpenTagNS('', Name, Attributes);
end;

procedure TmnXMLWriter.WriteOpenTag(Name: string);
begin
  WriteOpenTagNS('', Name, '');
end;

procedure TmnXMLWriter.SetSmart(const Value: Boolean);
begin
  if FSmart <> Value then
  begin
    if Active then
      raise EmnXMLException.Create('Can not change smart value white opened');
    FSmart := Value;
  end;
end;

procedure TmnXMLWriter.WriteHeader;
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

procedure TmnXMLWriter.WriteEmptyTag(Name: string);
begin
  Stream.WriteString('<' + Name + '/>');
  FTaging := True;
end;

procedure TmnXMLWriter.WriteText(Value: string);
begin
  Stream.WriteString(EntityEncode(Value));
  FTaging := False;
end;

procedure TmnXMLWriter.WriteTextTagNS(NameSpace, Name, Value: string);
begin
  WriteOpenTagNS(NameSpace, Name);
  WriteText(Value);
  WriteCloseTag(NameSpace, Name);
end;

procedure TmnXMLWriter.WriteTextTag(Name, Value: string);
begin
  WriteTextTagNS('', Name, Value);
end;

function TmnXMLWriter.CharsetEncode(const Value: string): string;
begin
  Result := Value;
end;

procedure TmnXMLWriter.WriteComment(Value: string);
begin
  Stream.WriteString('<!--' + EntityEncode(Value) + '-->');
end;

procedure TmnXMLWriter.Write(const Name, Value: string; const Attributes: string);
var
  s: string;
begin
  if Smart and (Tabs <> '') then
    s := Stream.EndOfLine + RepeatString(Tabs, FOpenedTags.Count)
  else
    s := '';
  s := s + '<' + Name + Enclose(Attributes, ' ') + '>' + EntityEncode(Value) + '</' + Name + '>';
  Stream.WriteString(s)
end;

procedure TmnXMLWriter.WriteStartTag(Name: string);
begin
  WriteStartTagNS('', Name);
end;

procedure TmnXMLWriter.WriteStartTagNS(NameSpace, Name: string);
var
  s: string;
begin
  if NameSpace <> '' then
    Name := NameSpace + sNameSpaceSeparator + Name;

  AttributesCount := 0;
  if Smart and FTaging and (Tabs <> '') then
    s := Stream.EndOfLine + RepeatString(Tabs, FOpenedTags.Count);
  s := s + '<' + Name;
  Stream.WriteString(S);
  FTaging := True;
  FTagStarted := True;
  if FSmart then
    FOpenedTags.Add(Name);
end;

procedure TmnXMLWriter.WriteStopTag(Name: string; Empty: Boolean; const Attributes: string);
var
  s: string;
begin
  if not FTagStarted then
    raise EmnXMLException.Create('Tag not started');
  s := Enclose(Attributes, ' ');
  if Empty then
  begin
    if Smart then
    begin
      if Name <> '' then
        if (FOpenedTags.Count = 0) or (FOpenedTags[FOpenedTags.Count - 1] <> Name) then //not the last tag
          raise EmnXMLException.Create('Tag name must not close now');
      FOpenedTags.Delete(FOpenedTags.Count - 1); //close last tag must remove it
    end;
    s := s + '/>'
  end
  else
    s := s + '>';
  Stream.WriteString(S);
  FTagStarted := False;
  AttributesCount := 0;
end;

procedure TmnXMLWriter.WriteAttributes(Attributes: string);
begin
  if Smart then
  begin
    if (AttributesCount > 0) then
    begin
      if (Length(Attributes) > cMaxLine) then
      begin
        Stream.WriteLine;
        Stream.WriteString(GetIndents(FOpenedTags.Count));
      end
      else
        Stream.WriteString(' ');
    end;
    Stream.WriteString(Attributes);
  end
  else
    Stream.WriteString(Enclose(Attributes, ' '));
  Inc(AttributesCount);
end;

procedure TmnXMLWriter.WriteAttribute(Name, Value: string);
begin
  WriteAttributes(Name + '='+ QuoteStr(Value));
end;

procedure TmnXMLWriter.WriteXMLNS(NameSpace, Value: string);
begin
  NameSpace := 'xmlns' + sNameSpaceSeparator + NameSpace;
  WriteAttribute(NameSpace, Value);
end;

procedure TmnXMLWriter.WriteStopTag(Empty: Boolean; const Attributes: string);
begin
  WriteStopTag('', Empty, Attributes);
end;

end.

