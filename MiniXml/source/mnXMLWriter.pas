unit mnXMLWriter;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Classes, SysUtils, mnXML, mnXMLUtils;

type
  TmnXMLWriter = class(TmnXMLFiler)
  private
    FOpenedTags: TStringList;
    FSmart: Boolean;
    FTabs: string;
    FEncoding: Boolean;
    FTaging: Boolean;
    FTagStarted: Boolean; //Work with StartTag
    procedure SetSmart(const Value: Boolean);
  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure WriteHeader;
    function CharsetEncode(const Value: string): string;
  public
    constructor Create; override;
    destructor Destroy; override;
    //WriteStartTag like <name wotk wit StopTag
    //without add attribute and te > sign
    procedure WriteStartTag(const Name: string);
    procedure WriteAttributes(const Attributes: string = '');
    //StopTag add attribute and close it by > or /> if empty = true
    procedure WriteStopTag(Empty: Boolean = False; const Attributes: string = '');
    //OpenTag add Tag with attributes <name att1="val1">
    procedure WriteOpenTag(const Name: string; const Attributes: string = '');
    //CloseTag work with WriteStartTag and WriteOpenTag
    procedure WriteCloseTag(Name: string = '');
    procedure WriteEmptyTag(Name: string);
    procedure WriteText(Value: string);
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

{ TmnXMLWriter }

procedure TmnXMLWriter.WriteCloseTag(Name: string);
begin
  if FSmart then
  begin
    if Name = '' then
    begin
      Name := FOpenedTags[FOpenedTags.Count - 1];
      FOpenedTags.Delete(FOpenedTags.Count - 1);
    end
    else
    begin
      if (FOpenedTags.Count = 0) or (FOpenedTags[FOpenedTags.Count - 1] <> Name) then //not the last tag
        raise EmnXMLException.Create('Tag name must not close now');
      FOpenedTags.Delete(FOpenedTags.Count - 1);
    end;
  end;
  if Smart and FTaging and (Tabs <> '') then
    Stream.WriteString(Stream.EndOfLine + RepeatString(Tabs, FOpenedTags.Count) + '</' + Name + '>')
  else
    Stream.WriteString('</' + Name + '>');
  FTaging := True;
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

procedure TmnXMLWriter.WriteOpenTag(const Name, Attributes: string);
var
  s: string;
begin
  if Smart and FTaging and (Tabs <> '') then
    s := Stream.EndOfLine + RepeatString(Tabs, FOpenedTags.Count);
  s := s + '<' + Name + Enclose(Attributes, ' ') + '>';
  Stream.WriteString(S);
  FTaging := True;
  if FSmart then
    FOpenedTags.Add(Name);
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
  Stream.WriteLn(s);
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

function TmnXMLWriter.CharsetEncode(const Value: string): string;
begin
  Result := Value;
end;

procedure TmnXMLWriter.WriteComment(Value: string);
begin
  Stream.WriteString('<!--' + EntityEncode(Value) + '-->');
end;

procedure TmnXMLWriter.Write(const Name, Value, Attributes: string);
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

procedure TmnXMLWriter.WriteStartTag(const Name: string);
var
  s: string;
begin
  if Smart and FTaging and (Tabs <> '') then
    s := Stream.EndOfLine + RepeatString(Tabs, FOpenedTags.Count);
  s := s + '<' + Name;
  Stream.WriteString(S);
  FTaging := True;
  FTagStarted := True;
  if FSmart then
    FOpenedTags.Add(Name);
end;

procedure TmnXMLWriter.WriteStopTag(Empty: Boolean; const Attributes: string);
var
  s: string;
begin
  if not FTagStarted then
    raise EmnXMLException.Create('Tag not started');
  s := Enclose(Attributes, ' ');
  if Empty then
  begin
    if Smart then
      FOpenedTags.Delete(FOpenedTags.Count - 1); //close last tag must remove it
    s := s + '/>'
  end
  else
    s := s + '>';
  Stream.WriteString(S);
  FTagStarted := False;
end;

procedure TmnXMLWriter.WriteAttributes(const Attributes: string);
begin
  Stream.WriteString(Enclose(Attributes, ' '));
end;

end.

