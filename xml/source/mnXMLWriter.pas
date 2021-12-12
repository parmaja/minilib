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
    FBreaks: Boolean;
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
    procedure DoWriteStopTag(Name: string; CloseIt: Boolean); virtual; abstract;
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
    procedure StartTag(NameSpace, Name: string); overload;
    procedure StartTag(Name: string); overload;
    procedure AddAttribute(Name, Value: string);
    procedure AddAttributes(AttNames, AttValues: TStringArray);
    procedure AddXMLNS(NameSpace, Value: string); overload;
    //StopTag add attribute and close it by > or /> if empty = true
    procedure StopTag(NameSpace, Name: string; CloseIt: Boolean = False); overload;
    procedure StopTag(Name: string); overload;
    procedure StopTag; overload;

    //OpenTag add Tag with attributes <name att1="val1">
    //procedure WriteOpenTag(const Name, NameSpace: string); overload;
    procedure OpenTag(NameSpace, Name: string; AttNames, AttValues: TStringArray; CloseIt: Boolean = False); overload;
    procedure OpenTag(Name: string; AttNames, AttValues: TStringArray); overload;
    procedure OpenTag(NameSpace, Name: string; AttName, AttValue: string); overload;
    procedure OpenTag(Name: string; AttName, AttValue: string); overload;
    procedure OpenTag(NameSpace, Name: string); overload;
    procedure OpenTag(Name: string); overload;
    //CloseTag work with WriteStartTag and WriteOpenTag
    procedure CloseTag(NameSpace, Name: string); overload;
    procedure CloseTag(Name: string); overload;
    procedure CloseTag; overload;
    //Add closed tage
    procedure AddTag(NameSpace, Name: string; AttNames, AttValues: TStringArray); overload;
    procedure AddTag(NameSpace, Name: string; AttName, AttValue: string); overload;
    procedure AddTag(Name: string); overload;
    //Add Closed tag with text
    //If Text is empty tag will NOT added
    procedure AddText(NameSpace, TagName, Text: string; AttNames: TStringArray = []; AttValues: TStringArray = []); overload;
    procedure AddText(TagName, Text: string); overload;
    //Add Text value only
    procedure AddText(Value: string);
    procedure AddComment(Value: string);
    procedure AddCommentLine(Value: string);
    procedure AddCDATA(Value: string);
    //States
    property TagStarted: Boolean read FTagStarted;
    //Smart engine
    property Smart: Boolean read FSmart write SetSmart default False;
    property Encoding: Boolean read FEncoding write FEncoding default True;
    property Tabs: string read FTabs write FTabs;
    property Breaks: Boolean read FBreaks write FBreaks; //auto break everyrhing, but hearts :)
    //End smart engine
  end;

  TmnCustomXMLWriterClass = class of TmnCustomXMLWriter;

  { TmnXMLWriter }

  TmnXMLWriter = class(TmnCustomXMLWriter)
  protected
    procedure DoWriteStartTag(Name: string); override;
    procedure DoWriteAttribute(Name, Value: string); override;
    procedure DoWriteStopTag(Name: string; CloseIt: Boolean); override;
    procedure DoWriteCloseTag(Name: string); override;
    procedure DoWriteText(Value: string); override;
    procedure DoWriteComment(Value: string); override;
    procedure DoWriteCDATA(Value: string); override;
  end;

  { TmnPascalWriter }

  TmnPascalWriter = class(TmnCustomXMLWriter)
  protected
    procedure DoWriteStartTag(Name: string); override;
    procedure DoWriteAttribute(Name, Value: string); override;
    procedure DoWriteStopTag(Name: string; CloseIt: Boolean); override;
    procedure DoWriteCloseTag(Name: string); override;
    procedure DoWriteText(Value: string); override;
    procedure DoWriteComment(Value: string); override;
    procedure DoWriteCDATA(Value: string); override;
  end;

  { TmnJSONWriter }

  TmnJSONWriter = class(TmnCustomXMLWriter) //for fun
  protected
    procedure DoWriteStartTag(Name: string); override;
    procedure DoWriteAttribute(Name, Value: string); override;
    procedure DoWriteStopTag(Name: string; CloseIt: Boolean); override;
    procedure DoWriteCloseTag(Name: string); override;
    procedure DoWriteText(Value: string); override;
    procedure DoWriteComment(Value: string); override;
    procedure DoWriteCDATA(Value: string); override;
  end;

implementation

uses
  mnUtils;

{ TmnPascalWriter }

procedure TmnPascalWriter.DoWriteStartTag(Name: string);
begin
  Stream.WriteString('AddStartTag(' + QuoteStr(Name, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteAttribute(Name, Value: string);
begin
  Stream.WriteString('AddAttribute(' + QuoteStr(Name, '''') + ', ' + QuoteStr(Name, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteStopTag(Name: string; CloseIt: Boolean);
begin
  if CloseIt then
    Stream.WriteString('StopTag(' + QuoteStr(Name, '''') + ');') //TODO
  else
    Stream.WriteString('StopTag(' + QuoteStr(Name, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteCloseTag(Name: string);
begin
  Stream.WriteString('CloseTag(' + QuoteStr(Name, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteText(Value: string);
begin
  Stream.WriteString('AddText(' + QuoteStr(Value, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteComment(Value: string);
begin
  Stream.WriteString('AddComment(' + QuoteStr(Value, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteCDATA(Value: string);
begin
  Stream.WriteString('AddCDATA(' + QuoteStr(Value, '''') + ');') ;
end;

{ TmnJSONWriter }

procedure TmnJSONWriter.DoWriteStartTag(Name: string);
begin
  Stream.WriteString(QuoteStr(Name) + ' = {');
end;

procedure TmnJSONWriter.DoWriteAttribute(Name, Value: string);
begin
  Stream.WriteString(QuoteStr(Name) + ' = ' + QuoteStr(Value));
end;

procedure TmnJSONWriter.DoWriteStopTag(Name: string; CloseIt: Boolean);
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

procedure TmnXMLWriter.DoWriteStopTag(Name: string; CloseIt: Boolean);
begin
  if CloseIt then
    Stream.WriteString('/>')
  else
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

procedure TmnCustomXMLWriter.CloseTag(Name: string);
begin
  CloseTag('', Name);
end;

procedure TmnCustomXMLWriter.CloseTag;
begin
  CloseTag('');
end;

procedure TmnCustomXMLWriter.AddTag(NameSpace, Name: string; AttNames, AttValues: TStringArray);
begin
  OpenTag(NameSpace, Name, AttNames, AttValues, True);
end;

procedure TmnCustomXMLWriter.AddTag(NameSpace, Name: string; AttName, AttValue: string);
begin
  AddTag(NameSpace, Name, [AttName], [AttValue]);
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
      CloseTag;
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

procedure TmnCustomXMLWriter.StartTag(NameSpace, Name: string);
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

procedure TmnCustomXMLWriter.OpenTag(Name: string; AttNames, AttValues: TStringArray);
begin
  OpenTag('', Name, AttNames, AttValues);
end;

procedure TmnCustomXMLWriter.OpenTag(NameSpace, Name: string; AttName, AttValue: string);
begin
  OpenTag(NameSpace, Name, [AttName], [AttValue]);
end;

procedure TmnCustomXMLWriter.OpenTag(Name: string; AttName, AttValue: string);
begin
  OpenTag(Name, [AttName], [AttValue]);
end;

procedure TmnCustomXMLWriter.OpenTag(Name: string);
begin
  OpenTag(Name, [], []);
end;

procedure TmnCustomXMLWriter.CloseTag(NameSpace, Name: string);
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

procedure TmnCustomXMLWriter.AddTag(Name: string);
begin
  AddTag('', Name, [], []);
end;

procedure TmnCustomXMLWriter.AddText(Value: string);
begin
  if Value <> '' then
  begin
    DoWriteText(Value);
    FTaging := False;
  end;
end;

procedure TmnCustomXMLWriter.AddText(NameSpace, TagName, Text: string; AttNames: TStringArray; AttValues: TStringArray);
begin
  if Text <> '' then
  begin
    OpenTag(NameSpace, TagName, AttNames, AttValues);
    AddText(Text);
    CloseTag(NameSpace, TagName);
  end;
end;

procedure TmnCustomXMLWriter.AddText(TagName, Text: string);
begin
  AddText('', TagName, Text, [], []);
end;

procedure TmnCustomXMLWriter.AddComment(Value: string);
begin
  if Smart then
    Stream.WriteString(' ');
  DoWriteComment(Value);
end;

procedure TmnCustomXMLWriter.AddCommentLine(Value: string);
begin
  if Smart and (Tabs <> '') then
  begin
    Stream.WriteLine;
    Stream.WriteString(GetIndents);
  end;
  DoWriteComment(Value);
end;

procedure TmnCustomXMLWriter.AddCDATA(Value: string);
begin
  if Smart and (Tabs <> '') then
    Stream.WriteString(GetIndents);
  DoWriteCDATA(Value);
end;

function TmnCustomXMLWriter.CharsetEncode(const Value: string): string;
begin
  Result := Value;
end;

procedure TmnCustomXMLWriter.StartTag(Name: string);
begin
  StartTag('', Name);
end;

procedure TmnCustomXMLWriter.AddAttribute(Name, Value: string);
begin
  if not FTagStarted then
    raise EmnXMLException.Create('There is no tag started');
  if Smart then
  begin
    if Breaks then
    begin
      Stream.WriteLine;
      Stream.WriteString(GetIndents(FOpenedTags.Count));
    end
    else if (AttributesCount > 0) then
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

procedure TmnCustomXMLWriter.AddAttributes(AttNames, AttValues: TStringArray);
var
  i: Integer;
begin
  for i := 0 to Length(AttNames) -1 do
  begin
    if i < Length(AttValues) then
      AddAttribute(AttNames[i], AttValues[i]);
  end;
end;

procedure TmnCustomXMLWriter.StopTag(Name: string);
begin
  StopTag('', Name);
end;

procedure TmnCustomXMLWriter.AddXMLNS(NameSpace, Value: string);
begin
  NameSpace := 'xmlns' + sNameSpaceSeparator + NameSpace;
  AddAttribute(NameSpace, Value);
end;

procedure TmnCustomXMLWriter.StopTag(NameSpace, Name: string; CloseIt: Boolean);
begin
  if NameSpace <> '' then
    Name := NameSpace + sNameSpaceSeparator + Name;

  if not FTagStarted then
    raise EmnXMLException.Create('Tag not started');

  DoWriteStopTag(Name, CloseIt);

  FTagStarted := False;
  AttributesCount := 0;

  if CloseIt then
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
        if (FOpenedTags.Count = 0) then
          raise EmnXMLException.Create('Tag name not opened')
        else if (FOpenedTags[FOpenedTags.Count - 1] <> Name) then //not the last tag
          raise EmnXMLException.Create('Tag name "' + Name + '" is not "' + FOpenedTags[FOpenedTags.Count - 1] + '"');
        FOpenedTags.Delete(FOpenedTags.Count - 1);
      end;
    end;

    FTaging := True; //TODO: not sure
  end;
end;

procedure TmnCustomXMLWriter.StopTag;
begin
  StopTag('');
end;

procedure TmnCustomXMLWriter.OpenTag(NameSpace, Name: string; AttNames, AttValues: TStringArray; CloseIt: Boolean);
var
  i: Integer;
begin
  StartTag(NameSpace, Name);
  for i := 0 to Length(AttNames) -1 do
  begin
    if i < Length(AttValues) then
      AddAttribute(AttNames[i], AttValues[i])
    else
      AddAttribute(AttNames[i], '');
  end;
  StopTag(NameSpace, Name, CloseIt);
end;

procedure TmnCustomXMLWriter.OpenTag(NameSpace, Name: string);
begin
  OpenTag(NameSpace, Name, [], []);
end;

end.

