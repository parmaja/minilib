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
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$modeswitch multihelpers}
{$ENDIF}

interface

uses
  Classes, SysUtils, mnClasses, mnXML, mnXMLUtils, mnStreams;

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
    FMaxLine: Integer;
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
    procedure StreamWriteString(const vStr: string);
    procedure StreamWriteLine(const vStr: string = '');


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
    procedure AddAttributes(AttNames, AttValues: TArray<string>);
    procedure AddXMLNS(NameSpace, Value: string); overload;
    //StopTag add attribute and close it by > or /> if empty = true
    procedure StopTag(NameSpace, Name: string; CloseIt: Boolean = False); overload;
    procedure StopTag(NameSpace, Name: string; vCallback: TProc); overload;
    procedure StopTag(Name: string); overload;
    procedure StopTag; overload;

    //OpenTag add Tag with attributes <name att1="val1">
    //procedure WriteOpenTag(const Name, NameSpace: string); overload;
    procedure OpenTag(NameSpace, Name: string; AttNames, AttValues: TArray<string>; CloseIt: Boolean = False); overload;

    procedure OpenTag(Name: string; AttNames, AttValues: TArray<string>); overload;
    procedure OpenTag(NameSpace, Name: string; AttName, AttValue: string); overload;
    procedure OpenTag(Name: string; AttName, AttValue: string); overload;
    procedure OpenTag(NameSpace, Name: string); overload;
    procedure OpenTag(Name: string); overload;
    //CloseTag work with WriteStartTag and WriteOpenTag
    procedure CloseTag(NameSpace, Name: string); overload;
    procedure CloseTag(Name: string); overload;
    procedure CloseTag; overload;
    //Add closed tage
    procedure AddTag(NameSpace, Name: string; AttNames, AttValues: TArray<string>; vCallback: TProc = nil); overload;
    procedure AddTag(NameSpace, Name: string; AttName, AttValue: string; vCallback: TProc = nil); overload;
    procedure AddTag(NameSpace, Name: string; vCallback: TProc = nil); overload;
    procedure AddTag(Name: string; vCallback: TProc = nil); overload;
    //Add Closed tag with text
    //If Text is empty tag will NOT added
    procedure AddText(NameSpace, TagName, Text: string; AttNames: TArray<string> = []; AttValues: TArray<string> = []); overload;
    procedure AddText(TagName, Text: string); overload;
    //Add Text value only
    procedure AddText(Value: string); overload;
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
    property MaxLine: Integer read FMaxLine write FMaxLine; //auto break everyrhing, but hearts :)
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
  StreamWriteString('AddStartTag(' + QuoteStr(Name, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteAttribute(Name, Value: string);
begin
  StreamWriteString('AddAttribute(' + QuoteStr(Name, '''') + ', ' + QuoteStr(Name, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteStopTag(Name: string; CloseIt: Boolean);
begin
  if CloseIt then
    StreamWriteString('StopTag(' + QuoteStr(Name, '''') + ');') //TODO
  else
    StreamWriteString('StopTag(' + QuoteStr(Name, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteCloseTag(Name: string);
begin
  StreamWriteString('CloseTag(' + QuoteStr(Name, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteText(Value: string);
begin
  StreamWriteString('AddText(' + QuoteStr(Value, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteComment(Value: string);
begin
  StreamWriteString('AddComment(' + QuoteStr(Value, '''') + ');') ;
end;

procedure TmnPascalWriter.DoWriteCDATA(Value: string);
begin
  StreamWriteString('AddCDATA(' + QuoteStr(Value, '''') + ');') ;
end;

{ TmnJSONWriter }

procedure TmnJSONWriter.DoWriteStartTag(Name: string);
begin
  StreamWriteString(QuoteStr(Name) + ' = {');
end;

procedure TmnJSONWriter.DoWriteAttribute(Name, Value: string);
begin
  StreamWriteString(QuoteStr(Name) + ' = ' + QuoteStr(Value));
end;

procedure TmnJSONWriter.DoWriteStopTag(Name: string; CloseIt: Boolean);
begin
  //StreamWriteString(Name + '}');
end;

procedure TmnJSONWriter.DoWriteCloseTag(Name: string);
begin
  StreamWriteString('}');
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
  StreamWriteString('<' + Name);
end;

procedure TmnXMLWriter.DoWriteAttribute(Name, Value: string);
begin
  StreamWriteString(Name+'=' + QuoteStr(EntityEncode(Value)));
end;

procedure TmnXMLWriter.DoWriteStopTag(Name: string; CloseIt: Boolean);
begin
  if CloseIt then
    StreamWriteString('/>')
  else
    StreamWriteString('>');
end;

procedure TmnXMLWriter.DoWriteCloseTag(Name: string);
begin
  StreamWriteString('</' + Name + '>');
end;

procedure TmnXMLWriter.DoWriteText(Value: string);
begin
  StreamWriteString(EntityEncode(Value));
end;

procedure TmnXMLWriter.DoWriteComment(Value: string);
begin
  StreamWriteString('<!--' + EntityEncode(Value) + '-->');
end;

procedure TmnXMLWriter.DoWriteCDATA(Value: string);
begin
  StreamWriteString('<![CDATA[' + EntityEncode(Value) + ']]');
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

procedure TmnCustomXMLWriter.AddTag(NameSpace, Name: string; AttNames, AttValues: TArray<string>; vCallback: TProc);
begin
  if Assigned(vCallback) then
  begin
    OpenTag(NameSpace, Name, AttNames, AttValues, False);
    vCallback;
    CloseTag(NameSpace, Name);
  end
  else
    OpenTag(NameSpace, Name, AttNames, AttValues, True);
end;

procedure TmnCustomXMLWriter.AddTag(NameSpace, Name: string; AttName, AttValue: string; vCallback: TProc);
begin
  AddTag(NameSpace, Name, [AttName], [AttValue], vCallback);
end;

constructor TmnCustomXMLWriter.Create;
begin
  inherited Create;
  FMaxLine := cMaxLine;
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
    StreamWriteString(Stream.EndOfLine + RepeatString(Tabs, FOpenedTags.Count));

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

procedure TmnCustomXMLWriter.OpenTag(Name: string; AttNames, AttValues: TArray<string>);
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
    StreamWriteString(Stream.EndOfLine + GetIndents);

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
  StreamWriteLine(s);
end;

procedure TmnCustomXMLWriter.AddTag(Name: string; vCallback: TProc);
begin
  AddTag('', Name, [], [], vCallback);
end;

procedure TmnCustomXMLWriter.AddTag(NameSpace, Name: string; vCallback: TProc);
begin
  AddTag(NameSpace, Name, [], [], vCallback);
end;

procedure TmnCustomXMLWriter.AddText(Value: string);
begin
  if Value <> '' then
  begin
    DoWriteText(Value);
    FTaging := False;
  end;
end;

procedure TmnCustomXMLWriter.AddText(NameSpace, TagName, Text: string; AttNames: TArray<string>; AttValues: TArray<string>);
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
    StreamWriteString(' ');
  DoWriteComment(Value);
end;

procedure TmnCustomXMLWriter.AddCommentLine(Value: string);
begin
  if Smart and (Tabs <> '') then
  begin
    StreamWriteLine;
    StreamWriteString(GetIndents);
  end;
  DoWriteComment(Value);
end;

procedure TmnCustomXMLWriter.AddCDATA(Value: string);
begin
  if Smart and (Tabs <> '') then
    StreamWriteString(GetIndents);
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
      StreamWriteLine;
      StreamWriteString(GetIndents(FOpenedTags.Count));
    end
    else if (AttributesCount > 0) then
    begin
      if (Length(Value) > MaxLine) then
      begin
        StreamWriteLine;
        StreamWriteString(GetIndents(FOpenedTags.Count));
      end
      else
        StreamWriteString(' ');
    end
    else
      StreamWriteString(' ');
    DoWriteAttribute(Name, Value);
  end
  else
  begin
    StreamWriteString(' ');
    DoWriteAttribute(Name, Value);
  end;
  Inc(AttributesCount);
end;

procedure TmnCustomXMLWriter.AddAttributes(AttNames, AttValues: TArray<string>);
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
  if NameSpace='' then
    AddAttribute('xmlns', Value)
  else
    AddAttribute('xmlns' + sNameSpaceSeparator + NameSpace, Value);
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

procedure TmnCustomXMLWriter.StopTag(NameSpace, Name: string; vCallback: TProc);
begin
  StopTag(NameSpace, Name, False);
  vCallback;
  CloseTag(NameSpace, Name);
end;

procedure TmnCustomXMLWriter.StreamWriteLine(const vStr: string);
begin
  Stream.WriteLineUTF8(UTF8Encode(vStr));
end;

procedure TmnCustomXMLWriter.StreamWriteString(const vStr: string);
begin
  Stream.WriteUTF8(UTF8Encode(vStr));
end;

procedure TmnCustomXMLWriter.OpenTag(NameSpace, Name: string; AttNames, AttValues: TArray<string>; CloseIt: Boolean);
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

