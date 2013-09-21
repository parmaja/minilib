unit mnXMLScanner;
{**
 *  This file is part of the "Mini Library"
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
  Classes, SysUtils, StrUtils, mnXML, mnXMLUtils;

type
  TmnScanState = (ssNone, ssHeader, ssProlog, ssText, ssReady, ssOpenTag, ssAttributes, ssCloseEmptyTag, ssCloseTag,
    ssPI, ssEscape, ssComment, ssBracket, ssCDATA, ssDeclare,
    ssScanStricted); //usfull for more than state

  TmnDeclarePhases = (dpELEMENT, dpENTITY, dpATTLIST, dpDOCTYPE);

  TmnParserProc = procedure(const Text: string; Line: Integer; var Column: Integer) of object;
  TmnParsers = array[TmnScanState] of TmnParserProc;

  TmnFlushBufferProc = procedure(const Text: string) of object;
  TmnFlushBuffers = array[TmnScanState] of TmnFlushBufferProc;

  TmnDeclarePhase = array[TmnDeclarePhases] of TmnParserProc;

  TmnXMLScanner = class(TmnXMLFiler)
  private
    FBuffer: string;
    FCurrentTag: string; 
    FState: TmnScanState;
    FParsers: TmnParsers;
    FFlushBuffers: TmnFlushBuffers;
    //
    FDeclareState: TmnDeclarePhases;
    FDeclarePhase: TmnDeclarePhase;
    FDepthIn: Integer;
    FDepthOut: Integer;
    FSection: TmnScanState;

    FNextScan: string;
    FNextState: TmnScanState;
    FCompleted: Boolean;
    FStarted: Boolean;
    procedure FlushBuffer(State: TmnScanState);
    procedure ScanBody(NextState: TmnScanState; const SubStr, Text: string; Line: Integer; var Column: Integer);
    procedure ScanStricted(NextState: TmnScanState; const SubStr, Text: string; Line: Integer; var Column: Integer);
    //Parser phases
    procedure ssOnNone(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnHeader(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnProlog(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnText(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnReady(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnOpenTag(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnAttributes(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnCloseEmptyTag(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnCloseTag(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnEscape(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnComment(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnCDATA(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnBracket(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnPI(const Text: string; Line: Integer; var Column: Integer);
    procedure ssOnDeclare(const Text: string; Line: Integer; var Column: Integer);
    //Declare idendifiers
    procedure dpOnELEMENT(const Text: string; Line: Integer; var Column: Integer);
    procedure dpOnDOCTYPE(const Text: string; Line: Integer; var Column: Integer);
    //utils
    procedure ssOnScanStricted(const Text: string; Line: Integer; var Column: Integer);
    function GetDepth: Integer;
  protected
    procedure ChangeState(NextState: TmnScanState);
    procedure AddBuffer(const Text: string; NextState: TmnScanState);
    //Events
    procedure ReadHeader(const Text: string); virtual;
    procedure ReadOpenTag(const Name: string); virtual;
    procedure ReadAttributes(const Text: string); virtual;//must manual ecnode decode entity for Attributes
    procedure PlainReadText(const Text: string); virtual;// before decode entity 
    procedure ReadText(const Text: string); virtual;
    procedure ReadComment(const Text: string); virtual;
    procedure ReadCDATA(const Text: string); virtual;
    procedure ReadCloseTag(const Name: string); virtual;
    //end Events
    procedure DoStart; override;
    procedure DoStop; override;
    property State: TmnScanState read FState;
    property Depth: Integer read GetDepth;
    property Started:Boolean read FStarted;
    property Completed:Boolean read FCompleted;
    property CurrentTag: string read FCurrentTag; //use with attributes or leaf elements
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ParseLine(const Text: string; Line: Integer);
  end;

implementation

{ TmnXMLScanner }

procedure TmnXMLScanner.ReadText(const Text: string);
begin
end;

procedure TmnXMLScanner.ReadCloseTag(const Name: string);
begin
end;

procedure TmnXMLScanner.DoStop;
begin
  inherited;
  FlushBuffer(State);
end;

procedure TmnXMLScanner.DoStart;
begin
  inherited;
end;

procedure TmnXMLScanner.ssOnCloseTag(const Text: string; Line: Integer; var Column: Integer);
var
  p: Integer;
begin
  FCurrentTag := '';
  p := ScanIdentifier(Text, Column);
  FNextScan := '>';
  AddBuffer(RangeStr(Text, Column, p), ssScanStricted); //must be in CloseTag state, ScanStricted will make it
  Column := p + 1; //eat the > char
  FDepthOut := FDepthOut + 1;
  if FStarted and (Depth = 0) then
    FCompleted := True;
end;

procedure TmnXMLScanner.ssOnEscape(const Text: string; Line: Integer; var Column: Integer);
var
  p: Integer;
  aIdent: string;
begin
  case Text[Column] of
    '[':
      begin
        FState := ssBracket;
      end;
    '-':
      begin
        if MidStr(Text, Column, 2) <> '--' then
          raise EmnXMLParserException.Create('Comment ' + sNotWellFormed, Line, Column);
        ChangeState(ssComment);
        Column := Column + 1; //eat the second "-" char
      end;
  else
    begin
      p := ScanIdentifier(Text, Column);
      aIdent := RangeStr(Text, Column, p);
      if aIdent = 'DOCTYPE' then
      begin
        FDeclareState := dpDOCTYPE;
        if FDepthIn > 0 then
          raise EmnXMLParserException.Create(sCanotHaveDOCTYPEDeclaration, Line, Column);
      end;
      AddBuffer(aIdent, ssDeclare);
      Column := p + 1;
    end;
  end;
  Inc(Column);
end;

procedure TmnXMLScanner.ssOnReady(const Text: string; Line: Integer; var Column: Integer);
begin
  case Text[Column] of
    '/':
      begin
        if FSection = ssProlog then
          raise EmnXMLParserException.Create('Can not close tag in prolog section');
        FState := ssCloseTag;
        Inc(Column);
      end;
    '?':
      begin
        FState := ssPI;
        Inc(Column);
      end;
    '!':
      begin
        FState := ssEscape;
        Inc(Column);
      end;
  else
    begin
      FSection := ssText;
      FState := ssOpenTag;
    end
  end;
end;

procedure TmnXMLScanner.ssOnPI(const Text: string; Line: Integer; var Column: Integer);
begin
  ScanBody(FSection, '?>', Text, Line, Column)
end;

procedure TmnXMLScanner.ssOnText(const Text: string; Line: Integer; var Column: Integer);
var
  p: Integer;
begin
  p := PosEx('<', Text, Column);
  if p > 0 then
  begin
    AddBuffer(RangeStr(Text, Column, p - 1), ssReady);
    Column := p + 1;
  end
  else
  begin
    AddBuffer(RangeStr(Text, Column, MaxInt), State);
    Column := Length(Text) + 1;
  end;
end;

procedure TmnXMLScanner.ReadOpenTag(const Name: string);
begin
end;

procedure TmnXMLScanner.ParseLine(const Text: string; Line: Integer);
var
  Column, l: Integer;
begin
  if not Active then
    raise EmnXMLException.Create('Scanner not started');
  Column := 1; //start of delphi string is 1
  l := Length(Text);
  while (Column <= l) do
  begin
    if not Assigned(FParsers[FState]) then
      raise EmnXMLException.Create('Parser state not assigned');
    FParsers[FState](Text, Line, Column);
  end;
end;

constructor TmnXMLScanner.Create;
begin
  inherited;
  FSection := ssProlog;

  FParsers[ssNone] := ssOnNone;
  FParsers[ssHeader] := ssOnHeader;
  FParsers[ssScanStricted] := ssOnScanStricted;
  FParsers[ssProlog] := ssOnProlog;
  FParsers[ssReady] := ssOnReady;
  FParsers[ssText] := ssOnText;
  FParsers[ssComment] := ssOnComment;
  FParsers[ssCDATA] := ssOnCDATA;
  FParsers[ssBracket] := ssOnBracket;
  FParsers[ssPI] := ssOnPI;
  FParsers[ssEscape] := ssOnEscape;
  FParsers[ssOpenTag] := ssOnOpenTag;
  FParsers[ssAttributes] := ssOnAttributes;
  FParsers[ssCloseEmptyTag] := ssOnCloseEmptyTag;
  FParsers[ssCloseTag] := ssOnCloseTag;
  FParsers[ssDeclare] := ssOnDeclare;

  FFlushBuffers[ssText] := PlainReadText;//ReadText;
//  FFlushBuffers[ssReady] := AddReady;
  FFlushBuffers[ssComment] := ReadComment;
  FFlushBuffers[ssCDATA] := ReadCDATA;
//  FFlushBuffers[ssPI] := AddPI;
//  FFlushBuffers[ssEscape] := AddEscape;
  FFlushBuffers[ssOpenTag] := ReadOpenTag;
  FFlushBuffers[ssAttributes] := ReadAttributes;
  FFlushBuffers[ssCloseEmptyTag] := ReadCloseTag;
  FFlushBuffers[ssCloseTag] := ReadCloseTag;
//  FFlushBuffers[ssElement] := AddElement;

  FDeclarePhase[dpELEMENT] := dpOnELEMENT;
  FDeclarePhase[dpDOCTYPE] := dpOnDOCTYPE;
end;

destructor TmnXMLScanner.Destroy;
begin

  inherited;
end;

procedure TmnXMLScanner.ssOnComment(const Text: string; Line: Integer; var Column: Integer);
begin
  ScanBody(FSection, sCloseComment, Text, Line, Column)
end;

procedure TmnXMLScanner.ssOnDeclare(const Text: string; Line: Integer; var Column: Integer);
begin
  if not Assigned(FDeclarePhase[FDeclareState]) then
    raise EmnXMLException.Create('Declare state not assigned');
  FDeclarePhase[FDeclareState](Text, Line, Column);
end;

procedure TmnXMLScanner.ssOnOpenTag(const Text: string; Line: Integer; var Column: Integer);
var
  p: Integer;
begin
  p := ScanIdentifier(Text, Column);
  FCurrentTag := RangeStr(Text, Column, p);
  AddBuffer(FCurrentTag, ssAttributes);
  Column := p + 1;
end;

procedure TmnXMLScanner.ssOnAttributes(const Text: string; Line: Integer; var Column: Integer);
var
  p, l: Integer;
  Quoted: string;
begin
//tag must be multi line
  p := Column;
  l := Length(Text);
  Quoted := '';
  while p <= l do
  begin
    if (Text[p] = Quoted) then
      Quoted := ''
    else if Quoted <> '' then
    else if (Text[p] = '/') or (Text[p] = '>') then
    begin
      if (Text[p] = '/') then
      begin
        if not (MidStr(Text, p + 1, 1) = '>') then
          raise EmnXMLParserException.Create('Tag ' + sNotWellFormed, Line, Column);
        AddBuffer(RangeStr(Text, Column, p - 1), ssCloseEmptyTag);
        FDepthIn := FDepthIn + 1;
        p := p + 2;
      end
      else
      begin
        AddBuffer(RangeStr(Text, Column, p - 1), ssText);
        FDepthIn := FDepthIn + 1;
        p := p + 1;
      end;
      Break;
    end
    else if (Text[p] = '"') or (Text[p] = '''') then
    begin
      Quoted := Text[p];
    end;
    Inc(p);
  end;
  Column := p;
end;

procedure TmnXMLScanner.ReadComment(const Text: string);
begin
end;

procedure TmnXMLScanner.ReadAttributes(const Text: string);
begin
end;

procedure TmnXMLScanner.AddBuffer(const Text: string; NextState: TmnScanState);
begin
  FBuffer := FBuffer + Text;
  ChangeState(NextState);
end;

procedure TmnXMLScanner.FlushBuffer(State: TmnScanState);
begin
  if Assigned(FFlushBuffers[State]) then
    FFlushBuffers[State](FBuffer);
  FBuffer := '';
end;

procedure TmnXMLScanner.ssOnCloseEmptyTag(const Text: string; Line: Integer; var Column: Integer);
begin
  AddBuffer(FCurrentTag, ssText);
  FCurrentTag := '';
end;

procedure TmnXMLScanner.ssOnCDATA(const Text: string; Line: Integer;
  var Column: Integer);
begin
  ScanBody(ssText, sCloseCDATA, Text, Line, Column)
end;

procedure TmnXMLScanner.ReadCDATA(const Text: string);
begin
end;

procedure TmnXMLScanner.ssOnBracket(const Text: string; Line: Integer;
  var Column: Integer);
var
  p: Integer;
  Ident: string;
begin                          
  p := ScanIdentifier(Text, Column);
  Ident := RangeStr(Text, Column, p + 1); //We take a [ char with the Identifier
  if Ident <> sOpenCDATA then
    raise EmnXMLParserException.Create('CDATA expected but ' + Ident + ' found', Line, Column);
  Column := p + 2; // p + 1 + [
  ChangeState(ssCDATA);
end;

procedure TmnXMLScanner.ChangeState(NextState: TmnScanState);
begin
  if FState <> NextState then
  begin
    FlushBuffer(FState);
    FState := NextState;
  end;
end;

procedure TmnXMLScanner.dpOnELEMENT(const Text: string; Line: Integer;
  var Column: Integer);
begin
  ScanBody(FSection, '>', Text, Line, Column)
end;

procedure TmnXMLScanner.dpOnDOCTYPE(const Text: string; Line: Integer;
  var Column: Integer);
begin
  ScanBody(FSection, '>', Text, Line, Column)
end;

procedure TmnXMLScanner.ssOnHeader(const Text: string; Line: Integer; var Column: Integer);
begin
  ScanBody(ssProlog, '?>', Text, Line, Column)
end;

procedure TmnXMLScanner.ssOnProlog(const Text: string; Line: Integer; var Column: Integer);
begin
  ScanStricted(ssReady, '<', Text, Line, Column);
end;

procedure TmnXMLScanner.ReadHeader(const Text: string);
begin
  ExtractStrings([' '], [], PChar(Text), Header);
end;

procedure TmnXMLScanner.ssOnNone(const Text: string; Line: Integer; var Column: Integer);
begin
  if MidStr(Text, 1, Length(sXMLAnsiOpen)) = sXMLAnsiOpen then
  begin
    //There is a header and it is a Ansi document
    FStarted := True;
    FCompleted := False;
    Column := Column + Length(sXMLAnsiOpen); //put the column to the first char of attributes of xml document
    ChangeState(ssHeader);
  end
  else
    ChangeState(ssProlog); //nop there is no header... skip to prolog section
end;

procedure TmnXMLScanner.ScanBody(NextState: TmnScanState; const SubStr, Text: string; Line: Integer; var Column: Integer);
var
  p: integer;
begin
  p := PosEx(SubStr, Text, Column);
  if p > 0 then
  begin
    AddBuffer(RangeStr(Text, Column, p - 1), NextState);
    Column := p + Length(SubStr);
  end
  else
  begin
    AddBuffer(RangeStr(Text, Column, MaxInt), State);
    Column := Length(Text) + 1;
  end;
end;

procedure TmnXMLScanner.ScanStricted(NextState: TmnScanState; const SubStr, Text: string; Line: Integer; var Column: Integer);
var
  p: integer;
  l, c, i: integer;
begin
  p := 0;
  c := 1;
  l := Length(SubStr);
  for i := Column to Length(Text) do
  begin
    if not (Text[i] in sWhitespace) then
    begin
      if Text[i] = SubStr[c] then
      begin
        if c = l then
        begin
          p := i + 1;
          break;
        end;
        Inc(c);
      end
      else
        raise EmnXMLParserException.Create('syntax error', Line, Column);
    end;
  end;

  if p > 0 then
  begin
    Column := p;
    ChangeState(NextState);
  end
  else
  begin
    Column := Length(Text) + 1;
    ChangeState(State);
  end;
end;

procedure TmnXMLScanner.ssOnScanStricted(const Text: string; Line: Integer;
  var Column: Integer);
begin
  ScanStricted(FNextState, FNextScan, Text, Line, Column);
end;

function TmnXMLScanner.GetDepth: Integer;
begin
  Result := FDepthIn - FDepthOut;
end;

procedure TmnXMLScanner.PlainReadText(const Text: string);
begin
  ReadText(EntityDecode(Text));
end;

end.

