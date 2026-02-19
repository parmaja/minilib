unit mnJSON;
{ **
  *  JSON Parser
  *    without object tree
  *
  *  This file is part of the "Mini Library"
  *
  * @license   The MIT License (MIT)
  *
  *            See the file COPYING.MLGPL, included in this distribution,
  * @author    Zaher Dirkey <zaher, zaherdirkey>
  * @author    Belal AlHamad
  *
  * }

{$A8,C+,O+,W-,Z1}
{$STRINGCHECKS OFF}

{$IFDEF FPC}
{$MODE delphi}
{$ModeSwitch arrayoperators}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}
{$ENDIF}
{$M+}{$H+}
{$ifdef mswindows}
{$define windows}
{$endif}

{$STRINGCHECKS OFF}
{$POINTERMATH ON}

{.$define verbose}

interface

uses
  {$IFDEF windows}Windows, {$ENDIF}
  Classes, SysUtils, StrUtils, DateUtils, Types, Character,
  mnUtils;

type
  TJSONParseOption = (
    jsoModern, //* JSON5 compatiple (as possible
    jsoComments, //* Read comment in object tree, do not skip it
    jsoSafe, //* No Exceptions
    jsoNoDuplicate,//TODO do not allow duplicate names
    jsoUTF8 //TODO , no, always UTF8
  );
  TJSONParseOptions = set of TJSONParseOption;

  TmnJsonAcquireType = (
    aqComment,
    aqPair,
    aqObject,
    aqArray,
    aqString,
    aqIdentifier,
    aqNumber,
    aqBoolean
  );

  TmnJsonTypeOptions = set of (
    jtoMultiLine,
    jtoSingleQuote
  );

  TmnJsonAcquireProc = procedure(AParentObject: TObject; const Value: String; const ValueType: TmnJsonAcquireType; TypeOptions: TmnJsonTypeOptions; out AObject: TObject);

  { TmnJSONParser }

  TmnJSONParser = record
  private
    type
      TState = (stNone, stOpen);

      TExpect = (
        exValue,
        exName,
        exAssign, // :
        exNext, // ,
        exEnd // End of line
      );
      TExpects = set of TExpect;

      TContext = (
        cxPair,
        cxArray
      );

      TContexts = set of TContext;

      TToken = (
        tkNone,
        tkDQString,
        tkSQString,
        tkEscape,
        tkEscapeChar,
        tkNumber,
        tkIdentifire,
        tkCommentOpen,
        tkSLComment,
        tkMLComment,
        tkReturn //End of line to escape #10
      );

      TStackItem = record
        State: TState;
        Context: TContext;
        Parent: TObject;
      end;

      TStack = array of TStackItem;

      { TStringCollector }

      TStringCollector = record
      public
        Token: TToken;
        Started: Integer;
        Buffer: UTF8String;
        Escape: UTF8String;
        IsMultiLine: Boolean;
        procedure Reset(AIndex: Integer; AToken: TToken); inline;
        procedure Append(const s: string); inline;
        //* Take partial content
        procedure Collect(const Content: PByte; Index: Integer); inline;
        function GetSize(Index: Integer): Integer;
        function GetTypeOptions: TmnJsonTypeOptions;
      end;

    var
      AcquireProc: TmnJsonAcquireProc;

      Stack: TStack;
      Parent: TObject;
      Pair: TObject;
      Context: TContext;
      State: TState;
      StackIndex: Integer;
      Expect: TExpect;

      LineNumber: Int64;
      ColumnNumber: Int64;

      Token: TToken;

      Collector: TStringCollector;

      CommentStarted: Integer;
      CommentBuffer: UTF8String;

      Index: Integer;
      Options: TJSONParseOptions;
      ErrorMessage: String;
      LastChar: UTF8Char;
    procedure RaiseError(AError: string; Line: Integer = 0; Column: Integer = 0);
    procedure Push; inline;
    procedure Pop; {$ifndef DEBUG}inline; {$endif}
    procedure Next; inline;
    procedure CheckExpected(AExpected: TExpects; AContexts: TContexts = [cxPair, cxArray]); inline;
    procedure Error(const Msg: string); inline;
    procedure ErrorNotExpected(AExpected: TExpects; AContexts: TContexts = [cxPair, cxArray]); //not inline
  public
    //Always Init and Finish
    procedure Init(AParent: TObject; vAcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
    procedure Parse(const Content: PByte; Size: Integer; From: Integer = 0); overload;
    procedure Parse(const Content: UTF8String); overload;
    procedure Finish;
  end;

procedure JsonParseCallback(const Content: UTF8String; out Error: string; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
function JsonLintString(const S: string; Options: TJSONParseOptions = []): string; //Return Error message
//For testing
function JsonLintChunks(const Content: string; Options: TJSONParseOptions = []; ChunkSize: Integer =3): string; //Return Error message

implementation

const
  sNumberChars = ['.', '-', '+',
                  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                  'a', 'b', 'c', 'd', 'e', 'f', 'h', 'x',
                  'A', 'B', 'C', 'D', 'E', 'F', 'H', 'X'
                 ];

function CopyString(const Value: PByte; Start, Count: Integer): String; {$ifndef DEBUG}inline; {$endif}
begin
  if Count = 0 then
    Result := ''
  else
  begin
    //Result := TEncoding.UTF8.GetString(Value, Start, Count);
    {$ifdef FPC}
    SetLength(Result, Count);
    CopyMemory(@Result[1], @Value[Start], Count);
    {$else}
    Result := TEncoding.UTF8.GetString(Value, Start, Count);
    {$endif}
  end;
end;

{ TStringCollector }

procedure TmnJSONParser.TStringCollector.Reset(AIndex: Integer; AToken: TToken);
begin
  Token := AToken;
  Started := AIndex;
  Buffer := '';
  Escape := '';
  IsMultiLine := False;
end;

procedure TmnJSONParser.TStringCollector.Append(const s: string);
begin
  Buffer := Buffer + s;
end;

procedure TmnJSONParser.TStringCollector.Collect(const Content: PByte; Index: Integer);
begin
  Append(CopyString(Content, Started, Index - Started));
end;

function TmnJSONParser.TStringCollector.GetSize(Index: Integer): Integer;
begin
  Result := Index - Started;
end;

function TmnJSONParser.TStringCollector.GetTypeOptions: TmnJsonTypeOptions;
begin
  if Token = tkSQString then
    Result := [jtoSingleQuote]
  else
    Result := [];
  if IsMultiLine then
    Result := Result + [jtoMultiLine];
end;

procedure TmnJSONParser.RaiseError(AError: string; Line: Integer = 0; Column: Integer = 0);
begin
  if Line > 0 then
    ErrorMessage := AError + ' [line: ' + Line.ToString + ', column: ' + Column.ToString+']'
  else
    ErrorMessage := AError + ' [column: '+ Column.ToString+']';

  if not (jsoSafe in Options) then
    raise Exception.Create(ErrorMessage)
  {$ifdef DEBUG}
{  else if IsConsole then
    WriteLn(ErrorMessage);}
  {$endif}
end;

procedure TmnJSONParser.ErrorNotExpected(AExpected: TExpects; AContexts: TContexts);
var
  Result: string;
begin
  if not (Expect in AExpected) then
  begin
    Result := 'Expected';
    case Expect of
      exName: Result := Result + ' name';
      exValue: Result := Result + ' value';
      exAssign: Result := Result + ' colon `:`';
      exNext: Result := Result + ' comma `,`';
      exEnd:;
    end;

    Error(Result)
  end;

  if not (Context in AContexts) then
  begin
    if StackIndex<0 then
      Error('Expected EOF')
    else if Context = cxArray then
      Error('Expected in Array')
    else
      Error('Expected in Pairs');
    Error(Result);
  end;
end;

procedure TmnJSONParser.Push; {$ifdef FPC} inline; {$endif}
begin
  {$ifdef verbose}
  Writeln(Format('%0.4d ', [LineNumber])+ RepeatString('    ', Length(Stack))+ 'Push '+ TRttiEnumerationType.GetName(Context)+ ' ' +TRttiEnumerationType.GetName(Expect));
  {$endif}
  if StackIndex >= Length(Stack) then
    SetLength(Stack, StackIndex + 1);
  Stack[StackIndex].Parent := Parent;
  Stack[StackIndex].Context := Context;
  Stack[StackIndex].State := State;
  StackIndex := StackIndex + 1;
end;

procedure TmnJSONParser.Next;
begin
  Inc(Index);
  Inc(ColumnNumber);
end;

procedure TmnJSONParser.CheckExpected(AExpected: TExpects; AContexts: TContexts);
begin
  if not (Expect in AExpected) or not (Context in AContexts) then
  begin
    ErrorNotExpected(AExpected, AContexts)
  end;
end;

procedure TmnJSONParser.Error(const Msg: string);
begin
  RaiseError(Msg, LineNumber, ColumnNumber);
end;

procedure TmnJSONParser.Init(AParent: TObject; vAcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
begin
  Options := vOptions;
  AcquireProc := vAcquireProc;
  Parent := AParent;
  StackIndex := 0;
  SetLength(Stack, 100); //* Buffing it for fast grow
  Context := cxPair;
  Expect := exValue;
  State := stOpen;
  LineNumber := 1;
  ColumnNumber := 1;
  Pair := nil;
  Token := tkNone;
  LastChar := #0;
end;

procedure TmnJSONParser.Finish;
begin
  if (Expect = exNext) then
  begin
    if StackIndex > 0 then
    begin
      if Context = cxPair then
        Error('Expected } but found EOF')
      else
        Error('Expected ] but found EOF');
    end
  end
  else if (Expect <> exEnd) then
    Error('Expected EOF');
end;

procedure TmnJSONParser.Parse(const Content: UTF8String);
begin
  Parse(PByte(Content), Length(Content));
end;

procedure TmnJSONParser.Pop; {$ifdef FPC} inline; {$endif}
begin
  if StackIndex = 0 then
  begin
    Error('Expected EOF');
    exit;
  end;
  Parent := Stack[StackIndex-1].Parent;
  Context := Stack[StackIndex-1].Context;
  State := Stack[StackIndex-1].State;
  StackIndex := StackIndex - 1;
  {$ifdef verbose}
  Writeln(Format('%0.4d ', [LineNumber])+RepeatString('    ', Length(Stack)) + 'Pop '+ TRttiEnumerationType.GetName(Context) +' '+TRttiEnumerationType.GetName(Expect));
  {$endif}
end;

procedure TmnJSONParser.Parse(const Content: PByte; Size: Integer; From: Integer = 0);
var
  Ch: UTF8Char;
  AObject: TObject;

  procedure ContinueString;
  begin
    if not (jsoModern in Options) then
    begin
    end;

    if Ch = '\' then
    begin
      Collector.Collect(Content, Index);
      Collector.Started := Index + 1;
      Token := tkEscape;
    end
    else if jsoModern in Options then
    begin
      if Ch = #0 then
      begin
        Collector.Collect(Content, Index);
        Collector.Started := Index + 1;
      end
      else if Ch = #13 then
      begin
        Collector.Collect(Content, Index);
        Collector.Started := Index + 1;
      end
      else if Ch = #10 then
      begin
        if LastChar <> #13 then
        begin
          Collector.Collect(Content, Index);
          Collector.Started := Index + 1;
        end;
      end
    end
    else if CharInSet(Ch, [#0, #10, #13]) then
      Error('End of line in string!');
  end;

  procedure EndString;
  begin
    if Expect = exName then
    begin
      //Creating a Pair Item
      Collector.Collect(Content, Index);
      AcquireProc(Parent, Collector.Buffer, aqPair, [], Pair);
      Expect := exAssign;
    end
    else if Expect = exValue then
    begin
      Collector.Collect(Content, Index);
      AcquireProc(Parent, Collector.Buffer, aqString, Collector.GetTypeOptions, AObject);
      Expect := exNext;
    end
    else
      CheckExpected([exName, exValue], [Context]);
    Token := tkNone;
    Collector.Token := tkNone;
  end;

  procedure SetEscapeChar(Ch: UTF8Char);
  begin
    Collector.Buffer := Collector.Buffer + Ch;
    Next;
    Collector.Started := Index;
    Token := Collector.Token;
  end;

  procedure IlligalCharacter(Ch: UTF8Char);
  begin
    Error('Illigal character: ' + Ch + ' '+ IntToHex(ord(Ch)));
  end;
var
  SkipObject: TObject;
begin
  if (@AcquireProc = nil) then
    Error('JSON Parser: Acquire is nil');
{  if (Parent = nil) then //* nope Linting pass nil
    Error('JSON Parser: Parent is nil');}

  if Content = nil then
  begin
    Inc(LineNumber);
    exit;
  end;

  Index := From;
  Collector.Started := 0;
  try
    repeat
      LastChar := Ch;
      Ch := UTF8Char(Content[Index]);
      case Token of
        tkReturn:
        begin
          if Ch = #10 then
            Next;
          Token := tkNone;
        end;
        tkCommentOpen:
        begin
          if Ch = '/' then
          begin
            Token := tkSLComment;
            Next;
            CommentBuffer := '';
            CommentStarted := Index;
          end
          else if Ch = '*' then
          begin
            Token := tkMLComment;
            Next;
          end
          else
            Error('Expected / or * for comment, but found ' + Ch);
        end;
        tkSLComment:
        begin
          if CharInSet(Ch, [#0, #10, #13]) then
          begin
            Token := tkNone;
            //CommentBuffer := CommentBuffer + CopyString(Content, CommentStarted, Index - CommentStarted);
            //AcquireProc(Parent, CommentBuffer, aqComment, [], SkipObject);
          end;
          Next;
        end;
        tkMLComment:
        begin
          if (Ch = '/') and (LastChar = '*') then
            Token := tkNone;
          Next;
        end;
        tkEscapeChar:
        begin
          if Length(Collector.Escape) < 4 then
          begin
            if not (jsoModern in Options) then
            begin
              if CharInSet(Ch, [#0, #10, #13]) then
                Error('End of line in string!');
            end;
            Collector.Escape := Collector.Escape + Ch;
            Next;
        end
          else
          begin
            if Collector.Escape <> '' then
            begin
              Collector.Buffer := Collector.Buffer + UTF8Encode({$ifdef FPC}Character{$else}Char{$endif}.ConvertFromUtf32(StrToInt('$'+Collector.Escape)));
              Collector.Escape := '';
            end;
            Collector.Started := Index;
            Token := Collector.Token;
//            Next;
          end;
        end;
        tkEscape:
        begin
          if not (jsoModern in Options) then
          begin
            if CharInSet(Ch, [#0, #10, #13]) then
              Error('End of line in string!');
          end;

          case Ch of
            'u':
            begin
              Collector.Escape := '';
              Token := tkEscapeChar;
              Next;
              Collector.Started := Index;
            end;
            #13:
            begin
              Collector.Buffer := Collector.Buffer + #13;
              Collector.IsMultiLine := True;
              inc(LineNumber);
              ColumnNumber := 1;
              Next;
              Collector.Started := Index;
            end;
            #10:
            begin
              Collector.Buffer := Collector.Buffer + #10;
              if LastChar <> #13 then
              begin
                Collector.IsMultiLine := True;
                Inc(LineNumber);
                ColumnNumber := 1;
              end;
              Next;
              Collector.Started := Index;
            end;
            //* We need use map instead
            'b': SetEscapeChar(#8);
            't': SetEscapeChar(#9);
            'n': SetEscapeChar(#10);
            'f': SetEscapeChar(#12);
            'r': SetEscapeChar(#13);
            '0': SetEscapeChar(#0);
            else
              SetEscapeChar(Ch);
          end;
        end;
        tkDQString:
        begin
          if Ch = '"' then
            EndString
          else
            ContinueString;
          //Next char yes, we do not need " anymore
          Next;
        end;
        tkSQString:
        begin
          if Ch = '''' then
            EndString
          else
            ContinueString;
          //Next char yes, we do not need ' anymore
          Next;
        end;
        tkIdentifire:
        begin
          if not CharInSet(Ch, ['A'..'Z', 'a'..'z', '0'..'9',  '_']) then
          begin
            if Expect = exName then
            begin
              //Creating a Pair Item
              Collector.Collect(Content, Index);
              AcquireProc(Parent, Collector.Buffer, aqPair, [], Pair);
              Expect := exAssign;
            end
            else if Expect = exValue then
            begin
              Collector.Collect(Content, Index);
              AcquireProc(Parent, Collector.Buffer, aqIdentifier, [], AObject);
              Expect := exNext;
            end
            else
              CheckExpected([exName, exValue], [Context]);
            Token := tkNone;
          end
          else
          begin
            Next;
          end;
        end;
        tkNumber:
        begin
          if not CharInSet(Ch, sNumberChars) then
          begin
            if Expect = exValue then
            begin
              Collector.Collect(Content, Index);
              AcquireProc(Parent, Collector.Buffer, aqNumber, [], AObject);
              Expect := exNext;
            end
            else
              CheckExpected([exValue], [Context]);
            Token := tkNone;
          end
          else
          begin
            Next;
          end;
        end;
        else //* Open
        begin
          case Ch of
            ' ', #8, #9:; //* Nothing to do
            #13:
            begin
              Inc(LineNumber);
              ColumnNumber := 1;
              Token := tkReturn;
            end;
            #10:
            begin
              if LastChar <> #13 then
              begin
                Inc(LineNumber);
                ColumnNumber := 1;
              end;
            end;
            'A'..'Z', 'a'..'z', '_':
            begin
              CheckExpected([exName, exValue, exEnd]);
              Collector.Reset(Index, tkIdentifire);
              Token := tkIdentifire;
            end;
            '-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.': //may start with . ?
            begin
              CheckExpected([exValue, exEnd]);
              Collector.Reset(Index, tkNumber);
              Token := tkNumber;
            end;
            '"':
            begin
              CheckExpected([exName, exValue, exEnd]);
              Collector.Reset(Index + 1, tkDQString);
              Token := tkDQString;
            end;
            '''':
            begin
              if not (jsoModern in Options) then
                IlligalCharacter(Ch)
              else
              begin
                CheckExpected([exName, exValue, exEnd]);
                Collector.Reset(Index + 1, tkSQString);
                Token := tkSQString;
              end;
            end;
            '/':
            begin
              if not (jsoModern in Options) then
                IlligalCharacter(Ch)
              else
                Token := tkCommentOpen;
            end;
            ':':
            begin
              CheckExpected([exAssign], [cxPair]);
              Expect := exValue;
              Push;
              Parent := Pair;
            end;
            ',':
            begin
              CheckExpected([exNext]);
              if Context = cxPair then
              begin
                Pop;
                Expect := exName;
              end
              else
                Expect := exValue;
              State := stNone;
            end;
            '{' :
            begin
              CheckExpected([exValue]);
              Push;
              AcquireProc(Parent, '', aqObject, [], AObject);
              Parent := AObject;
              Context := cxPair;
              Expect := exName;
              State := stOpen;
            end;
            '}' :
            begin
              if State = stOpen then
                CheckExpected([exNext, exName], [cxPair])
              else
              begin
                if not (jsoModern in Options) then
                  CheckExpected([exNext], [cxPair]);
              end;
              if Expect = exNext then
                Pop;
              Pop;
              if StackIndex < 0 then
                Expect := exEnd
              else
                Expect := exNext;
            end;
            '[':
            begin
              CheckExpected([exValue]);
              Push;
              AcquireProc(Parent, '', aqArray, [], AObject);
              Parent := AObject;
              Context := cxArray;
              Expect := exValue;
              State := stOpen;
            end;
            ']':
            begin
              if State = stOpen then
                CheckExpected([exNext, exValue], [cxArray])
              else
              begin
                if not (jsoModern in Options) then
                  CheckExpected([exNext], [cxArray]);
              end;
              Pop;
              if StackIndex < 0 then
                Expect := exEnd
              else
                Expect := exNext;
            end;
            else
              IlligalCharacter(Ch);
          end;
          Next;
        end;
      end;
    until (Ch=#0) or (Index >= Size);
    if Token in [tkSQString, tkDQString] then
    begin

    end
    else if Token = tkSLComment then
    begin

    end;
  except
    on E: Exception do
    begin
      if (jsoSafe in Options) then
        RaiseError(E.Message, LineNumber, ColumnNumber)
      else
        raise;
    end;
  end;
end;

procedure JsonParseCallback(const Content: UTF8String; out Error: string; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
var
  JSONParser: TmnJSONParser;
begin
  JSONParser.Init(AParent, AcquireProc, vOptions);
  JSONParser.Parse(Content);
  JSONParser.Finish;
  Error := JSONParser.ErrorMessage;
end;

procedure JsonLintAcquireCallback(AParentObject: TObject; const Value: string; const ValueType: TmnJsonAcquireType; TypeOptions: TmnJsonTypeOptions; out AObject: TObject);
begin
  AObject := nil;
end;

function JsonLintString(const S: string; Options: TJSONParseOptions): string;
begin
  try
    JsonParseCallback(Utf8Encode(s), Result, nil, JsonLintAcquireCallback, Options);
  except
    on E: Exception do
    begin
      raise;
    end;
  end
end;

function JsonLintChunks(const Content: string; Options: TJSONParseOptions;
  ChunkSize: Integer): string;
var
  JSONParser: TmnJSONParser;
  s: string;
  i: Integer;
begin
  i:=1;
  JSONParser.Init(nil, JsonLintAcquireCallback, Options);
  while i < Length(Content) do
  begin
    s := copy(Content, i, ChunkSize);
    JSONParser.Parse(s);
    i := i + ChunkSize;
  end;
  JSONParser.Finish;
  Result := JSONParser.ErrorMessage;
end;

initialization
end.
