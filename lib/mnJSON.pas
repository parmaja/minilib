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

  TmnJsonAcquireSubType = set of (
    astSingleLine,
    astSingleQuote
  );

  TmnJsonAcquireProc = procedure(AParentObject: TObject; const Value: String; const ValueType: TmnJsonAcquireType; SubType: TmnJsonAcquireSubType; out AObject: TObject);

  { TmnJSONParser }

  TmnJSONParser = record
  private
    type
      TState = (stNone, stOpen);

      TExpect = (exValue, exName, exAssign, exNext, exEnd);
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

      TokenString: TToken;
      StringStarted: Integer;
      StringBuffer: UTF8String;
      EscapeBuffer: UTF8String;

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
    procedure Parse(const Content: PByte; Size: Integer; Start: Integer = 0); overload;
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

procedure TmnJSONParser.RaiseError(AError: string; Line: Integer = 0; Column: Integer = 0);
begin
  if Line > 0 then
    ErrorMessage := AError + ' :: line: ' + Line.ToString + ', column: ' + Column.ToString
  else
    ErrorMessage := AError + ' :: column: '+ Column.ToString;

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

procedure TmnJSONParser.Parse(const Content: PByte; Size: Integer; Start: Integer = 0);
var
  Ch: UTF8Char;
  AObject: TObject;

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

  procedure ContinueString;
  begin
    if not (jsoModern in Options) then
    begin
      if CharInSet(Ch, [#0, #10, #13]) then
      begin
        Error('End of line in string!');
        exit;
      end;
    end;

    if Ch = '\' then
    begin
      StringBuffer := StringBuffer + CopyString(Content, StringStarted, Index - StringStarted);
      StringStarted := Index + 1;
      Token := tkEscape;
    end
  end;

  procedure EndString;
  begin
    if Expect = exName then
    begin
      //Creating a Pair Item
      StringBuffer := StringBuffer + CopyString(Content, StringStarted, Index - StringStarted);
      AcquireProc(Parent, StringBuffer, aqPair, [], Pair);
      Expect := exAssign;
    end
    else if Expect = exValue then
    begin
      StringBuffer := StringBuffer + CopyString(Content, StringStarted, Index - StringStarted);
      if TokenString = tkSQString then
        AcquireProc(Parent, StringBuffer, aqString, [astSingleQuote], AObject)
      else
        AcquireProc(Parent, StringBuffer, aqString, [], AObject);
      Expect := exNext;
    end
    else
      CheckExpected([exName, exValue], [Context]);
    if StringBuffer <> '' then
      StringBuffer := '';
    Token := tkNone;
    TokenString := tkNone;
  end;

  procedure SetEscapeChar(Ch: UTF8Char);
  begin
    StringBuffer := StringBuffer + Ch;
    Next;
    StringStarted := Index;
    Token := TokenString;
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

  Index := Start;
  StringStarted := -1; //* for strings
//  Token := tkNone;
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
          if Length(EscapeBuffer) < 4 then
          begin
            if not (jsoModern in Options) then
            begin
              if CharInSet(Ch, [#0, #10, #13]) then
                Error('End of line in string!');
            end;
            EscapeBuffer := EscapeBuffer + Ch;
            Next;
        end
          else
          begin
            if EscapeBuffer <> '' then
            begin
              StringBuffer := StringBuffer + UTF8Encode({$ifdef FPC}Character{$else}Char{$endif}.ConvertFromUtf32(StrToInt('$'+EscapeBuffer)));
              EscapeBuffer := '';
            end;
            StringStarted := Index;
            Token := TokenString;
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
              EscapeBuffer := '';
              Token := tkEscapeChar;
              Next;
              StringStarted := Index;
            end;
            #13:
            begin
              StringBuffer := StringBuffer + #13;
              inc(LineNumber);
              ColumnNumber := 1;
              Next;
              StringStarted := Index;
            end;
            #10:
            begin
              StringBuffer := StringBuffer + #10;
              if LastChar <> #13 then
              begin
                Inc(LineNumber);
                ColumnNumber := 1;
              end;
              Next;
              StringStarted := Index;
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
              AcquireProc(Parent, CopyString(Content, StringStarted, Index - StringStarted), aqPair, [], Pair);
              Expect := exAssign;
            end
            else if Expect = exValue then
            begin
              AcquireProc(Parent, CopyString(Content, StringStarted, Index - StringStarted), aqIdentifier, [], AObject);
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
              AcquireProc(Parent, CopyString(Content, StringStarted, Index - StringStarted), aqNumber, [], AObject);
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
        else
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
              StringStarted := Index;
              Token := tkIdentifire;
            end;
            '-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.': //may start with . ?
            begin
              CheckExpected([exValue, exEnd]);
              StringStarted := Index;
              Token := tkNumber;
            end;
            '"':
            begin
              CheckExpected([exName, exValue, exEnd]);
              StringBuffer := '';
              StringStarted := Index + 1;
              Token := tkDQString;
              TokenString := tkDQString;
            end;
            '''':
            begin
              if not (jsoModern in Options) then
                IlligalCharacter(Ch)
              else
              begin
                CheckExpected([exName, exValue, exEnd]);
                StringBuffer := '';
                StringStarted := Index + 1;
                Token := tkSQString;
                TokenString := tkSQString;
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

procedure JsonLintAcquireCallback(AParentObject: TObject; const Value: string; const ValueType: TmnJsonAcquireType; SubType: TmnJsonAcquireSubType; out AObject: TObject);
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
