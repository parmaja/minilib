unit mnJSON;
{ **
  *  JSON Parser
  *  Without object tree, see DON
  *
  *  This file is part of the "Mini Library"
  *
  * @license   The MIT License (MIT)  *
  *
  * @author    Zaher Dirkey <zaher, zaherdirkey>
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
    jsoModernPlus, //* JSON5 compatiple (as possible
//    jsoComments, //* Read comment in object tree, do not skip it, now in Modern
    jsoSafe //* No Exceptions
  );
  TJSONParseOptions = set of TJSONParseOption;

  TmnJsonType = (
    aqComment,
    aqString,
    aqIdentifier,
    aqNumber,
    aqBoolean,
    aqArray,
    aqPair,
    aqObject
  );

  TmnJsonStringOptions = set of (
    jtoSingleQuote,
    jtoBackQuote,
    jtoMultiLine
  );

  TmnJsonStringType = record
    Name: string;
    Options: TmnJsonStringOptions;
  end;

  TmnJsonAcquireProc = procedure(out AObject: TObject; AParentObject: TObject; const Value: String; const ValueType: TmnJsonType; const StringOptions: TmnJsonStringType);

  { TmnJSONParser }

  TmnJSONParser = record
  private
    type
      TState = (stNone, stOpen);

      TExpect = (
        exValue,
        exName,
        exAssign, // :
        exNext,   // ,
        exEnd     // End of line
      );
      TExpects = set of TExpect;

      TContext = (
        cxPair,
        cxArray
      );

      TContexts = set of TContext;

      TToken = (
        tkNone,
        tkDoubleQuoteString,  //Double Quote "
        tkSingleQuoteString,  //Single Quote '
        tkBackQuoteString,  //Back Quote `
        tkBackQuoteType,    // Example `SQL select * from employees
        tkEscape,
        tkEscapeHex,
        tkNumber,
        tkIdentifire,
        tkCommentOpen,
        tkSingleLineComment,
        tkMultiLineComment
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
        Name: string; //Type name
        Token: TToken;
        Started: Integer;
        Buffer: UTF8String;
        Escape: UTF8String;
        EscapeLength: Integer;
        IsMultiLine: Boolean;
        procedure Reset(AIndex: Integer; AToken: TToken); inline;
        procedure Append(const s: string); inline;
        //* Take partial content
        procedure Collect(const Content: PByte; Index: Integer); inline;
        function GetSize(Index: Integer): Integer;
        function GetStringOptions: TmnJsonStringType;
        function CopyString(const Value: PByte; Start, Count: Integer): String;
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
      Comments: array of string;

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
    procedure NewLine; {$ifdef DEBUG}inline;{$endif}
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

  sHexChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
               'a', 'b', 'c', 'd', 'e', 'f',
               'A', 'B', 'C', 'D', 'E', 'F'
              ];


function TmnJSONParser.TStringCollector.CopyString(const Value: PByte; Start, Count: Integer): String;
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
  EscapeLength := 2;
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

function TmnJSONParser.TStringCollector.GetStringOptions: TmnJsonStringType;
begin
  Result.Name := Name;
  if Token = tkSingleQuoteString then
    Result.Options := [jtoSingleQuote]
  else if Token = tkBackQuoteString then
    Result.Options := [jtoSingleQuote, jtoBackQuote]
  else
    Result.Options := [];
  if IsMultiLine then
    Result.Options := Result.Options + [jtoMultiLine];
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
      exName: Result := Result + ' a Name';
      exValue: Result := Result + ' a Value';
      exAssign: Result := Result + ' a Colon `:`';
      exNext: Result := Result + ' a Comma `,`';
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

procedure TmnJSONParser.NewLine;
begin
  inc(LineNumber);
  ColumnNumber := 1;
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
    CheckExpected([exEnd], [Context]);
    //Error('Expected EOF');
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
        NewLine;
      end
      else if (Ch = #13) or ((Ch = #10) and (LastChar <> #13)) then
      begin
        if Collector.Token = tkBackQuoteString then
        begin
          Collector.IsMultiLine := True;
          Collector.Collect(Content, Index + 1);
          Collector.Started := Index + 1;
        end
        else
        begin
          Collector.Collect(Content, Index);
          Collector.Started := Index + 1;
        end;
        NewLine;
      end;
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
      AcquireProc(Pair, Parent, Collector.Buffer, aqPair, Default(TmnJsonStringType));
      Expect := exAssign;
    end
    else if Expect = exValue then
    begin
      Collector.Collect(Content, Index);
      AcquireProc(AObject, Parent, Collector.Buffer, aqString, Collector.GetStringOptions);
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
    Error('Illigal character: `' + Ch + '` '+ IntToHex(ord(Ch)));
  end;
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
        tkCommentOpen:
        begin
          if Ch = '/' then
          begin
            Token := tkSingleLineComment;
            Next;
            CommentBuffer := '';
            CommentStarted := Index;
          end
          else if Ch = '*' then
          begin
            Token := tkMultiLineComment;
            Next;
          end
          else
            Error('Expected / or * for comment, but found ' + Ch);
        end;
        tkSingleLineComment:
        begin
          if CharInSet(Ch, [#0, #10, #13]) then
          begin
            Token := tkNone;
            NewLine;
          end;
          Next;
        end;
        tkMultiLineComment:
        begin
          if (Ch = '/') and (LastChar = '*') then
            Token := tkNone;
          Next;
        end;
        tkEscapeHex:
        begin
          if (Length(Collector.Escape) < Collector.EscapeLength) and (Ch in sHexChars) then
          begin
            Collector.Escape := Collector.Escape + Ch;
            Next;
          end
          else
          begin
            if Collector.Escape <> '' then
            begin
              Collector.Append(UTF8Encode({$ifdef FPC}Character{$else}Char{$endif}.ConvertFromUtf32(StrToInt('$'+Collector.Escape))));
              Collector.Escape := '';
            end;
            Collector.Started := Index;
            Token := Collector.Token;
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
            'x', 'u':
            begin
              Collector.Escape := '';
              if Ch = 'u' then
                Collector.EscapeLength := 4
              else
                Collector.EscapeLength := 2;
              Token := tkEscapeHex;
              Next;
              Collector.Started := Index;
            end;
            #13:
            begin
              Collector.Buffer := Collector.Buffer;
              Collector.IsMultiLine := True;
              NewLine;
              Next;
              Collector.Started := Index;
            end;
            #10:
            begin
              Collector.Buffer := Collector.Buffer;
              if LastChar <> #13 then
              begin
                Collector.IsMultiLine := True;
                NewLine;
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
        tkDoubleQuoteString:
        begin
          if Ch = '"' then
            EndString
          else
            ContinueString;
          //Next char yes, we do not need " anymore
          Next;
        end;
        tkSingleQuoteString:
        begin
          if Ch = '''' then
            EndString
          else
            ContinueString;
          //Next char yes, we do not need ' anymore
          Next;
        end;
        tkBackQuoteType:
        begin
          if CharInSet(Ch, [' ', #10, #13]) then
          begin
            Token := tkBackQuoteString;
            Collector.Collect(Content, Index);
            Collector.Name := Collector.Buffer;
            Collector.Reset(Index, tkBackQuoteString);
            if (Ch = #13) or ((Ch = #10) and (LastChar <> #13)) then
              NewLine;
            Next;
          end
          else
            Next;
        end;
        tkBackQuoteString:
        begin
          if Ch = '`' then
            EndString
          else
            ContinueString;
          Next;
        end;
        tkNumber:
        begin
          if not CharInSet(Ch, sNumberChars) then
          begin
            if Expect = exValue then
            begin
              Collector.Collect(Content, Index);
              AcquireProc(AObject, Parent, Collector.Buffer, aqNumber, Default(TmnJsonStringType));
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
        tkIdentifire: /// Should be last one in `Case`
        begin
          if not CharInSet(Ch, ['A'..'Z', 'a'..'z', '0'..'9',  '_']) then
          begin
            if Expect = exName then
            begin
              //Creating a Pair Item
              Collector.Collect(Content, Index);
              AcquireProc(Pair, Parent, Collector.Buffer, aqPair, Default(TmnJsonStringType));
              Expect := exAssign;
            end
            else if Expect = exValue then
            begin
              Collector.Collect(Content, Index);
              AcquireProc(AObject, Parent, Collector.Buffer, aqIdentifier, Default(TmnJsonStringType));
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
        else //* Open
        begin
          case Ch of
            '"':
            begin
              CheckExpected([exName, exValue, exEnd]);
              Collector.Reset(Index + 1, tkDoubleQuoteString);
              Token := tkDoubleQuoteString;
            end;
            '''':
            begin
              if not (jsoModern in Options) then
                IlligalCharacter(Ch)
              else
              begin
                CheckExpected([exName, exValue, exEnd]);
                Collector.Reset(Index + 1, tkSingleQuoteString);
                Token := tkSingleQuoteString;
              end;
            end;
            '`':
            begin
              if not (jsoModern in Options) then
                IlligalCharacter(Ch)
              else
              begin
                CheckExpected([exName, exValue, exEnd]);
                Collector.Reset(Index + 1, tkBackQuoteType);
                Token := tkBackQuoteType;
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
              AcquireProc(AObject, Parent, '', aqObject, Default(TmnJsonStringType));
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
              AcquireProc(AObject, Parent, '', aqArray, Default(TmnJsonStringType));
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
            ' ', #8, #9:; //* Nothing to do
            #13:
              NewLine;
            #10:
            begin
              if LastChar <> #13 then
                NewLine;
            end;
            '-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.': //may start with . ?
            begin
              CheckExpected([exValue, exEnd]);
              Collector.Reset(Index, tkNumber);
              Token := tkNumber;
            end;
            'A'..'Z', 'a'..'z', '_': //* Should be last one in `Case`
            begin
              CheckExpected([exName, exValue, exEnd]);
              Collector.Reset(Index, tkIdentifire);
              Token := tkIdentifire;
            end;
            else
              IlligalCharacter(Ch);
          end;
          Next;
        end;
      end;
    until (Ch=#0) or (Index >= Size);

    {if Collector.Token > tkNone then //* TODO for chunks
    begin
    end}
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

procedure JsonLintAcquireCallback(out AObject: TObject; AParentObject: TObject; const Value: string; const ValueType: TmnJsonType; const StringOptions: TmnJsonStringType);
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
