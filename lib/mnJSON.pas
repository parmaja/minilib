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

{$IFDEF FPC}
{$MODE delphi}
{$ModeSwitch arrayoperators}
{$ModeSwitch advancedrecords}
{$ModeSwitch ArrayOperators}
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
  Classes, SysUtils, StrUtils, DateUtils, Types, Character;

type
  TJSONParseOption = (
    jsoStrict,
    jsoNoDuplicate,//TODO do not allow duplicate names
    jsoSafe,
    jsoUTF8 //TODO
  );
  TJSONParseOptions = set of TJSONParseOption;

  TmnJsonAcquireType = (
    aqPair,
    aqObject,
    aqArray,
    aqString,
    aqIdentifier,
    aqNumber,
    aqBoolean
  );

  TmnJsonAcquireProc = procedure(AParentObject: TObject; const Value: String; const ValueType: TmnJsonAcquireType; out AObject: TObject);

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
        tkString,
        tkEscape,
        tkEscapeChar,
        tkNumber,
        tkIdentifire,
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
      StringBuffer: UTF8String;
      EscapeBuffer: UTF8String;
      StartString: Integer;
      Index: Integer;
      Options: TJSONParseOptions;
      ErrorMessage: String;
    procedure RaiseError(AError: string; Line: Integer = 0; Column: Integer = 0);
    procedure Push; inline;
    procedure Pop; inline;
    procedure Next; inline;
    procedure CheckExpected(AExpected: TExpects; AContexts: TContexts = [cxPair, cxArray]); inline;
    procedure Error(const Msg: string); inline;
    procedure ErrorNotExpected(AExpected: TExpects; AContexts: TContexts = [cxPair, cxArray]); //not inline
  public
    procedure Init(AParent: TObject; vAcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
    procedure Parse(const Content: PByte; Size: Integer; Start: Integer = 0); overload;
    procedure Parse(const Content: UTF8String); overload;
    procedure Finish;
  end;

procedure JsonParseCallback(const Content: UTF8String; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);

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
  begin
    ErrorMessage := AError + ' :: line: ' + Line.ToString + ' column: '+ Column.ToString;
    raise Exception.Create(ErrorMessage);
  end
  else
  begin
    ErrorMessage := AError + ' :: column: '+ Column.ToString;
  end;
  if not (jsoSafe in Options) then
    raise Exception.Create(ErrorMessage)
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
  Parse(@Content[1], Length(Content));
end;

procedure TmnJSONParser.Pop; {$ifdef FPC} inline; {$endif}
begin
  if StackIndex = 0 then
    Error('Expected EOF');
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
  function CopyString(const Value: PByte; Start, Count: Integer): UTF8String;
  begin
    if Count = 0 then
      Result := ''
    else
    begin
      SetLength(Result, Count);
      CopyMemory(@Result[1], @Value[Start], Count);
    end;
  end;
begin
  if (@AcquireProc = nil) then
    Error('JSON Parser: Acquire is nil');
  if (Parent = nil) then
    Error('JSON Parser: Parent is nil');

  Index := Start;
  StartString := -1; //* for strings

  Token := tkNone;
  try
    repeat
      Ch := UTF8Char(Content[Index]);
      case Token of
        tkReturn:
        begin
          if Ch = #10 then
            Next;
          Token := tkNone;
        end;
        tkEscapeChar:
        begin
          if Length(EscapeBuffer) < 4 then
          begin
            if (jsoStrict in Options) then
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
              StringBuffer := StringBuffer + ConvertFromUtf32(StrToInt('$'+EscapeBuffer));
              EscapeBuffer := '';
            end;
            StartString := Index;
            Token := tkString;
//            Next;
          end;
        end;
        tkEscape:
        begin
          if (jsoStrict in Options) then
          begin
            if CharInSet(Ch, [#0, #10, #13]) then
              Error('End of line in string!');
          end;
          case Ch of
            'b': StringBuffer := StringBuffer + #8;
            't': StringBuffer := StringBuffer + #9;
            'n': StringBuffer := StringBuffer + #10;
            'f': StringBuffer := StringBuffer + #12;
            'r': StringBuffer := StringBuffer + #13;
            '0': StringBuffer := StringBuffer + #0;
            'u':
            begin
              EscapeBuffer := '';
              Token := tkEscapeChar;
            end
            else
              StringBuffer := StringBuffer + Ch;
          end;
          Next;
          StartString := Index;
          if Token = tkEscape then //* not in \u
            Token := tkString;
        end;
        tkString:
          begin
            if Ch = '"' then
            begin
              if Expect = exName then
              begin
                //Creating a Pair Item
                AcquireProc(Parent, StringBuffer + CopyString(Content, StartString, Index - StartString), aqPair, Pair);
                Expect := exAssign;
              end
              else if Expect = exValue then
              begin
                AcquireProc(Parent, StringBuffer + CopyString(Content, StartString, Index - StartString), aqString, AObject);
                Expect := exNext;
              end
              else
                CheckExpected([exName, exValue], [Context]);
              StringBuffer := '';
              Token := tkNone;
            end
            else
            begin
              if (jsoStrict in Options) then
              begin
                if CharInSet(Ch, [#0, #10, #13]) then
                  Error('End of line in string!');
              end;

              if Ch = '\' then
              begin
                StringBuffer := StringBuffer + CopyString(Content, StartString, Index - StartString);
                StartString := Index + 1;
                Token := tkEscape;
              end
            end;

            //Next char yes, we do not need " anymore
            Next;
          end;
        tkIdentifire:
          begin
            if not CharInSet(Ch, ['A'..'Z', 'a'..'z', '0'..'9',  '_']) then
            begin
              if Expect = exName then
              begin
                //Creating a Pair Item
                AcquireProc(Parent, CopyString(Content, StartString, Index - StartString), aqPair, Pair);
                Expect := exAssign;
              end
              else if Expect = exValue then
              begin
                AcquireProc(Parent, CopyString(Content, StartString, Index - StartString), aqIdentifier, AObject);
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
                AcquireProc(Parent, CopyString(Content, StartString, Index - StartString), aqNumber, AObject);
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
              Inc(LineNumber);
              ColumnNumber := 1;
            end;
            'A'..'Z', 'a'..'z', '_':
            begin
              CheckExpected([exName, exValue, exEnd]);
              StartString := Index;
              Token := tkIdentifire;
            end;
            '-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9': //may start with . ?
            begin
              CheckExpected([exValue, exEnd]);
              StartString := Index;
              Token := tkNumber;
            end;
            '"':
            begin
              CheckExpected([exName, exValue, exEnd]);
              StartString := Index + 1;
              Token := tkString;
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
              AcquireProc(Parent, '', aqObject, AObject);
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
                CheckExpected([exNext], [cxPair]);
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
              AcquireProc(Parent, '', aqArray, AObject);
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
                CheckExpected([exNext], [cxArray]);
              Pop;
              if StackIndex < 0 then
                Expect := exEnd
              else
                Expect := exNext;
            end;
            else
            begin
              Error('Illigal character: ' + Ch + ' '+ IntToHex(ord(Ch)));
            end
          end;
          Next;
        end;
      end;
    until (Ch=#0) or (Index >= Size);
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

procedure JsonParseCallback(const Content: UTF8String; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
var
  JSONParser: TmnJSONParser;
begin
  JSONParser.Init(AParent, AcquireProc, vOptions);
  JSONParser.Parse(Content);
  JSONParser.Finish;
end;

initialization
end.
