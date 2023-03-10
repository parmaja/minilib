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
    jpoUnstrict,
    jpoSafe,
    jpoUTF8 //TODO
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
      TExpect = (exValue, exName, exAssign, exNext);

      TToken = (
        tkNone,
        tkString,
        tkEscape,
        tkNumber,
        tkIdentifire,
        tkReturn //End of line to escape #10
      );

      TContext = (cxPairs, cxArray);

      TStackItem = record
        Parent: TObject;
        Context: TContext;
      end;

      TStack = array of TStackItem;

    var
      AcquireProc: TmnJsonAcquireProc;

      Stack: TStack;
      Parent: TObject;
      Pair: TObject;
      Context: TContext;
      StackIndex: Integer;
      Expect: TExpect;

      LineNumber: Int64;
      ColumnNumber: Int64;

      Token: TToken;
      StringBuffer: String;
      StartString: Integer;
      Index: Integer;
      Options: TJSONParseOptions;
    procedure Pop; inline;
    procedure Push; inline;
    procedure Next; inline;
    procedure Error(const Msg: string);
  public
    procedure Init(AParent: TObject; vAcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
    procedure Parse(const Content: String);
    procedure Finish;
  end;

procedure JsonParseCallback(const Content: String; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);

implementation

const
  sNumberOpenChars = ['-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
  sNumberChars = sNumberOpenChars + ['.', 'x', 'h', 'a', 'b', 'c', 'd', 'e', 'f'];

procedure RaiseError(AError: string; Line: Integer = 0; Column: Integer = 0);
begin
  if Line > 0 then
  begin
    raise Exception.Create(AError + ' line: ' + Line.ToString + ' column: '+ Column.ToString)
    {$ifdef FPC}
    at get_caller_addr(get_frame), get_caller_frame(get_frame)
    {$endif};
  end
  else
  begin
    raise Exception.Create(AError)
    {$ifdef FPC}
    at get_caller_addr(get_frame), get_caller_frame(get_frame)
    {$endif};
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
  StackIndex := StackIndex + 1;
end;

procedure TmnJSONParser.Next;
begin
  Inc(Index);
  Inc(ColumnNumber);
end;

procedure TmnJSONParser.Error(const Msg: string);
begin
  if not (jpoSafe in Options) then
    RaiseError(Msg, LineNumber, ColumnNumber);
end;

procedure TmnJSONParser.Init(AParent: TObject; vAcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
begin
  Options := vOptions;
  AcquireProc := vAcquireProc;
  Parent := AParent;
  StackIndex := 0;
  SetLength(Stack, 100); //* Buffing it for fast grow
  //initial/or continue(TODO: by param)
  Context := cxPairs;
  Expect := exValue;
  LineNumber := 1;
  ColumnNumber := 1;
  Pair := nil;
end;

procedure TmnJSONParser.Finish;
begin
  if (Expect <> exNext) then
    Error('End of string/file not expected');
end;

procedure TmnJSONParser.Pop; {$ifdef FPC} inline; {$endif}
begin
  Parent := Stack[StackIndex-1].Parent;
  Context := Stack[StackIndex-1].Context;
//    SetLength(Stack, High(Stack));
  StackIndex := StackIndex - 1;
  {$ifdef verbose}
  Writeln(Format('%0.4d ', [LineNumber])+RepeatString('    ', Length(Stack)) + 'Pop '+ TRttiEnumerationType.GetName(Context) +' '+TRttiEnumerationType.GetName(Expect));
  {$endif}
end;

procedure TmnJSONParser.Parse(const Content: String);
var
  AObject: TObject;

var
  Len: Int64;
  Ch: Char;
begin
  if Content = '' then
    exit;

  if (@AcquireProc = nil) then
    Error('JSON Parser: Acquire is nil');
  if (Parent = nil) then
    Error('JSON Parser: Parent is nil');

  Index := 1;
  Len := Length(Content);

  StartString := -1; //* for strings

  Token := tkNone;
  try
    repeat
      Ch := Content[Index];
      case Token of
        tkReturn:
        begin
          if Ch = #10 then
            Next;
          Token := tkNone;
        end;
        tkEscape:
        begin
          case Ch of
            'b': StringBuffer := StringBuffer + #8;
            't': StringBuffer := StringBuffer + #9;
            'n': StringBuffer := StringBuffer + #10;
            'f': StringBuffer := StringBuffer + #12;
            'r': StringBuffer := StringBuffer + #13;
            '0': StringBuffer := StringBuffer + #0;
            'u':
            begin
              //TODO: unicode escape
            end
            else
              StringBuffer := StringBuffer + Ch;
          end;
          Next;
          StartString := Index;
          Token := tkString;
        end;
        tkString:
          begin
            if Ch = '"' then
            begin
              if Expect = exName then
              begin
                //Creating a Pair Item
                AcquireProc(Parent, StringBuffer + Copy(Content, StartString, Index - StartString), aqPair, Pair);
                Expect := exAssign;
              end
              else if Expect = exValue then
              begin
                AcquireProc(Parent, StringBuffer + Copy(Content, StartString, Index - StartString), aqString, AObject);
                Expect := exNext;
              end
              else
                Error('Expected Next or End');
              StringBuffer := '';
              Token := tkNone;
            end
            else
            begin
              if Ch = '\' then
              begin
                StringBuffer := StringBuffer + Copy(Content, StartString, Index - StartString);
                StartString := Index + 1;
                Token := tkEscape;
              end;
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
                AcquireProc(Parent, Copy(Content, StartString, Index - StartString), aqPair, Pair);
                Expect := exAssign;
              end
              else if Expect = exValue then
              begin
                AcquireProc(Parent, Copy(Content, StartString, Index - StartString), aqIdentifier, AObject);
                Expect := exNext;
              end
              else
                Error('Expected Next or End');
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
                AcquireProc(Parent, Copy(Content, StartString, Index - StartString), aqNumber, AObject);
                Expect := exNext;
              end
              else
                Error('Expected Next or End');
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
              StartString := Index;
              Token := tkIdentifire;
            end;
            '-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
            begin
              StartString := Index;
              Token := tkNumber;
            end;
            '"':
            begin
              StartString := Index + 1;
              Token := tkString;
            end;
            ':':
            begin
              if Expect <> exAssign then
                Error('Expected assign symbol :');
              Expect := exValue;
              Push;
              Parent := Pair;
            end;
            ',':
            begin
              if Expect <> exNext then
                Error('Not expected ,');
              if Context = cxPairs then
              begin
                Pop;
                Expect := exName;
              end
              else
                Expect := exValue;
            end;
            '{' :
            begin
              if Expect <> exValue then
                Error('Expected Value');
              Push;
              AcquireProc(Parent, '', aqObject, AObject);
              Parent := AObject;
              Context := cxPairs;
              Expect := exName;
            end;
            '}' :
            begin
              if Expect = exNext then
                Pop;
              Pop;
              Expect := exNext;
            end;
            '[' :
            begin
              if Expect <> exValue then
                Error('Expected Value');
              Push;
              AcquireProc(Parent, '', aqArray, AObject);
              Parent := AObject;
              Context := cxArray;
            end;
            ']' :
            begin
              Pop;
              Expect := exNext;
            end;
          end;
          Next;
        end;
      end;
    until (Ch=#0) or (Index > Len);
  except
    on E: Exception do
    begin
      RaiseError(E.Message, LineNumber, ColumnNumber);
    end;
  end;
end;

procedure JsonParseCallback(const Content: String; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
var
  JSONParser: TmnJSONParser;
begin
  JSONParser.Init(AParent, AcquireProc, vOptions);
  JSONParser.Parse(Content);
  JSONParser.Finish;
end;

initialization
end.
