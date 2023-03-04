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
  * }

{$IFDEF FPC}
{$MODE delphi}
{$ModeSwitch arrayoperators}
{$ModeSwitch advancedrecords}
{$ModeSwitch ArrayOperators}
{$ModeSwitch typehelpers}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}
{$mode delphi}
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
  {$ifdef verbose}
//  mnUtils,
  {$endif}
  System.Rtti;

type
  TJSONParseOption = (jpoStrict);
  TJSONParseOptions = set of TJSONParseOption;

  TmnJsonAcquireType = (
    aqPair,
    aqObject,
    aqArray,
    aqString,
    aqNumber,
    aqIdentifier,
    aqBoolean
  );

  TmnJsonAcquireProc = procedure(AParentObject: TObject; const Value: String; const ValueType: TmnJsonAcquireType; out AObject: TObject);

  TmnJSONParser = record
  private
    type
      TExpect = (exValue, exName, exAssign, exNext);

      TToken = (
        tkNone,
        tkString,
        tkEscape,
        tkNumber,
        tkIdentifire
      );

      TContext = (cxPairs, cxArray);

      TStackItem = record
        Parent: TObject;
        Context: TContext;
      end;

      TStack = array of TStackItem;

  public
    Stack: TStack;
    Parent: TObject;
    Expect: TExpect;
    Context: TContext;
    StackIndex: Integer;
    Token: TToken;
    LineNumber: Int64;
    ColumnNumber: Int64;
    StringBuffer: String;
    StartString: Integer;
    procedure JsonParseCallback(const Content: String; Index: Integer; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
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

procedure TmnJSONParser.JsonParseCallback(const Content: String; Index: Integer; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
var
  AObject: TObject;
  Pair: TObject;

  procedure Error(const Msg: string);
  begin
    RaiseError(Msg, LineNumber, ColumnNumber);
  end;

  procedure Push; {$ifdef FPC} inline; {$endif}
  begin
    {$ifdef verbose}
    Writeln(Format('%0.4d ', [LineNumber])+ RepeatString('    ', Length(Stack))+ 'Push '+ TRttiEnumerationType.GetName(Context)+ ' ' +TRttiEnumerationType.GetName(Expect));
    {$endif}
  //  SetLength(Stack, Length(Stack) + 1);
    Stack[StackIndex].Parent := Parent;
    Stack[StackIndex].Context := Context;
    StackIndex := StackIndex + 1;
  end;

  procedure Pop; {$ifdef FPC} inline; {$endif}
  begin
    Parent := Stack[StackIndex-1].Parent;
    Context := Stack[StackIndex-1].Context;
//    SetLength(Stack, High(Stack));
    StackIndex := StackIndex - 1;
    {$ifdef verbose}
    Writeln(Format('%0.4d ', [LineNumber])+RepeatString('    ', Length(Stack)) + 'Pop '+ TRttiEnumerationType.GetName(Context) +' '+TRttiEnumerationType.GetName(Expect));
    {$endif}
  end;

var
  Size: Int64;
  Ch: Char;
begin

  StackIndex := 0;
  SetLength(Stack, 1000);

  if Content = '' then
    exit;

  if (@AcquireProc = nil) then
    Error('JSON Parser: Acquire is nil');

  Index := 1;

  Size := Length(Content);

  StartString := Index;

  //initial/or continue(TODO: by param)
  Context := cxPairs;
  Expect := exValue;
  Parent := AParent;
  Pair := nil;

  Token := tkNone;
  LineNumber := 1;
  ColumnNumber := 1;

  repeat
    Ch := Content[Index];
    case Token of
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
        Inc(Index);
        Inc(ColumnNumber);
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
          Inc(Index);
          Inc(ColumnNumber);
        end;
      tkIdentifire:
        begin
          if not CharInSet(Ch, ['A'..'Z', 'a'..'z', '_']) then
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
            Inc(Index);
            Inc(ColumnNumber);
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
            Inc(Index);
            Inc(ColumnNumber);
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
        Inc(Index);
        Inc(ColumnNumber);
      end;
    end;
  until (Ch=#0) or (Index > Size);

  if (Expect <> exNext) then
    Error('End of string not expected');
end;

procedure JsonParseCallback(const Content: String; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
var
  JSONParser: TmnJSONParser;
begin
  JSONParser.JsonParseCallback(Content, 1, AParent, AcquireProc, vOptions);
end;

{ TmnJSONParser }

initialization
end.
