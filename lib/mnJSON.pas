unit mnJSON;
{ **
  *  This file is part of the "Mini Library"
  *
  * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
  *            See the file COPYING.MLGPL, included in this distribution,
  * @author    Zaher Dirkey <zaher, zaherdirkey>
  * }

{$IFDEF FPC}
{$MODE delphi}
{$modeswitch arrayoperators}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}
{$ENDIF}
{$M+}{$H+}
{$ifdef mswindows}
{$define windows}
{$endif}

interface

uses
{$IFDEF windows}Windows, {$ENDIF}
  Classes, SysUtils, StrUtils, DateUtils, Types, Character;

const
  sUTF8BOM: array [1 .. 3] of Char = (#$EF, #$BB, #$BF);

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

  TmnJsonAcquireProc = procedure(AParentObject: TObject; Value: string; ValueType: TmnJsonAcquireType; out AObject: TObject);

procedure JsonParseCallback(Content: string; FromIndex: Integer; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);

implementation

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

procedure JsonParseCallback(Content: string; FromIndex: Integer; AParent: TObject; const AcquireProc: TmnJsonAcquireProc; vOptions: TJSONParseOptions);
const
  sWhiteSpace = [#0, #10, #13, ' '];
type
  TExpect = (exValue, exName, exAssign, exNext);
  TContext = (cxPairs, cxArray);

  TToken = (
    tkNone,
    tkString,
    tkNumber,
    tkIdentifire
  );

  TStackItem = record
    Parent: TObject;
    Context: TContext;
  end;

  TStack = array of TStackItem;

var
  Stack: TStack;
  Parent: TObject;
  Expect: TExpect;
  Context: TContext;

  AObject: TObject;
  Pair: TObject;

  Index: Integer;
  Start: Integer;

  Token: TToken;

  LineNumber: Int64;
  ColumnNumber: Int64;
  Ch: Char;

  procedure Push;
  begin
    SetLength(Stack, Length(Stack) + 1);
    Stack[High(Stack)].Parent := Parent;
    Stack[High(Stack)].Context := Context;
  end;

  procedure Pop;
  begin
    Parent := Stack[High(Stack)].Parent;
    Context := Stack[High(Stack)].Context;
    SetLength(Stack, High(Stack));
  end;

  procedure Error(const Msg: string);
  begin
    RaiseError(Msg, LineNumber, ColumnNumber);
  end;

begin
  if Content = '' then
    exit;

  Index := 0;
  if (@AcquireProc = nil) then
    Error('JSON Parser: Acquire is nil');

  Index := FromIndex;

  if Index = 0 then
    Index := 1;

  Start := Index;

  //initial/or continue(TODO: by param)
  Context := cxPairs;
  Expect := exValue;
  Parent := AParent;
  Pair := nil;

  Token := tkNone;
  LineNumber := 0;
  ColumnNumber := 0;

  repeat
    Ch := Content[Index];
    case Token of
      tkString:
      begin
        if Ch = '"' then
        begin
          //
          if Expect = exName then
          begin
            //Creating a Pair Item
            AcquireProc(Parent, Copy(Content, Start, Index - Start), aqPair, Pair);
            Push;
            Parent := Pair;
            Expect := exAssign;
          end
          else if Expect = exValue then
          begin
            AcquireProc(Parent, Copy(Content, Start, Index - Start), aqString, AObject);
            Expect := exNext;
          end
          else
            Error('Expected Next or End');
          Start := Index;
          Token := tkNone;
        end;
        Inc(Index);
      end;
      else
      begin
        case Ch of
          ' ', #8, #9:; //* Nothing to do
          #13:
          begin
            Inc(LineNumber);
            ColumnNumber := 0;
          end;
          '"':
          begin
            Token := tkString;
            Start := Index + 1;
          end;
          ':':
          begin
            if Expect <> exAssign then
              Error('Expected assign symbol :');
            Expect := exValue
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
      end;
    end;
  until (Ch=#0) or (Index > Length(Content));

  if (Expect <> exNext) then
    Error('End of string not expected');
end;

initialization

end.
