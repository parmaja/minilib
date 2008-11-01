unit mncSQL;
{**
 *  This file is part of the "Mini Connections"
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
  Classes, SysUtils,
  mnStreams, mncConnections;

type
  TmncParseSQLOptions = set of (psoGenerateParams, psoAddParamsNames);
  
  TmncSQLCommand = class(TmncCommand)
  private
  protected
    function ParseSQL(Options:TmncParseSQLOptions): string;
  public
    property SQL: TStrings read FRequest;//Alias of Request
  end;

implementation

function TmncSQLCommand.ParseSQL(Options:TmncParseSQLOptions): string;
var
  cCurChar, cNextChar, cQuoteChar: Char;
  sSQL, sProcessedSQL, sParamName: string;
  i, LenSQL: Integer;
  iCurState, iCurParamState: Integer;
  iParamSuffix: Integer;
  slNames: TStrings;
const
  DefaultState = 0;
  CommentState = 1;
  QuoteState = 2;
  ParamState = 3;
  ParamDefaultState = 0;
  ParamQuoteState = 1;

  procedure AddToSQL(s: string);
  begin
    sProcessedSQL := sProcessedSQL + s;
  end;
var
  aParam:TmncParam;
begin
  //TODO stored procedures and trigger must not check param in budy procedure
  sProcessedSQL := '';
  sParamName := '';
  slNames := TStringList.Create;
  try
    { Do some initializations of variables }
    iParamSuffix := 0;
    cQuoteChar := '''';
    sSQL := Trim(SQL.Text) + ' ';//zaher that dummy
    i := 1;
    iCurState := DefaultState;
    iCurParamState := ParamDefaultState;
    { Now, traverse through the SQL string, character by character,
     picking out the parameters and formatting correctly for Firebird }
    LenSQL := Length(sSQL);
    while (i <= LenSQL) do
    begin
      { Get the current token and a look-ahead }
      cCurChar := sSQL[i];
      if i = LenSQL then
        cNextChar := #0
      else
        cNextChar := sSQL[i + 1];
      { Now act based on the current state }
      case iCurState of
        DefaultState:
          begin
            case cCurChar of
              '''', '"':
                begin
                  cQuoteChar := cCurChar;
                  iCurState := QuoteState;
                end;
              '?':
                begin
                  iCurState := ParamState;
                  AddToSQL('?');
                end;
              '/': if (cNextChar = '*') then
                begin
                  AddToSQL(cCurChar);
                  Inc(i);
                  iCurState := CommentState;
                end;
            end;
          end;
        CommentState:
          begin
            if (cNextChar = #0) then
              raise EmncException.Create('EOF in comment detected: ' + IntToStr(i))
            else if (cCurChar = '*') then
            begin
              if (cNextChar = '/') then
                iCurState := DefaultState;
            end;
          end;
        QuoteState:
          begin
            if cNextChar = #0 then
              raise EmncException.Create('EOF in string detected: ' + IntToStr(i))
            else if (cCurChar = cQuoteChar) then
            begin
              if (cNextChar = cQuoteChar) then
              begin
                AddToSQL(cCurChar);
                Inc(i);
              end
              else
                iCurState := DefaultState;
            end;
          end;
        ParamState:
          begin
          { collect the name of the parameter }
            if iCurParamState = ParamDefaultState then
            begin
              if cCurChar = '"' then
                iCurParamState := ParamQuoteState
              else if (cCurChar in ['A'..'Z', 'a'..'z', '0'..'9', '_', ' ', '$']) then //Quoted can include spaces
              begin
                sParamName := sParamName + cCurChar;
                if psoAddParamsNames in Options then
                  AddToSQL(cCurChar);
              end
              else if psoGenerateParams in Options then//if passed ? without name of params
              begin
                sParamName := '_Param_' + IntToStr(iParamSuffix);
                Inc(iParamSuffix);
                iCurState := DefaultState;
                slNames.Add(sParamName);
                sParamName := '';
              end
              else
                raise EmncException.Create('Parameter name expected');
            end
            else
            begin
            { determine if Quoted parameter name is finished }
              if cCurChar = '"' then
              begin
                Inc(i);
                slNames.Add(sParamName);
                SParamName := '';
                iCurParamState := ParamDefaultState;
                iCurState := DefaultState;
              end
              else
                sParamName := sParamName + cCurChar
            end;
          { determine if the unquoted parameter name is finished }
            if (iCurParamState <> ParamQuoteState) and
              (iCurState <> DefaultState) then
            begin
              if not (cNextChar in ['A'..'Z', 'a'..'z',
                '0'..'9', '_', '$']) then
              begin
                Inc(i);
                iCurState := DefaultState;
                //slNames.Add(UpperCase(sParamName));
                slNames.Add(sParamName);
                sParamName := '';
              end;
            end;
          end;
      end;
      if iCurState <> ParamState then
        AddToSQL(sSQL[i]);
      Inc(i);
    end;
    Params.Clear; 
    ParamList.Clear;
    for i := 0 to slNames.Count - 1 do
    begin
      aParam := Params.AddExists(slNames[i]);
      ParamList.Add(aParam);
    end;
    Result := sProcessedSQL;
  finally
    slNames.Free;
  end;
end;

end.

