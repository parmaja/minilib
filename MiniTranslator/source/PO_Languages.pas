unit PO_Languages;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}

interface

uses
  Windows, Messages,
  SysUtils, Variants, Classes, Contnrs,
  LangClasses;

type
  TPO_LangItem = class(TLangItem)
  public
  end;

  TPO_LangContents = class(TLangContents)
  protected
    function DoCreateLangItem: TLangItem; override;
  public
  end;

  TPO_Language = class(TLanguage)
  protected
    function DoCreateContents: TLangContents; override;
  public
    function CreateParser: TLangParser; override;
  end;

  TPO_State = (poNone, poComment, poMsgId, poMsgStr);

  TPO_Parser = class(TLangParser)
  private
    FState: TPO_State;
  protected
    FLangItem: TLangItem; //for multiline need to keep it in object body
  public
    procedure ParseLine(ALine: string); //if you like to parse line by line not use a Contents property
    procedure Parse(Strings: TStringList); override;
    procedure Generate(Strings: TStringList); override;
    class function GetFileExtensions: string; override;
    class function GetTitle: string; override;
  end;

implementation

uses
  StrUtils;

const
  ssMsgId = 'msgid ';
  ssMsgStr = 'msgstr ';
  ssComment = '#';
  ssFlags = '#, ';
  ssReference = '#: ';
  ssAutoComment = '#. ';
  ssMsgPlural = 'msgid_plural '; //not yet
  ssMsgStrs = 'msgstr['; //not yet

function DequoteStr(Str: string): string;
begin
  if Str = '' then
    Result := ''
  else
  begin
    if Str[1] = '"' then
    begin
      if Str[Length(Str)] = '"' then
        Result := MidStr(Str, 2, Length(Str) - 2)
      else
        Result := MidStr(Str, 2, Length(Str) - 1)
    end
    else if Str[1] = '''' then
    begin
      if Str[Length(Str)] = '''' then
        Result := MidStr(Str, 2, Length(Str) - 2)
      else
        Result := MidStr(Str, 2, Length(Str) - 1)
    end
    else
      Result := Str;
  end;
end;

function QuoteStr(Str: string; QuoteChar: string = '"'): string;
begin
  if Str = '' then
    Result := QuoteChar + QuoteChar
  else
  begin
    if Str[1] = '"' then
    begin
      if Str[Length(Str)] <> '"' then
        Result := Result + '"'
    end
    else if Str[1] = '''' then
    begin
      if Str[Length(Str)] <> '''' then
        Result := Result + '''';
    end
    else
      Result := QuoteChar + Str + QuoteChar;
  end;
end;

function CutStr(const ID, S: string; Dequote: Boolean = False): string;
begin
  Result := MidStr(S, Length(ID) + 1, MaxInt);
  if Dequote then
    Result := DequoteStr(Trim(Result));
end;

procedure TPO_Parser.ParseLine(ALine: string);
var
  s: string;

  procedure CreateLangItem;
  begin
    FLangItem := Contents.CreateLangItem;
  end;

  function CheckAndCut(var S: string; ID: string): Boolean;
  begin
    Result := LeftStr(aLine, Length(ID)) = ID;
    if Result then
    begin
      if S <> '' then
        S := S + #13;
      S := S + CutStr(ID, aLine);
    end;
  end;

  function CheckComments: Boolean;
  begin
    Result := CheckAndCut(FLangItem.AutoComment, ssAutoComment) or
      CheckAndCut(FLangItem.Flags, ssFlags) or
      CheckAndCut(FLangItem.Reference, ssReference) or
      CheckAndCut(FLangItem.Comment, ssComment);
  end;

  function CheckNewText: Boolean;
  begin
    Result := True;
    if LeftStr(aLine, Length(ssMsgID)) = ssMsgID then
    begin
      CreateLangItem;
      FState := poMsgID;
      FLangItem.ID := DequoteStr(Trim(MidStr(aLine, Length(ssMsgID) + 1, MaxInt)));
    end
    else if LeftStr(aLine, Length(ssAutoComment)) = ssAutoComment then
    begin
      CreateLangItem;
      FState := poComment;
      FLangItem.AutoComment := MidStr(aLine, Length(ssAutoComment), MaxInt);
    end
    else if LeftStr(aLine, Length(ssFlags)) = ssFlags then
    begin
      CreateLangItem;
      FState := poComment;
      FLangItem.Flags := MidStr(aLine, Length(ssFlags), MaxInt);
    end
    else if LeftStr(aLine, Length(ssComment)) = ssComment then
    begin
      CreateLangItem;
      FState := poComment;
      FLangItem.Comment := MidStr(aLine, Length(ssComment) + 1, MaxInt);
    end
    else
      Result := False;
  end;
begin
  if aLine <> '' then
  begin
    case FState of
      poNone:
        begin
          CheckNewText;
        end;
      poComment:
        begin
          if LeftStr(aLine, Length(ssMsgID)) = ssMsgID then
          begin
            s := CutStr(ssMsgID, aLine, True);
            FLangItem.ID := s;
            FState := poMsgID;
          end
          else
            CheckComments;
        end;
      poMsgId:
        begin
          if LeftStr(aLine, Length(ssMsgStr)) = ssMsgStr then
          begin
            FLangItem.Text := CutStr(ssMsgStr, aLine, True);
            FState := poMsgStr;
          end
          else if not CheckComments then
            FLangItem.ID := FLangItem.ID + DequoteStr(aLine);
        end;
      poMsgStr:
        begin
          if CheckNewText then
          else if not CheckComments then
          begin
            //stupid :( we need optimized function
            s := aLine;
            s := StringReplace(DequoteStr(s), '\n', #13, [rfReplaceAll]);
            s := StringReplace(DequoteStr(s), '\t', #9, [rfReplaceAll]);
            s := StringReplace(DequoteStr(s), '\"', '"', [rfReplaceAll]);
            s := StringReplace(DequoteStr(s), '\\', '\', [rfReplaceAll]);
            FLangItem.Text := FLangItem.Text + s;
          end;
        end;
    end;
  end;
end;

procedure TPO_Parser.Parse(Strings: TStringList);
var
  l: Integer;
  s: string;
  p: Integer;
  i: Integer;
  aList: TStringList;
  ForceUTF8: Boolean;
begin
  ForceUTF8 := False;
  l := 0;
  while l < Strings.Count do
  begin
    s := Strings[l];
    if l = 0 then
    begin
      if (Length(s) > 0) and (Copy(s, 1, 3) = sUTF8BOM) then
      begin
        ForceUTF8 := True;
        Delete(s, 1, 3);
      end;
    end;
    ParseLine(s);
    Inc(l);
  end;
  if Contents is TPO_LangContents then
  begin
    s := Contents.GetText('');
    ExtractStrings([#13], [' '], PChar(s), Contents.Settings);
    for i := 0 to Contents.Settings.Count - 1 do
    begin
      s := Trim(Contents.Settings[i]);
      p := Pos(':', s);
      if p > 0 then
      begin
        s := Trim(Copy(s, 1, p - 1)) + '=' + Trim(Copy(s, p + 1, MaxInt));
        Contents.Settings[i] := s;
      end
      else
        Contents.Settings[i] := Trim(s);
    end;
    Contents.Name := Contents.Settings.Values['X-Name'];
    s := Trim(Contents.Settings.Values['Content-Type']);
    if s <> '' then
    begin
      p := Pos(';', s);
      if p > 0 then
      begin
        s := Trim(copy(s, p + 1, MaxInt));
        aList := TStringList.Create;
        try
          ExtractStrings([';'], [' '], PChar(s), aList);
          Contents.Charset := aList.Values['CharSet'];
        finally
          aList.Free;
        end;
      end;
    end;
    if ForceUTF8 or SameText(Contents.Charset, 'utf-8') then
      Contents.Encoding := lncUTF8
    else
      Contents.Encoding := lncAnsi;
  end;
end;

{procedure TPO_Parser.New;
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aStrings.Add('msgid ""');
    aStrings.Add('msgstr ""');
    aStrings.Add('"Project-Id-Version: \n"');
    aStrings.Add('"MIME-Version: 1.0\n"');
    aStrings.Add('"Content-Type: text/plain; charset=UTF-8\n"');
    aStrings.Add('"Content-Transfer-Encoding: 8bit\n"');
    aStrings.Add('"Last-Translator: \n"');
    aStrings.Add('"Language-Team: \n"');
    aStrings.Add('"X-Poedit-Language: \n"');
    aStrings.Add('"X-Poedit-Country: \n"');
    aStrings.Add('"POT-Creation-Date: \n"');
    aStrings.Add('"PO-Revision-Date:\n"');
    aStrings.Add('"X-Poedit-KeywordsList: \n"');
  finally
    aStrings.Free;
  end;
end;}

procedure TPO_Parser.Generate(Strings: TStringList);
  procedure WriteItem(Item: TLangItem);
    procedure WriteStrings(Ident, S: string);
    var
      aStrings: TStringList;
      i: Integer;
    begin
      if PosEx('\n', s) = 0 then
        Strings.Add(Ident + QuoteStr(s))
      else
      begin
        Strings.Add(Ident + QuoteStr(''));
        aStrings := TStringList.Create;
        try
          aStrings.Text := StringReplace(s, '\n', '\n'#10, [rfReplaceAll]);
          for i := 0 to aStrings.Count - 1 do
          begin
            if aStrings[i] <> '' then
              Strings.Add(QuoteStr(aStrings[i]));
          end
        finally
          aStrings.Free;
        end;
      end;
    end;

    procedure WriteComments(Ident, s: string);
    var
      aStrings: TStringList;
      i: Integer;
    begin
      aStrings := TStringList.Create;
      try
        ExtractStrings([#13], [], PChar(s), aStrings);
        for i := 0 to aStrings.Count - 1 do
        begin
          if aStrings[i] <> '' then
            Strings.Add(Ident + aStrings[i]);
        end;
      finally
        aStrings.Free;
      end
    end;

  begin
    with Item do
    begin
      WriteComments(ssComment + ' ', Comment);
      WriteComments(ssAutoComment, AutoComment);
      WriteComments(ssFlags, Flags);
      WriteComments(ssReference, Reference);
      WriteStrings(ssMsgID, ID);
      WriteStrings(ssMsgStr, Text);
      Strings.Add('');
    end;
  end;
var
  i: Integer;
begin
  for i := 0 to Contents.Count - 1 do
  begin
    WriteItem(Contents[i]);
  end;
end;

class function TPO_Parser.GetFileExtensions: string;
begin
  Result := '.po'
end;

class function TPO_Parser.GetTitle: string;
begin
  Result := 'GNU gettext po files'
end;

{function TPO_Parser.IsRightToLeft: Boolean;
var
  aStrings: TStringList;
begin
  aStrings := GetOptions;
  try
    Result := SameText(Trim(aStrings.Values['X-DIRECTION']), 'RTL');
  finally
    aStrings.Free;
  end;
end;
}

{ TPOWordItem }

{ TPO_LangContents }

function TPO_LangContents.DoCreateLangItem: TLangItem;
begin
  Result := TPO_LangItem.Create;
end;

{ TPO_Language }

function TPO_Language.CreateParser: TLangParser;
begin
  Result := TPO_Parser.Create;
end;

function TPO_Language.DoCreateContents: TLangContents;
begin
  Result := TPO_LangContents.Create;
end;

initialization
finalization
end.

