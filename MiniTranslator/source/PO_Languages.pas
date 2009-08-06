unit PO_Languages;
{**
 *  This file is part of the "Mini Library" http://www.sourceforge.net/projects/minilib
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE objfpc}
{$ENDIF}
{$M+}{$H+}

interface

uses
  SysUtils, Variants, Classes, Contnrs,
  LangClasses;

type
  //keep poMsgStr in first
  TPO_State = (poMsgStr, poMsgId, poMsgCtxt, poComment, poAutoComment, poReference, poFlags, poPreviousID, poPreviousText);
  //

  { TPO_Parser }

  TPO_Parser = class(TLangParser)
  private
    FState: TPO_State;
  protected
    FLangItem: TLangItem; //for multiline need to keep it in object body
    procedure DoParse(Strings: TStringList); override;
    procedure DoGenerate(Strings: TStringList); override;
  public
    procedure Clear;
    procedure ParseLine(Number:Integer; ALine: string); //if you like to parse line by line not use a Contents property
    class function GetName: string; override;
    class function GetExtension: string; override;
    class function GetTitle: string; override;
  end;

  { TPOFileFiler }

  TPOFileFiler = class(TLangFiler)
  protected
    procedure DoLoadFrom(vSource: string; vLanguage: TLanguage); override;
    procedure DoSaveTo(vSource: string; vLanguage: TLanguage); override;
  public
    constructor Create; override;
    function CreateParser: TLangParser; override;
    class function GetName: string; override;
    class function GetTitle: string; override;
    class function GetExtension: string; override;
    class function GetFlags: TLangFilerFlags; override;
  end;

  { TPODirFiler }
{
  this not standard directory PO files and setting.ini file
}
  TPODirectoryFiler = class(TLangFiler)
  protected
    procedure DoLoadFrom(vSource: string; vLanguage: TLanguage); override;
    procedure DoSaveTo(vSource: string; vLanguage: TLanguage); override;
  public
    constructor Create; override;
    function CreateParser: TLangParser; override;
    class function GetName: string; override;
    class function GetTitle: string; override;
    class function GetExtension: string; override;
    class function GetFlags: TLangFilerFlags; override;
  end;

  { TPODirectoryExFiler }

  TPODirectoryExFiler = class(TPODirectoryFiler) //with setting.ini
  protected
    procedure DoLoadFrom(vSource: string; vLanguage: TLanguage); override;
  public
    class function GetName: string; override;
    class function GetTitle: string; override;
  end;

implementation

uses
  StrUtils, mnUtils;

const
  ssMsgId = 'msgid';
  ssMsgCtxt = 'msgctxt';
  ssMsgStr = 'msgstr';
  ssComment = '#';
  ssFlags = '#,';
  ssReference = '#:';
  ssAutoComment = '#.';
  ssPreviousText = '#| ';
  ssPreviousID = '#| msgid';

//not supported
  ssMsgPlural = 'msgid_plural ';
  ssMsgStrs = 'msgstr[';

{$IFDEF WINDOWS}
  sOSEOL = #13#10;
{$ELSE}
  sOSEOL = #13;
{$ENDIF}

  sPOCommands: array[TPO_State] of string = (ssMsgStr, ssMsgId, ssMsgCtxt,
    ssComment, ssAutoComment, ssReference, ssFlags, ssPreviousID, ssPreviousText);

function EscapePOString(s: string): string;
begin
  Result := EscapeString(s, '\', [#8, #9, #13, '\', '"'], ['b', 't', 'n', '\', '"']);
end;

function DescapePOString(s: string): string;
begin
  Result := DescapeString(s, '\', [#8, #9, #13, '\', '"'], ['b', 't', 'n', '\', '"']);
end;

function CutStrID(const ID, S: string; Dequote, Descape: Boolean): string;
begin
  Result := TrimLeft(MidStr(S, Length(ID) + 1, MaxInt));
  if Dequote then
    Result := DequoteStr(Trim(Result));
  if Descape then
    Result := DescapePOString(Result);
end;

procedure TPO_Parser.ParseLine(Number:Integer; ALine: string);

  function Check(const vLine: string; const WithText: string): Boolean;
  begin
    Result := LeftStr(vLine, Length(WithText)) = WithText;
  end;

  function CheckAndAssign(vState: TPO_State; CreateNew, Convert: Boolean; const vLine: string): Boolean;
  var
    S: string;
    W: string;
  begin
    W := sPOCommands[vState];
    Result := Check(vLine, W);
    if Result then
    begin
      if CreateNew then
        FLangItem := Contents.CreateLangItem;
      FState := vState;
      S := CutStrID(W, aLine, Convert, Convert);
      case vState of
        poMsgID: FLangItem.ID := FLangItem.ID + S;
        poMsgStr: FLangItem.Text := FLangItem.Text + S;
        poMsgCtxt: FLangItem.Context := FLangItem.Context + S;
        poAutoComment: FLangItem.AutoComment := FLangItem.AutoComment + S;
        poFlags: FLangItem.Flags := FLangItem.Flags + S;
        poReference: FLangItem.Reference := FLangItem.Reference + S;
        poPreviousID: FLangItem.PreviousID := FLangItem.PreviousID + S;
        poPreviousText: FLangItem.PreviousText := FLangItem.PreviousText + S;
        poComment: FLangItem.Comment := FLangItem.Comment + S;
      end;
      if CreateNew and (FState = poMsgID) and (FLangItem.ID = '') then
        FLangItem.Visible := False;
    end;
  end;

  function CheckText(CheckNew: Boolean): Boolean;
  begin
    Result :=
      CheckAndAssign(poMsgCtxt, CheckNew, True, aLine) or
      CheckAndAssign(poAutoComment, CheckNew, False, aLine) or
      CheckAndAssign(poFlags, CheckNew, False, aLine) or
      CheckAndAssign(poReference, CheckNew, False, aLine) or
      CheckAndAssign(poPreviousID, CheckNew, False, aLine) or
      CheckAndAssign(poPreviousText, CheckNew, False, aLine) or
      CheckAndAssign(poComment, CheckNew, False, aLine);
  end;
begin
  if aLine <> '' then
  begin
    case FState of
      poMsgCtxt, poComment..poPreviousText:
        begin
          if CheckAndAssign(poMsgID, False, True, aLine) then
          else if CheckText(False) then
          else if LeftStr(aLine, 1) <> '"' then
            raise ELangException.Create('PO file Malformed at line ' + IntToStr(Number) + ' in ' + Contents.Source)
          else
            FLangItem.ID := FLangItem.ID + DescapePOString(DequoteStr(aLine));
        end;
      poMsgId:
        begin
          if CheckAndAssign(poMsgStr, False, True, aLine) then
          else if CheckText(False) then
          else if LeftStr(aLine, 1) <> '"' then
            raise ELangException.Create('PO file Malformed at line ' + IntToStr(Number) + ' in ' + Contents.Source)
          else
            FLangItem.ID := FLangItem.ID + DescapePOString(DequoteStr(aLine));
        end;
      poMsgStr:
        begin
          if CheckAndAssign(poMsgID, True, True, aLine) then
          else if CheckText(True) then //if there is a MsgID or any comment then it is new Item
          else if LeftStr(aLine, 1) <> '"' then
            raise ELangException.Create('PO file Malformed at line ' + IntToStr(Number) + ' in ' + Contents.Source)
          else
            FLangItem.Text := FLangItem.Text + DescapePOString(DequoteStr(aLine));
        end;
    end;
  end;
end;

procedure TPO_Parser.DoParse(Strings: TStringList);
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
    Inc(l);//before parse the line for first line take 1
    ParseLine(l, Trim(s));  
  end;

  s := Contents.GetText('');
  ExtractStrings([#13], [' '], PChar(s), Contents.Attributes);
  for i := 0 to Contents.Attributes.Count - 1 do
  begin
    s := Trim(Contents.Attributes[i]);
    p := Pos(':', s);
    if p > 0 then
    begin
      s := Trim(Copy(s, 1, p - 1)) + '=' + Trim(Copy(s, p + 1, MaxInt));
      Contents.Attributes[i] := s;
    end
    else
      Contents.Attributes[i] := Trim(s);
  end;
  Contents.Title := Contents.Attributes.Values['X-Name'];
  s := Trim(Contents.Attributes.Values['Content-Type']);
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
  Contents.IsRightToLeft := SameText(Trim(Contents.Attributes.Values['X-DIRECTION']), 'RTL');

  if ForceUTF8 or SameText(Contents.Charset, 'utf-8') then
    Contents.Encoding := lncUTF8
  else
    Contents.Encoding := lncAnsi;
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

procedure TPO_Parser.DoGenerate(Strings: TStringList);
  procedure WriteItem(Item: TLangItem);
    procedure WriteStrings(Ident, S: string; Force: Boolean);
    var
      aStrings: TStringList;
      i: Integer;
      function GetEOL: string;
      begin
        if i < (aStrings.Count - 1) then
          Result := '\n'
        else
          Result := '';
      end;
    begin
      if Force or (s <> '') then
      begin
        aStrings := TStringList.Create;
        try
          aStrings.Text := s;
          if (aStrings.Count <= 1) then
            Strings.Add(Ident + ' ' +QuoteStr(EscapePOString(s)))
          else
          begin
            Strings.Add(Ident + ' ""');
            for i := 0 to aStrings.Count - 1 do
            begin
              Strings.Add(QuoteStr(EscapePOString(aStrings[i]) + GetEOL));
            end
          end;
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
      if s <> '' then
      begin
        aStrings := TStringList.Create;
        try
          ExtractStrings([#13], [], PChar(s), aStrings);
          for i := 0 to aStrings.Count - 1 do
          begin
            if aStrings[i] <> '' then
              Strings.Add(Ident + ' ' + aStrings[i]);
          end;
        finally
          aStrings.Free;
        end
      end;
    end;

  begin
    with Item do
    begin
      WriteComments(ssComment, Comment);
      WriteComments(ssAutoComment, AutoComment);
      WriteComments(ssReference, Reference);
      WriteComments(ssFlags, Flags);
      WriteComments(ssPreviousID, PreviousID);
      WriteComments(ssPreviousText, PreviousText);
      WriteStrings(ssMsgCtxt, Context, False);
      WriteStrings(ssMsgID, ID, True);
      WriteStrings(ssMsgStr, Text, True);
      Strings.Add('');
    end;
  end;
var
  i: Integer;
begin
  //TODO: Write Attributes
  for i := 0 to Contents.Count - 1 do
  begin
    WriteItem(Contents[i]);
  end;
end;

procedure TPO_Parser.Clear;
begin
  FLangItem := nil;
  FState := poMsgStr;
end;

class function TPO_Parser.GetName: string;
begin
  Result := 'GetTextPOFiles';
end;

class function TPO_Parser.GetExtension: string;
begin
  Result := 'PO'
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

{ TPODirectoryFiler }

constructor TPODirectoryFiler.Create;
begin
  inherited Create;
end;

function TPODirectoryFiler.CreateParser: TLangParser;
begin
  Result := TPO_Parser.Create;
end;

procedure TPODirectoryFiler.DoLoadFrom(vSource: string; vLanguage: TLanguage);
var
  I: Integer;
  SearchRec: TSearchRec;
  aPath: string;
  aParser: TLangParser;
begin
  with vLanguage do
  begin
    aPath := IncludeTrailingPathDelimiter(vSource);
    try
      Clear;
      try
        I := FindFirst(aPath + '*.' + GetExtension, 0, SearchRec);
        while I = 0 do
        begin
          aParser := CreateParser;
          try
            ParseLanguageFile(aPath + SearchRec.Name, vLanguage, aParser);
          finally
            aParser.Free;
          end;
          I := FindNext(SearchRec);
        end;
      finally
        FindClose(SearchRec);
      end;
    except
      raise;
    end;
  end;
  if vLanguage.Count > 0 then
    vLanguage.IsRightToLeft := vLanguage[0].IsRightToLeft;
end;

procedure TPODirectoryFiler.DoSaveTo(vSource: string; vLanguage: TLanguage);
begin
end;

class function TPODirectoryFiler.GetName: string;
begin
  Result := 'PODir';
end;

class function TPODirectoryFiler.GetTitle: string;
begin
  Result := 'PO Directory';
end;

class function TPODirectoryFiler.GetExtension: string;
begin
  Result := 'PO';
end;

class function TPODirectoryFiler.GetFlags: TLangFilerFlags;
begin
  Result := [lffDirectory, lffMultiple];
end;

{ TPOFileFiler }

constructor TPOFileFiler.Create;
begin
  inherited Create;
end;

function TPOFileFiler.CreateParser: TLangParser;
begin
  Result := TPO_Parser.Create;
end;

procedure TPOFileFiler.DoLoadFrom(vSource: string; vLanguage: TLanguage);
var
  aParser: TLangParser;
begin
  with vLanguage do
  begin
    try
      Clear;
      aParser := CreateParser;
      try
        ParseLanguageFile(vSource, vLanguage, aParser);
      finally
        aParser.Free;
      end;
    except
      raise;
    end;
    Source := vSource;
    Name := ExtractFileName(vSource);
    if vLanguage.Count > 0 then
      vLanguage.IsRightToLeft := vLanguage[0].IsRightToLeft;
    ID := 0;
  end;
end;

procedure TPOFileFiler.DoSaveTo(vSource: string; vLanguage: TLanguage);
var
  aParser: TLangParser;
begin
  with vLanguage do
  begin
    try
      aParser := CreateParser;
      try
        if vLanguage.Count > 0 then
        begin
          GenerateLanguageFile(vSource, vLanguage[0], aParser); //single file
          //vLanguage.Source := vSource;//like as save as
        end;
      finally
        aParser.Free;
      end;
    except
      raise;
    end;
  end;
end;

class function TPOFileFiler.GetName: string;
begin
  Result := 'POFile'
end;

class function TPOFileFiler.GetTitle: string;
begin
  Result := 'PO File';
end;

class function TPOFileFiler.GetExtension: string;
begin
  Result := 'PO';
end;

class function TPOFileFiler.GetFlags: TLangFilerFlags;
begin
  Result := [lffAlone];
end;

{ TPODirectoryExFiler }

procedure TPODirectoryExFiler.DoLoadFrom(vSource: string; vLanguage: TLanguage);
var
  aPath, aName, aFile: string;
  aStrings: TStringList;
begin
  inherited;
  with vLanguage do
  begin
    aPath := IncludeTrailingPathDelimiter(vSource);
    aName := ExtractFileName(ExcludeTrailingPathDelimiter(vSource));
    aFile := aPath + '\language.conf';
    if FileExists(aFile) then
    begin
      aStrings := TStringList.Create;
      try
        aStrings.LoadFromFile(aFile);
        Name := aStrings.Values['Name'];
        if Name = '' then
          Name := aName;
        IsRightToLeft := StrToBoolDef(aStrings.Values['RightToLeft'], False);
        ID := StrToIntDef(aStrings.Values['ID'], 0);
      finally
        aStrings.Free;
      end;
    end;
  end;
end;

class function TPODirectoryExFiler.GetName: string;
begin
  Result := 'PODirEx';
end;

class function TPODirectoryExFiler.GetTitle: string;
begin
  Result := 'PO Config Directory'
end;

initialization
finalization
  LangOptions.RegisterFilerClass(TPOFileFiler);
  LangOptions.RegisterFilerClass(TPODirectoryFiler);
  LangOptions.RegisterFilerClass(TPODirectoryExFiler);
end.

