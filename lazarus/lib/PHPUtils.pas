unit PHPUtils;
{$mode delphi}
{**
 *  Light PHP Edit project
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  SysUtils, StrUtils, Classes, Contnrs, Dialogs, IniFiles;

type
  TPHPStrings = class(THashedStringList)
  public
    function GetSplitName(S: string): string;
    function GetSplitValue(S: string): string;
    function SetSplitValue(S, Value: string): string;
    function IndexOfName(const Name: string): Integer; override;
    function IndexOfLastName(const Name: string): Integer;
    function IndexOfNameValue(const Name, Value: string): Integer;
    function FindValue(const Name, Value: string): Integer;
  end;

  { TPHPIniFile }

  TPHPIniFile = class(TCustomIniFile)
  private
    FSections: TStringList;
    function AddSection(const Section: string): TPHPStrings;
    function GetCaseSensitive: Boolean;
    procedure LoadValues;
    procedure SetCaseSensitive(Value: Boolean);
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); override;
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteKey(const Section, Ident: string); override;
    procedure EraseSection(const Section: string); override;
    procedure GetStrings(List: TStrings);
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure SetStrings(List: TStrings);
    procedure UpdateFile; override;
    procedure WriteString(const Section, Ident, Value: string); override;
    procedure AppendString(const Section, Ident, Value: string);
    function FindValue(const Section, Ident, Value: string): string;
    procedure CommentString(const Section, Ident, Value: string);
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

implementation

{ TPHPStrings }

function TPHPStrings.FindValue(const Name, Value: string): Integer;
var
  P: Integer;
  S: string;
  I: Integer;
begin
  Result := -1;
  for I := 0 to GetCount - 1 do
  begin
    S := Get(I);
    P := AnsiPos(NameValueSeparator, S);
    if (P <> 0) and (SameText(GetSplitName(S), Name)) and (AnsiPos(UpperCase(Value), UpperCase(GetSplitValue(S))) > 0) then
    begin
      Result := I;
      break;
    end;
  end;
end;

function TPHPStrings.GetSplitName(S: string): string;
var
  P: Integer;
begin
  P := AnsiPos(NameValueSeparator, S);
  if (P <> 0) then
  begin
    Result := TrimRight(Copy(S, 1, P - 1));
  end
  else
    Result := '';
end;

function TPHPStrings.GetSplitValue(S: string): string;
var
  P: Integer;
begin
  P := AnsiPos(NameValueSeparator, S);
  if (P <> 0) then
  begin
    Result := Copy(S, P + 1, MaxInt);
    P := AnsiPos(';', Result); //must ingore the qouted strings when search for ;
    if P > 0 then
      Result := Trim(Copy(Result, 1, P - 1))
    else
      Result := Trim(Result);
  end;
end;

function TPHPStrings.IndexOfLastName(const Name: string): Integer;
var
  P: Integer;
  S: string;
  I: Integer;
begin
  Result := -1;
  for I := 0 to GetCount - 1 do
  begin
    S := Get(I);
    P := AnsiPos(NameValueSeparator, S);
    if (P <> 0) and (SameText(GetSplitName(S), Name)) then
    begin
      Result := I;
    end;
  end;
end;

function TPHPStrings.IndexOfName(const Name: string): Integer;
var
  P: Integer;
  S: string;
  I: Integer;
begin
  Result := -1;
  for I := 0 to GetCount - 1 do
  begin
    S := Get(I);
    P := AnsiPos(NameValueSeparator, S);
    if (P <> 0) and (SameText(GetSplitName(S), Name)) then
    begin
      Result := I;
      break;
    end;
  end;
end;

function TPHPStrings.IndexOfNameValue(const Name, Value: string): Integer;
var
  P: Integer;
  S: string;
  I: Integer;
begin
  Result := -1;
  for I := 0 to GetCount - 1 do
  begin
    S := Get(I);
    P := AnsiPos(NameValueSeparator, S);
    if (P <> 0) and (SameText(GetSplitName(S), Name)) and (SameText(GetSplitValue(S), Value)) then
    begin
      Result := I;
      break;
    end;
  end;
end;

function TPHPStrings.SetSplitValue(S, Value: string): string;
var
  P: Integer;
  T: string;
begin
  P := AnsiPos(NameValueSeparator, S);
  if (P <> 0) then
  begin
    Result := Trim(Copy(S, 1, P - 1));
    T := Copy(S, P + 1, MaxInt);
    P := AnsiPos(';', S);
    if P > 0 then
      Result := Result + ' = ' + Value + Copy(S, P, MaxInt)
    else
      Result := Result + ' = ' + Value;
  end;
end;

{ TPHPIniFile }

constructor TPHPIniFile.Create(const AFileName: string;
  AEscapeLineFeeds: Boolean);
begin
  inherited Create(AFileName);
  FSections := TPHPStrings.Create;
//  FSections.CaseSensitive := True;
  LoadValues;
end;

destructor TPHPIniFile.Destroy;
begin
  if FSections <> nil then
    Clear;
  FSections.Free;
  inherited Destroy;
end;

function TPHPIniFile.AddSection(const Section: string): TPHPStrings;
begin
  Result := TPHPStrings.Create;
  try
    TPHPStrings(Result).CaseSensitive := CaseSensitive;
    FSections.AddObject(Section, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TPHPIniFile.Clear;
var
  I: Integer;
begin
  for I := 0 to FSections.Count - 1 do
    TObject(FSections.Objects[I]).Free;
  FSections.Clear;
end;

procedure TPHPIniFile.DeleteKey(const Section, Ident: string);
var
  I, J: Integer;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(FSections.Objects[I]);
    J := Strings.IndexOfName(Ident);
    if J >= 0 then
      Strings.Delete(J);
  end;
end;

procedure TPHPIniFile.EraseSection(const Section: string);
var
  I: Integer;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    TStrings(FSections.Objects[I]).Free;
    FSections.Delete(I);
  end;
end;

function TPHPIniFile.GetCaseSensitive: Boolean;
begin
  Result := FSections.CaseSensitive;
end;

procedure TPHPIniFile.GetStrings(List: TStrings);
var
  I, J: Integer;
  Strings: TStrings;
  S: string;
begin
  List.BeginUpdate;
  try
    for I := 0 to FSections.Count - 1 do
    begin
      List.Add('[' + FSections[I] + ']');
      Strings := TStrings(FSections.Objects[I]);
      for J := 0 to Strings.Count - 1 do
      begin
        S := Strings[J];
        List.Add(S);
      end;
      if S <> '' then //if last line in section not empty we add a new empty line
        List.Add('');
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TPHPIniFile.LoadValues;
var
  List: TStringList;
begin
  if (FileName <> '') and FileExists(FileName) then
  begin
    List := TStringList.Create;
    try
      List.LoadFromFile(FileName);
      SetStrings(List);
    finally
      List.Free;
    end;
  end
  else
    Clear;
end;

procedure TPHPIniFile.ReadSection(const Section: string;
  Strings: TStrings);
var
  I, J: Integer;
  SectionStrings: TStrings;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
    begin
      SectionStrings := TStrings(FSections.Objects[I]);
      for J := 0 to SectionStrings.Count - 1 do
        Strings.Add(SectionStrings.Names[J]);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TPHPIniFile.ReadSections(Strings: TStrings);
begin
  Strings.Assign(FSections);
end;

procedure TPHPIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
      Strings.Assign(TStrings(FSections.Objects[I]));
  finally
    Strings.EndUpdate;
  end;
end;

function TPHPIniFile.ReadString(const Section, Ident, Default: string): string;
var
  I: Integer;
  Strings: TPHPStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := (FSections.Objects[I] as TPHPStrings);
    I := Strings.IndexOfName(Ident);
    if I >= 0 then
    begin
      Result := Strings.GetSplitValue(Strings[I]);
      Exit;
    end;
  end;
  Result := Default;
end;

procedure TPHPIniFile.SetCaseSensitive(Value: Boolean);
var
  I: Integer;
begin
  if Value <> FSections.CaseSensitive then
  begin
    FSections.CaseSensitive := Value;
    for I := 0 to FSections.Count - 1 do
      with TPHPStrings(FSections.Objects[I]) do
      begin
        CaseSensitive := Value;
        Changed;
      end;
    TPHPStrings(FSections).Changed;
  end;
end;

procedure TPHPIniFile.SetStrings(List: TStrings);
var
  I: Integer;
  S: string;
  Strings: TStrings;
begin
  Clear;
  Strings := nil;
  for I := 0 to List.Count - 1 do
  begin
    S := Trim(List[I]);
    if (S <> '') and (S[1] = '[') and (S[Length(S)] = ']') then
    begin
      Delete(S, 1, 1);
      SetLength(S, Length(S) - 1);
      Strings := AddSection(Trim(S));
    end
    else if Strings <> nil then
      Strings.Add(S);
  end;
end;

procedure TPHPIniFile.UpdateFile;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    GetStrings(List);
    List.SaveToFile(FileName);
  finally
    List.Free;
  end;
end;

procedure TPHPIniFile.WriteString(const Section, Ident, Value: string);
var
  I: Integer;
  Strings: TPHPStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := (FSections.Objects[I] as TPHPStrings)
  else
    Strings := AddSection(Section);
  I := Strings.IndexOfName(Ident);
  if I >= 0 then
    Strings[I] := Strings.SetSplitValue(Strings[I], Value)
  else
  begin
    I := Strings.IndexOfName(';' + Ident);
    if I >= 0 then
      Strings[I] := Strings.SetSplitValue(Strings[I], Value)
    else
      Strings.Add(Ident + ' = ' + Value);
  end;
end;

procedure TPHPIniFile.AppendString(const Section, Ident, Value: string);
var
  I, J: Integer;
  Strings: TPHPStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := (FSections.Objects[I] as TPHPStrings)
  else
    Strings := AddSection(Section);
  I := Strings.IndexOfNameValue(Ident, Value); // if ident with value is found
  if I >= 0 then
    Strings[I] := Strings.SetSplitValue(Strings[I], Value)
  else
  begin
    I := Strings.IndexOfNameValue(';' + Ident, Value); // if ident with value is commented
    if I >= 0 then
      Strings[I] := Strings.SetSplitValue(Strings[I], Value)
    else
    begin
      I := Strings.IndexOfLastName(Ident); // find the near of idents
      J := Strings.IndexOfLastName(';' + Ident);
      if I < J then
        I := J;
      if I >= 0 then
        Strings.Insert(I + 1, Ident + ' = ' + Value)
      else
        Strings.Add(Ident + ' = ' + Value);
    end;
  end
end;

procedure TPHPIniFile.CommentString(const Section, Ident, Value: string);
var
  I: Integer;
  Strings: TPHPStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := (FSections.Objects[I] as TPHPStrings)
  else
    Strings := AddSection(Section);
  I := Strings.IndexOfNameValue(Ident, Value); // if ident with value is found
  if I >= 0 then
    Strings[I] := ';' + Ident + ' = ' + Value;
end;

function TPHPIniFile.FindValue(const Section, Ident, Value: string): string;
var
  I: Integer;
  Strings: TPHPStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := (FSections.Objects[I] as TPHPStrings)
  else
    Strings := AddSection(Section);
  I := Strings.FindValue(Ident, Value); // if ident with value is found
  if I >= 0 then
    Result := Strings.GetSplitValue(Strings[I])
  else
  begin
    I := Strings.FindValue(';' + Ident, Value); // if ident with value is commented
    if I >= 0 then
      Result := Strings.GetSplitValue(Strings[I]);
  end;
end;

end.