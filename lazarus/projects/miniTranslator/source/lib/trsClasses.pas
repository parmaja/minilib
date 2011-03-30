unit trsClasses;
{**
 * This file is part of the "Mini Translator" http://www.sourceforge.net/projects/minilib
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Controls,
  LangClasses, LangUtils,
  PO_Languages, FluxBB_Parser, contnrs,
  mnXMLRtti, mnXMLRttiProfile;

type

  { TtrsIndex }

  TtrsIndex = class(TLangList)
  private
  public
  end;

  { TtrsLanguage }

  TtrsLanguage = class(TLanguage)
  private
  public
    function CreateIndex: TtrsIndex;
    function RemoveDublicated(TestMode: Boolean): TStringList;
    function ProgressReport: TStringList;
    function FindTranslatedValue(S: string): TLangItem;
    function Retranslate(vLanguage: TLanguage; TestMode: Boolean): TStringList;
    procedure CheckWith(WithLanguage: TLanguage);
  end;

  { TtrsDictionary }

  TtrsDictionary = class(TObject)
  private
  public
    Local: TtrsLanguage;
    Original: TtrsLanguage;
    constructor Create;
    destructor Destroy; override;
  end;

function GetDialogFilter(vFlags: TLangFilerFlags): string;

implementation
//show count of non translated items

function TtrsLanguage.ProgressReport: TStringList;
var
  aList: TtrsIndex;
  i: Integer;
  aItem: TLangItem;
  aCount: Integer;
begin
  aList := CreateIndex;
  Result := TStringList.Create;
  aCount := 0;
  try
    i := 0;
    while i < aList.Count do
    begin
      aItem := aList[i];
      if aItem.Text = '' then
        Inc(aCount);
      Inc(i);
    end;
    Result.Add('Not translated: ' + IntToStr(aCount) + ' of ' + IntToStr(aList.Count));
  finally
    aList.Free;
  end;
end;

function TtrsLanguage.RemoveDublicated(TestMode: Boolean): TStringList;
var
  aList: TtrsIndex;
  aDeleteList: TtrsIndex;
  i, j: Integer;
  aItem: TLangItem;
begin
  aDeleteList := TtrsIndex.Create(not TestMode);
  aList := CreateIndex;
  Result := TStringList.Create;
  try
    i := 0;
    while i < aList.Count do
    begin
      aItem := aList[i];
      j := i + 1;
      while j < aList.Count do
      begin
        if SameText(aList[j].Text, aItem.Text) then
        begin
          Result.Add(aList[j].ID + '=' + aItem.ID + ';' + aList[j].DisplayText);
          aDeleteList.Add(aList[j]);
          aList.Delete(j);
        end
        else
          inc(j);
      end;
      Inc(i)
    end;
    aDeleteList.Clear; //delete all word if no test mode
  finally
    aList.Free;
    aDeleteList.Free;
  end;
end;

{ TtrsDictionary }

constructor TtrsDictionary.Create;
begin
  inherited Create;
end;

destructor TtrsDictionary.Destroy;
begin
  FreeAndNil(Local);
  FreeAndNil(Original);
  inherited Destroy;
end;

{ TtrsLanguage }

function TtrsLanguage.CreateIndex: TtrsIndex;
var
  i, j: Integer;
  aItem: TLangItem;
begin
  Result := TtrsIndex.Create(False);
  for i := 0 to Count - 1 do
  begin
    for j := 0 to Items[i].Count - 1 do
    begin
      aItem := Items[i].Items[j];
      Result.Add(aItem);
    end;
  end;
end;

function TtrsLanguage.FindTranslatedValue(S: string): TLangItem;
var
  i, j: Integer;
  aItem: TLangItem;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    for j := 0 to Items[i].Count - 1 do
    begin
      aItem := Items[i].Items[j];
      if SameText(aItem.DisplayText, S) then
      begin
        Result := aItem;
        exit;
      end;
    end;
  end;
end;

function TtrsLanguage.Retranslate(vLanguage: TLanguage; TestMode: Boolean): TStringList;
var
  aList: TtrsIndex;
  i, j: Integer;
  aItem, aLocalItem, aAnotherItem: TLangItem;
begin
  aList := CreateIndex;
  Result := TStringList.Create;
  try
    i := 0;
    while i < aList.Count do
    begin
      aItem := aList[i];
      if aItem.ID <> '' then
      begin
        aLocalItem := vLanguage.FindID(aItem.ID);
        if (aLocalItem <> nil) and (aLocalItem.Text <> '') then
        begin
          j := i + 1;
          while j < aList.Count do
          begin
            if SameText(aList[j].Text, aItem.Text) then
            begin
              aAnotherItem := vLanguage.FindID(aList[j].ID);
              if aAnotherItem.Text = '' then
              begin
                if not TestMode then
                  aAnotherItem.Text := aLocalItem.Text;
                Result.Add(aAnotherItem.ID + '=' + aLocalItem.ID + ';' + aLocalItem.DisplayText);
              end;
            end;
            inc(j);
          end;
        end;
      end;
      Inc(i)
    end;
  finally
    aList.Free;
  end;
end;

procedure TtrsLanguage.CheckWith(WithLanguage: TLanguage);
var
  i, j: Integer;
  aItem1: TLangItem;
  aItem2: TLangItem;
begin
  for i := 0 to Count - 1 do
  begin
    for j := 0 to Items[i].Count - 1 do
    begin
      aItem1 := Items[i].Items[j];
      //aItem2 := WithLanguage.Values[Items[i].Name].Find(Items[i].Items[j].ID);
      if (RightStr(aItem2.Text, 1) = '.') and (RightStr(aItem1.Text, 1) <> '.') then
      begin
        aItem1.Text := aItem1.Text + '.';
      end
      else if (RightStr(aItem2.Text, 1) <> '.') and (RightStr(aItem1.Text, 1) = '.') then
      begin
        aItem1.Text := Copy(aItem1.Text, 1, Length(aItem1.Text) - 1);
      end
    end;
  end;
end;

function GetDialogFilter(vFlags: TLangFilerFlags): string;
var
  i: Integer;
  s: string;
  aAll: string;
begin
  aAll := '';
  Result := '';
  try
    for i := 0 to LangOptions.FilerClasses.Count - 1 do
    if (vFlags * LangOptions.FilerClasses[i].GetFlags) = vFlags then
    begin
      if Result <> '' then
        Result := Result + '|';
        s := '*.' + LangOptions.FilerClasses[i].GetExtension;
        if aAll <> '' then
          aAll := aAll + ';';
        aAll := aAll + '*.' + LangOptions.FilerClasses[i].GetExtension;
      Result := Result + LangOptions.FilerClasses[i].GetTitle + ' (' + s + ')|' + s;
    end;
  finally
  end;

  if Result <> '' then
    Result := 'All files ('+aAll+')|' + aAll + '|' + Result;

  if Result <> '' then
    Result := Result + '|';
  Result := Result + 'Any file (*.*)|*.*';
end;

initialization
finalization
end.

