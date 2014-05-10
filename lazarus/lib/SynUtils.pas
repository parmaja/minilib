unit SynUtils;
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
 {**
   Useful functions and classes to use it in Highlighters
 *}
 
interface

uses
  SysUtils, Classes, SynHighlighterHashEntries, SynEditTypes;

type
  TProcTableProc = procedure of object;

  PIdentifierTable = ^TIdentifierTable;
  TIdentifierTable = array[AnsiChar] of ByteBool;

  PHashCharTable = ^THashCharTable;
  THashCharTable = array[AnsiChar] of Integer;

  { TSynPersistent }

  TSynPersistent = class(TObject)
   private
     HashNumber: AnsiChar;
   protected
     function NewNumber: AnsiChar;
     function KeyHash(ToHash: PChar; out L: Integer): Integer; virtual;
     function KeyComp(const aKey: string; aWithKey: PChar; L: Integer): Boolean; virtual;
     procedure DoAddKeyword(AKeyword: string; AKind: integer); virtual;
     procedure MakeIdentifiers; virtual;
     procedure MakeHashes; virtual;
     function GetDefaultKind: Integer; virtual; abstract;
   public
     Keywords: TSynHashEntryList;
     Identifiers: TIdentifierTable;
     HashTable: THashCharTable;
     function GetIdentChars: TSynIdentChars; virtual;
     function GetSampleSource: string; virtual;
     function GetIdentKind(MayBe: PChar; out L: Integer): Integer; overload; virtual;
     constructor Create; virtual;
     destructor Destroy; override;
   end;

implementation

procedure TSynPersistent.MakeIdentifiers;
var
  c: char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for c := 'a' to 'z' do
    Identifiers[c] := True;
  for c := 'A' to 'Z' do
    Identifiers[c] := True;
  for c := '0' to '9' do
    Identifiers[c] := True;
  Identifiers['_'] := True;
end;

procedure TSynPersistent.MakeHashes;
var
  c: AnsiChar;
begin
  FillChar(HashTable, SizeOf(HashTable), 0);
  HashTable['_'] := 1;
  for c := 'a' to 'z' do
    HashTable[c] := 2 + Ord(c) - Ord('a');

  for c := 'A' to 'Z' do
    HashTable[c] := 2 + Ord(c) - Ord('A');
end;

function TSynPersistent.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynPersistent.GetSampleSource: string;
begin
  Result := '';
end;

procedure TSynPersistent.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue, L: integer;
begin
  HashValue := KeyHash(PChar(AKeyword), L);
  Keywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

constructor TSynPersistent.Create;
begin
  inherited Create;
  Keywords := TSynHashEntryList.Create;
  MakeIdentifiers;
  MakeHashes;
end;

destructor TSynPersistent.Destroy;
begin
  inherited;
  FreeAndNil(Keywords);
end;

function TSynPersistent.NewNumber: AnsiChar;
begin
  Inc(HashNumber);
  Result := HashNumber;
end;

function TSynPersistent.KeyHash(ToHash: PChar; out L: Integer): Integer;
begin
  Result := 0;
  L := 0;
  while ToHash^ in GetIdentChars do
  begin
    Inc(Result, HashTable[ToHash^]);
    Inc(ToHash);
    Inc(L);
  end;
end;

function TSynPersistent.KeyComp(const aKey: string; aWithKey: PChar; L: Integer): Boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := aWithKey;
  pKey2 := Pointer(aKey);
  for i := 1 to L do
  begin
    if HashTable[pKey1^] <> HashTable[pKey2^] then
    begin
      Result := False;
      exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := True;
end;

function TSynPersistent.GetIdentKind(MayBe: PChar; out L: Integer): Integer;
var
  Entry: TSynHashEntry;
begin
  Entry := Keywords[KeyHash(MayBe, L)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > L then
      break
    else if Entry.KeywordLen = L then
      if KeyComp(Entry.Keyword, MayBe, L) then
      begin
        Result := Entry.Kind;
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := GetDefaultKind;
end;

end.
