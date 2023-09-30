unit mnHeaders;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *
 *}

{$ifdef FPC}
{$mode delphi}{$H+}
{$ModeSwitch ArrayOperators}
{$endif}

{$define minilib}

{

1
	n: v
2
	n: v; n1=v1; n2=v2
3
	n: v,x,y; n1=l1,l2; n1=v3


1
v := h[n].value; //v

3
v := h[n].text; //v,x,y; n1=l1,l2; n1=v3
v := h[n].value; //v,x,y
v := h[n].list[1]; //x

v := h[n].items[n1].list[1]; //l2

 }

interface

uses
  Classes, SysUtils, DateUtils, Variants, Contnrs,
  {$ifndef FPC} Types,{$endif}
  mnClasses, mnStreams, mnUtils;

type
  TmnHeaderList = class;

  { TmnCustomHeaderItem }

  TmnCustomHeaderItem = class abstract(TObject)
  private
    FName: string;
    FText: string;
    FDelimiter: Char;
    FValueSeparator: Char;
    FItems: TmnHeaderList;

    type

	    { TmnHeaderItemsEnumerator }

      TmnHeaderItemsEnumerator = class(TObject)
      private
        FList: TmnCustomHeaderItem;
        FIndex: Integer;
      public
        constructor Create(AList: TmnCustomHeaderItem);
        function GetCurrent: TmnCustomHeaderItem;
        function MoveNext: Boolean;
        property Current: TmnCustomHeaderItem read GetCurrent;
      end;

    function GetAsInteger: Integer;
    function GetItem(Index: string): TmnCustomHeaderItem;
    function GetItems: TmnHeaderList;
    function GetText: string;
    function GetValue: string;
    function GetValues(const vName: string): string;
    procedure SetText(AValue: string);
    procedure SetNameValue(const AName, AValue: string);
    procedure SetValues(const AName: string; AValue: string);
  protected
    function DoCreateItem: TmnCustomHeaderItem; virtual; abstract;
    procedure CheckItems;
    procedure Parse;
  function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateItem: TmnCustomHeaderItem;
    function GetEnumerator: TmnHeaderItemsEnumerator; inline;
    property Count: Integer read GetCount;
    function GetNameValue(ADelimiter: string = ''): String;
    function Collect: string;
    procedure Clear;

    property Name: string read FName write FName;
    property Value: string read GetValue;
    property AsInteger: Integer read GetAsInteger;
    function Find(AName: String): TmnCustomHeaderItem; inline;
		function Have(AName: String): Boolean; inline;
    function IsExists: Boolean;
    function Add(S: string; TrimIt: Boolean = True): TmnCustomHeaderItem; overload;
    function Add(AName, AValue: string): TmnCustomHeaderItem; overload;
    function Change(AName, AValue: String): TmnCustomHeaderItem;

    property Delimiter: Char read FDelimiter;
    property ValueSeparator: Char read FValueSeparator;

    property Items: TmnHeaderList read GetItems;
    property Text: string read GetText write SetText;
    property Values[const vName: string]: string read GetValues write SetValues;
		property Item[Index: string]: TmnCustomHeaderItem read GetItem; default;
  end;

  { TmnHeaderSubItem }

  TmnHeaderSubItem = class(TmnCustomHeaderItem)
  public
    constructor Create;
    function DoCreateItem: TmnCustomHeaderItem; override;
  end;

  TmnHeaderItem = class(TmnCustomHeaderItem)
  public
    constructor Create;
    function DoCreateItem: TmnCustomHeaderItem; override;
  end;

  { TmnHeaderList }

  TmnHeaderList = class(TmnNamedObjectList<TmnCustomHeaderItem>)
  private
  public
  end;

  { TmnHeader }

  TmnHeader = class(TmnCustomHeaderItem)
  public
    constructor Create;
    function DoCreateItem: TmnCustomHeaderItem; override;
    procedure ReadHeader(Stream: TmnBufferStream);
    procedure WriteHeader(Stream: TmnBufferStream);
  end;

implementation

{ TmnHeader }

constructor TmnHeader.Create;
begin
  Inherited Create;
  FDelimiter := #0;
  FValueSeparator := ':';
  CheckItems;
end;

function TmnHeader.DoCreateItem: TmnCustomHeaderItem;
begin
  Result := TmnHeaderItem.Create;
end;

procedure TmnHeader.ReadHeader(Stream: TmnBufferStream);
var
  line: String;
begin
  if Stream <> nil then
  begin
    while not (cloRead in Stream.Done) do
    begin
      line := UTF8ToString(Stream.ReadLineUTF8);
      if line = '' then
        break
      else
      begin
        Add(line, True);
      end;
    end;
  end;
end;

procedure TmnHeader.WriteHeader(Stream: TmnBufferStream);
var
  f: TmnCustomHeaderItem;
begin
  for f in Self do
    Stream.WriteLineUTF8(f.Collect);
end;

{ TmnCustomHeaderItem }

function TmnCustomHeaderItem.GetItems: TmnHeaderList;
begin
  Result := FItems;
end;

function TmnCustomHeaderItem.GetText: string;
begin
  if Self = nil then
    Result := ''
  else
    Result := FText;
end;

function TmnCustomHeaderItem.GetValue: string;
begin
  if Self = nil then
    Result := ''
  else
  begin
    CheckItems;
    if FItems <> nil then
      Result := Item[Name].Text
    else
      Result := FText;
  end;
end;

function TmnCustomHeaderItem.GetValues(const vName: string): string;
begin
  CheckItems;
  Result := Find(vName).Value;
end;

procedure TmnCustomHeaderItem.SetText(AValue: string);
begin
  if FText =AValue then Exit;
  FText := AValue;
  if FItems<>nil then
    Parse;
end;

procedure TmnCustomHeaderItem.SetNameValue(const AName, AValue: string);
begin
  FName := AName;
  FText := AValue;
end;

procedure TmnCustomHeaderItem.SetValues(const AName: string; AValue: string);
var
  Item: TmnCustomHeaderItem;
begin
  CheckItems;
  Item := Find(AName);
  if Item = nil then
    Item := Add(AName, AValue)
  else
    Item.SetNameValue(AName, AValue)

  //Items.SetItem(vName);
end;

procedure TmnCustomHeaderItem.CheckItems;
begin
  if FItems = nil then
    Parse;
end;

function TmnCustomHeaderItem.GetItem(Index: string): TmnCustomHeaderItem;
begin
  Result := Find(Index);
end;

function TmnCustomHeaderItem.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Value, 0);
end;

procedure ParseHeadersCallback(Sender: Pointer; Index: Integer; AName, AValue: string; IsSwitch: Boolean; var Resume: Boolean);
begin
  if (Index = 0) and (AName = '') then
    TmnCustomHeaderItem(Sender).Add(Trim(TmnCustomHeaderItem(Sender).FName), Trim(AValue))
  else
    TmnCustomHeaderItem(Sender).Add(Trim(AName), Trim(AValue));
end;

procedure TmnCustomHeaderItem.Parse;
begin
  if FItems = nil then
    FItems := TmnHeaderList.Create(0); //no hasing
  FItems.Clear;
  ParseArgumentsCallback(FText, @ParseHeadersCallback, Self, [], [pargValues, pargDeqoute], [Delimiter], [' ', #9], ['"', ''''], ['=']);
end;

constructor TmnCustomHeaderItem.Create;
begin
  inherited Create;
  FDelimiter := ';';
  FValueSeparator := '=';
end;

destructor TmnCustomHeaderItem.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TmnCustomHeaderItem.CreateItem: TmnCustomHeaderItem;
begin
  Result := DoCreateItem;
  if FItems = nil then
    raise Exception.Create('Header Items is nil');
  Items.Add(Result);
end;

function TmnCustomHeaderItem.GetNameValue(ADelimiter: string): String;
begin
  if ADelimiter = '' then
    ADelimiter := Delimiter;
  Result := Name + ADelimiter + Value;
end;

function TmnCustomHeaderItem.Collect: string;
begin

end;

procedure TmnCustomHeaderItem.Clear;
begin
  if FItems <> nil then
    FItems.Clear;
end;

function TmnCustomHeaderItem.Find(AName: String): TmnCustomHeaderItem;
begin
  if FItems <> nil then
    Result := Items.Find(AName)
  else
    Result := nil
end;

function TmnCustomHeaderItem.Have(AName: String): Boolean;
begin
  CheckItems;
  Result := Find(AName) <> nil;
end;

function TmnCustomHeaderItem.IsExists: Boolean;
begin
  Result := Self = nil;
end;

function TmnCustomHeaderItem.Add(AName, AValue: string): TmnCustomHeaderItem;
begin
  Result := CreateItem;
  Result.FName := AName;
  Result.FText := AValue;
end;

function TmnCustomHeaderItem.Add(S: string; TrimIt: Boolean): TmnCustomHeaderItem;
var
  p: Integer;
  aName: string;
  aValue: string;
begin
  p := Pos(FValueSeparator, S);
  if p > 0 then
  begin
    aName := Trim(Copy(S, 1, P - 1));
    aValue := Copy(S, P + 1, MaxInt);
  end
  else
  begin
    aName := '';
    aValue := S;
  end;
  if TrimIt then
    aValue := Trim(aValue);
  Result := Add(aName, aValue);
end;

function TmnCustomHeaderItem.Change(AName, AValue: String): TmnCustomHeaderItem;
begin
  Result := Find(AName);
  if Result = nil then
    Add(AName, AValue)
  else
    Result.FText := AValue;
end;

{ TmnCustomHeaderItem }

function TmnCustomHeaderItem.GetCount: Integer;
begin
  if FItems <> nil then
    Result := FItems.Count
  else
    Result := 0;
end;

function TmnCustomHeaderItem.GetEnumerator: TmnHeaderItemsEnumerator;
begin
  Result := TmnHeaderItemsEnumerator.Create(Self);
end;

{ TmnCustomHeaderItem.TmnHeaderItemsEnumerator }

constructor TmnCustomHeaderItem.TmnHeaderItemsEnumerator.Create(AList: TmnCustomHeaderItem);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TmnCustomHeaderItem.TmnHeaderItemsEnumerator.GetCurrent: TmnCustomHeaderItem;
begin
  Result := FList.Items[FIndex];
end;

function TmnCustomHeaderItem.TmnHeaderItemsEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TmnHeaderSubItem }

constructor TmnHeaderSubItem.Create;
begin
  FDelimiter := ';';
  FValueSeparator := '=';
end;

function TmnHeaderSubItem.DoCreateItem: TmnCustomHeaderItem;
begin
  Result := TmnHeaderSubItem.Create;
end;

{ TmnHeaderItem }

constructor TmnHeaderItem.Create;
begin
  inherited Create;
  FDelimiter := ',';
  FValueSeparator := '=';
end;

function TmnHeaderItem.DoCreateItem: TmnCustomHeaderItem;
begin
  Result := TmnHeaderSubItem.Create;
end;

end.
