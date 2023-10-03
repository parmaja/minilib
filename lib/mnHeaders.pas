unit mnHeaders;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *
 * Ref:
 *    https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Large-Allocation
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
  TmnHeaderItemOption = (
	  hoMultiItem,  // can have more than one item
    hoEmptyValues

	);
  TmnHeaderItemOptions = set of TmnHeaderItemOption;

  TmnHeaderList = class;

  { TmnCustomHeaderItem }

  TmnCustomHeaderItem = class abstract(TmnNamedObject)
  private
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
    function GetItemByName(Index: string): TmnCustomHeaderItem;
    function GetItemByIndex(Index: Integer): TmnCustomHeaderItem;
    function GetItems: TmnHeaderList;
    function GetText: string;
    function GetValue: string; virtual;
    function GetValues(const vName: string): string;
    procedure SetText(AValue: string);
    procedure SetNameValue(const AName, AValue: string);
    procedure SetValues(const AName: string; AValue: string);
  protected
    Options: TmnHeaderItemOptions;
    procedure RequireItems;
    procedure Parse(AValue: String); virtual;
    function CreateItem: TmnCustomHeaderItem; virtual;
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetEnumerator: TmnHeaderItemsEnumerator; inline;
    property Count: Integer read GetCount;
    function GetNameValue(ADelimiter: string = ''): String;
    function Collect: string; virtual;
    procedure Clear;

    property AsInteger: Integer read GetAsInteger;
    function Find(AName: String): TmnCustomHeaderItem; inline;
		function Have(AName: String): Boolean; inline;
    function IsExists: Boolean;
    function AddItem(AName, AValue: string): TmnCustomHeaderItem; virtual;
    function Change(AName, AValue: String): TmnCustomHeaderItem;

    //class operator Implicit(n: TmnCustomHeaderItem): string;

    property Delimiter: Char read FDelimiter;
    property ValueSeparator: Char read FValueSeparator;

    property Text: string read GetText write SetText;
    property Value: string read GetValue;

    property Items: TmnHeaderList read GetItems;
    property Values[const vName: string]: string read GetValues write SetValues;
		property Item[Index: string]: TmnCustomHeaderItem read GetItemByName; default;
    property ItemByIndex[Index: Integer]: TmnCustomHeaderItem read GetItemByIndex; {$ifndef FPC} default; {$endif Delphi}
  end;

  TmnCustomHeaderItemClass = class of TmnCustomHeaderItem;

  { TmnHeaderListItem }

  TmnHeaderListItem = class(TmnCustomHeaderItem)
  protected
    function CreateItem: TmnCustomHeaderItem; override;
  public
    constructor Create; override;
  end;

  TmnHeaderItem = class(TmnCustomHeaderItem)
  public
    constructor Create; override;
  end;

  { TmnHeaderList }

  TmnHeaderList = class(TmnNamedObjectList<TmnCustomHeaderItem>)
  private
  public
  end;

  TmnHeader = class;

  TmnHeaderDictionaryItem = class(TmnNamedObject)
  public
    Options: TmnHeaderItemOptions;
    ItemClass: TmnCustomHeaderItemClass;
  end;

  TmnHeaderDictionary = class(TmnNamedObjectList<TmnHeaderDictionaryItem>)
  private
  public
  end;

  { TmnHeader }

  TmnHeader = class(TmnHeaderListItem)
  protected
    Dictionary: TmnHeaderDictionary;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RegisterDictionary(const AName: string; AItemClass: TmnCustomHeaderItemClass; AOption: TmnHeaderItemOptions);
    procedure CreateSubItem(const AName, AValue: string; out AItem: TmnCustomHeaderItem);
    function Add(const S: string; TrimIt: Boolean = True): TmnCustomHeaderItem; overload;
    function Add(const AName, AValue: string): TmnCustomHeaderItem; overload;
    function AddItem(AName, AValue: string): TmnCustomHeaderItem; override;
    procedure ReadHeader(Stream: TmnBufferStream);
    procedure WriteHeader(Stream: TmnBufferStream);
  end;

  { THeader_ContentType }

  THeader_ContentType = class(TmnHeaderListItem)
  public
    procedure Created; override;
    function CreateItem: TmnCustomHeaderItem; override;
    procedure Parse(AValue: string); override;
  end;

implementation

procedure ParseHeadersCallback(Sender: Pointer; Index: Integer; AName, AValue: string; IsSwitch: Boolean; var Resume: Boolean);
begin
  if (Index = 0) and (AName = '') then
    TmnCustomHeaderItem(Sender).AddItem(Trim(TmnCustomHeaderItem(Sender).Name), Trim(AValue))
  else
    TmnCustomHeaderItem(Sender).AddItem(Trim(AName), Trim(AValue));
end;

{ TmnHeader }

constructor TmnHeader.Create;
begin
  Inherited;
  Dictionary:= TmnHeaderDictionary.Create;
  FDelimiter := #0;
  FValueSeparator := ':';
end;

destructor TmnHeader.Destroy;
begin
  FreeAndNil(Dictionary);
  inherited;
end;

procedure TmnHeader.RegisterDictionary(const AName: string; AItemClass: TmnCustomHeaderItemClass; AOption: TmnHeaderItemOptions);
var
  AItem: TmnHeaderDictionaryItem;
begin
  AItem := TmnHeaderDictionaryItem.Create();
  AItem.Name := AName;
  AItem.ItemClass := AItemClass;
  AItem.Options := AOption;
  Dictionary.Add(AItem);
end;

procedure TmnHeader.CreateSubItem(const AName, AValue: string; out AItem: TmnCustomHeaderItem);
var
  c: TmnHeaderDictionaryItem;
begin
  c := Dictionary.Find(AName);
  if c <> nil then
  begin
    AItem := c.ItemClass.Create;
    AItem.Options := c.Options;
  end
  else
    AItem := TmnHeaderItem.Create;
  AItem.Name := AName;
  AItem.Text := AValue;
end;

function TmnHeader.Add(const S: string; TrimIt: Boolean): TmnCustomHeaderItem;
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
  Result := Add(AName, AValue);
end;

function TmnHeader.Add(const AName, AValue: string): TmnCustomHeaderItem;
begin
  CreateSubItem(AName, AValue, Result);
  FItems.Add(Result);
end;

function TmnHeader.AddItem(AName, AValue: string): TmnCustomHeaderItem;
begin
  Result := Add(AName, AValue);
end;

procedure TmnHeader.ReadHeader(Stream: TmnBufferStream);
var
  line: String;
begin
  if Stream <> nil then
  begin
    while not (cloRead in Stream.Done) do
    begin
      line := UTF8Decode(Stream.ReadLineUTF8);
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

{ THeader_ContentType }

procedure THeader_ContentType.Created;
begin
  inherited;
  FDelimiter := ';';
  FValueSeparator := '=';
end;

function THeader_ContentType.CreateItem: TmnCustomHeaderItem;
begin
  Result := TmnHeaderItem.Create;
  Result.FDelimiter := ',';
end;

procedure THeader_ContentType.Parse(AValue: string);
begin
  FItems.Clear;
  ParseArgumentsCallback(AValue, @ParseHeadersCallback, Self, [], [pargValues, pargDeqoute], [Delimiter], [' ', #9], ['"', ''''], [FValueSeparator]);
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
    if FItems <> nil then
      Result := Item[Name].Text
    else
      Result := FText;
  end;
end;

function TmnCustomHeaderItem.GetValues(const vName: string): string;
begin
  Result := Find(vName).Value;
end;

procedure TmnCustomHeaderItem.SetText(AValue: string);
begin
  if FText =AValue then
	  Exit;
  FText := AValue;
  if FItems <> nil then
    Parse(AValue);
end;

procedure TmnCustomHeaderItem.SetNameValue(const AName, AValue: string);
begin
  Name := AName;
  Text := AValue;
end;

procedure TmnCustomHeaderItem.SetValues(const AName: string; AValue: string);
var
  AItem: TmnCustomHeaderItem;
begin
  AItem := Find(AName);
  if (AItem = nil) then
  begin
	  if (AValue <> '') or (hoEmptyValues in Options) then
      AItem := AddItem(AName, AValue)
  end
  else
  begin
    if (AValue = '') and not (hoEmptyValues in Options) then
      Items.Remove(AItem)
    else
      AItem.SetNameValue(AName, AValue)
  end;
end;

procedure TmnCustomHeaderItem.RequireItems;
begin
  FItems := TmnHeaderList.Create(0);
end;

function TmnCustomHeaderItem.GetItemByName(Index: string): TmnCustomHeaderItem;
begin
  if Index = '' then
    Result := Find(Name)
  else
    Result := Find(Index);
end;

function TmnCustomHeaderItem.GetItemByIndex(Index: Integer): TmnCustomHeaderItem;
begin
  Result := Items[Index];
end;

function TmnCustomHeaderItem.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Value, 0);
end;

procedure TmnCustomHeaderItem.Parse(AValue: String);
begin
  FText := AValue;
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
  Result := nil;
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
  Result := Find(AName) <> nil;
end;

function TmnCustomHeaderItem.IsExists: Boolean;
begin
  Result := Self = nil;
end;

function TmnCustomHeaderItem.AddItem(AName, AValue: string): TmnCustomHeaderItem;
begin
  Result := CreateItem;
  Result.Name := AName;
  Result.FText := AValue;
  Items.Add(Result);
end;

function TmnCustomHeaderItem.Change(AName, AValue: String): TmnCustomHeaderItem;
begin
  Result := Find(AName);
  if Result = nil then
    AddItem(AName, AValue)
  else
    Result.Text := AValue;
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

{ TmnHeaderListItem }

function TmnHeaderListItem.CreateItem: TmnCustomHeaderItem;
begin
  Result := TmnHeaderItem.Create;
  Items.Add(Result);
end;

constructor TmnHeaderListItem.Create;
begin
  inherited;
  RequireItems;
end;

{ TmnHeaderItem }

constructor TmnHeaderItem.Create;
begin
  inherited;
  FDelimiter := #0;
  FValueSeparator := #0;
end;

end.
