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
    hoList, // no names just values set into name not value
    hoGroupValues,
    hoEmptyValues
	);
  TmnHeaderItemOptions = set of TmnHeaderItemOption;

  TmnHeaderList = class;

  { TmnHeaderItem }

  TmnHeaderItem = class(TmnNamedObject)
  private
    FText: string;
    FDelimiter: Char;
    FValueSeparator: Char;
    FItems: TmnHeaderList;

    type

	    { TmnHeaderItemsEnumerator }

      TmnHeaderItemsEnumerator = class(TObject)
      private
        FList: TmnHeaderItem;
        FIndex: Integer;
      public
        constructor Create(AList: TmnHeaderItem);
        function GetCurrent: TmnHeaderItem;
        function MoveNext: Boolean;
        property Current: TmnHeaderItem read GetCurrent;
      end;

    function GetAsInteger: Integer;
    function GetItemByName(Index: string): TmnHeaderItem;
    function GetItemByIndex(Index: Integer): TmnHeaderItem;
    function GetItems: TmnHeaderList;
    function GetText: string;
    function GetValue: string;
    function GetValues(const vName: string): string;
    procedure SetText(AValue: string);
    procedure SetNameValue(const AName, AValue: string);
    procedure SetValues(const AName: string; AValue: string);
  protected
    Options: TmnHeaderItemOptions;
    procedure RequireItems;
    procedure Parse(AValue: String); virtual;
    function CreateItem: TmnHeaderItem; virtual;
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
    function Find(AName: String): TmnHeaderItem; inline;
		function Have(AName: String): Boolean; inline;
    function IsExists: Boolean;
    function AddItem(AName, AValue: string): TmnHeaderItem; virtual;
    function Change(AName, AValue: String): TmnHeaderItem;

    //class operator Implicit(n: TmnHeaderItem): string;

    property Delimiter: Char read FDelimiter;
    property ValueSeparator: Char read FValueSeparator;

    property Text: string read GetText write SetText;
    property Value: string read GetValue;

    property Items: TmnHeaderList read GetItems;
    property Values[const vName: string]: string read GetValues write SetValues;
		property Item[Index: string]: TmnHeaderItem read GetItemByName; default;
    property ItemByIndex[Index: Integer]: TmnHeaderItem read GetItemByIndex; //{$ifndef FPC} default; {$endif Delphi}
  end;

  TmnCustomHeaderItemClass = class of TmnHeaderItem;

  { TmnHeaderListItem }

  TmnHeaderListItem = class(TmnHeaderItem)
  protected
    function CreateItem: TmnHeaderItem; override;
    procedure Parse(AValue: string); override;
  public
    constructor Create; override;
  end;

  { TmnHeaderList }

  TmnHeaderList = class(TmnNamedObjectList<TmnHeaderItem>)
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
    constructor Create; override;
    destructor Destroy; override;
    procedure RegisterDictionary(const AName: string; AItemClass: TmnCustomHeaderItemClass; AOption: TmnHeaderItemOptions);
    procedure CreateSubItem(const AName, AValue: string; out AItem: TmnHeaderItem);
    function Add(const S: string; TrimIt: Boolean = True): TmnHeaderItem; overload;
    function Add(const AName, AValue: string): TmnHeaderItem; overload;
    function AddItem(AName, AValue: string): TmnHeaderItem; override;
    procedure ReadHeader(Stream: TmnBufferStream);
    procedure WriteHeader(Stream: TmnBufferStream);
  end;

  { THeader_ContentType }

  THeader_ContentType = class(TmnHeaderListItem)
  public
    procedure Created; override;
    function CreateItem: TmnHeaderItem; override;
  end;

  { THeader_AcceptEncoding }

  THeader_AcceptEncoding = class(TmnHeaderListItem)
  public
    procedure Created; override;
    function CreateItem: TmnHeaderItem; override;
  end;

  { TmnWebHeader }

  TmnWebHeader = class(TmnHeader)
  public
    constructor Create; override;
  end;

implementation

procedure ParseHeadersCallback(Sender: Pointer; Index: Integer; AName, AValue: string; IsSwitch: Boolean; var Resume: Boolean);
begin
  //if (Index = 0) and (AName = '') then
  if (AName = '') then
    TmnHeaderItem(Sender).AddItem(Trim(TmnHeaderItem(Sender).Name), Trim(AValue))
  else
    TmnHeaderItem(Sender).AddItem(Trim(AName), Trim(AValue));
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

procedure TmnHeader.CreateSubItem(const AName, AValue: string; out AItem: TmnHeaderItem);
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

function TmnHeader.Add(const S: string; TrimIt: Boolean): TmnHeaderItem;
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

function TmnHeader.Add(const AName, AValue: string): TmnHeaderItem;
begin
  CreateSubItem(AName, AValue, Result);
  FItems.Add(Result);
end;

function TmnHeader.AddItem(AName, AValue: string): TmnHeaderItem;
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
  f: TmnHeaderItem;
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

function THeader_ContentType.CreateItem: TmnHeaderItem;
begin
  Result := TmnHeaderItem.Create;
  Result.FDelimiter := ',';
  Result.FValueSeparator := #0;
end;

{ THeader_AcceptEncoding }

procedure THeader_AcceptEncoding.Created;
begin
  inherited;
  FDelimiter := ',';
  FValueSeparator := #0;
end;

function THeader_AcceptEncoding.CreateItem: TmnHeaderItem;
begin
  Result := TmnHeaderListItem.Create;
  Result.FDelimiter := ';';
  Result.FValueSeparator := '=';
end;

{ TmnWebHeader }

constructor TmnWebHeader.Create;
begin
  inherited;
  RegisterDictionary('Content-Type', THeader_ContentType, []);
  RegisterDictionary('Accept-Encoding', THeader_AcceptEncoding, []);
end;

{ TmnHeaderItem }

function TmnHeaderItem.GetItems: TmnHeaderList;
begin
  Result := FItems;
end;

function TmnHeaderItem.GetText: string;
begin
  if Self = nil then
    Result := ''
  else
    Result := FText;
end;

function TmnHeaderItem.GetValue: string;
begin
  if Self = nil then
    Result := ''
  else
  begin
    if FItems <> nil then
    begin
      Result := Item[Name].Value;
    end
    else
      Result := FText;
  end;
end;

function TmnHeaderItem.GetValues(const vName: string): string;
begin
  Result := Find(vName).Value;
end;

procedure TmnHeaderItem.SetText(AValue: string);
begin
  if FText =AValue then
	  Exit;
  FText := AValue;
  Parse(AValue);
end;

procedure TmnHeaderItem.SetNameValue(const AName, AValue: string);
begin
  Name := AName;
  Text := AValue;
end;

procedure TmnHeaderItem.SetValues(const AName: string; AValue: string);
var
  AItem: TmnHeaderItem;
begin
  AItem := Find(AName);
  if (AItem = nil) then
  begin
	  if (AValue <> '') or (hoEmptyValues in Options) then
      AddItem(AName, AValue)
  end
  else
  begin
    if (AValue = '') and not (hoEmptyValues in Options) then
      Items.Remove(AItem)
    else
      AItem.SetNameValue(AName, AValue)
  end;
end;

procedure TmnHeaderItem.RequireItems;
begin
  FItems := TmnHeaderList.Create(0);
end;

function TmnHeaderItem.GetItemByName(Index: string): TmnHeaderItem;
begin
  if Index = '' then
    Result := Find(Name)
  else
    Result := Find(Index);
end;

function TmnHeaderItem.GetItemByIndex(Index: Integer): TmnHeaderItem;
begin
  Result := Items[Index];
end;

function TmnHeaderItem.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Value, 0);
end;

procedure TmnHeaderItem.Parse(AValue: String);
begin
  FText := AValue;
end;

constructor TmnHeaderItem.Create;
begin
  inherited Create;
  FDelimiter := ';';
  FValueSeparator := '=';
end;

destructor TmnHeaderItem.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TmnHeaderItem.CreateItem: TmnHeaderItem;
begin
  Result := nil;
end;

function TmnHeaderItem.GetNameValue(ADelimiter: string): String;
begin
  if ADelimiter = '' then
    ADelimiter := Delimiter;
  Result := Name + ADelimiter + Value;
end;

function TmnHeaderItem.Collect: string;
begin

end;

procedure TmnHeaderItem.Clear;
begin
  if FItems <> nil then
    FItems.Clear;
end;

function TmnHeaderItem.Find(AName: String): TmnHeaderItem;
begin
  if (Self <> nil) and (FItems <> nil) then
    Result := Items.Find(AName)
  else
    Result := nil
end;

function TmnHeaderItem.Have(AName: String): Boolean;
begin
  Result := Find(AName) <> nil;
end;

function TmnHeaderItem.IsExists: Boolean;
begin
  Result := Self <> nil;
end;

function TmnHeaderItem.AddItem(AName, AValue: string): TmnHeaderItem;
begin
  Result := CreateItem;
  Result.Name := AName;
  Result.Text := AValue;
  Items.Add(Result);
end;

function TmnHeaderItem.Change(AName, AValue: String): TmnHeaderItem;
begin
  Result := Find(AName);
  if Result = nil then
    AddItem(AName, AValue)
  else
    Result.Text := AValue;
end;

{ TmnHeaderItem }

function TmnHeaderItem.GetCount: Integer;
begin
  if (Self<> nil) and (FItems <> nil) then
    Result := FItems.Count
  else
    Result := 0;
end;

function TmnHeaderItem.GetEnumerator: TmnHeaderItemsEnumerator;
begin
  Result := TmnHeaderItemsEnumerator.Create(Self);
end;

{ TmnHeaderItem.TmnHeaderItemsEnumerator }

constructor TmnHeaderItem.TmnHeaderItemsEnumerator.Create(AList: TmnHeaderItem);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TmnHeaderItem.TmnHeaderItemsEnumerator.GetCurrent: TmnHeaderItem;
begin
  Result := FList.Items[FIndex];
end;

function TmnHeaderItem.TmnHeaderItemsEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TmnHeaderListItem }

function TmnHeaderListItem.CreateItem: TmnHeaderItem;
begin
  Result := TmnHeaderItem.Create;
end;

procedure TmnHeaderListItem.Parse(AValue: string);
var
  aValueSeparators: TSysCharSet;
  aDelimiters: TSysCharSet;
  aOptions: TParseArgumentsOptions;
begin
  FItems.Clear;
  if Delimiter = #0 then
    aDelimiters := []
  else
    aDelimiters := [Delimiter];

  if ValueSeparator = #0 then
    aValueSeparators := []
  else
    aValueSeparators := [ValueSeparator];

  aOptions := [pargDeqoute];
  if not (hoList in Options) then
    aOptions := aOptions + [pargValues];

  ParseArgumentsCallback(AValue, @ParseHeadersCallback, Self, [], aOptions, aDelimiters, [' ', #9], ['"', ''''], aValueSeparators);
end;

constructor TmnHeaderListItem.Create;
begin
  inherited;
  RequireItems;
end;


end.
