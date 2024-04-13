unit mnClasses;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey
 *}

{$IFDEF FPC}
{$MODE delphi}
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
{$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
{$M+}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils, Types,
  Generics.Collections, Contnrs;

type

  {$IFDEF FPC}
  TProc = Reference to procedure;
  TProc<T> = reference to procedure (Arg1: T);
  TProc<T1,T2> = reference to procedure (Arg1: T1; Arg2: T2);
  TProc<T1,T2,T3> = reference to procedure (Arg1: T1; Arg2: T2; Arg3: T3);
  TProc<T1,T2,T3,T4> = reference to procedure (Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4);
  {$endif}

  { TmnObject }

  TmnObject = class(TObject)
  protected
    procedure Created; virtual;
  public
    procedure AfterConstruction; override;
  end;

  { TmnObjectList }

  //USAGE Delphi: TMyObjectList = class(TmnObjectList<TMyObject>)
  //USAGE FPC: TMyObjectList = class(specialize TmnObjectList<TMyObject>)

  {$ifdef FPC}
  TmnObjectList<_Object_> = class(Contnrs.TObjectList)
  {$else}
  TmnObjectList<_Object_: class> = class(TObjectList<_Object_>)
  {$endif}
  private
    function GetItem(Index: Integer): _Object_;
    procedure SetItem(Index: Integer; AObject: _Object_);
  protected
    type

      { TmnObjectListEnumerator }

      TmnObjectListEnumerator = class(TObject)
      private
        FList: TmnObjectList<_Object_>;
        FIndex: Integer;
      public
        constructor Create(AList: TmnObjectList<_Object_>);
        function GetCurrent: _Object_;
        function MoveNext: Boolean;
        property Current: _Object_ read GetCurrent;
      end;

    function _AddRef: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    function _Release: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};

    {$ifdef FPC}
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    {$else}
    procedure Notify(const Value: _Object_; Action: TCollectionNotification); override;
    {$endif}
    //override this function of u want to check the item or create it before returning it
    {$H-}procedure Removing(Item: _Object_); virtual;{$H+}
    {$H-}procedure Added(Item: _Object_); virtual;{$H+}

    procedure Created; virtual;
    function RequireItem: _Object_; virtual;
  public
    function GetEnumerator: TmnObjectListEnumerator; inline;
    function QueryInterface({$ifdef FPC}constref{$else}const{$endif} iid : TGuid; out Obj):HResult; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    procedure AfterConstruction; override;
    function Add(Item: _Object_): Integer;
    procedure Insert(Index: Integer; Item: _Object_);
    function Extract(Item: _Object_): _Object_;
    function IndexOfObject(Item: TObject): Integer;
    {$ifdef FPC} //not now
    function Require(Index: Integer): _Object_;
    {$endif}
    function Peek(Index: Integer): _Object_;

    property Items[Index: Integer]: _Object_ read GetItem write SetItem; default;
    function Last: _Object_;
    function First: _Object_;
    //procedure Clear; {$ifdef FPC} override; {$else} virtual; {TODO talk with belal} {$endif}
  end;

    { TmnNamedObjectList }

    TmnNamedObject = class(TmnObject)
    private
      FName: string;
    protected
      procedure SetName(const Value: string); virtual;
    public
      property Name: string read FName write SetName;
    end;

    //USAGE: TMyNamedObjectList = class(TmnNamedObjectList<TMyNamedObject>)

    {$ifdef FPC}
    TmnNamedObjectList<_Object_> = class(TmnObjectList<_Object_>)
    {$else}
    TmnNamedObjectList<_Object_: TmnNamedObject> = class(TmnObjectList<_Object_>)
    {$endif}
    private
      FDicSize: Integer;
      FDic: TDictionary<string, _Object_>;

    protected
      procedure Created; override;
      {$ifdef FPC}
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
      {$else}
      procedure Notify(const Value: _Object_; Action: TCollectionNotification); override;
      {$endif}
    public
      //Set DicSize to 0 for not creating hashing list
      //if you use hashing list, you need to assign name to object on creation not after adding it to the list
      constructor Create(ADicSize: Integer = 1024; FreeObjects : boolean = True); overload;
      destructor Destroy; override;
      procedure AfterConstruction; override;
      function Find(const Name: string): _Object_;
      function IndexOfName(vName: string): Integer;
      {$ifdef FPC} //not now
      procedure Clear; override;
      {$endif}
      property Item[const Index: string]: _Object_ read Find;
    end;

    { TmnNameValueObjectList }

    TmnNameValueObject = class(TmnNamedObject)
    private
      FValue: string;
    public
      constructor Create(const vName: string; const AValue: string = ''); virtual; //must be virtual for generic function
      property Value: string read FValue write FValue;
    end;

    //USAGE: TMyNameValueObjectList = class(TmnNameValueObjectList<TMyNameValueObject>)

    {$ifdef FPC}
    TmnNameValueObjectList<_Object_> = class(TmnNamedObjectList<_Object_>)
    {$else}
    TmnNameValueObjectList<_Object_: TmnNameValueObject> = class(TmnNamedObjectList<_Object_>)
    {$endif}
    private
      FAutoRemove: Boolean;
      function GetValues(Index: string): string;
      procedure SetValues(Index: string; AValue: string);
    public
      function Add(Name, Value: string): _Object_; overload;
      property Values[Index: string]: string read GetValues write SetValues; default;
      property AutoRemove: Boolean read FAutoRemove write FAutoRemove;
    end;

implementation

function TmnObjectList<_Object_>.GetItem(Index: Integer): _Object_;
begin
  Result := _Object_(inherited Items[Index]);
end;

procedure TmnObjectList<_Object_>.SetItem(Index: Integer; AObject: _Object_);
begin
  inherited Items[Index] := AObject;
end;

function TmnObjectList<_Object_>.Last: _Object_;
begin
  if Count<>0 then
    Result := _Object_(inherited Last)
  else
    Result := nil;
end;

{$ifdef FPC}
procedure TmnObjectList<_Object_>.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action in [lnExtracted, lnDeleted]) then
    Removing(_Object_(Ptr));
  inherited;
  if (Action = lnAdded) then
    Added(_Object_(Ptr));
end;
{$else}
procedure TmnObjectList<_Object_>.Notify(const Value: _Object_; Action: TCollectionNotification);
begin
  if (Action in [cnExtracted, cnRemoved]) then
    Removing(Value);
  inherited;
  if (Action = cnAdded) then
    Added(Value);
end;
{$endif}

{$ifdef FPC} //not now
function TmnObjectList<_Object_>.Require(Index: Integer): _Object_;
begin
  if (Index < Count) then
    Result := Items[Index]
  else
  begin
    Count := Index + 1;
    Result := nil;
  end;

  if Result = nil then
  begin
    Result := RequireItem;
    Put(Index, Result);
  end;
end;
{$endif}

function TmnObjectList<_Object_>.Peek(Index: Integer): _Object_;
begin
  if (Index < Count) then
    Result := Items[Index]
  else
    Result := nil;
end;

function TmnObjectList<_Object_>._AddRef: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
begin
  Result := 0;
end;

function TmnObjectList<_Object_>._Release: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
begin
  Result := 0;
end;

procedure TmnObjectList<_Object_>.Removing(Item: _Object_);
begin

end;

function TmnObjectList<_Object_>.QueryInterface({$ifdef FPC}constref{$else}const{$endif} iid : TGuid; out Obj): HResult; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TmnObjectList<_Object_>.Added(Item: _Object_);
begin
end;

function TmnObjectList<_Object_>.Add(Item: _Object_): Integer;
begin
  Result := inherited Add(Item);
end;

function TmnObjectList<_Object_>.IndexOfObject(Item: TObject): Integer;
begin
  if Item is TClass(_Object_) then
    Result := IndexOf(_Object_(Item))
  else
    Result := -1;
end;

procedure TmnObjectList<_Object_>.Insert(Index: Integer; Item: _Object_);
begin
  inherited Insert(Index, Item);
end;

function TmnObjectList<_Object_>.Extract(Item: _Object_): _Object_;
begin
  Result := _Object_(inherited Extract(Item));
end;

function TmnObjectList<_Object_>.First: _Object_;
begin
  if Count<>0 then
    Result := _Object_(inherited First)
  else
    Result := nil;
end;

procedure TmnObjectList<_Object_>.Created;
begin
end;

{procedure TmnObjectList<_Object_>.Clear;
begin
  inherited Create;
end;}

function TmnObjectList<_Object_>.RequireItem: _Object_;
begin
  Result := nil;
end;

function TmnObjectList<_Object_>.GetEnumerator: TmnObjectListEnumerator;
begin
  Result := TmnObjectListEnumerator.Create(Self);
end;

procedure TmnObjectList<_Object_>.AfterConstruction;
begin
  inherited;
  Created;
end;

{ TmnNamedObjectList }

procedure TmnNamedObjectList<_Object_>.Created;
begin
  inherited;
end;

procedure TmnNamedObjectList<_Object_>.AfterConstruction;
begin
  inherited;
  if FDicSize > 0 then
    FDic := TDictionary<string, _Object_>.Create(FDicSize);
end;

destructor TmnNamedObjectList<_Object_>.Destroy;
begin
  FreeAndNil(FDic);
  inherited;
end;

{$ifdef FPC} //not now
procedure TmnNamedObjectList<_Object_>.Clear;
begin
  inherited;
  if FDic <> nil then //because there is a clear in Destroy
    FDic.Clear;
end;
{$endif}

function  TmnNamedObjectList<_Object_>.Find(const Name: string): _Object_;
var
  i: integer;
begin
  if FDic <> nil then
    FDic.TryGetValue(Name.ToLower, Result)
  else
  begin
    Result := nil;
		if Name <> '' then
      for i := 0 to Count - 1 do
      begin
        if SameText(Items[i].Name, Name) then
        begin
          Result := Items[i];
          break;
        end;
      end;
  end;
end;

function TmnNamedObjectList<_Object_>.IndexOfName(vName: string): Integer;
var
  t: _Object_;
begin
  t := Find(vName);
  if t<>nil then
    Result := IndexOf(t)
  else
    Result := -1;
end;

{
var
  i: integer;
begin

  Result := -1;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := i;
        break;
      end;
    end;
end;
}

{$ifdef FPC}
procedure TmnNamedObjectList<_Object_>.Notify(Ptr: Pointer; Action: TListNotification);
{$else}
procedure TmnNamedObjectList<_Object_>.Notify(const Value: _Object_; Action: TCollectionNotification);
{$endif}
begin
  if FDic <> nil then
  begin
    {$ifdef FPC}
    if Action in [lnExtracted, lnDeleted] then //Need it in FPC https://forum.lazarus.freepascal.org/index.php/topic,60984.0.html
      FDic.Remove(_Object_(Ptr).Name.ToLower);//bug in fpc
    {$else}
    if Action in [cnExtracting, cnDeleting] then
      FDic.Remove(Value.Name.ToLower);
    {$endif}
    inherited;
    {$ifdef FPC}
    if Action = lnAdded then
      FDic.AddOrSetValue(_Object_(Ptr).Name.ToLower, Ptr);
    {$else}
    if Action = cnAdded then
      FDic.AddOrSetValue(Value.Name.ToLower, Value);
    {$endif}
  end
  else
    inherited;
end;

constructor TmnNamedObjectList<_Object_>.Create(ADicSize: Integer; FreeObjects: boolean);
begin
  inherited Create(FreeObjects);
  FDicSize := ADicSize;
end;

{ TmnObjectList.TmnObjectListEnumerator }

constructor TmnObjectList<_Object_>.TmnObjectListEnumerator.Create(AList: TmnObjectList<_Object_>);
begin
  inherited Create;
  FList := Alist;
  FIndex := -1;
end;

function TmnObjectList<_Object_>.TmnObjectListEnumerator.GetCurrent: _Object_;
begin
  Result := FList[FIndex];
end;

function TmnObjectList<_Object_>.TmnObjectListEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TmnNameValueObjectList }

function TmnNameValueObjectList<_Object_>.GetValues(Index: string): string;
var
  itm: _Object_;
begin
  itm := Find(Index);
  if itm <> nil then
    Result := itm.Value
  else
    Result := '';
end;

procedure TmnNameValueObjectList<_Object_>.SetValues(Index: string; AValue: string);
var
  itm : _Object_;
begin
  itm := Find(Index);
  if AutoRemove and (AValue = '') then
  begin
    if (itm <> nil) then
      Remove(itm);
  end
  else
  begin
    if itm <> nil then
      itm.Value := AValue
    else
      Add(Index, AValue);
  end;
end;

function TmnNameValueObjectList<_Object_>.Add(Name, Value: string): _Object_;
begin
  Result := _Object_.Create(Name, Value);
  Add(Result);
end;

{ TmnObject }

procedure TmnObject.Created;
begin
end;

procedure TmnObject.AfterConstruction;
begin
  inherited AfterConstruction;
  Created;
end;

{ TmnNameValueObject }

constructor TmnNameValueObject.Create(const vName, AValue: string);
begin
  inherited Create;
  Name := vName;
  Value := AValue;
end;

{ TmnNamedObject }

procedure TmnNamedObject.SetName(const Value: string);
begin
  FName := Value;
end;

end.


