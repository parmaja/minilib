unit mnrLists;

{$IFDEF FPC}
{$MODE delphi}
{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes;

type
  TmnrNode = class;
  TmnrNodes = class;
  TmnrLinkNode = class;
  TmnrLinkNodes = class;
  TmnrRowNode = class;
  TmnrRowNodes = class;

  TmnrNode = class(TInterfacedPersistent)
  private
    FNodes: TmnrNode;
    FNext: TmnrNode;
    FPrior: TmnrNode;
    FFirst: TmnrNode;
    FLast: TmnrNode;
  protected
    procedure SetNodes(const Value: TmnrNode);
    function GetNodes: TmnrNode;
    procedure Link(vNode: TmnrNode); virtual;
    procedure UnLink(vNode: TmnrNode); virtual;

    procedure Attach; virtual;
    procedure Detach; virtual;
    function GetIndex: Integer; virtual;

    function GetCount: Integer; virtual;

    function GetHead: TmnrNode;
    function GetFirst: TmnrNode;
    function GetLast: TmnrNode;
    function GetNext: TmnrNode;
    function GetPrior: TmnrNode;

    procedure SetFirst(const Value: TmnrNode);
    procedure SetLast(const Value: TmnrNode);
    procedure SetNext(const Value: TmnrNode);
    procedure SetPrior(const Value: TmnrNode);

    function DoGetHead: TmnrNode; virtual;
  public
    constructor Create(vNodes: TmnrNode);
    destructor Destroy; override;
    property Nodes: TmnrNode read GetNodes write SetNodes;

    property Next: TmnrNode read GetNext write SetNext;
    property Prior: TmnrNode read GetPrior write SetPrior;
    property First: TmnrNode read GetFirst write SetFirst;
    property Last: TmnrNode read GetLast write SetLast;
    property Head: TmnrNode read GetHead;
    property Count: Integer read GetCount;
    procedure IncCount(Value: Integer); virtual;
    procedure DecCount(Value: Integer); virtual;
    property Index: Integer read GetIndex;

    procedure MoveAfter(vNode: TmnrNode);
    procedure MoveBefore(vNode: TmnrNode);

    procedure MoveUp;
    procedure MoveDown;
    procedure MoveFirst;
    procedure MoveLast;
  end;

  TmnrNodes = class(TmnrNode)
  public
    constructor Create(vNodes: TmnrNode); overload;
    constructor Create; overload;
    destructor Destroy; override;
    procedure Clear; virtual;
  end;

  TmnrIndex = class
  private
    FNodes: TmnrNode;
  protected
    function GetNodes: TmnrNode;
    procedure Compute; virtual;
    function GetCount: Integer; virtual;
    function GetItems(Index: Integer): TmnrNode;
  public
    constructor Create(vNodes: TmnrNode);
    destructor Destroy; override;
    property Nodes: TmnrNode read GetNodes;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TmnrNode read GetItems; default;
  end;

  TmnrLinkNode = class(TmnrNode)
  private
  protected
    procedure SetNodes(const Value: TmnrLinkNodes);
    function GetNodes: TmnrLinkNodes;
  public
    property Nodes: TmnrLinkNodes read GetNodes write SetNodes;
  end;

  TmnrValueNode = class(TmnrLinkNode)
  protected
    function GetAsBoolean: Boolean; virtual; abstract;
    function GetAsCurrency: Currency; virtual; abstract;
    function GetAsDateTime: TDateTime; virtual; abstract;
    function GetAsDouble: Double; virtual; abstract;
    function GetAsInteger: Longint; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    function GetAsVariant: Variant; virtual; abstract;
    function GetAsData: Integer; virtual; abstract;

    procedure SetAsBoolean(const Value: Boolean); virtual; abstract;
    procedure SetAsCurrency(const Value: Currency); virtual; abstract;
    procedure SetAsDateTime(const Value: TDateTime); virtual; abstract;
    procedure SetAsDouble(const Value: Double); virtual; abstract;
    procedure SetAsInteger(const Value: Longint); virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
    procedure SetAsVariant(const Value: Variant); virtual; abstract;
    procedure SetAsData(const Value: Integer); virtual; abstract;
  public

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsData: Integer read GetAsData write SetAsData;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  TmnrLinkNodes = class(TmnrNodes)
  private
    FCount: Integer;
  protected
    function GetByIndex(vIndex: Integer): TmnrNode;
    function GetCount: Integer; override;
  public
    function Add: TmnrLinkNode;
    procedure IncCount(Value: Integer); override;
    procedure DecCount(Value: Integer); override;
    procedure Clear; override;
    property ByIndex[vIndex: Integer]: TmnrNode read GetByIndex;

  end;

  TmnrNodeArray = array of TmnrNode;

  TmnrLinkNodesListIndex = class(TmnrIndex)
  protected
    FArray: TmnrNodeArray;
    procedure Compute; override;
    function GetCount: Integer; override;
    function GetItems(Index: Integer): TmnrNode;
    function GetNodes: TmnrLinkNodes;
  public
    constructor Create(vNodes: TmnrLinkNodes);
    destructor Destroy; override;
    property Nodes: TmnrLinkNodes read GetNodes;
    property Items[Index: Integer]: TmnrNode read GetItems; default; 
  end;

  TmnrRowNode = class(TmnrLinkNodes)
  private
  protected
    FID: Integer;
    procedure SetNodes(const Value: TmnrRowNodes);
    function GetNodes: TmnrRowNodes;
    function GetNext: TmnrRowNode;
    function GetPrior: TmnrRowNode;

    procedure Attach; override;
    procedure Detach; override;

  public
    constructor Create(vNodes: TmnrNode);
    destructor Destroy; override;
    property ID: Integer read FID;

    property Next: TmnrRowNode read GetNext;
    property Prior: TmnrRowNode read GetPrior;
    property Nodes: TmnrRowNodes read GetNodes write SetNodes; //parent nodes of row
  end;

  TmnrRowNodes = class(TmnrLinkNodes)
  protected
    function GetFirst: TmnrRowNode;
    function GetLast: TmnrRowNode;
    procedure SetFirst(const Value: TmnrRowNode);
    procedure SetLast(const Value: TmnrRowNode);
    procedure Link(vNode: TmnrNode); override;
    procedure UnLink(vNode: TmnrNode); override;

  public
    function Add: TmnrRowNode;
    property First: TmnrRowNode read GetFirst write SetFirst;
    property Last: TmnrRowNode read GetLast write SetLast;
  end;


function CurrentNodesCount: Cardinal;

implementation


var
  FNodesCount: Cardinal = 0;

function CurrentNodesCount: Cardinal;
begin
  Result := FNodesCount;
end;


{ TmnrNode }

procedure TmnrNode.Attach;
begin
  Nodes.Link(Self);

  {if Nodes<>nil then
  begin
    if Nodes.Last=nil then
    begin
      Nodes.First := Self;
      Nodes.Last := Self;
    end
    else
    begin
      Prior := Nodes.Last;
      Prior.Next := Self;
      Nodes.Last := Self;
    end;
    Nodes.IncCount(Count);
  end;}
end;

constructor TmnrNode.Create(vNodes: TmnrNode);
begin
  inherited Create;
  FFirst := nil;
  FLast := nil;
  FPrior := nil;
  FNext := nil;
  Nodes := vNodes;
  {$IFOPT D+}
  Inc(FNodesCount);
  {$ENDIF}
end;

procedure TmnrNode.DecCount;
begin

end;

destructor TmnrNode.Destroy;
begin
  if FNodes<>nil then Detach;
  {$IFOPT D+}
  Dec(FNodesCount);
  {$ENDIF}
  inherited;
end;

procedure TmnrNode.Detach;
begin
  Nodes.UnLink(Self);
  FNodes := nil;
  FNext := nil;
  FPrior := nil;
end;

function TmnrNode.DoGetHead: TmnrNode;
begin
  Result := First;
end;

function TmnrNode.GetCount: Integer;
begin
  Result := 1;
end;

function TmnrNode.GetFirst: TmnrNode;
begin
  Result := FFirst;
end;

function TmnrNode.GetHead: TmnrNode;
begin
  Result := DoGetHead;
end;

function TmnrNode.GetIndex: Integer;
var
  p: TmnrNode;
  i: Integer;
begin
  Result := -1;
  if Nodes<>nil then
  begin
    p := FNodes.First;
    i := 0;
    while p<>nil do
    begin
      if p=Self then
      begin
        Result := i;
        Break
      end
      else
      begin
        Inc(i);
        p := p.Next;
      end;
    end;
  end;
end;

function TmnrNode.GetLast: TmnrNode;
begin
  Result := FLast;
end;

function TmnrNode.GetNext: TmnrNode;
begin
  Result := FNext;
end;

function TmnrNode.GetNodes: TmnrNode;
begin
  Result := FNodes;
end;

function TmnrNode.GetPrior: TmnrNode;
begin
  Result := FPrior;
end;

procedure TmnrNode.IncCount;
begin

end;

procedure TmnrNode.Link(vNode: TmnrNode);
begin
  if Last=nil then
  begin
    FFirst := vNode;
    FLast := vNode;
  end
  else
  begin
    vNode.FPrior := FLast;
    Last.FNext := vNode;
    FLast := vNode;
  end;
  IncCount(vNode.Count);
end;

procedure TmnrNode.MoveAfter(vNode: TmnrNode);
begin
  if vNode.Nodes=Nodes then
  begin
    if Nodes.First=Self then Nodes.First := Next;
    if Nodes.Last=Self then Nodes.Last := Prior;
    if Prior<>nil then Prior.Next := Next;
    if Next<>nil then Next.Prior := Prior;

    if vNode.Next<>nil then vNode.Next.Prior := Self;
    Next := vNode.Next;
    Prior := vNode;
    vNode.Next := Self;

    if Nodes.Last=vNode then Nodes.Last := Self;
  end;
end;

procedure TmnrNode.MoveBefore(vNode: TmnrNode);
begin
  if vNode.Nodes=Nodes then
  begin
    if Nodes.First=Self then Nodes.First := Next;
    if Nodes.Last=Self then Nodes.Last := Prior;
    if Prior<>nil then Prior.Next := Next;
    if Next<>nil then Next.Prior := Prior;

    if vNode.Prior<>nil then vNode.Prior.Next := Self;
    Prior := vNode.Prior;
    Next := vNode;
    vNode.Prior := Self;
    if Nodes.First=vNode then Nodes.First := Self;
  end;
end;

procedure TmnrNode.MoveUp;
begin
  if Prior<>nil then MoveBefore(Prior);
end;

procedure TmnrNode.MoveDown;
begin
  if Next<>nil then MoveAfter(Next);
end;

procedure TmnrNode.MoveFirst;
begin
  if (Nodes<>nil) and (Nodes.First<>Self) then MoveBefore(Nodes.First);
end;

procedure TmnrNode.MoveLast;
begin
  if (Nodes<>nil) and (Nodes.Last<>Self) then MoveAfter(Nodes.Last);
end;


procedure TmnrNode.SetFirst(const Value: TmnrNode);
begin
  FFirst := Value;
end;

procedure TmnrNode.SetLast(const Value: TmnrNode);
begin
  FLast := Value;
end;

procedure TmnrNode.SetNext(const Value: TmnrNode);
begin
  FNext := Value;
end;

procedure TmnrNode.SetNodes(const Value: TmnrNode);
begin
  if FNodes<>Value then
  begin
    if FNodes<>nil then Detach;
    FNodes := Value;
    if FNodes<>nil then Attach;
  end;
end;

procedure TmnrNode.SetPrior(const Value: TmnrNode);
begin
  FPrior := Value;
end;

procedure TmnrNode.UnLink(vNode: TmnrNode);
begin
  if First=vNode then
  begin
    First := vNode.Next;
    if First<>nil then First.Prior := nil;
  end;

  if Last=vNode then
  begin
    Last := vNode.Prior;
    if Last<>nil then Last.Next := nil;
  end;
  if vNode.Prior<>nil then vNode.Prior.Next := vNode.Next;
  if vNode.Next<>nil then vNode.Next.Prior := vNode.Prior;
  DecCount(vNode.Count);
end;

{ TmnrNodes }

procedure TmnrNodes.Clear;
var
  h, n: TmnrNode;
begin
  {h := Head;
  while h<>nil do
  begin
    //
    h.Free;
    h := Head;
  end;}

  h := First;
  while h<>nil do
  begin
    n := h;
    h := h.Next;
    n.FNodes := nil;
    n.Free;
  end;
  First := nil;
  Last := nil;
end;

constructor TmnrNodes.Create(vNodes: TmnrNode);
begin
  inherited Create(vNodes);
end;

constructor TmnrNodes.Create;
begin
  Create(nil);
end;

destructor TmnrNodes.Destroy;
begin
  Clear;
  inherited;
end;

{ TmnrLinkNode }

function TmnrLinkNode.GetNodes: TmnrLinkNodes;
begin
  Result := TmnrLinkNodes(inherited GetNodes);
end;

procedure TmnrLinkNode.SetNodes(const Value: TmnrLinkNodes);
begin
  inherited SetNodes(Value);
end;

{ TmnrLinkNodes }

function TmnrLinkNodes.Add: TmnrLinkNode;
begin
  Result := TmnrLinkNode.Create(Self);
end;

procedure TmnrLinkNodes.Clear;
begin
  inherited;
  FCount := 0;
end;

procedure TmnrLinkNodes.DecCount(Value: Integer);
begin
  Dec(FCount, Value);
end;

function TmnrLinkNodes.GetByIndex(vIndex: Integer): TmnrNode;
var
  i: Integer;
begin
  Result := First;
  i := 0;
  while (Result<>nil) and (i<vIndex) do
  begin
    Result := Result.Next;
    Inc(i);
  end;
end;

function TmnrLinkNodes.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TmnrLinkNodes.IncCount(Value: Integer);
begin
  Inc(FCount, Value);
end;

{ TmnrRowNode }

procedure TmnrRowNode.Attach;
begin
  inherited;
  if Nodes<>nil then
  begin
    FID := Nodes.Count-1;
  end;
end;

constructor TmnrRowNode.Create(vNodes: TmnrNode);
begin
  inherited Create(vNodes);
end;

destructor TmnrRowNode.Destroy;
begin
  inherited;
end;

procedure TmnrRowNode.Detach;
begin
  inherited;

end;

function TmnrRowNode.GetNext: TmnrRowNode;
begin
  Result := TmnrRowNode(inherited GetNext);
end;

function TmnrRowNode.GetNodes: TmnrRowNodes;
begin
  Result := TmnrRowNodes(inherited GetNodes);
end;

function TmnrRowNode.GetPrior: TmnrRowNode;
begin
  Result := TmnrRowNode(inherited GetPrior);
end;

procedure TmnrRowNode.SetNodes(const Value: TmnrRowNodes);
begin
  inherited SetNodes(Value);
end;

{ TmnrRowNodes }

function TmnrRowNodes.Add: TmnrRowNode;
begin
  Result := TmnrRowNode.Create(Self);
end;

function TmnrRowNodes.GetFirst: TmnrRowNode;
begin
  Result := TmnrRowNode(inherited GetFirst);
end;

function TmnrRowNodes.GetLast: TmnrRowNode;
begin
  Result := TmnrRowNode(inherited GetLast);
end;

procedure TmnrRowNodes.Link(vNode: TmnrNode);
begin
  inherited;
  Inc(FCount, 1);
end;

procedure TmnrRowNodes.SetFirst(const Value: TmnrRowNode);
begin
  inherited SetFirst(Value);
end;

procedure TmnrRowNodes.SetLast(const Value: TmnrRowNode);
begin
  inherited SetLast(Value);
end;

procedure TmnrRowNodes.UnLink(vNode: TmnrNode);
begin
  inherited;
  Dec(FCount, 1);
end;

{ TmnrIndex }

procedure TmnrIndex.Compute;
begin

end;

constructor TmnrIndex.Create(vNodes: TmnrNode);
begin
  inherited Create;
  FNodes := vNodes; //vNodes <> nil
  Compute;
end;

destructor TmnrIndex.Destroy;
begin

  inherited;
end;

function TmnrIndex.GetCount: Integer;
begin
  Result := 0;
end;

function TmnrIndex.GetItems(Index: Integer): TmnrNode;
begin
  Result := nil;
end;

function TmnrIndex.GetNodes: TmnrNode;
begin
  Result := FNodes;
end;

{ TmnrLinkNodesListIndex }

procedure TmnrLinkNodesListIndex.Compute;
var
  p: TmnrNode;
  i: Integer;
begin
  if Nodes.Count>0 then
  begin
    SetLength(FArray, Nodes.Count);
    p := Nodes.First;
    i := 0;
    while p<>nil do
    begin
      FArray[i] := p;
      Inc(i);
      p := p.Next;
    end;
  end;
end;

constructor TmnrLinkNodesListIndex.Create(vNodes: TmnrLinkNodes);
begin
  inherited Create(vNodes);
end;

destructor TmnrLinkNodesListIndex.Destroy;
begin
  FArray := nil;
  inherited;
end;

function TmnrLinkNodesListIndex.GetCount: Integer;
begin
  Result := Nodes.Count;
end;

function TmnrLinkNodesListIndex.GetItems(Index: Integer): TmnrNode;
begin
  Result := FArray[Index];
end;

function TmnrLinkNodesListIndex.GetNodes: TmnrLinkNodes;
begin
  Result := TmnrLinkNodes(inherited GetNodes);
end;

initialization

finalization
  {$IFOPT D+}
    //if FNodesCount<>0 then
      //MessageBox(0, PChar('Remain '+IntToStr(FNodesCount)+' node.'), PChar('warning'), 0);
  {$ENDIF}


end.
