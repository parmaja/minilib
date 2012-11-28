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

  TmnrNode = class(TPersistent)
  private
    FNodes: TmnrNode;
  protected
    procedure SetNodes(const Value: TmnrNode);
    function GetNodes: TmnrNode;
    procedure Attach; virtual;
    procedure Detach; virtual; 

    function GetCount: Integer; virtual;

    function GetFirst: TmnrNode;
    function GetHead: TmnrNode;
    function GetLast: TmnrNode;
    function GetNext: TmnrNode;
    function GetPrior: TmnrNode;

    procedure SetFirst(const Value: TmnrNode);
    procedure SetLast(const Value: TmnrNode);
    procedure SetNext(const Value: TmnrNode);
    procedure SetPrior(const Value: TmnrNode);

    function DoGetHead: TmnrNode; virtual;
    function DoGetFirst: TmnrNode; virtual;
    function DoGetLast: TmnrNode; virtual;
    function DoGetNext: TmnrNode; virtual;
    function DoGetPrior: TmnrNode; virtual;
    procedure DoSetFirst(const Value: TmnrNode); virtual;
    procedure DoSetLast(const Value: TmnrNode); virtual;
    procedure DoSetNext(const Value: TmnrNode); virtual;
    procedure DoSetPrior(const Value: TmnrNode); virtual;
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
    procedure IncCount; virtual;
    procedure DecCount; virtual;
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
    FPrior: TmnrNode;
    FNext: TmnrNode;
  protected
    procedure SetNodes(const Value: TmnrLinkNodes);
    function GetNodes: TmnrLinkNodes;
    function DoGetNext: TmnrNode; override;
    function DoGetPrior: TmnrNode; override;
    procedure DoSetNext(const Value: TmnrNode); override;
    procedure DoSetPrior(const Value: TmnrNode); override;
  public
    property Nodes: TmnrLinkNodes read GetNodes write SetNodes;
  end;

  TmnrValueNode = class(TmnrLinkNode)
  protected
    function GetAsBoolean: Boolean; virtual; abstract;
    function GetAsCurrency: Currency; virtual; abstract;
    function GetAsDateTime: TDateTime; virtual; abstract;
    function GetAsFloat: Double; virtual; abstract;
    function GetAsInteger: Longint; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    function GetAsVariant: Variant; virtual; abstract;

    procedure SetAsBoolean(const Value: Boolean); virtual; abstract;
    procedure SetAsCurrency(const Value: Currency); virtual; abstract;
    procedure SetAsDateTime(const Value: TDateTime); virtual; abstract;
    procedure SetAsFloat(const Value: Double); virtual; abstract;
    procedure SetAsInteger(const Value: Longint); virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
    procedure SetAsVariant(const Value: Variant); virtual; abstract;
  public

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  TmnrLinkNodes = class(TmnrNodes)
  private
    FLast: TmnrNode;
    FFirst: TmnrNode;
    FCount: Integer;
  protected
    function GetByIndex(vIndex: Integer): TmnrNode;
    function GetCount: Integer; override;
    function DoGetFirst: TmnrNode; override;
    function DoGetLast: TmnrNode; override;
    procedure DoSetFirst(const Value: TmnrNode); override;
    procedure DoSetLast(const Value: TmnrNode); override;

  public
    function Add: TmnrLinkNode;
    procedure IncCount; override;
    procedure DecCount; override;
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
    FNext: TmnrNode;
    FPrior: TmnrNode;
  protected
    FID: Integer;
    procedure SetNodes(const Value: TmnrRowNodes);
    function GetNodes: TmnrRowNodes;
    function GetNext: TmnrRowNode;
    function GetPrior: TmnrRowNode;


    function DoGetNext: TmnrNode; override;
    function DoGetPrior: TmnrNode; override;
    procedure DoSetNext(const Value: TmnrNode); override;
    procedure DoSetPrior(const Value: TmnrNode); override;
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
  public
    function Add: TmnrRowNode;
    property First: TmnrRowNode read GetFirst;
    property Last: TmnrRowNode read GetLast;
  end;


implementation

{$IFOPT D+}
var
  FNodesCount: Cardinal = 0;
{$ENDIF}

{ TmnrNode }

procedure TmnrNode.Attach;
begin
  if Nodes<>nil then
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
    Nodes.IncCount;
  end;
end;

constructor TmnrNode.Create(vNodes: TmnrNode);
begin
  inherited Create;
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
  if Nodes<>nil then
  begin
    if Nodes.First=Self then Nodes.First := Next;
    if Nodes.First<>nil then Nodes.First.Prior := nil;

    if Nodes.Last=Self then Nodes.Last := Prior;
    if Nodes.Last<>nil then Nodes.Last.Next := nil;
    Nodes.DecCount;
    FNodes := nil;
  end;
  if Prior<>nil then Prior.Next := Next;
  if Next<>nil then Next.Prior := Prior;

  Next := nil;
  Prior := nil;
end;

function TmnrNode.DoGetFirst: TmnrNode;
begin
  Result := nil;
end;

function TmnrNode.DoGetHead: TmnrNode;
begin
  Result := First;
end;

function TmnrNode.DoGetLast: TmnrNode;
begin
  Result := nil;
end;

function TmnrNode.DoGetNext: TmnrNode;
begin
  Result := nil;
end;

function TmnrNode.DoGetPrior: TmnrNode;
begin
  Result := nil;
end;

procedure TmnrNode.DoSetFirst(const Value: TmnrNode);
begin
end;

procedure TmnrNode.DoSetLast(const Value: TmnrNode);
begin
end;

procedure TmnrNode.DoSetNext(const Value: TmnrNode);
begin
end;

procedure TmnrNode.DoSetPrior(const Value: TmnrNode);
begin
end;

function TmnrNode.GetCount: Integer;
begin
  Result := 0;
end;

function TmnrNode.GetFirst: TmnrNode;
begin
  Result := DoGetFirst;
end;

function TmnrNode.GetHead: TmnrNode;
begin
  Result := DoGetHead;
end;

function TmnrNode.GetLast: TmnrNode;
begin
  Result := DoGetLast;
end;

function TmnrNode.GetNext: TmnrNode;
begin
  Result := DoGetNext;
end;

function TmnrNode.GetNodes: TmnrNode;
begin
  Result := FNodes;
end;

function TmnrNode.GetPrior: TmnrNode;
begin
  Result := DoGetPrior;
end;

procedure TmnrNode.IncCount;
begin

end;

procedure TmnrNode.SetFirst(const Value: TmnrNode);
begin
  if GetFirst<>Value then DoSetFirst(Value);
end;

procedure TmnrNode.SetLast(const Value: TmnrNode);
begin
  if GetLast<>Value then DoSetLast(Value);
end;

procedure TmnrNode.SetNext(const Value: TmnrNode);
begin
  if GetNext<>Value then DoSetNext(Value);
end;

procedure TmnrNode.SetNodes(const Value: TmnrNode);
begin
  if FNodes<>Value then
  begin
    Detach;
    FNodes := Value;
    Attach;
  end;
end;

procedure TmnrNode.SetPrior(const Value: TmnrNode);
begin
  if GetPrior<>Value then DoSetPrior(Value);
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

function TmnrLinkNode.DoGetNext: TmnrNode;
begin
  Result := FNext;
end;

function TmnrLinkNode.GetNodes: TmnrLinkNodes;
begin
  Result := TmnrLinkNodes(inherited GetNodes);
end;

function TmnrLinkNode.DoGetPrior: TmnrNode;
begin
  Result := FPrior;
end;

procedure TmnrLinkNode.DoSetNext(const Value: TmnrNode);
begin
  FNext := Value;
end;

procedure TmnrLinkNode.DoSetPrior(const Value: TmnrNode);
begin
  FPrior := Value;
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

procedure TmnrLinkNodes.DecCount;
begin
  Dec(FCount);
end;

function TmnrLinkNodes.DoGetFirst: TmnrNode;
begin
  Result := FFirst;
end;

function TmnrLinkNodes.DoGetLast: TmnrNode;
begin
  Result := FLast;
end;

procedure TmnrLinkNodes.DoSetFirst(const Value: TmnrNode);
begin
  FFirst := Value;
end;

procedure TmnrLinkNodes.DoSetLast(const Value: TmnrNode);
begin
  FLast := Value;
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

procedure TmnrLinkNodes.IncCount;
begin
  Inc(FCount);
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

function TmnrRowNode.DoGetNext: TmnrNode;
begin
  Result := FNext;
end;

function TmnrRowNode.DoGetPrior: TmnrNode;
begin
  Result := FPrior;
end;

procedure TmnrRowNode.DoSetNext(const Value: TmnrNode);
begin
  FNext := Value;
end;

procedure TmnrRowNode.DoSetPrior(const Value: TmnrNode);
begin
  FPrior := Value;
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
