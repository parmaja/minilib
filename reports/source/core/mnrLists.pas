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
    FNodes: TmnrNodes;
  protected
    procedure SetNodes(const Value: TmnrNodes);
    function GetNodes: TmnrNodes;
    procedure Attach; virtual; abstract;
    procedure Detach; virtual; abstract;
  public
    constructor Create(vNodes: TmnrNodes); virtual;
    destructor Destroy; override;
    property Nodes: TmnrNodes read GetNodes write SetNodes;
  end;

  TmnrIndex = class
  private
    FNodes: TmnrNodes;
  protected
    function GetNodes: TmnrNodes;
    procedure Compute; virtual;
    function GetCount: Integer; virtual;
    function GetItems(Index: Integer): TmnrNode;
  public
    constructor Create(vNodes: TmnrNodes); 
    destructor Destroy; override;
    property Nodes: TmnrNodes read GetNodes;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TmnrNode read GetItems; default;
  end;

  TmnrNodes = class
  private
  protected
    function GetHead: TmnrNode; virtual;
    function GetCount: Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Head: TmnrNode read GetHead;
    procedure Clear; virtual;
    property Count: Integer read GetCount;
  end;

  TmnrLinkNode = class(TmnrNode)
  private
    FPrior: TmnrLinkNode;
    FNext: TmnrLinkNode;
  protected
    procedure SetNodes(const Value: TmnrLinkNodes);
    function GetNodes: TmnrLinkNodes;
    procedure Attach; override;
    procedure Detach; override;
    function GetNext: TmnrLinkNode;
    function GetPrior: TmnrLinkNode;
  public
    property Next: TmnrLinkNode read GetNext;
    property Prior: TmnrLinkNode read GetPrior;
    property Nodes: TmnrLinkNodes read GetNodes write SetNodes;
  end;

  TmnrLinkNodes = class(TmnrNodes)
  private
    FLast: TmnrLinkNode;
    FFirst: TmnrLinkNode;
    FCount: Integer;
    function GetByIndex(vIndex: Integer): TmnrLinkNode;
  protected
    function GetHead: TmnrNode; override;
    function GetFirst: TmnrLinkNode;
    function GetLast: TmnrLinkNode;
    function GetCount: Integer; override;

  public
    function Add: TmnrLinkNode;
    property First: TmnrLinkNode read GetFirst;
    property Last: TmnrLinkNode read GetLast;
    property ByIndex[vIndex: Integer]: TmnrLinkNode read GetByIndex;
  end;

  TmnrNodeArray = array of TmnrLinkNode;

  TmnrLinkNodesListIndex = class(TmnrIndex)
  protected
    FArray: TmnrNodeArray;
    procedure Compute; override;
    function GetCount: Integer; override;
    function GetItems(Index: Integer): TmnrLinkNode;
    function GetNodes: TmnrLinkNodes;
  public
    constructor Create(vNodes: TmnrLinkNodes);
    destructor Destroy; override;
    property Nodes: TmnrLinkNodes read GetNodes;
    property Items[Index: Integer]: TmnrLinkNode read GetItems; default; 
  end;

  TmnrRowCells = class(TmnrLinkNodes)
  private
    FRow: TmnrRowNode;
  protected
    function GetRow: TmnrRowNode;
  public
    property Row: TmnrRowNode read GetRow;
  end;

  TmnrRowNode = class(TmnrLinkNode)
  private
    function GetCells: TmnrRowCells;
  protected
    FCells: TmnrRowCells;
    FID: Integer;
    procedure SetNodes(const Value: TmnrRowNodes);
    function GetNodes: TmnrRowNodes;
    function GetNext: TmnrRowNode;
    function GetPrior: TmnrRowNode;
    function CreateCells: TmnrRowCells; virtual;
    procedure Attach; override;
    procedure Detach; override;

  public
    constructor Create(vNodes: TmnrNodes); override;
    destructor Destroy; override;
    property ID: Integer read FID;

    property Next: TmnrRowNode read GetNext;
    property Prior: TmnrRowNode read GetPrior;
    property Nodes: TmnrRowNodes read GetNodes write SetNodes; //parent nodes of row
    property Cells: TmnrRowCells read GetCells; //cells in row
  end;

  TmnrRowNodes = class(TmnrLinkNodes)
  protected
    function GetHead: TmnrNode; override;
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

constructor TmnrNode.Create(vNodes: TmnrNodes);
begin
  inherited Create;
  Nodes := vNodes;
  {$IFOPT D+}
    Inc(FNodesCount);
  {$ENDIF}
end;

destructor TmnrNode.Destroy;
begin
  Detach;
  {$IFOPT D+}
    Dec(FNodesCount);
  {$ENDIF}
  inherited;
end;

function TmnrNode.GetNodes: TmnrNodes;
begin
  Result := FNodes;
end;

procedure TmnrNode.SetNodes(const Value: TmnrNodes);
begin
  if FNodes<>Value then
  begin
    Detach;
    FNodes := Value;
    Attach;
  end;
end;

{ TmnrNodes }

procedure TmnrNodes.Clear;
var
  h: TmnrNode;
begin
  h := Head;
  while h<>nil do
  begin
    h.Detach;
    //
    h.Free;
    h := Head;
  end;
end;

constructor TmnrNodes.Create;
begin
  inherited Create;
end;

destructor TmnrNodes.Destroy;
begin
  Clear;
  inherited;
end;

function TmnrNodes.GetCount: Integer;
begin
  Result := 0;
end;

function TmnrNodes.GetHead: TmnrNode;
begin
  Result := nil;
end;

{ TmnrLinkNodes }

{ TmnrLinkNode }

procedure TmnrLinkNode.Attach;
begin
  if Nodes<>nil then
  begin
    if Nodes.Last=nil then
    begin
      Nodes.FFirst := Self;
      Nodes.FLast := Self;
    end
    else
    begin
      FPrior := Nodes.Last;
      FPrior.FNext := Self;
      Nodes.FLast := Self;
    end;

    Inc(Nodes.FCount);
  end;
end;

procedure TmnrLinkNode.Detach;
begin
  if Nodes<>nil then
  begin
    if Nodes.First=Self then Nodes.FFirst := Next;
    if Nodes.First<>nil then Nodes.FFirst.FPrior := nil;

    if Nodes.Last=Self then Nodes.FLast := Prior;
    if Nodes.Last<>nil then Nodes.FLast.FNext := nil;
    Dec(Nodes.FCount);
  end;
  if Prior<>nil then Prior.FNext := Next;
  if Next<>nil then Next.FPrior := Prior;

  FNext := nil;
  FPrior := nil;
end;

function TmnrLinkNode.GetNext: TmnrLinkNode;
begin
  Result := FNext;
end;

function TmnrLinkNode.GetNodes: TmnrLinkNodes;
begin
  Result := TmnrLinkNodes(inherited GetNodes);
end;

function TmnrLinkNode.GetPrior: TmnrLinkNode;
begin
  Result := FPrior;
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

function TmnrLinkNodes.GetHead: TmnrNode;
begin
  Result := First;
end;

function TmnrLinkNodes.GetByIndex(vIndex: Integer): TmnrLinkNode;
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

function TmnrLinkNodes.GetFirst: TmnrLinkNode;
begin
  Result := FFirst;
end;

function TmnrLinkNodes.GetLast: TmnrLinkNode;
begin
  Result := FLast;
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

constructor TmnrRowNode.Create(vNodes: TmnrNodes);
begin
  inherited Create(vNodes);
  FCells := CreateCells;
  if FCells=nil then
    FCells := TmnrRowCells.Create;
  FCells.FRow := Self; 
end;

function TmnrRowNode.CreateCells: TmnrRowCells;
begin
  Result := nil;
end;

destructor TmnrRowNode.Destroy;
begin
  FreeAndNil(FCells);
  inherited;
end;

procedure TmnrRowNode.Detach;
begin
  inherited;

end;

function TmnrRowNode.GetCells: TmnrRowCells;
begin
  Result := FCells;
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

function TmnrRowNodes.GetHead: TmnrNode;
begin
  Result := First;
end;

function TmnrRowNodes.GetLast: TmnrRowNode;
begin
  Result := TmnrRowNode(inherited GetLast);
end;

{ TmnrIndex }

procedure TmnrIndex.Compute;
begin

end;

constructor TmnrIndex.Create(vNodes: TmnrNodes);
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

function TmnrIndex.GetNodes: TmnrNodes;
begin
  Result := FNodes;  
end;

{ TmnrLinkNodesListIndex }

procedure TmnrLinkNodesListIndex.Compute;
var
  p: TmnrLinkNode;
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

function TmnrLinkNodesListIndex.GetItems(Index: Integer): TmnrLinkNode;
begin
  Result := FArray[Index];
end;

function TmnrLinkNodesListIndex.GetNodes: TmnrLinkNodes;
begin
  Result := TmnrLinkNodes(inherited GetNodes);
end;

{ TmnrRowCells }

function TmnrRowCells.GetRow: TmnrRowNode;
begin
  Result := FRow;
end;

initialization

finalization
  {$IFOPT D+}
    //if FNodesCount<>0 then
      //MessageBox(0, PChar('Remain '+IntToStr(FNodesCount)+' node.'), PChar('warning'), 0);
  {$ENDIF}


end.
