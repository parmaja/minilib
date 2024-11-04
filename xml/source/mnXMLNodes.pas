unit mnXMLNodes;

{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs,
  mnClasses, mnUtils, mnStreams,
  mnXML, mnXMLReader, mnXMLWriter;

type
  TmnXMLNodeKind = (xmlnText, xmlnCDATA, xmlnComment);
  TmnXMLNodeState = (xmlsOpened, xmlsClosed);

  TmnXMLNodes = class;
  TmnXMLNode = class;
  TmnXMLNodesList = class;

  { TmnCustomNode }

  TmnCustomNode = class(TmnXMLObject)
  protected
  type

    { TmnXMLNodeEnumerator }

    TmnXMLNodeEnumerator = class(TObject)
    private
      FList: TmnXMLNode;
      FIndex: Integer;
    public
      constructor Create(AList: TmnXMLNode);
      function GetCurrent: TmnXMLNode;
      function MoveNext: Boolean;
      property Current: TmnXMLNode read GetCurrent;
    end;

  end;

  { TmnXMLNodesList }

  TmnXMLNodesList = class(TObjectList)
  private
    function GetItem(Index: Integer): TmnXMLNode;
    procedure SetItem(Index: Integer; const Value: TmnXMLNode);
  public
    function Find(Name: String): TmnXMLNode;
    property Items[Index: Integer]: TmnXMLNode read GetItem write SetItem; default;
  end;

  { TmnXMLNode }

  TmnXMLNode = class(TmnCustomNode)
  private
    FNameSpace: String;
    FNodes: TmnXMLNodes;
    FParent: TmnXMLNode;
    FValue: String;
    FAttributes: TmnXMLAttributes;
    FName: String;
    FKind: TmnXMLNodeKind;
    FState: TmnXMLNodeState;
    FItems: TmnXMLNodesList;
    function GetCount: Integer;
    function GetEmpty: Boolean;
    function GetItem(Index: Integer): TmnXMLNode;
  protected
  public
    constructor Create(Nodes: TmnXMLNodes; Parent: TmnXMLNode); virtual;
    destructor Destroy; override;
    function GetEnumerator: TmnCustomNode.TmnXMLNodeEnumerator; inline;
    procedure Close;
    procedure Add(Node: TmnXMLNode);
    //this will split Name to NameSpace, Name
    procedure SetName(Name: String); virtual;
    property Nodes: TmnXMLNodes read FNodes;
    property Parent: TmnXMLNode read FParent;
    property State: TmnXMLNodeState read FState;
    property Items: TmnXMLNodesList read FItems;
    property Attributes: TmnXMLAttributes read FAttributes;
    property Empty: Boolean read GetEmpty;
    property NameSpace: String read FNameSpace write FNameSpace;
    property Value: String read FValue write FValue;
    property Name: String read FName write FName;
    property Kind: TmnXMLNodeKind read FKind write FKind;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TmnXMLNode read GetItem; default;
  end;

  TmnXMLNodeOption = (
    xnoCDATA, //CDATA as node
    xnoText, //Text node, not prefered
    xnoComment,  //it is useful when need to rewrite the xml data, when Enhanced = false mean we take the nodes for process the data, the comment will ignored and all text and cdata merged
    xnoNameSpace, //Split NameSpace
    xnoTrimValue  //Trim CDATA, TEXT value
    );

  TmnXMLNodeOptions = set of TmnXMLNodeOption;

  { TmnXMLNodes }

  TmnXMLNodes = class(TmnCustomNode)
  private
    FOptions: TmnXMLNodeOptions;
    FRoot: TmnXMLNode;
    function GetItems(Index: String): TmnXMLNode;
    function GetEmpty: Boolean;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetEnumerator: TmnCustomNode.TmnXMLNodeEnumerator; inline;

    procedure LoadFromString(S: String);
    procedure LoadFromFile(AFileName: String);
    procedure LoadFromStream(AStream: TStream);

    procedure SaveToFile(AFileName: String);
    procedure SaveToStream(AStream: TStream);

    procedure Clear;
    function GetAttribute(Name, Attribute: String; Default: String = ''): String;
    property Empty: Boolean read GetEmpty;
    property Options: TmnXMLNodeOptions read FOptions write FOptions;
    property Root: TmnXMLNode read FRoot;
    property Items[Index: String]: TmnXMLNode read GetItems; default;
  end;

  { TmnXMLNodeReader }

  TmnXMLNodeReader = class(TmnXMLReader)
  protected
    FCurrent: TmnXMLNode;
    FNodes: TmnXMLNodes;
    procedure CheckClosed;
    procedure ReadOpenTag(const Name: String); override;
    procedure ReadAttributes(const Text: String); override;
    procedure ReadText(const Text: String); override;
    procedure ReadComment(const Text: String); override;
    procedure ReadCDATA(const Text: String); override;
    procedure ReadCloseTag(const Name: String); override;
    procedure DoStart; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Nodes: TmnXMLNodes read FNodes write FNodes;
    property Current: TmnXMLNode read FCurrent;
  end;

  { TmnXMLNodeWriter }

  TmnXMLNodeWriter = class(TmnXMLWriter)
  private
    FNodes: TmnXMLNodes;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    property Nodes: TmnXMLNodes read FNodes write FNodes;
  end;

procedure XMLNodeSaveToStream(ANodes: TmnXMLNodes; AStream: TStream; WriterClass: TmnCustomXMLWriterClass);
function FormatXML(const vXML: string): string;

implementation

function FormatXML(const vXML: string): string;
var
  n: TmnXMLNodes;
  m: TMemoryStream;
  t: UTF8String;
begin
  n := TmnXMLNodes.Create;
  m := TMemoryStream.Create;
  try
    t := UTF8Encode(vXML);
    //n.Options := [xnoCDATA, xnoText, xnoNameSpace, xnoComment, xnoTrimValue];
    n.Options := [xnoNameSpace, xnoTrimValue];
    n.LoadFromString(t);
    n.SaveToStream(m); //save as utf8
    Result := TEncoding.UTF8.GetString(m.Memory, m.Size);
  finally
    n.Free;
    m.Free;
  end;
end;

procedure XMLNodeSaveToStream(ANodes: TmnXMLNodes; AStream: TStream; WriterClass: TmnCustomXMLWriterClass);
var
  aWriter: TmnCustomXMLWriter;

  procedure WriteNow(node: TmnXMLNode);
  var
    n: TmnXMLNode;
    a: TmnXMLAttribute;
  begin
    with aWriter do
    begin
      if node.Kind = xmlnComment then
        AddComment(node.Value)
      else if node.Kind = xmlnCDATA then
        AddCDATA(node.Value)
      else
      begin
        StartTag(node.NameSpace, node.Name);
        if node.Attributes.Count > 0 then
        begin
          for a in node.Attributes do
          begin
            AddAttribute(a.Name, a.Value);
          end;
        end;
        StopTag(node.NameSpace, node.Name);
        if node.Count > 0 then
        begin
          for n in node do
            WriteNow(n);
        end;
        AddText(node.Value);
        CloseTag(node.NameSpace, node.Name);
      end;
    end;
  end;

var
  aWrapperStream: TmnWrapperStream;
begin
  aWrapperStream := TmnWrapperStream.Create(AStream, False);
  aWriter := WriterClass.Create(aWrapperStream, False);
  try
    aWriter.Smart := True;
    aWriter.Start;
    WriteNow(ANodes.Root);
  finally
    aWriter.Free;
    aWrapperStream.Free;
  end;
end;

constructor TmnXMLNodeWriter.Create;
begin
  inherited Create;
end;

destructor TmnXMLNodeWriter.Destroy;
begin
  inherited Destroy;
end;

{ TmnCustomNode.TmnXMLNodeEnumerator }

constructor TmnCustomNode.TmnXMLNodeEnumerator.Create(AList: TmnXMLNode);
begin
  inherited Create;
  FList := Alist;
  FIndex := -1;
end;

function TmnCustomNode.TmnXMLNodeEnumerator.GetCurrent: TmnXMLNode;
begin
  Result := FList[FIndex];
end;

function TmnCustomNode.TmnXMLNodeEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TmnXMLNode }

procedure TmnXMLNode.Add(Node: TmnXMLNode);
begin
  if Kind in [xmlnCDATA, xmlnComment] then
    raise EmnXMLException.Create('You can not add child nodes to CDATA or Comment node');
  Items.Add(Node);
end;

procedure TmnXMLNode.SetName(Name: String);
begin
  if (xnoNameSpace in Nodes.Options) and (Pos(':', Name) > 0) then
    SpliteStr(Name, ':', FNameSpace, FName)
  else
    FName := Name;
end;

procedure TmnXMLNode.Close;
begin
  FState := xmlsClosed;
end;

constructor TmnXMLNode.Create(Nodes: TmnXMLNodes; Parent: TmnXMLNode);
begin
  inherited Create;
  FNodes := Nodes;
  FParent := Parent;
  FAttributes := TmnXMLAttributes.Create();
  FItems := TmnXMLNodesList.Create;
end;

destructor TmnXMLNode.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(FItems);
  inherited;
end;

function TmnXMLNode.GetEmpty: Boolean;
begin
  Result := Items.Count = 0;
end;

function TmnXMLNode.GetCount: Integer;
begin
  Result := Items.Count;
end;

function TmnXMLNode.GetItem(Index: Integer): TmnXMLNode;
begin
  Result := Items[Index];
end;

function TmnXMLNode.GetEnumerator: TmnCustomNode.TmnXMLNodeEnumerator;
begin
  Result := TmnCustomNode.TmnXMLNodeEnumerator.Create(Self);
end;

{ TmnXMLNodeReader }

constructor TmnXMLNodeReader.Create;
begin
  inherited;
end;

destructor TmnXMLNodeReader.Destroy;
begin
  inherited;
end;

procedure TmnXMLNodeReader.ReadAttributes(const Text: String);
begin
  inherited;
  CheckClosed;
  FCurrent.Attributes.SetText(EntityDecode(Text));
end;

procedure TmnXMLNodeReader.ReadCDATA(const Text: String);
var
  aNode: TmnXMLNode;
begin
  inherited;
  CheckClosed;
  if xnoCDATA in Nodes.Options then //we add the text as node
  begin
    aNode := TmnXMLNode.Create(Nodes, FCurrent);
    aNode.FKind := xmlnCDATA;
    aNode.FValue := Text;
    FCurrent.Add(aNode);
  end
  else
  begin
    FCurrent.FValue := FCurrent.FValue + Text;
  end;
end;

procedure TmnXMLNodeReader.ReadCloseTag(const Name: String);
begin
  inherited;
  CheckClosed;
  FCurrent.FState := xmlsClosed;
  if FCurrent.Parent <> nil then
    FCurrent := FCurrent.Parent;
end;

procedure TmnXMLNodeReader.DoStart;
begin
  inherited DoStart;
end;

procedure TmnXMLNodeReader.ReadComment(const Text: String);
var
  aNode: TmnXMLNode;
begin
  inherited;
  //CheckClosed; //svg have comment not inside
  if xnoComment in Nodes.Options then //we ignore the comment if not
  begin
    if FCurrent <> nil then
    begin
      aNode := TmnXMLNode.Create(Nodes, FCurrent);
      aNode.FKind := xmlnComment;
      aNode.Value := Text;
      FCurrent.Add(aNode);
    end;
  end;
end;

procedure TmnXMLNodeReader.CheckClosed;
begin
  if FCurrent = nil then
    raise EmnXMLException.Create('There is not tag opened');
  if FCurrent.State = xmlsClosed then
    raise EmnXMLException.Create(FCurrent.Name + ' is already close tag');
end;

procedure TmnXMLNodeReader.ReadOpenTag(const Name: String);
var
  aNode: TmnXMLNode;
begin
  inherited;
  aNode := TmnXMLNode.Create(Nodes, FCurrent);
  aNode.SetName(Name);
  if Nodes.FRoot = nil then
    Nodes.FRoot := aNode
  else
  begin
    CheckClosed;
    FCurrent.Add(aNode);
  end;
  FCurrent := aNode;
end;

procedure TmnXMLNodeReader.ReadText(const Text: String);
var
  Node: TmnXMLNode;
  aText: string;
begin
  inherited;
  CheckClosed;
  if xnoTrimValue in Nodes.Options then
    aText := Trim(Text)
  else
    aText := Text;
  if xnoText in Nodes.Options then //we add the text as node
  begin
    Node := TmnXMLNode.Create(Nodes, FCurrent);
    Node.FKind := xmlnText;
    Node.FValue := aText;
    FCurrent.Add(Node);
  end
  else
  begin
    FCurrent.FValue := FCurrent.FValue + aText;
  end;
end;

{ TmnXMLNodesList }

function TmnXMLNodesList.Find(Name: String): TmnXMLNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Name, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmnXMLNodesList.GetItem(Index: Integer): TmnXMLNode;
begin
  Result := inherited Items[Index] as TmnXMLNode;
end;

procedure TmnXMLNodesList.SetItem(Index: Integer; const Value: TmnXMLNode);
begin
  inherited Items[Index] := Value;
end;

{ TmnXMLNodes }

procedure TmnXMLNodes.Clear;
begin
  FreeAndNil(FRoot);
end;

constructor TmnXMLNodes.Create;
begin
  inherited Create;
end;

destructor TmnXMLNodes.Destroy;
begin
  FreeAndNil(FRoot);
  inherited;
end;

function TmnXMLNodes.GetEnumerator: TmnCustomNode.TmnXMLNodeEnumerator;
begin
  Result := TmnCustomNode.TmnXMLNodeEnumerator.Create(FRoot);
end;

procedure TmnXMLNodes.LoadFromString(S: String);
var
  Reader: TmnXMLNodeReader;
begin
  Reader := TmnXMLNodeReader.Create;
  try
    Reader.Nodes := Self;
    Reader.Start;
    Reader.Parse(S);
  finally
    Reader.Free;
  end;
end;

procedure TmnXMLNodes.LoadFromFile(AFileName: String);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TmnXMLNodes.LoadFromStream(AStream: TStream);
var
  AWrapperStream: TmnWrapperStream;
  Reader: TmnXMLNodeReader;
begin
  AWrapperStream := TmnWrapperStream.Create(AStream, False);
  try
    Reader := TmnXMLNodeReader.Create(AWrapperStream, False);
    try
      Reader.Nodes := Self;
      Reader.Start;
    finally
      Reader.Free;
    end;
  finally
    AWrapperStream.Free;
  end;
end;

procedure TmnXMLNodes.SaveToFile(AFileName: String);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TmnXMLNodes.SaveToStream(AStream: TStream);
begin
  XMLNodeSaveToStream(Self, AStream, TmnXMLWriter);
end;

function TmnXMLNodes.GetAttribute(Name, Attribute: String; Default: String): String;
var
  aNode: TmnXMLNode;
  aAttribute: TmnXMLAttribute;
begin
  if FRoot = nil then
    Result := Default
  else
  begin
    if FRoot.Name = Name then
      aNode := FRoot
    else
      aNode := Items[Name];
    if aNode = nil then
      Result := Default
    else
    begin
      aAttribute := aNode.Attributes.Find(Attribute);
      if aAttribute = nil then
        Result := Default
      else
        Result := aAttribute.Value;
    end;
  end;
end;

function TmnXMLNodes.GetEmpty: Boolean;
begin
  Result := (Root = nil) or (Root.Empty);
end;

function TmnXMLNodes.GetItems(Index: String): TmnXMLNode;
begin
  if FRoot <> nil then
  begin
    if SameText(FRoot.Name, Index) then
      Result := FRoot
    else
      Result := FRoot.Items.Find(Index);
  end
  else
    Result := nil;
end;

end.
