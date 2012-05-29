unit mncDS;

{$mode objfpc}

interface

uses
  SysUtils, Classes, DB, Types,
  mncConnections, mncSQL;

type
  { TmncDataset }

  TmncDataset = class(TDataSet)
  private
    FConnection: TmncConnection;
    FKeyFields: string;
    FSelectSQL: TStrings;
    FInsertSQL: TStrings;
    FUpdateSQL: TStrings;
    FDeleteSQL: TStrings;
    FRefreshSQL: TStrings;
    procedure SetSelectSQL(AValue: TStrings);
    procedure SetInsertSQL(AValue: TStrings);
    procedure SetUpdateSQL(AValue: TStrings);
    procedure SetDeleteSQL(AValue: TStrings);
    procedure SetRefreshSQL(AValue: TStrings);

    procedure InitFieldDefs;
  protected
    procedure DoSQLChanged(Sender: TObject);

    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;

    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;

    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;

    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

    procedure Prepare;
    procedure Unprepare;
    property SelectSQL: TStrings read FSelectSQL write SetSelectSQL;
    property UpdateSQL: TStrings read FUpdateSQL write SetUpdateSQL;
    property InsertSQL: TStrings read FInsertSQL write SetInsertSQL;
    property DeleteSQL: TStrings read FDeleteSQL write SetDeleteSQL;
    property RefreshSQL: TStrings read FRefreshSQL write SetRefreshSQL;
    property KeyFields: string read FKeyFields write FKeyFields;

    property Connection: TmncConnection read FConnection write FConnection;
  published
    property Active;
    property FieldDefs;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
  end;

implementation

{ TmncDataset }

procedure TmncDataset.SetDeleteSQL(AValue: TStrings);
begin
  if FDeleteSQL =AValue then Exit;
  FDeleteSQL.Assign(AValue);
end;

procedure TmncDataset.SetInsertSQL(AValue: TStrings);
begin
  if FInsertSQL =AValue then Exit;
  FInsertSQL.Assign(AValue);
end;

procedure TmncDataset.SetRefreshSQL(AValue: TStrings);
begin
  if FRefreshSQL =AValue then Exit;
  FRefreshSQL.Assign(AValue);
end;

procedure TmncDataset.InitFieldDefs;
begin

end;

procedure TmncDataset.DoSQLChanged(Sender: TObject);
begin

end;

procedure TmncDataset.SetSelectSQL(AValue: TStrings);
begin
  if FSelectSQL =AValue then Exit;
  FSelectSQL.Assign(AValue);
end;

procedure TmncDataset.SetUpdateSQL(AValue: TStrings);
begin
  if FUpdateSQL =AValue then Exit;
  FUpdateSQL.Assign(AValue);
end;

function TmncDataset.AllocRecordBuffer: PChar;
begin
end;

procedure TmncDataset.FreeRecordBuffer(var Buffer: PChar);
begin
end;

procedure TmncDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
end;

function TmncDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
end;

function TmncDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
end;

function TmncDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
end;

function TmncDataset.GetRecordSize: Word;
begin
end;

procedure TmncDataset.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
begin
end;

procedure TmncDataset.InternalClose;
begin
end;

procedure TmncDataset.InternalDelete;
begin
end;

procedure TmncDataset.InternalFirst;
begin
end;

procedure TmncDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
end;

procedure TmncDataset.InternalInitFieldDefs;
begin
end;

procedure TmncDataset.InternalInitRecord(Buffer: PChar);
begin
end;

procedure TmncDataset.InternalLast;
begin
end;

procedure TmncDataset.InternalOpen;
begin
end;

procedure TmncDataset.InternalPost;
begin
end;

procedure TmncDataset.InternalSetToRecord(Buffer: PChar);
begin
end;

function TmncDataset.IsCursorOpen: Boolean;
begin
end;

procedure TmncDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
end;

procedure TmncDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
end;

procedure TmncDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
end;

function TmncDataset.GetRecordCount: Integer;
begin
end;

procedure TmncDataset.SetRecNo(Value: Integer);
begin
end;

function TmncDataset.GetRecNo: Integer;
begin
end;

constructor TmncDataset.Create(AOwner: TComponent);
begin
  FSelectSQL := TStringList.Create;
  FUpdateSQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  FRefreshSQL := TStringList.Create;

  (FSelectSQL as TStringList).OnChange := @DoSQLChanged;
  (FUpdateSQL as TStringList).OnChange := @DoSQLChanged;
  (FInsertSQL as TStringList).OnChange := @DoSQLChanged;
  (FDeleteSQL as TStringList).OnChange := @DoSQLChanged;
  (FRefreshSQL as TStringList).OnChange := @DoSQLChanged;
  inherited;
end;

destructor TmncDataset.Destroy;
begin
  FreeAndNil(FSelectSQL);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FInsertSQL);
  FreeAndNil(FDeleteSQL);
  FreeAndNil(FRefreshSQL);
  inherited;
end;

function TmncDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
begin
end;

procedure TmncDataset.Prepare;
begin

end;

procedure TmncDataset.Unprepare;
begin

end;

end.
