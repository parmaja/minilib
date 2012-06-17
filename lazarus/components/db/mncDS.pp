unit mncDS;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *}
 {$mode objfpc}

{$warning 'Still working on it, not finished}

interface

uses
  SysUtils, Classes, DB, Types,
  mncConnections, mncSQL;

type
  { TmncDataset }

  { TmncSQLDataset }

  TmncSQLDataset = class(TDataSet)
  private
    FConnection: TmncSQLConnection;
    FIsOpen: Boolean;
    FKeepAlive: Boolean;
    FKeyFields: string;
    FSelectSQL: TStrings;
    FInsertSQL: TStrings;
    FUpdateSQL: TStrings;
    FDeleteSQL: TStrings;
    FRefreshSQL: TStrings;
    procedure SetKeepAlive(AValue: Boolean);
    procedure SetSelectSQL(AValue: TStrings);
    procedure SetInsertSQL(AValue: TStrings);
    procedure SetUpdateSQL(AValue: TStrings);
    procedure SetDeleteSQL(AValue: TStrings);
    procedure SetRefreshSQL(AValue: TStrings);

  protected
    FCurrentFields: TmncFields; //Field that contain data;
    function CreateSession: TmncSQLSession;

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

    property Connection: TmncSQLConnection read FConnection write FConnection;
    //Keep the select command opened for forard cursor, i dislike it, so may be it deprecated
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive; deprecated;
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

function DataTypeToFieldType(DataType: TmncDataType):TFieldType;

implementation

{ TmncSQLDataset }

procedure TmncSQLDataset.SetDeleteSQL(AValue: TStrings);
begin
  if FDeleteSQL =AValue then Exit;
  FDeleteSQL.Assign(AValue);
end;

procedure TmncSQLDataset.SetInsertSQL(AValue: TStrings);
begin
  if FInsertSQL =AValue then Exit;
  FInsertSQL.Assign(AValue);
end;

procedure TmncSQLDataset.SetRefreshSQL(AValue: TStrings);
begin
  if FRefreshSQL =AValue then Exit;
  FRefreshSQL.Assign(AValue);
end;

function TmncSQLDataset.CreateSession: TmncSQLSession;
begin
  Result := FConnection.CreateSession;
  Result.Start;
  Result.Action := sdaCommit; //commit it when free it
end;

procedure TmncSQLDataset.DoSQLChanged(Sender: TObject);
begin

end;

procedure TmncSQLDataset.SetSelectSQL(AValue: TStrings);
begin
  if FSelectSQL =AValue then Exit;
  FSelectSQL.Assign(AValue);
end;

procedure TmncSQLDataset.SetKeepAlive(AValue: Boolean);
begin
  if FKeepAlive =AValue then Exit;
  FKeepAlive :=AValue;
end;

procedure TmncSQLDataset.SetUpdateSQL(AValue: TStrings);
begin
  if FUpdateSQL =AValue then Exit;
  FUpdateSQL.Assign(AValue);
end;

function TmncSQLDataset.AllocRecordBuffer: PChar;
begin
end;

procedure TmncSQLDataset.FreeRecordBuffer(var Buffer: PChar);
begin
end;

procedure TmncSQLDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
end;

function TmncSQLDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
end;

function TmncSQLDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
end;

function TmncSQLDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
end;

function TmncSQLDataset.GetRecordSize: Word;
begin
end;

procedure TmncSQLDataset.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
begin
end;

procedure TmncSQLDataset.InternalClose;
begin
  FIsOpen := False;
end;

procedure TmncSQLDataset.InternalDelete;
begin
end;

procedure TmncSQLDataset.InternalFirst;
var
  i: Integer;
  Cmd: TmncSQLCommand;
  Se: TmncSQLSession;
begin
  Se := CreateSession;
  try
    Cmd := Se.CreateCommand as TmncSQLCommand;
    try
      Cmd.SQL.Assign(SelectSQL);
      Cmd.Prepare;
      for i := 0 to Cmd.Columns.Count -1 do
      begin
        with Cmd.Columns[i] do
          TFieldDef.Create(FieldDefs, Name, DataTypeToFieldType(DataType), Size, doRequired in Options, I);
      end;
    finally
      Cmd.Free;
    end;
  finally
    Se.Free;
  end;
end;

procedure TmncSQLDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
end;

procedure TmncSQLDataset.InternalInitFieldDefs;
var
  i: Integer;
  Cmd: TmncSQLCommand;
  Se: TmncSQLSession;
begin
  Se := CreateSession;
  try
    Cmd := Se.CreateCommand;
    try
      Cmd.SQL.Assign(SelectSQL);
      Cmd.Prepare;
      for i := 0 to Cmd.Columns.Count -1 do
      begin
        with Cmd.Columns[i] do
          TFieldDef.Create(FieldDefs, Name, DataTypeToFieldType(DataType), Size, doRequired in Options, I);
      end;
    finally
      Cmd.Free;
    end;
  finally
    Se.Free;
  end;
end;

procedure TmncSQLDataset.InternalInitRecord(Buffer: PChar);
begin
end;

procedure TmncSQLDataset.InternalLast;
begin
end;

procedure TmncSQLDataset.InternalOpen;
begin
  FIsOpen := True;
end;

procedure TmncSQLDataset.InternalPost;
begin
end;

procedure TmncSQLDataset.InternalSetToRecord(Buffer: PChar);
begin
end;

function TmncSQLDataset.IsCursorOpen: Boolean;
begin
  Result := FIsOpen;
end;

procedure TmncSQLDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
end;

procedure TmncSQLDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
end;

procedure TmncSQLDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
end;

function TmncSQLDataset.GetRecordCount: Integer;
begin
end;

procedure TmncSQLDataset.SetRecNo(Value: Integer);
begin
end;

function TmncSQLDataset.GetRecNo: Integer;
begin
end;

constructor TmncSQLDataset.Create(AOwner: TComponent);
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

destructor TmncSQLDataset.Destroy;
begin
  FreeAndNil(FSelectSQL);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FInsertSQL);
  FreeAndNil(FDeleteSQL);
  FreeAndNil(FRefreshSQL);
  inherited;
end;

function TmncSQLDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
begin
end;

procedure TmncSQLDataset.Prepare;
begin

end;

procedure TmncSQLDataset.Unprepare;
begin

end;

const
  cDT2FT: array[TmncDataType] of TFieldType = (ftUnknown, ftString, ftBoolean, ftInteger, ftCurrency, ftFloat, ftDate, ftTime, ftDateTime, ftMemo, ftBlob);

function DataTypeToFieldType(DataType: TmncDataType): TFieldType;
begin
  Result := cDT2FT[DataType];
end;

end.
