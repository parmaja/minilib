unit mncPGMeta;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @author    Belal Hamed <belalhamed at gmail dot com>  
 * 
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, contnrs,
  mncSQL, mncMeta, mncConnections, mncPostgre;

type
  { TmncPGMeta }

  TmncPGMeta = class(TmncMeta)
  private
  protected
    function CreateCMD(SQL: string): TmncPGCommand;
    procedure EnumCMD(Meta: TmncMetaItems; SQL: string; Fields: array of string); overload;//use field 'name'
    procedure EnumCMD(Meta: TmncMetaItems; SQL: string); overload;
    procedure FetchCMD(Strings:TStringList; SQL: string);//use field 'name'
    function GetSortSQL(Options: TschmEnumOptions):string;
  public
    procedure EnumTables(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFields(Meta: TmncMetaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumViews(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumProcedures(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumSequences(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFunctions(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumExceptions(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;

    procedure EnumConstraints(Meta: TmncMetaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumTriggers(Meta: TmncMetaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumIndices(Meta: TmncMetaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    //source
    procedure GetTriggerSource(Strings:TStringList; MemberName: string; Options: TschmEnumOptions = []); override;
    procedure GetIndexInfo(Meta: TmncMetaItems; MemberName: string; Options: TschmEnumOptions = []); override;
  end;

implementation

{ TmncMetaItems }

function TmncPGMeta.CreateCMD(SQL: string): TmncPGCommand;
begin
  Result := TmncPGCommand.CreateBy(Link as TmncPGSession);
  Result.SQL.Text := SQL;
end;

procedure TmncPGMeta.EnumCMD(Meta: TmncMetaItems; SQL: string; Fields: array of string);
begin
end;

procedure TmncPGMeta.EnumCMD(Meta: TmncMetaItems; SQL: string);
begin
end;

procedure TmncPGMeta.FetchCMD(Strings: TStringList; SQL: string);
begin
end;

function TmncPGMeta.GetSortSQL(Options: TschmEnumOptions): string;
begin
end;

procedure TmncPGMeta.EnumTables(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin
end;

procedure TmncPGMeta.EnumViews(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin

end;

procedure TmncPGMeta.EnumProcedures(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin

end;

procedure TmncPGMeta.EnumSequences(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin

end;

procedure TmncPGMeta.EnumFunctions(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin

end;

procedure TmncPGMeta.EnumExceptions(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin

end;

procedure TmncPGMeta.EnumConstraints(Meta: TmncMetaItems; MemberName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncPGMeta.EnumTriggers(Meta: TmncMetaItems; MemberName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncPGMeta.EnumIndices(Meta: TmncMetaItems; MemberName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncPGMeta.GetTriggerSource(Strings: TStringList; MemberName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncPGMeta.GetIndexInfo(Meta: TmncMetaItems; MemberName: string; Options: TschmEnumOptions);
begin
end;

procedure TmncPGMeta.EnumFields(Meta: TmncMetaItems; MemberName: string; Options: TschmEnumOptions);
begin
end;

initialization
  Meta.RegisterMeta('Postgres', 'PostgresSQL Database', TmncPGMeta, TmncPGConnection);
end.
