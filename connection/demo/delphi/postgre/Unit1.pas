unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mncConnections, mncPostgre, mncSQL, SynEdit, mncPGHeader;

type

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    PassEdit: TEdit;
    BinaryResultChk: TCheckBox;
    Button3: TButton;
    SynEdit1: TSynEdit;
    TreeBtn: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure TreeBtnClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
  public
    function ConnectData: TmncPGConnection;
  end;

  TMatRec = record
    ID: Integer;
    Parent: Integer;
    Name: string;
    Code: string;
    Hit: Byte;
  end;

  TMatArr = array of TMatRec;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Conn: TmncPGConnection;
  Session: TmncPGSession;
  Cmd: TmncPGCommand;
begin
  Conn := ConnectData;
  try
    Session := TmncPGSession.Create(Conn);
    try
      Session.Start;
      Cmd := TmncPGCommand(Session.CreateCommand);
      try
        Cmd.SQL.Text := 'insert into companies';
        Cmd.SQL.Add('(id, name, nationality)');
        Cmd.SQL.Add('values (?id, ?name, ?nationality)');
        Cmd.Prepare;
        Cmd.Params['id'].AsVariant := Null;
        Cmd.Params['name'].AsString := 'าวๅั';
        Cmd.Params['nationality'].AsInteger := 22;
        Cmd.Execute;

        Session.Commit;
        Cmd.SQL.Text := 'select * from companies';
        Cmd.SQL.Add('where name = ?name');
        Cmd.Prepare;
        Cmd.Param['name'].AsString := 'zaher';
        if Cmd.Execute then
          ShowMessage(Cmd.Field['name'].AsString)
        else
          ShowMessage('not found');

      finally
//        Cmd.Free;
      end;
    finally
//      Session.Free;
    end;
  //  Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  Conn: TmncPGConnection;
  Session: TmncPGSession;
  Cmd: TmncPGCommand;
  i: Integer;
  t: Cardinal;
begin
  Conn := ConnectData;
  SynEdit1.BeginUpdate;
  try
    Session := TmncPGSession.Create(Conn);
    ListBox1.Items.Clear;
    try
      Session.Start;
      Cmd := TmncPGCommand.CreateBy(Session);
      try
        if BinaryResultChk.Checked then
          cmd.ResultFormat := mrfBinary;


        Cmd.SQL.Text := 'select "DocID" , "DocUser" ,"DocArchive" ,"DocName" ,"DocCode" from "Documents"';
        //Cmd.SQL.Text := 'select "AccID" from "Accounts"';
  //      Cmd.SQL.Add('where name = ?name');
        //Cmd.Prepare;
  //      Cmd.Param['name'].AsString := 'Ferrari';
        t := GetTickCount;
        if Cmd.Execute then
        begin
          while not Cmd.EOF do
          begin
            for I := 0 to Cmd.Fields.Count - 1 do
              SynEdit1.Lines.Add(Cmd.Fields.Items[i].Column.Name+': '+Cmd.Fields.Items[i].AsString);
            SynEdit1.Lines.Add('-------------------------------');
            //ListBox1.Items.Add(Cmd.Field['AccID'].AsString + ' - ' + Cmd.Field['AccName'].AsString+ ' - ' + Cmd.Field['AccCode'].AsString);
            //ListBox1.Items.Add(Cmd.Field['AccID'].AsString);
            Cmd.Next;
          end;
        end;
      finally
        Cmd.Free;
      end;
      SynEdit1.Lines.Add(IntToStr(GetTickCount-t));
    finally
      Session.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
    SynEdit1.EndUpdate;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Conn: TmncPGConnection;
  Session: TmncPGSession;
  Cmd: TmncPGCommand;
  i: Integer;
  t: Cardinal;
  s: string;
begin
  Conn := TmncPGConnection.Create;
  try
    Conn.Resource := 'afraa2011';
    Conn.Host := '';
    Conn.UserName := 'postgres';
    Conn.Password := 'masterkey';
    Conn.Connect;
    Conn.Execute('SET CLIENT_ENCODING TO ''WIN1256'';');
    Session := TmncPGSession.Create(Conn);
    ListBox1.Items.Clear;
    try
      Session.Start;
      Cmd := TmncPGCommand.CreateBy(Session);
      if BinaryResultChk.Checked then
        cmd.ResultFormat := mrfBinary;


      {Cmd.SQl.Add('set enable_seqscan = false;');
      Cmd.SQl.Add('set enable_sort = false;');

      Cmd.SQl.Add('with recursive MatTree("ID", "Code", "Name") as');
      Cmd.SQl.Add('(');
      Cmd.SQl.Add('  select "MatID", "MatCode", "MatName"');
      Cmd.SQl.Add('  from "viewMatParentCode"');
      Cmd.SQl.Add('  where "MatParent" =  0');
      Cmd.SQl.Add('  union all');
      Cmd.SQl.Add('  select "MatID", "MatCode", "MatName"');
      Cmd.SQl.Add('  from MatTree');
      Cmd.SQl.Add('  inner join "viewMatParentCode" on "MatParent" = "ID"');
      Cmd.SQl.Add(')');
      Cmd.SQl.Add('select * from MatTree');}

      //Cmd.SQl.Add('select "BtmMaterial", "BtmSite", "BtmMaster", "BtmNumber", "BtmDate",  lag("BtmDate") over (order by "BtmMaterial" , "BtmDate" , "BtmMaster" , "BtmNumber")');
      //Cmd.SQl.Add('from "BillItems"');
      //Cmd.SQl.Add('order by "BtmMaterial" , "BtmDate" , "BtmMaster" , "BtmNumber"');
      //Cmd.SQl.Add('limit 20000');

      Cmd.SQl.Add('with recursive MatTree("TreeCode", "TreeLevel", "ID", "Parent", "Name", "Code") as');
      Cmd.SQl.Add('    (');
      Cmd.SQl.Add('      select cast("MatCode" as varchar(1000)) "TreeCode", 0, "MatID", "MatParent", "MatName", "MatCode"');
      Cmd.SQl.Add('      from "Materials"');
      Cmd.SQl.Add('      where "MatParent" = 0');
      Cmd.SQl.Add('      union all');
      Cmd.SQl.Add('      select cast("TreeCode"||''.''||"MatCode" as varchar(1000)), "TreeLevel" + 1, "MatID", "MatParent", "MatName", "MatCode"');
      Cmd.SQl.Add('      from "Materials"');
      Cmd.SQl.Add('      join MatTree  on "MatParent" = "ID"');
      Cmd.SQl.Add('    )');

      Cmd.SQl.Add('select tree."ID", tree."Parent", tree."Name", tree."Code"');
      Cmd.SQl.Add('from MatTree tree');
      Cmd.SQl.Add('--where "TreeLevel" = 3');
      Cmd.SQl.Add('order by tree."TreeCode";');


        t := GetTickCount;
      //Cmd.SQL.Text := 'select "AccID" from "Accounts"';
//      Cmd.SQL.Add('where name = ?name');
      //Cmd.Prepare;
//      Cmd.Param['name'].AsString := 'Ferrari';
      if Cmd.Execute then
      begin
        SynEdit1.Lines.BeginUpdate;
        SynEdit1.Lines.Clear;
        while not Cmd.EOF do
        begin
          s := '';
          for I := 0 to Cmd.Fields.Count - 1 do
            s := s + Cmd.Fields.Items[i].Column.Name+': '+Cmd.Fields.Items[i].AsString + ', ';
          SynEdit1.Lines.Add(s);
          //SynEdit1.Lines.Add('-------------------------------');
          //ListBox1.Items.Add(Cmd.Field['AccID'].AsString + ' - ' + Cmd.Field['AccName'].AsString+ ' - ' + Cmd.Field['AccCode'].AsString);
          //ListBox1.Items.Add(Cmd.Field['AccID'].AsString);
          Cmd.Next;
        end;
        SynEdit1.Lines.EndUpdate;
        ShowMessage(IntToStr(GetTickCount-t));
      end;
      Cmd.Close;
    finally
      Session.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Conn: TmncPGConnection;
  Session: TmncPGSession;
  Cmd: TmncPGCursorCommand;
  i: Integer;
  t: Cardinal;
begin
  Conn := TmncPGConnection.Create;
  SynEdit1.BeginUpdate;
  try
    Conn.Resource := 'Data';
    Conn.Host := '';
    Conn.UserName := 'postgres';
    Conn.Password := 'masterkey';
    Conn.Connect;
    Session := TmncPGSession.Create(Conn);
    ListBox1.Items.Clear;
    try
      Session.Start;
      Cmd := TmncPGCursorCommand.CreateBy(Session);
      try
        if BinaryResultChk.Checked then
          cmd.ResultFormat := mrfBinary;


        Cmd.SQL.Text := 'select "DocID" , "DocUser" ,"DocArchive" ,"DocName" ,"DocCode" from "Documents" order by "DocID"';
        t := GetTickCount;
        if Cmd.Execute then
        begin
          while not Cmd.EOF do
          begin
            for I := 0 to Cmd.Fields.Count - 1 do
              SynEdit1.Lines.Add(Cmd.Fields.Items[i].Column.Name+': '+Cmd.Fields.Items[i].AsString);
            SynEdit1.Lines.Add('-------------------------------');
            Cmd.Next;
          end;
        end;
      finally
        Cmd.Free;
      end;
      SynEdit1.Lines.Add(IntToStr(GetTickCount-t));
    finally
      Session.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
    SynEdit1.EndUpdate;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  Conn: TmncPGConnection;
  Session: TmncPGSession;
  Cmd: TmncPGCommand;
  aOID: OID;
begin
  Conn := ConnectData;
  try
    Session := TmncPGSession.Create(Conn);
    try
      Session.Start;
      Cmd := TmncPGCommand(Session.CreateCommand);
      try
        aOID := lo_import(Conn.Handle, PChar('c:\worldcitiespop.txt'));

        Cmd.SQL.Text := 'update "Test" set "Data" = ?Data where "ID" = 5';
        Cmd.Prepare;
        Cmd.Params['Data'].AsInteger := aOID;
        Cmd.Execute;

        Session.Commit;

      finally
        Cmd.Free;
      end;
    finally
      Session.Free;
    end;
  finally
    Conn.Free;
  end;
end;

function TForm1.ConnectData: TmncPGConnection;
begin
  Result := TmncPGConnection.Create;
  try
    Result.Resource := 'data';
    Result.Host := '';
    Result.UserName := 'postgres';
    Result.Password := 'masterkey';
    Result.Connect;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TForm1.TreeBtnClick(Sender: TObject);
var
  Conn: TmncPGConnection;
  Session: TmncPGSession;
  Cmd: TmncPGCommand;
begin
  Conn := ConnectData;
  try
    Session := TmncPGSession.Create(Conn);
    try
      Session.Start;
      Cmd := TmncPGCommand(Session.CreateCommand);
      try
        Cmd.SQL.Text := 'insert into companies';
        Cmd.SQL.Add('(id, name, nationality)');
        Cmd.SQL.Add('values (?id, ?name, ?nationality)');
        Cmd.Prepare;
        Cmd.Params['id'].AsVariant := Null;
        Cmd.Params['name'].AsString := 'าวๅั';
        Cmd.Params['nationality'].AsInteger := 22;
        Cmd.Execute;

        Session.Commit;
        Cmd.SQL.Text := 'select * from companies';
        Cmd.SQL.Add('where name = ?name');
        Cmd.Prepare;
        Cmd.Param['name'].AsString := 'zaher';
        if Cmd.Execute then
          ShowMessage(Cmd.Field['name'].AsString)
        else
          ShowMessage('not found');

      finally
        Cmd.Free;
      end;
    finally
      Session.Free;
    end;
  finally
    Conn.Free;
  end;
end;

end.
