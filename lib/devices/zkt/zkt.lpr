program zkt;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  //Crt,
  SysUtils,
  Classes,
  zktClients,
  mnStreams;

var
  Client: TZKClient;
  Users: TZKUsers;
  i: Integer;
  procedure ListAttendances;
  var
    Attendances: TZKAttendances;
  begin
    Attendances:=TZKAttendances.Create;
    try
      WriteLn('Number'#9'ID'#9'UserID'#9'Verfied'#9'State'#9'WorkCode'#9'Time');
      if Client.GetAddendances(Attendances) then
      begin
        for i := 0 to Attendances.Count - 1 do
        begin
          WriteLn(IntToStr(i) +#9 + IntToStr(Attendances[i].Number) + #9 + Attendances[i].UserID + #9 + IntToStr(Attendances[i].Verified) + #9 + IntToStr(Attendances[i].State) + #9 + IntToStr(Attendances[i].WorkCode) + #9 + DateTimeToStr(Attendances[i].Time));
        end;
      end;
    finally
      FreeAndNil(Attendances);
    end;
  end;
begin
  Client := TZKClient.Create('192.168.1.201');
  Client.Connect;
  WriteLn(Client.GetVersion);
  Client.DisableDevice;
  //Client.TestVoice;
  try
    ListAttendances;
    WriteLn('Press any to exit');
    ReadLn();

    Users := TZKUsers.Create;
    Client.GetUsers(Users);
    try
      for i := 0 to Users.Count -1 do
      begin
        WriteLn(IntToStr(i) +#9 + IntToStr(Users[i].Number) + #9 + Users[i].Name + #9 + Users[i].Password + #9#9 + Users[i].UserID + #9#9 + Users[i].Card+ #9#9 + IntToStr(Users[i].Group) + #9 + IntToStr(Users[i].Role) + #9 + IntToStr(Users[i].TimeZone));
      end;
    finally
      FreeAndNil(Users);
    end;
  finally
    Client.EnableDevice;
  end;
  Client.Free;
  WriteLn();
  WriteLn('Press any to exit');
  ReadLn();
end.

