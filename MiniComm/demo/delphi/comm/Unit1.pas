unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  mnCommClasses, mnCommStreams, mnCommThreads;

type
  TmnMyCommThread = class(TmnCommStreamThread)
  protected
    FBuffer: string;
    procedure DoStringArrived; override;
  public
  end;

  TForm1 = class(TForm)
    PortEdit: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Edit2: TEdit;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    FCancel: Boolean;
    FCommStream: TmnCommStream;
    FThread : TmnCommThread;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  aStream: TmnCommStream;
  Buf, s : string;
  ComEvents: TComEvents;
  aStrings:TStringList;
  c: Integer;
begin
  FCancel := False;
  aStream := TmnCommStream.Create(True, PortEdit.Text, 9600);
  try
    aStream.ReadTimeout := 2;
    aStream.Timeout := 10000;
    aStream.Connect;
    while True do
    begin
      ComEvents := aStream.WaitEvent([evRxChar]);
      if ComEvents <> [] then
      begin
        SetLength(s, 255);
        c := aStream.Read(S[1], 255);
        SetLength(s, c);
        Buf := Buf + s;
        Memo1.Lines.Add(s);
      end;
      Application.ProcessMessages;
      if FCancel then
        break;
    end
  finally
    aStrings:=TStringList.Create;
    aStrings.Text := Buf;
    aStrings.SaveToFile('c:\com.log');
    aStream.Free;
    FCancel := False;
  end
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  aStream: TmnCommStream;
begin
  aStream := TmnCommStream.Create(True, PortEdit.Text, 9600);
  try
    aStream.Connect;
    aStream.WriteString('Hello Terminals'#13#10);
  finally
    aStream.Free;
  end
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if FThread = nil then
  begin
    FCommStream := TmnCommStream.Create(True, PortEdit.Text, 9600);
    FCommStream.EventChar := #10;
//    FCommStream.ReadTimeout := 10; or use QueMode
    FCommStream.QueMode := True;
    FCommStream.Connect;
    FThread := TmnMyCommThread.Create(True, FCommStream);
    FThread.FreeOnTerminate := True;
    FThread.Resume;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread := nil;
  end;
  FCancel := True;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  aStream: TmnCommStream;
  Buf, s : string;
  ComEvents: TComEvents;
  aStrings:TStringList;
  c: Integer;
begin
  FCancel := False;
  aStream := TmnCommStream.Create(True, PortEdit.Text, 9600);
  try
    aStream.UseOverlapped := False;
    aStream.EventChar := #13;
//    aStream.ReadTimeout := 10; or use QueMode
    aStream.QueMode := True;
    aStream.Connect;
    while True do
    begin
      ComEvents := aStream.WaitEvent([evRxFlag]);
      if ComEvents <> [] then
      begin
        SetLength(s, 255);
        c := aStream.Read(S[1], 255);
        SetLength(s, c);
        Buf := Buf + s;
        Memo1.Lines.Add(s);
      end;
      Application.ProcessMessages;
      if FCancel then
        break;
    end
  finally
    aStrings:=TStringList.Create;
    aStrings.Text := Buf;
    aStrings.SaveToFile('c:\com.log');
    aStream.Free;
    FCancel := False;
  end
end;

{ TmnMyCommThread }

procedure TmnMyCommThread.DoStringArrived;
begin
  inherited;
  Form1.Memo1.Lines.Add(Trim(FBuffer));
end;

end.
