unit ciphers;
{**
 *  This file is part of the "MiniLib"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math;

const
  cBufferSize = 512;

type
  TCipherStream = class;
  TCipherStreamClass = class of TCipherStream;

  ECipherException = class(Exception);

  TCipherBuffer = class(TObject)
  private
    FBuffer: PChar;
    FPosition: PChar;
    FEOB: PChar;
    function GetAsString: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeRooms(vCount: Integer);
    procedure WriteBuffer(const vBuffer; vCount: Integer);
    function ReadBuffer(var vBuffer; vCount: Integer): Integer;

    procedure PutChar(vChar: Char);
    procedure IncPos(vCount: Integer=1);
    property Buffer: PChar read FBuffer;
    property Position: PChar read FPosition;
    property EOB: PChar read FEOB; //end of Buffer ...
    property AsString: string read GetAsString;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(FileName: TFileName);
    procedure SetSize(vSize: integer);
    procedure Clear;
    procedure DeleteReaded(vCount: Integer);

    function Count: Integer;
    function Size: Integer;
  end;

  TCipher = class(TObject)
  public
    {
      Because the Encrypted size not same as the original size we make 2 of buffer
      Some Ciphers will create memory for out buffer if you passed nil to OutBuffer
      so it need to free it (OutBuffer) after calling this functions
    }
    procedure Encrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); virtual; abstract;
    procedure Decrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); virtual; abstract;
  end;


  TCipherMode = (cimRead, cimWrite);

  //Create the stream fro Encrypt/Decrypt if mixed with Mode you will have 4 state for that stream
  //most of developer need only 2 state, Write+Encrypt and Read+Decrypt, but not for me :)
  TCipherWay = (cyEncrypt, cyDecrypt);


  TExCipher = class(TObject)
  private
    FMode: TCipherMode;
    FWay: TCipherWay;
    FExDataBuffer: TCipherBuffer;
    FExBuffer: TCipherBuffer;
    function GetBufferCount: Integer;
    function GetDataCount: Integer;
  protected
    function SetSize(var vBuffer: PChar; vSize: Integer): Integer;
    procedure Encrypt(var ReadCount, WriteCount: Integer); virtual; abstract; //result bytes readed from data buffer
    procedure Decrypt(var ReadCount, WriteCount: Integer); virtual; abstract;
    procedure UpdateBuffer; virtual;
    function InternalRead(var Buffer; Count: Longint): Longint; virtual;
    function InternalWrite(const Buffer; Count: Longint): Longint; virtual;
    procedure AddData(const vBuffer; vCount: Longint);
    procedure SetBufferSize(vSize: Integer);
    procedure SetDataBufferSize(vSize: Integer);
    function HasData: Boolean; //use in read mode
  public
    constructor Create(vWay: TCipherWay; vMode: TCipherMode);
    destructor Destroy; override;
    function Read(var vBuffer; vCount: Longint): Longint; virtual;
    function Write(const vBuffer; vCount: Longint): Longint; virtual;
    property Way: TCipherWay read FWay;
    property Mode: TCipherMode read FMode;

    property ExBuffer: TCipherBuffer read FExBuffer;
    property ExDataBuffer: TCipherBuffer read FExDataBuffer;

    property DataCount: Integer read GetDataCount;
    property BufferCount: Integer read GetBufferCount;
  end;

  TExBufferdCipher = class(TExCipher)
  end;

  TExStreamCipher = class(TExCipher)
  private
    FStream: TStream;
  protected
    function InternalRead(var Buffer; Count: Longint): Longint; override;
    function InternalWrite(const Buffer; Count: Longint): Longint; override;
  public
    constructor Create(vStream: TStream; vWay: TCipherWay; vMode: TCipherMode);
    property Stream: TStream read FStream;
  end;

  TCipherStream = class(TStream)
  private
    FStreamOwned: Boolean;
    FStream: TStream;
    FCipherOwned: Boolean;
    FCipher: TCipher;
    FWay: TCipherWay;
    FMode: TCipherMode;
  protected
    procedure SetCipher(const Value: TCipher);
    function GetCipher: TCipher;

    function DoCreateCipher: TCipher; virtual;
    function CreateCipher: TCipher;

    procedure Prepare; virtual; //prepare custom data
    procedure Init; virtual; //init cipher
    procedure Finish; virtual; //init cipher
  public
    //if Owned = true, then AStream automatically destroyed by TCipherStream
    constructor Create(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean = True);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Way: TCipherWay read FWay;
    property Mode: TCipherMode read FMode;
    property Cipher: TCipher read GetCipher write SetCipher;
  end;

  TExCipherStream = class(TStream)
  private
    FStreamOwned: Boolean;
    FStream: TStream;
    FCipherOwned: Boolean;
    FCipher: TExStreamCipher;
    FWay: TCipherWay;
    FMode: TCipherMode;
  protected
    procedure SetCipher(const Value: TExStreamCipher);
    function GetCipher: TExStreamCipher;

    function DoCreateCipher: TExStreamCipher; virtual;
    function CreateCipher: TExStreamCipher;

    procedure Prepare; virtual; //prepare custom data
    procedure Init; virtual; //init cipher
    procedure Finish; virtual; //init cipher
  public
    //if Owned = true, then AStream automatically destroyed by TCipherStream
    constructor Create(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean = True);
    destructor Destroy; override;
    function Read(var vBuffer; vCount: Longint): Longint; override;
    function Write(const vBuffer; vCount: Longint): Longint; override;
    property Way: TCipherWay read FWay;
    property Mode: TCipherMode read FMode;
    property Cipher: TExStreamCipher read GetCipher write SetCipher;
    property Stream: TStream read FStream;
  end;

  TCipherKey = class(TObject)
  public
    constructor Create(KeyString: string); virtual;
  end; 

implementation

{ TCipherStream }

constructor TCipherStream.Create(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean = True);
begin
  inherited Create;
  if AStream = nil then
    raise ECipherException.Create('Stream = nil');
  FStreamOwned := Owned;
  FStream := AStream;
  FWay := Way;
  FMode := Mode;
  FCipher := CreateCipher;

  Prepare;
  Init;
end;

function TCipherStream.CreateCipher: TCipher;
begin
  Result := DoCreateCipher;
  FCipherOwned := Result <> nil;
end;

destructor TCipherStream.Destroy;
begin
  if FStreamOwned then
    FStream.Free;
  Finish;
  FCipher.Free;
  inherited;
end;

function TCipherStream.DoCreateCipher: TCipher;
begin
  Result := nil;
end;

procedure TCipherStream.Finish;
begin

end;

function TCipherStream.GetCipher: TCipher;
begin
  Result := FCipher;
end;

procedure TCipherStream.Init;
begin

end;

procedure TCipherStream.Prepare;
begin

end;

function TCipherStream.Read(var Buffer; Count: Integer): Longint;
begin
  if FMode = cimWrite  then
    raise ECipherException.Create('Stream created for Read');
  Result := FStream.Read(Buffer, Count);
end;

procedure TCipherStream.SetCipher(const Value: TCipher);
begin
  if FCipher <> Value then
  begin
    if FCipherOwned then
      FreeAndNil(FCipher);
    FCipher := Value;
    FCipherOwned := False;
  end;
end;

function TCipherStream.Write(const Buffer; Count: Integer): Longint;
begin
  if FMode = cimRead  then
    raise ECipherException.Create('Stream created for Write');
  Result := FStream.Write(Buffer, Count);
end;

{ TCipherKey }

constructor TCipherKey.Create(KeyString: string);
begin
  inherited Create;
end;


{ TExCipher }

procedure TExCipher.AddData(const vBuffer; vCount: Integer);
//var
  ///p: PChar;
begin
  ExDataBuffer.WriteBuffer(vBuffer, vCount);
  {if vCount<>0 then
  begin
    FDataCount := SetSize(FDataBuffer, vCount+DataPos);
    p := FDataBuffer;
    Inc(p, DataPos);
    Move(vBuffer, p^, vCount);
  end;}
end;

constructor TExCipher.Create(vWay: TCipherWay; vMode: TCipherMode);
begin
  inherited Create;
  FMode := vMode;
  FWay := vWay;

  FExBuffer := TCipherBuffer.Create;
  FExDataBuffer := TCipherBuffer.Create;
end;

destructor TExCipher.Destroy;
begin
  //if datacount <>0 then error some data not process

  FreeAndNil(FExBuffer);
  FreeAndNil(FExDataBuffer);
  inherited;
end;

function TExCipher.GetBufferCount: Integer;
begin
  Result := ExBuffer.Count;
end;

function TExCipher.GetDataCount: Integer;
begin
  Result := ExDataBuffer.Count;
end;

function TExCipher.HasData: Boolean;
var
  aBuffer: string;
  c: Integer;
begin
  if ExBuffer.Count>0 then
    Result := True
  else
  begin
    SetLength(aBuffer, cBufferSize);
    try
      c := InternalRead(aBuffer[1], cBufferSize);
      if c<>0 then
        AddData(aBuffer[1], c);
      UpdateBuffer;
    finally
      SetLength(aBuffer, 0);
    end;
    //Result := (FCount<>0) and (FPos<FCount);
    Result := ExBuffer.Count<>0;
  end;
end;

function TExCipher.InternalRead(var Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

function TExCipher.InternalWrite(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

function TExCipher.Read(var vBuffer; vCount: Integer): Longint;
var
  p: PChar;
  i, c: Integer;
begin
  Result := 0;
  i := vCount;
  p := @vBuffer;
  while (i>0) and HasData do
  begin
    c := ExBuffer.ReadBuffer(p^, i);
    Inc(Result, c);
    Dec(i, c);
    Inc(p, c);
  end;
end;

procedure TExCipher.SetBufferSize(vSize: Integer);
begin
  ExBuffer.MakeRooms(vSize);
end;

procedure TExCipher.SetDataBufferSize(vSize: Integer);
begin
  ExDataBuffer.SetSize(vSize);
end;

function TExCipher.SetSize(var vBuffer: PChar; vSize: Integer): Integer;
begin
  Result := vSize;
  ReallocMem(vBuffer, vSize);
end;

procedure TExCipher.UpdateBuffer;
var
  r, w: Integer;
begin
  if ExDataBuffer.Count<>0 then
  begin
    r := 0;
    w := 0;
    case Way of
      cyEncrypt: Encrypt(r, w);
      cyDecrypt: Decrypt(r, w);
    end;

    ExDataBuffer.DeleteReaded(r); //???????????????????
    ExBuffer.IncPos(w); //??????????????/
  end;
end;

function TExCipher.Write(const vBuffer; vCount: Integer): Longint;
begin
  AddData(vBuffer, vCount);
  UpdateBuffer;
  InternalWrite(ExBuffer.Buffer^, ExBuffer.Count);
  ExBuffer.DeleteReaded(ExBuffer.Count);
  Result := vCount;
end;

{ TExStreamCipher }

constructor TExStreamCipher.Create(vStream: TStream; vWay: TCipherWay; vMode: TCipherMode);
begin
  inherited Create(vWay, vMode);
  FStream := vStream;
end;

function TExStreamCipher.InternalRead(var Buffer; Count: Integer): Longint;
begin
  Result := Stream.read(Buffer, Count);
end;

function TExStreamCipher.InternalWrite(const Buffer; Count: Integer): Longint;
begin
  Result := Stream.Write(Buffer, Count);
end;


{ TexCipherStream }

constructor TexCipherStream.Create(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean = True);
begin
  inherited Create;
  if AStream = nil then
    raise ECipherException.Create('Stream = nil');
  FStreamOwned := Owned;
  FStream := AStream;
  FWay := Way;
  FMode := Mode;
  FCipher := CreateCipher;

  Prepare;
  Init;
end;

function TexCipherStream.CreateCipher: TExStreamCipher;
begin
  Result := DoCreateCipher;
  FCipherOwned := Result <> nil;
end;

destructor TexCipherStream.Destroy;
begin
  if FStreamOwned then
    FStream.Free;
  Finish;
  FCipher.Free;
  inherited;
end;

function TexCipherStream.DoCreateCipher: TExStreamCipher;
begin
  Result := nil;
end;

procedure TexCipherStream.Finish;
begin

end;

function TexCipherStream.GetCipher: TExStreamCipher;
begin
  Result := FCipher;
end;

procedure TexCipherStream.Init;
begin

end;

procedure TexCipherStream.Prepare;
begin

end;

function TexCipherStream.Read(var vBuffer; vCount: Integer): Longint;
begin
  if FMode = cimWrite  then
    raise ECipherException.Create('Stream created for Read');
  Result := Cipher.Read(vBuffer, vCount);
end;

procedure TexCipherStream.SetCipher(const Value: TExStreamCipher);
begin
  if FCipher <> Value then
  begin
    if FCipherOwned then
      FreeAndNil(FCipher);
    FCipher := Value;
    FCipherOwned := False;
  end;
end;

function TexCipherStream.Write(const vBuffer; vCount: Integer): Longint;
begin
  if FMode = cimRead  then
    raise ECipherException.Create('Stream created for Write');
  Result := Cipher.Write(vBuffer, vCount);
end;

const
  cDefaultAlloc = 2048;

procedure TCipherBuffer.DeleteReaded(vCount: Integer);
var
  t: PChar;
  c: Integer;
begin
  if vCount=Count then
    FPosition := FBuffer
  else if vCount<>0 then
  begin
    t := Buffer;
    Inc(t, vCount);
    Move(t^, Buffer^, Position-t);
    Dec(FPosition, vCount);
  end;
end;

destructor TCipherBuffer.Destroy;
begin
  Clear;
  inherited;
end;

function TCipherBuffer.GetAsString: string;
begin
  SetString(Result, Buffer, EOB - Buffer);
end;

procedure TCipherBuffer.IncPos(vCount: Integer);
begin
  Inc(FPosition, vCount);
end;

procedure TCipherBuffer.MakeRooms(vCount: Integer);
var
  c, s, x: Integer;
begin
  if (vCount<>0) then
  begin
    c := Count;
    s := Size;
    if (vCount>(s-c)) then
    begin
      x := ((vCount div cDefaultAlloc)+1) * cDefaultAlloc;
        
      ReallocMem(FBuffer, s + x);
      FPosition := FBuffer + c;
      FEOB      := FBuffer + s + vCount;
    end;
  end;
end;

procedure TCipherBuffer.PutChar(vChar: Char);
begin
  FPosition^ := vChar;
  Inc(FPosition);
end;

function TCipherBuffer.ReadBuffer(var vBuffer; vCount: Integer): Integer;
var
  p: PChar;
begin
  Result := Min(vCount, Count);
  if Result<>0 then
  begin
    p := @vBuffer;
    Move(FBuffer^, p^, Result);
    DeleteReaded(Result);
  end;
end;

procedure TCipherBuffer.SaveToFile(FileName: TFileName);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(f);
  finally
    f.Free;
  end;
end;

procedure TCipherBuffer.SaveToStream(Stream: TStream);
begin
  Stream.write(Buffer^, Position - Buffer);
end;

procedure TCipherBuffer.SetSize(vSize: integer);
begin
  if (vSize>0) and (vSize>Size) then
  begin
    ReallocMem(FBuffer, vSize);
    FPosition := FBuffer;
    FEOB := FBuffer + vSize;
  end
  else if vSize=0 then
    Clear;
end;

function TCipherBuffer.Size: Integer;
begin
  if FBuffer<>nil then
    Result := EOB - Buffer
  else
    Result := 0;
end;

procedure TCipherBuffer.WriteBuffer(const vBuffer; vCount: Integer);
var
  p, t: PChar;
begin
  if (vCount<>0) then //append buffer
  begin
    MakeRooms(vCount);

    p := @vBuffer;
    t := Position;
    Move(p^, t^, vCount);
    Inc(FPosition, vCount);
  end;
end;

procedure TCipherBuffer.Clear;
begin
  FreeMem(FBuffer, Size);
  FBuffer := nil;
  FEOB := nil;
  FPosition := nil;
end;

function TCipherBuffer.Count: Integer;
begin
  if FBuffer<>nil then
    Result := Position-Buffer
  else
    Result := 0;
end;

constructor TCipherBuffer.Create;
begin
  inherited Create;
  FBuffer := nil;
  FEOB := nil;
  FPosition := nil;

  //SetSize(cBufferSize*3);
end;

end.

