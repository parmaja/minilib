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
  Classes, SysUtils;

type
  TCipherStream = class;
  TCipherStreamClass = class of TCipherStream;

  ECipherException = class(Exception);

  TCipher = class(TObject)
  public
    {
      Because the Encrypted size not same as the original size we 2 of buffer
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
  inherited;
end;

function TCipherStream.DoCreateCipher: TCipher;
begin
  Result := nil;
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

end.

