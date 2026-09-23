unit mnOpenAI;

{ OpenAI v1 compatible client for OpenAI, LM Studio and local servers.

  The typed helpers cover the most common endpoints.  Request/Get/Post/Delete
  expose the remaining provider-specific OpenAI-compatible endpoints without
  tying applications to a particular server implementation.
}

{$H+}{$M+}
{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  SysUtils, Classes, mnHttpClient;

type
  TmnOpenAIStringArray = array of string;

  TmnOpenAIModel = record
    ID: string;
    ObjectName: string;
    OwnedBy: string;
    Created: Int64;
  end;

  TmnOpenAIModels = array of TmnOpenAIModel;

  TmnOpenAIImage = record
    URL: string;
    Base64JSON: UTF8String;
    RevisedPrompt: string;
  end;

  TmnOpenAIImages = array of TmnOpenAIImage;

  EmnOpenAIError = class(Exception)
  private
    FStatusCode: Integer;
    FResponseBody: UTF8String;
  public
    constructor Create(const AMessage: string; AStatusCode: Integer;
      const AResponseBody: UTF8String);
    property StatusCode: Integer read FStatusCode;
    property ResponseBody: UTF8String read FResponseBody;
  end;

  TmnOpenAIClient = class
  private
    FBaseURL: string;
    FAPIKey: string;
    FModel: string;
    FOrganization: string;
    FProject: string;
    FUserAgent: string;
    FConnectTimeout: Integer;
    FReadTimeout: Integer;
    FWriteTimeout: Integer;
    FLastStatusCode: Integer;
    FLastResponse: UTF8String;
    procedure SetBaseURL(const Value: string);
    function NormalizeBaseURL(const AURL: string): string;
    function EndpointURL(const AEndpoint: string): string;
    function NewHTTPClient: TmnHttpClient;
    procedure ApplyHeaders(AHTTP: TmnHttpClient; const AContentType: string);
    function ReadResponse(AHTTP: TmnHttpClient): UTF8String;
    procedure CheckResponse(AHTTP: TmnHttpClient; const ABody: UTF8String);
    procedure ClearLastResponse;
    function ExtractErrorMessage(const ABody: UTF8String): string;
    function ResolveModel(const AModel: string): string;
    function MimeTypeForFile(const AFileName: string): string;
    function FileToBase64(const AFileName: string): UTF8String;
    function BuildDataURL(const AFileName: string): UTF8String;
    procedure AddMultipartOptions(AStream: TStream; const ABoundary: string;
      const AOptionsJSON: UTF8String);
    function MultipartRequest(const AEndpoint: string; AStream: TMemoryStream;
      const ABoundary: string): UTF8String;
  public
    constructor Create(const ABaseURL: string = 'https://api.openai.com';
      const AAPIKey: string = '');

    { Generic access to every OpenAI-compatible v1 endpoint.  AEndpoint can be
      'models', '/models', '/v1/models', or an absolute URL. }
    function Request(const AMethod, AEndpoint: string; const ABody: UTF8String = '';
      const AContentType: string = 'application/json'): UTF8String;
    function Get(const AEndpoint: string): UTF8String;
    function Post(const AEndpoint: string; const ABody: UTF8String;
      const AContentType: string = 'application/json'): UTF8String;
    function Delete(const AEndpoint: string; const ABody: UTF8String = ''): UTF8String;

    function ListModels: TmnOpenAIModels;
    function ListModelIDs: TmnOpenAIStringArray;
    function RetrieveModel(const AModelID: string): TmnOpenAIModel;
    function DeleteModel(const AModelID: string): UTF8String;

    function ChatCompletion(const AMessagesJSON: UTF8String;
      const AModel: string = ''; const AOptionsJSON: UTF8String = ''): UTF8String;
    function Chat(const APrompt: string; const ASystemPrompt: string = '';
      const AModel: string = ''): string;
    function Completion(const APrompt: string; const AModel: string = '';
      const AOptionsJSON: UTF8String = ''): UTF8String;
    function CreateResponse(const AInputJSON: UTF8String;
      const AModel: string = ''; const AOptionsJSON: UTF8String = ''): UTF8String;
    function Respond(const APrompt: string; const AModel: string = ''): string;
    function Embeddings(const AInputJSON: UTF8String;
      const AModel: string = ''; const AOptionsJSON: UTF8String = ''): UTF8String;
    function Moderations(const AInputJSON: UTF8String;
      const AModel: string = ''; const AOptionsJSON: UTF8String = ''): UTF8String;

    { Vision: the picture is embedded as a data URL, so no multipart upload is
      needed and the same method works with OpenAI and compatible local servers. }
    function ChatWithImage(const APrompt, AImageFileName: string;
      const AModel: string = ''; const ADetail: string = 'auto'): string;

    function GenerateImages(const APrompt: string; const AModel: string = '';
      const ASize: string = ''; ACount: Integer = 1;
      const AResponseFormat: string = ''): TmnOpenAIImages;
    function GenerateImage(const APrompt, AOutputFileName: string;
      const AModel: string = ''; const ASize: string = ''): TmnOpenAIImage;
    function DownloadFile(const AURL, AFileName: string): Int64;

    { Images/edits and audio endpoints use multipart/form-data. }
    function EditImages(const APrompt, AImageFileName: string;
      const AMaskFileName: string = ''; const AModel: string = '';
      const ASize: string = ''; ACount: Integer = 1;
      const AResponseFormat: string = ''): TmnOpenAIImages;
    function AudioTranscription(const AFileName: string; const AModel: string = 'whisper-1';
      const AOptionsJSON: UTF8String = ''): UTF8String;
    function AudioTranslation(const AFileName: string; const AModel: string = 'whisper-1';
      const AOptionsJSON: UTF8String = ''): UTF8String;
    function AudioSpeech(const AInput, AVoice, AOutputFileName: string;
      const AModel: string = 'gpt-4o-mini-tts'; const AFormat: string = 'mp3'): Int64;

    property BaseURL: string read FBaseURL write SetBaseURL;
    property APIKey: string read FAPIKey write FAPIKey;
    property Model: string read FModel write FModel;
    property Organization: string read FOrganization write FOrganization;
    property Project: string read FProject write FProject;
    property UserAgent: string read FUserAgent write FUserAgent;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
    property WriteTimeout: Integer read FWriteTimeout write FWriteTimeout;
    property LastStatusCode: Integer read FLastStatusCode;
    property LastResponse: UTF8String read FLastResponse;
  end;

implementation

uses
  StrUtils, mnBase64, mnDON, mnJSON, mnUtils, mnModules;

function IsAbsoluteURL(const S: string): Boolean;
begin
  Result := StartsText('http://', S) or StartsText('https://', S);
end;

function JSONQuote(const S: string): UTF8String;
begin
  Result := UTF8String('"' + EscapeJSONString(S) + '"');
end;

function EncodePathSegment(const S: string): string;
const
  Hex: array[0..15] of Char = '0123456789ABCDEF';
var
  U: UTF8String;
  I: Integer;
  B: Byte;
begin
  U := UTF8String(S);
  Result := '';
  for I := 1 to Length(U) do
  begin
    B := Byte(U[I]);
    if ((B >= Ord('a')) and (B <= Ord('z'))) or
       ((B >= Ord('A')) and (B <= Ord('Z'))) or
       ((B >= Ord('0')) and (B <= Ord('9'))) or
       (B = Ord('-')) or (B = Ord('_')) or (B = Ord('.')) or (B = Ord('~')) then
      Result := Result + Char(B)
    else
      Result := Result + '%' + Hex[B shr 4] + Hex[B and $0F];
  end;
end;

function JSONText(const S: UTF8String): UTF8String;
begin
  if Trim(string(S)) = '' then
    Result := 'null'
  else
    Result := S;
end;

function ParseJSONValue(const S: UTF8String;
  const ADescription: string): TDON_Value;
begin
  try
    Result := JsonParseValueString(string(S));
  except
    on E: Exception do
      raise Exception.CreateFmt('Invalid %s JSON: %s', [ADescription, E.Message]);
  end;
  if Result = nil then
    raise Exception.CreateFmt('Invalid %s JSON', [ADescription]);
end;

procedure ValidateJSON(const S: UTF8String; const ADescription: string);
var
  Value: TDON_Value;
begin
  Value := ParseJSONValue(S, ADescription);
  Value.Free;
end;

function MergeJSONObject(const ARequired, AOptions: UTF8String): UTF8String;
var
  S: string;
  Value: TDON_Value;
begin
  S := Trim(string(AOptions));
  if (S = '') or (S = '{}') then
    Exit(ARequired);
  Value := ParseJSONValue(UTF8String(S), 'OpenAI options');
  try
    if not (Value is TDON_Object) then
      raise Exception.Create('OpenAI options must be a JSON object');
  finally
    Value.Free;
  end;
  if (Length(S) < 2) or (S[1] <> '{') or (S[Length(S)] <> '}') then
    raise Exception.Create('OpenAI options must be a JSON object');
  S := Trim(Copy(S, 2, Length(S) - 2));
  if S = '' then
    Result := ARequired
  else
    Result := UTF8String(Copy(string(ARequired), 1, Length(string(ARequired)) - 1) + ',' + S + '}');
end;

function JSONValue(AValue: TDON_Value; const AName: string): TDON_Value;
begin
  if AValue = nil then
    Result := nil
  else
    Result := AValue.Values[AName];
end;

function JSONString(AValue: TDON_Value; const AName: string): string;
var
  V: TDON_Value;
begin
  V := JSONValue(AValue, AName);
  if (V = nil) or V.IsNull then
    Result := ''
  else
    Result := V.AsString;
end;

function JSONInt64(AValue: TDON_Value; const AName: string): Int64;
var
  V: TDON_Value;
begin
  V := JSONValue(AValue, AName);
  if (V = nil) or V.IsNull then
    Result := 0
  else
    Result := StrToInt64Def(V.AsString, 0);
end;

function FirstArray(AValue: TDON_Value; const AName: string): TDON_Array;
var
  V: TDON_Value;
begin
  V := JSONValue(AValue, AName);
  if V is TDON_Array then
    Result := TDON_Array(V)
  else
    Result := nil;
end;

function StreamToUTF8(AStream: TMemoryStream): UTF8String;
begin
  SetLength(Result, AStream.Size);
  if AStream.Size > 0 then
    Move(AStream.Memory^, Result[1], AStream.Size);
end;

function ReadFileBytes(const AFileName: string): UTF8String;
var
  S: TFileStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, S.Size);
    if S.Size > 0 then
      S.ReadBuffer(Result[1], S.Size);
  finally
    S.Free;
  end;
end;

function SafeFileName(const AFileName: string): string;
begin
  Result := StringReplace(ExtractFileName(AFileName), '"', '', [rfReplaceAll]);
end;

function MakeBoundary: string;
begin
  Result := '----------------mnOpenAI' + IntToHex(Random(MaxInt), 8) +
    IntToHex(Random(MaxInt), 8);
end;

procedure WriteUTF8(AStream: TStream; const S: UTF8String);
begin
  if Length(S) > 0 then
    AStream.WriteBuffer(S[1], Length(S));
end;

procedure AddMultipartField(AStream: TStream; const Boundary, AName, AValue: string);
begin
  WriteUTF8(AStream, UTF8String('--' + Boundary + #13#10));
  WriteUTF8(AStream, UTF8String('Content-Disposition: form-data; name="' + AName + '"' + #13#10#13#10));
  WriteUTF8(AStream, UTF8String(AValue));
  WriteUTF8(AStream, UTF8String(#13#10));
end;

procedure AddMultipartFile(AStream: TStream; const Boundary, AName, AFileName,
  AContentType: string);
var
  F: TFileStream;
begin
  WriteUTF8(AStream, UTF8String('--' + Boundary + #13#10));
  WriteUTF8(AStream, UTF8String('Content-Disposition: form-data; name="' + AName +
    '"; filename="' + SafeFileName(AFileName) + '"' + #13#10));
  WriteUTF8(AStream, UTF8String('Content-Type: ' + AContentType + #13#10#13#10));
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    AStream.CopyFrom(F, 0);
  finally
    F.Free;
  end;
  WriteUTF8(AStream, UTF8String(#13#10));
end;

function MultipartValueText(AValue: TDON_Value): string;
var
  FormatSettings: TFormatSettings;
begin
  if (AValue = nil) or AValue.IsNull then
    Result := ''
  else if AValue is TDON_Object then
    JsonSaveString(TDON_Object(AValue), Result)
  else if AValue is TDON_Array then
    JsonSaveString(TDON_Array(AValue), Result)
  else if AValue is TDON_Number then
  begin
    FormatSettings := TFormatSettings.Create;
    FormatSettings.DecimalSeparator := '.';
    Result := FloatToStr(TDON_Number(AValue).Value, FormatSettings);
  end
  else if AValue is TDON_Boolean then
  begin
    if TDON_Boolean(AValue).Value then
      Result := 'true'
    else
      Result := 'false';
  end
  else
    Result := AValue.AsString;
end;

constructor EmnOpenAIError.Create(const AMessage: string; AStatusCode: Integer;
  const AResponseBody: UTF8String);
begin
  inherited Create(AMessage);
  FStatusCode := AStatusCode;
  FResponseBody := AResponseBody;
end;

constructor TmnOpenAIClient.Create(const ABaseURL, AAPIKey: string);
begin
  inherited Create;
  SetBaseURL(ABaseURL);
  FAPIKey := AAPIKey;
  FUserAgent := 'MiniLib mnOpenAI/1.0';
  FConnectTimeout := 15000;
  FReadTimeout := 120000;
  FWriteTimeout := 120000;
end;

procedure TmnOpenAIClient.SetBaseURL(const Value: string);
begin
  FBaseURL := NormalizeBaseURL(Value);
end;

function TmnOpenAIClient.NormalizeBaseURL(const AURL: string): string;
begin
  Result := Trim(AURL);
  while (Length(Result) > 0) and (Result[Length(Result)] = '/') do
    System.Delete(Result, Length(Result), 1);
  if Result = '' then
    Result := 'https://api.openai.com';
end;

function TmnOpenAIClient.EndpointURL(const AEndpoint: string): string;
var
  E, B, Suffix: string;
  P, QueryPos, FragmentPos: Integer;
begin
  E := Trim(AEndpoint);
  if IsAbsoluteURL(E) then
    Exit(E);

  QueryPos := Pos('?', E);
  FragmentPos := Pos('#', E);
  if (QueryPos = 0) or ((FragmentPos > 0) and (FragmentPos < QueryPos)) then
    P := FragmentPos
  else
    P := QueryPos;
  if P > 0 then
  begin
    Suffix := Copy(E, P, MaxInt);
    E := Copy(E, 1, P - 1);
  end
  else
    Suffix := '';

  B := NormalizeBaseURL(FBaseURL);
  if StartsText('/v1/', E) or SameText(E, '/v1') then
  begin
    // A leading /v1 path is rooted at the provider origin.  Preserve any
    // query/fragment, but drop a base path such as /proxy/v1.
    P := Pos('://', B);
    if P > 0 then
    begin
      P := PosEx('/', B, P + 3);
      if P > 0 then
        System.Delete(B, P, MaxInt);
    end
    else if EndsText('/v1', B) then
      System.Delete(B, Length(B) - 2, 3);
  end
  else
  begin
    while (Length(E) > 0) and (E[1] = '/') do
      System.Delete(E, 1, 1);
    if (E = '') and EndsText('/v1', B) then
      Exit(B + Suffix);
    if SameText(E, 'v1') then
    begin
      if EndsText('/v1', B) then
        Exit(B + Suffix)
      else
        Exit(B + '/v1' + Suffix);
    end;
    if not EndsText('/v1', B) then
      B := B + '/v1';
    E := '/' + E;
  end;
  Result := B + E + Suffix;
end;

function TmnOpenAIClient.NewHTTPClient: TmnHttpClient;
begin
  // BIO supplies TLS for cloud endpoints and falls back to the inherited
  // plain socket implementation for http:// URLs.
  Result := TmnBIOHttpClient.Create;
  Result.AutoClearHeaders := True;
  Result.Request.Use.AcceptCompressing := ovNo;
  Result.Request.Use.KeepAlive := ovNo;
  Result.ConnectTimeout := FConnectTimeout;
  Result.ReadTimeout := FReadTimeout;
  Result.WriteTimeout := FWriteTimeout;
end;

procedure TmnOpenAIClient.ApplyHeaders(AHTTP: TmnHttpClient;
  const AContentType: string);
begin
  AHTTP.Request.PutHeader('Accept', 'application/json');
  if AContentType <> '' then
    AHTTP.Request.PutHeader('Content-Type', AContentType);
  if FUserAgent <> '' then
    AHTTP.Request.UserAgent := UTF8String(FUserAgent);
  if FAPIKey <> '' then
    AHTTP.Request.PutHeader('Authorization', 'Bearer ' + FAPIKey);
  if FOrganization <> '' then
    AHTTP.Request.PutHeader('OpenAI-Organization', FOrganization);
  if FProject <> '' then
    AHTTP.Request.PutHeader('OpenAI-Project', FProject);
end;

function TmnOpenAIClient.ReadResponse(AHTTP: TmnHttpClient): UTF8String;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    AHTTP.ReceiveStream(M);
    Result := StreamToUTF8(M);
  finally
    M.Free;
  end;
end;

function TmnOpenAIClient.ExtractErrorMessage(const ABody: UTF8String): string;
var
  Root, Err: TDON_Value;
begin
  Result := '';
  Root := nil;
  try
    try
      Root := JsonParseValueString(string(ABody), [jsoSafe]);
    except
      Root := nil;
    end;
    Err := JSONValue(Root, 'error');
    if Err <> nil then
    begin
      Result := JSONString(Err, 'message');
      if Result = '' then
        Result := Err.AsString;
    end;
    if Result = '' then
      Result := JSONString(Root, 'message');
  finally
    Root.Free;
  end;
  if Result = '' then
    Result := Copy(string(ABody), 1, 1000);
end;

procedure TmnOpenAIClient.CheckResponse(AHTTP: TmnHttpClient;
  const ABody: UTF8String);
var
  Msg: string;
begin
  FLastStatusCode := AHTTP.Response.StatusCode;
  FLastResponse := ABody;
  if (FLastStatusCode < 200) or (FLastStatusCode >= 300) then
  begin
    Msg := ExtractErrorMessage(ABody);
    if Msg = '' then
      Msg := 'HTTP error';
    raise EmnOpenAIError.Create(Format('OpenAI request failed (%d): %s',
      [FLastStatusCode, Msg]), FLastStatusCode, ABody);
  end;
end;

procedure TmnOpenAIClient.ClearLastResponse;
begin
  FLastStatusCode := 0;
  FLastResponse := '';
end;

function TmnOpenAIClient.Request(const AMethod, AEndpoint: string;
  const ABody: UTF8String; const AContentType: string): UTF8String;
var
  HTTP: TmnHttpClient;
  Method: string;
  OK: Boolean;
begin
  ClearLastResponse;
  HTTP := NewHTTPClient;
  try
    ApplyHeaders(HTTP, AContentType);
    Method := UpperCase(Trim(AMethod));
    if Method = '' then
      raise Exception.Create('OpenAI HTTP method is empty');
    OK := HTTP.Execute(UTF8String(EndpointURL(AEndpoint)), UTF8String(Method), ABody);
    if not OK then
      raise Exception.Create('OpenAI request failed');
    Result := ReadResponse(HTTP);
    CheckResponse(HTTP, Result);
  finally
    HTTP.Disconnect;
    HTTP.Free;
  end;
end;

function TmnOpenAIClient.Get(const AEndpoint: string): UTF8String;
begin
  Result := Request('GET', AEndpoint, '', '');
end;

function TmnOpenAIClient.Post(const AEndpoint: string; const ABody: UTF8String;
  const AContentType: string): UTF8String;
begin
  Result := Request('POST', AEndpoint, ABody, AContentType);
end;

function TmnOpenAIClient.Delete(const AEndpoint: string; const ABody: UTF8String): UTF8String;
begin
  Result := Request('DELETE', AEndpoint, ABody, 'application/json');
end;

function TmnOpenAIClient.ListModels: TmnOpenAIModels;
var
  Body: UTF8String;
  Root: TDON_Value;
  Data: TDON_Array;
  I: Integer;
begin
  SetLength(Result, 0);
  Body := Get('models');
  Root := ParseJSONValue(Body, 'models response');
  try
    Data := FirstArray(Root, 'data');
    if (Data = nil) and (Root is TDON_Array) then
      Data := TDON_Array(Root);
    if Data = nil then
      raise Exception.Create('Models response does not contain a data array');
    SetLength(Result, Data.Count);
    for I := 0 to Data.Count - 1 do
    begin
      Result[I].ID := JSONString(Data[I], 'id');
      Result[I].ObjectName := JSONString(Data[I], 'object');
      Result[I].OwnedBy := JSONString(Data[I], 'owned_by');
      Result[I].Created := JSONInt64(Data[I], 'created');
    end;
  finally
    Root.Free;
  end;
end;

function TmnOpenAIClient.ListModelIDs: TmnOpenAIStringArray;
var
  Models: TmnOpenAIModels;
  I: Integer;
begin
  Models := ListModels;
  SetLength(Result, Length(Models));
  for I := 0 to High(Models) do
    Result[I] := Models[I].ID;
end;

function TmnOpenAIClient.RetrieveModel(const AModelID: string): TmnOpenAIModel;
var
  Root: TDON_Value;
begin
  FillChar(Result, SizeOf(Result), 0);
  Root := ParseJSONValue(Get('models/' + EncodePathSegment(AModelID)),
    'model response');
  try
    Result.ID := JSONString(Root, 'id');
    Result.ObjectName := JSONString(Root, 'object');
    Result.OwnedBy := JSONString(Root, 'owned_by');
    Result.Created := JSONInt64(Root, 'created');
  finally
    Root.Free;
  end;
end;

function TmnOpenAIClient.DeleteModel(const AModelID: string): UTF8String;
begin
  Result := Delete('models/' + EncodePathSegment(AModelID));
end;

function TmnOpenAIClient.ResolveModel(const AModel: string): string;
begin
  if AModel <> '' then
    Result := AModel
  else
    Result := FModel;
  if Result = '' then
    raise Exception.Create('OpenAI model is not defined');
end;

function TmnOpenAIClient.ChatCompletion(const AMessagesJSON: UTF8String;
  const AModel: string; const AOptionsJSON: UTF8String): UTF8String;
var
  Body: UTF8String;
begin
  ValidateJSON(AMessagesJSON, 'messages');
  Body := UTF8String('{"model":') + JSONQuote(ResolveModel(AModel)) +
    ',"messages":' + JSONText(AMessagesJSON) + '}';
  Result := Post('chat/completions', MergeJSONObject(Body, AOptionsJSON));
end;

function TmnOpenAIClient.Chat(const APrompt, ASystemPrompt, AModel: string): string;
var
  Messages, Body: UTF8String;
  Root, Choices, MessageValue: TDON_Value;
begin
  Messages := '[';
  if ASystemPrompt <> '' then
    Messages := Messages + '{"role":"system","content":' + JSONQuote(ASystemPrompt) + '},';
  Messages := Messages + '{"role":"user","content":' + JSONQuote(APrompt) + '}]';
  Body := ChatCompletion(Messages, AModel);
  Root := ParseJSONValue(Body, 'chat response');
  try
    Choices := JSONValue(Root, 'choices');
    if not (Choices is TDON_Array) or (TDON_Array(Choices).Count = 0) then
      raise Exception.Create('Chat response does not contain choices');
    MessageValue := JSONValue(TDON_Array(Choices)[0], 'message');
    Result := JSONString(MessageValue, 'content');
  finally
    Root.Free;
  end;
end;

function TmnOpenAIClient.Completion(const APrompt, AModel: string;
  const AOptionsJSON: UTF8String): UTF8String;
var
  Body: UTF8String;
begin
  Body := UTF8String('{"model":') + JSONQuote(ResolveModel(AModel)) +
    ',"prompt":' + JSONQuote(APrompt) + '}';
  Result := Post('completions', MergeJSONObject(Body, AOptionsJSON));
end;

function TmnOpenAIClient.CreateResponse(const AInputJSON: UTF8String;
  const AModel: string; const AOptionsJSON: UTF8String): UTF8String;
var
  Body: UTF8String;
begin
  ValidateJSON(AInputJSON, 'response input');
  Body := UTF8String('{"model":') + JSONQuote(ResolveModel(AModel)) +
    ',"input":' + JSONText(AInputJSON) + '}';
  Result := Post('responses', MergeJSONObject(Body, AOptionsJSON));
end;

function TmnOpenAIClient.Respond(const APrompt, AModel: string): string;
var
  Root, Output, Content: TDON_Value;
  I, J: Integer;
begin
  Root := ParseJSONValue(CreateResponse(JSONQuote(APrompt), AModel),
    'responses result');
  try
    Result := JSONString(Root, 'output_text');
    if Result <> '' then
      Exit;
    Output := JSONValue(Root, 'output');
    if Output is TDON_Array then
      for I := 0 to TDON_Array(Output).Count - 1 do
      begin
        Content := JSONValue(TDON_Array(Output)[I], 'content');
        if Content is TDON_Array then
          for J := 0 to TDON_Array(Content).Count - 1 do
          begin
            Result := JSONString(TDON_Array(Content)[J], 'text');
            if Result <> '' then
              Exit;
          end;
      end;
    raise Exception.Create('Responses result does not contain output text');
  finally
    Root.Free;
  end;
end;

function TmnOpenAIClient.Embeddings(const AInputJSON: UTF8String;
  const AModel: string; const AOptionsJSON: UTF8String): UTF8String;
var
  Body: UTF8String;
begin
  ValidateJSON(AInputJSON, 'embedding input');
  Body := UTF8String('{"model":') + JSONQuote(ResolveModel(AModel)) +
    ',"input":' + JSONText(AInputJSON) + '}';
  Result := Post('embeddings', MergeJSONObject(Body, AOptionsJSON));
end;

function TmnOpenAIClient.Moderations(const AInputJSON: UTF8String;
  const AModel: string; const AOptionsJSON: UTF8String): UTF8String;
var
  Body: UTF8String;
begin
  ValidateJSON(AInputJSON, 'moderation input');
  Body := UTF8String('{"input":') + JSONText(AInputJSON);
  if (AModel <> '') or (FModel <> '') then
    Body := Body + ',"model":' + JSONQuote(ResolveModel(AModel));
  Body := Body + '}';
  Result := Post('moderations', MergeJSONObject(Body, AOptionsJSON));
end;

function TmnOpenAIClient.MimeTypeForFile(const AFileName: string): string;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  if (Ext = '.jpg') or (Ext = '.jpeg') then Result := 'image/jpeg'
  else if Ext = '.png' then Result := 'image/png'
  else if Ext = '.gif' then Result := 'image/gif'
  else if Ext = '.webp' then Result := 'image/webp'
  else if Ext = '.wav' then Result := 'audio/wav'
  else if Ext = '.mp3' then Result := 'audio/mpeg'
  else if (Ext = '.mpeg') or (Ext = '.mpga') then Result := 'audio/mpeg'
  else if Ext = '.m4a' then Result := 'audio/mp4'
  else if Ext = '.mp4' then Result := 'audio/mp4'
  else if Ext = '.flac' then Result := 'audio/flac'
  else if Ext = '.ogg' then Result := 'audio/ogg'
  else if Ext = '.webm' then Result := 'audio/webm'
  else Result := 'application/octet-stream';
end;

function TmnOpenAIClient.FileToBase64(const AFileName: string): UTF8String;
begin
  Result := Base64Encode(ReadFileBytes(AFileName));
end;

function TmnOpenAIClient.BuildDataURL(const AFileName: string): UTF8String;
begin
  Result := UTF8String('data:' + MimeTypeForFile(AFileName) + ';base64,') +
    FileToBase64(AFileName);
end;

procedure TmnOpenAIClient.AddMultipartOptions(AStream: TStream;
  const ABoundary: string; const AOptionsJSON: UTF8String);
var
  Root: TDON_Value;
  Obj: TDON_Object;
  I: Integer;
begin
  if Trim(string(AOptionsJSON)) = '' then
    Exit;
  Root := ParseJSONValue(AOptionsJSON, 'OpenAI multipart options');
  if not (Root is TDON_Object) then
  begin
    Root.Free;
    raise Exception.Create('OpenAI multipart options must be a JSON object');
  end;
  try
    Obj := TDON_Object(Root);
    for I := 0 to Obj.Count - 1 do
    begin
      if SameText(Obj.Items[I].Name, 'file') or
         SameText(Obj.Items[I].Name, 'model') then
        raise Exception.CreateFmt('Multipart option "%s" is reserved',
          [Obj.Items[I].Name]);
      AddMultipartField(AStream, ABoundary, Obj.Items[I].Name,
        MultipartValueText(Obj.Items[I].Value));
    end;
  finally
    Root.Free;
  end;
end;

function TmnOpenAIClient.MultipartRequest(const AEndpoint: string;
  AStream: TMemoryStream; const ABoundary: string): UTF8String;
var
  HTTP: TmnHttpClient;
begin
  ClearLastResponse;
  HTTP := NewHTTPClient;
  try
    ApplyHeaders(HTTP, 'multipart/form-data; boundary=' + ABoundary);
    if not HTTP.Connect(UTF8String(EndpointURL(AEndpoint))) then
      raise Exception.Create('OpenAI connection failed');
    AStream.Position := 0;
    if (AStream.Size > MaxInt) then
      raise Exception.Create('Multipart request is too large for this HTTP client');
    if not HTTP.Execute('POST', PByte(AStream.Memory), Integer(AStream.Size)) then
      raise Exception.Create('OpenAI request failed');
    Result := ReadResponse(HTTP);
    CheckResponse(HTTP, Result);
  finally
    HTTP.Disconnect;
    HTTP.Free;
  end;
end;

function TmnOpenAIClient.ChatWithImage(const APrompt, AImageFileName,
  AModel, ADetail: string): string;
var
  Messages: UTF8String;
  Root, Choices: TDON_Value;
begin
  if not FileExists(AImageFileName) then
    raise Exception.CreateFmt('Image file not found: %s', [AImageFileName]);
  Messages := '[{"role":"user","content":[' +
    '{"type":"text","text":' + JSONQuote(APrompt) + '},' +
    '{"type":"image_url","image_url":{"url":' + JSONQuote(string(BuildDataURL(AImageFileName)));
  if ADetail <> '' then
    Messages := Messages + ',"detail":' + JSONQuote(ADetail);
  Messages := Messages + '}}]}]';
  { Reuse the normal chat endpoint while preserving the multimodal message. }
  Root := ParseJSONValue(ChatCompletion(Messages, AModel), 'vision response');
  try
    Choices := JSONValue(Root, 'choices');
    if not (Choices is TDON_Array) or (TDON_Array(Choices).Count = 0) then
      raise Exception.Create('Vision response does not contain choices');
    Result := JSONString(JSONValue(TDON_Array(Choices)[0], 'message'), 'content');
  finally
    Root.Free;
  end;
end;

function ParseImagesJSON(const ABody: UTF8String): TmnOpenAIImages;
var
  Root: TDON_Value;
  Data: TDON_Array;
  I: Integer;
begin
  SetLength(Result, 0);
  Root := ParseJSONValue(ABody, 'images response');
  try
    Data := FirstArray(Root, 'data');
    if Data = nil then
      raise Exception.Create('Images response does not contain a data array');
    SetLength(Result, Data.Count);
    for I := 0 to Data.Count - 1 do
    begin
      Result[I].URL := JSONString(Data[I], 'url');
      Result[I].Base64JSON := UTF8String(JSONString(Data[I], 'b64_json'));
      Result[I].RevisedPrompt := JSONString(Data[I], 'revised_prompt');
    end;
  finally
    Root.Free;
  end;
end;

function TmnOpenAIClient.GenerateImages(const APrompt, AModel, ASize: string;
  ACount: Integer; const AResponseFormat: string): TmnOpenAIImages;
var
  Body: UTF8String;
begin
  if ACount < 1 then
    ACount := 1;
  Body := UTF8String('{"prompt":') + JSONQuote(APrompt) + ',"n":' + UTF8String(IntToStr(ACount));
  if (AModel <> '') or (FModel <> '') then
    Body := Body + ',"model":' + JSONQuote(ResolveModel(AModel));
  if ASize <> '' then
    Body := Body + ',"size":' + JSONQuote(ASize);
  if AResponseFormat <> '' then
    Body := Body + ',"response_format":' + JSONQuote(AResponseFormat);
  Body := Body + '}';
  Result := ParseImagesJSON(Post('images/generations', Body));
end;

function TmnOpenAIClient.DownloadFile(const AURL, AFileName: string): Int64;
var
  HTTP: TmnHttpClient;
begin
  ClearLastResponse;
  HTTP := NewHTTPClient;
  try
    if not HTTP.Connect(UTF8String(AURL)) then
      raise Exception.Create('Image download connection failed');
    if not HTTP.Get then
      raise Exception.Create('Image download request failed');
    FLastStatusCode := HTTP.Response.StatusCode;
    if (FLastStatusCode < 200) or (FLastStatusCode >= 300) then
    begin
      FLastResponse := ReadResponse(HTTP);
      CheckResponse(HTTP, FLastResponse);
    end;
    Result := HTTP.ReadToFile(UTF8String(AFileName), -1);
    FLastResponse := '';
  finally
    HTTP.Disconnect;
    HTTP.Free;
  end;
end;

function TmnOpenAIClient.GenerateImage(const APrompt, AOutputFileName,
  AModel, ASize: string): TmnOpenAIImage;
var
  Images: TmnOpenAIImages;
  Raw: UTF8String;
  F: TFileStream;
begin
  Images := GenerateImages(APrompt, AModel, ASize, 1, '');
  if Length(Images) = 0 then
    raise Exception.Create('Image generation returned no image');
  Result := Images[0];
  if Result.URL <> '' then
    DownloadFile(Result.URL, AOutputFileName)
  else if Result.Base64JSON <> '' then
  begin
    Raw := Base64Decode(Result.Base64JSON);
    F := TFileStream.Create(AOutputFileName, fmCreate or fmShareDenyWrite);
    try
      if Length(Raw) > 0 then
        F.WriteBuffer(Raw[1], Length(Raw));
    finally
      F.Free;
    end;
  end
  else
    raise Exception.Create('Image response contains neither url nor b64_json');
end;

function TmnOpenAIClient.EditImages(const APrompt, AImageFileName,
  AMaskFileName, AModel, ASize: string; ACount: Integer;
  const AResponseFormat: string): TmnOpenAIImages;
var
  Boundary: string;
  M: TMemoryStream;
  ResponseBody: UTF8String;
begin
  if not FileExists(AImageFileName) then
    raise Exception.CreateFmt('Image file not found: %s', [AImageFileName]);
  Boundary := MakeBoundary;
  M := TMemoryStream.Create;
  try
    AddMultipartFile(M, Boundary, 'image', AImageFileName, MimeTypeForFile(AImageFileName));
    if AMaskFileName <> '' then
    begin
      if not FileExists(AMaskFileName) then
        raise Exception.CreateFmt('Mask file not found: %s', [AMaskFileName]);
      AddMultipartFile(M, Boundary, 'mask', AMaskFileName, MimeTypeForFile(AMaskFileName));
    end;
    AddMultipartField(M, Boundary, 'prompt', APrompt);
    if (AModel <> '') or (FModel <> '') then AddMultipartField(M, Boundary, 'model', ResolveModel(AModel));
    if ASize <> '' then AddMultipartField(M, Boundary, 'size', ASize);
    if ACount < 1 then ACount := 1;
    AddMultipartField(M, Boundary, 'n', IntToStr(ACount));
    if AResponseFormat <> '' then AddMultipartField(M, Boundary, 'response_format', AResponseFormat);
    WriteUTF8(M, UTF8String('--' + Boundary + '--' + #13#10));
    ResponseBody := MultipartRequest('images/edits', M, Boundary);
  finally
    M.Free;
  end;
  Result := ParseImagesJSON(ResponseBody);
end;

function TmnOpenAIClient.AudioTranscription(const AFileName, AModel: string;
  const AOptionsJSON: UTF8String): UTF8String;
var
  Boundary: string;
  M: TMemoryStream;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('Audio file not found: %s', [AFileName]);
  Boundary := MakeBoundary;
  M := TMemoryStream.Create;
  try
    AddMultipartFile(M, Boundary, 'file', AFileName, MimeTypeForFile(AFileName));
    AddMultipartField(M, Boundary, 'model', ResolveModel(AModel));
    AddMultipartOptions(M, Boundary, AOptionsJSON);
    WriteUTF8(M, UTF8String('--' + Boundary + '--' + #13#10));
    Result := MultipartRequest('audio/transcriptions', M, Boundary);
  finally
    M.Free;
  end;
end;

function TmnOpenAIClient.AudioTranslation(const AFileName, AModel: string;
  const AOptionsJSON: UTF8String): UTF8String;
var
  Boundary: string;
  M: TMemoryStream;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('Audio file not found: %s', [AFileName]);
  Boundary := MakeBoundary;
  M := TMemoryStream.Create;
  try
    AddMultipartFile(M, Boundary, 'file', AFileName, MimeTypeForFile(AFileName));
    AddMultipartField(M, Boundary, 'model', ResolveModel(AModel));
    AddMultipartOptions(M, Boundary, AOptionsJSON);
    WriteUTF8(M, UTF8String('--' + Boundary + '--' + #13#10));
    Result := MultipartRequest('audio/translations', M, Boundary);
  finally
    M.Free;
  end;
end;

function TmnOpenAIClient.AudioSpeech(const AInput, AVoice, AOutputFileName,
  AModel, AFormat: string): Int64;
var
  HTTP: TmnHttpClient;
  Body: UTF8String;
  F: TFileStream;
  ResponseBody: UTF8String;
begin
  ClearLastResponse;
  Body := UTF8String('{"model":') + JSONQuote(ResolveModel(AModel)) +
    ',"input":' + JSONQuote(AInput) + ',"voice":' + JSONQuote(AVoice);
  if AFormat <> '' then
    Body := Body + ',"response_format":' + JSONQuote(AFormat);
  Body := Body + '}';

  HTTP := NewHTTPClient;
  try
    ApplyHeaders(HTTP, 'application/json');
    if not HTTP.Connect(UTF8String(EndpointURL('audio/speech'))) then
      raise Exception.Create('OpenAI connection failed');
    if not HTTP.Post(Body) then
      raise Exception.Create('OpenAI request failed');
    FLastStatusCode := HTTP.Response.StatusCode;
    if (HTTP.Response.StatusCode < 200) or (HTTP.Response.StatusCode >= 300) then
    begin
      ResponseBody := ReadResponse(HTTP);
      CheckResponse(HTTP, ResponseBody);
    end;
    F := TFileStream.Create(AOutputFileName, fmCreate or fmShareDenyWrite);
    try
      Result := HTTP.ReceiveStream(F);
    finally
      F.Free;
    end;
    FLastResponse := '';
  finally
    HTTP.Disconnect;
    HTTP.Free;
  end;
end;

end.
