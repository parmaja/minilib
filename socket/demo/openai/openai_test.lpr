program openai_test;

{$mode delphi}{$H+}

uses
  SysUtils, mnOpenAI;

var
  AI: TmnOpenAIClient;
  Models: TmnOpenAIModels;
  I: Integer;
  BaseURL, APIKey, ModelName: string;
begin
  BaseURL := GetEnvironmentVariable('OPENAI_BASE_URL');
  if BaseURL = '' then
    BaseURL := 'http://127.0.0.1:1234';
  APIKey := GetEnvironmentVariable('OPENAI_API_KEY');
  ModelName := GetEnvironmentVariable('OPENAI_MODEL');
  if ModelName = '' then
    ModelName := 'bonsai-1.7b';

  AI := TmnOpenAIClient.Create(BaseURL, APIKey);
  try
    AI.Model := ModelName;
    Models := AI.ListModels;
    WriteLn('Models: ', Length(Models));
    for I := 0 to High(Models) do
      WriteLn('  ', Models[I].ID);
    WriteLn('Chat: ', AI.Chat('Reply exactly: LOCAL_OK'));
  finally
    AI.Free;
  end;
end.
