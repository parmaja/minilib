program openai_delphi_test;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  mnOpenAI;

var
  AI: TmnOpenAIClient;
  Models: TmnOpenAIModels;
  I: Integer;
  BaseURL, APIKey, ModelName: string;
begin
  try
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
      Writeln('Models: ', Length(Models));
      for I := 0 to High(Models) do
        Writeln('  ', Models[I].ID);
      Writeln('Chat: ', AI.Chat('Reply exactly: LOCAL_OK'));
    finally
      AI.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
