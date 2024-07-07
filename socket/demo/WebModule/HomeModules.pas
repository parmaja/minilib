unit HomeModules;

{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
{$endif}

interface

uses
  Classes, SysUtils, StrUtils,
  mnUtils, mnStreams, mnModules, mnWebModules, mnMultipartData,
	mnWebElements, mnBootstraps;

type
  THomeModule = class;

  TCustomHomeSchema = class(TmnwBootstrapHTML)
  public
    Module: THomeModule;
  end;

  { TAssetsSchema }

  TAssetsSchema = class(TCustomHomeSchema)
  private
  public
  protected
    procedure DoCompose; override;
  public
  end;

  { TWelcomeSchema }

  TWelcomeSchema = class(TCustomHomeSchema)
  private
  protected
    procedure DoCompose; override;
    class function GetCapabilities: TmnwSchemaCapabilities; override;
  public
  end;

  TWSShema = class(TCustomHomeSchema)
  private
  public
  protected
    procedure DoCompose; override;
  public
  end;

  TLoginSchema = class(TCustomHomeSchema)
  private
  public
  protected
    procedure DoCompose; override;
  public
  end;

  { TbsHttpGetHomeCommand }

  TbsHttpGetHomeCommand = class(TmodHttpCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  TWSEchoGetHomeCommand = class(TmodHttpCommand)
  protected
    FPool: TmnPool;
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
    property Pool: TmnPool read FPool;
  end;

  THomeSchemas = class(TmnwSchemas)
  protected
    Module: THomeModule;
    procedure SchemaCreated(Schema: TmnwSchema); override;
  end;

  { THomeModule }

  THomeModule = class(TmodWebModule)
  protected
    procedure DoRegisterCommands; override;
    procedure Start; override;
    procedure Created; override;

    procedure DoPrepareRequest(ARequest: TmodRequest); override;

  public
    Schemas: THomeSchemas;
    destructor Destroy; override;
  end;

implementation

type

  { TClockComposer }

  TClockCompose = class(THTML.TIntervalCompose)
  public
    procedure InnerCompose(Inner: TmnwElement); override;
  end;

  TThreadTimer = class(TThread)
  public
  end;

  { TMyAction }

  TMyAction = class(THTML.TAction)
  public
    procedure Execute; override;
  end;

{ TMyAction }

procedure TMyAction.Execute;
begin
  inherited;
  if Root <> nil then
    Root.Attachments.SendMessage('My Action');
end;


{ TClockComposer }

procedure TClockCompose.InnerCompose(Inner: TmnwElement);
begin
  with THTML do
  begin
    TParagraph.Create(Self.This, TimeToStr(Now));
    {with TImage.Create(Self) do
    begin
      Name := 'file_logo';
  //          Route := 'logo';
      Source := IncludeURLDelimiter(Module.HomeURL)+'assets/logo.png';
    end;}
  end;
end;

{ TWellcomeSchema }

procedure TWelcomeSchema.DoCompose;
begin
  inherited;
  Name := 'welcome';
  Route := 'welcome';

  with TDocument.Create(This) do
  begin
    Name := 'document';
    //Route := 'document';
    Title := 'MyHome';
    Direction := dirLTR;
    with Body do
    begin
//      TJSResource.Create(This, 'WebElements');
      TJSEmbedFile.Create(This, Module.AppPath + '../../source/mnWebElements.js');

      Header.Text := 'Creative Solutions';
      with TImage.Create(This) do
      begin
        Name := 'image_logo';
        Comment := 'Image from another module';
        Source := IncludeURLDelimiter(Module.HostURL)+'doc/logo.png';
      end;

      Header.RenderIt := True;
      Footer.RenderIt := True;

      with Container do
      begin
        Name := 'Container';
        with TParagraph.Create(This) do
        begin
          Text := 'Hello Word';
          Name := 'p1';
        end;

        with TMyAction.Create(This) do
        begin
          Route := 'myaction';
        end;

        with TCard.Create(This) do
        begin
          Caption := 'Welcome';
          Name := 'card';

          with TMemoryImage.Create(This) do
          begin
            Name := 'logo';
            Route := 'logo';
            LoadFromFile(IncludePathDelimiter(Module.HomePath) + 'logo.png');
          end;

{          with TImage.Create(This) do
          begin
            Name := 'logo';
  //          Route := 'logo';
              Source := IncludeURLDelimiter(Module.HomeURL)+'assets/logo';
          end;}

          with TRow.Create(This) do
          begin
            with TInput.Create(This) do
            begin
              Name := 'Input1';
              Caption := 'Number 1';
            end;

            with TInput.Create(This) do
            begin
              Name := 'Input2';
              Caption := 'Number 2';
            end;

            with TButton.Create(This) do
            begin
              Name := 'AddBtn';
              Caption := 'Add';
            end;
          end;

{$ifdef fpc}
{          with TClockCompose.Create(This) do
          begin
          end;}
{$else}
          with TIntervalCompose.Create(This) do
          begin
            Route := 'clock';
            OnCompose := procedure(Inner: TmnwElement)
            begin
              TParagraph.Create(Inner, TimeToStr(Now));
              {with TImage.Create(Inner) do
              begin
                Name := 'file_logo';
      //          Route := 'logo';
                Source := IncludeURLDelimiter(Module.HomeURL)+'assets/logo.png';
              end;}
            end;
          end;
{$endif}

        end;
      end;
    end;
  end;
end;

class function TWelcomeSchema.GetCapabilities: TmnwSchemaCapabilities;
begin
  Result := [schemaAttach] + Inherited GetCapabilities;
end;

{ TbsHttpGetHomeCommand }

procedure TbsHttpGetHomeCommand.RespondResult(var Result: TmodRespondResult);
var
  Renderer : TmnwBootstrapRenderer;
begin
  inherited;
  if Request.WebSocket then
  begin
    (Module as THomeModule).Schemas.Attach(DeleteSubPath('', Request.Path), Self, Respond.Stream); //Serve the websocket
    Result.Status := Result.Status - [mrKeepAlive]; // Disconnect
  end
  else
  begin
    Respond.PutHeader('Content-Type', DocumentToContentType('html'));
    Respond.HttpResult := hrOK;
    Renderer := TmnwBootstrapRenderer.Create;
    try
      Renderer.HomeUrl := (Module as THomeModule).HomeUrl;
      (Module as THomeModule).Schemas.Respond(DeleteSubPath('', Request.Path), Self, Renderer, Respond.Stream);
    finally
      Renderer.Free;
    end;
  end;
end;

{ THomeModule }

procedure THomeModule.DoPrepareRequest(ARequest: TmodRequest);
begin
  inherited;
  if StartsStr('.', ARequest.Route[ARequest.Route.Count - 1]) then
    ARequest.Command := ARequest.Route[ARequest.Route.Count - 1]
  else
    ARequest.Command := ARequest.Route[1];
  //ARequest.Path := DeleteSubPath(ARequest.Command, ARequest.Path);
end;

procedure THomeModule.DoRegisterCommands;
begin
  inherited;
  RegisterCommand('page', TbsHttpGetHomeCommand, true);
  RegisterCommand('.ws', TWSEchoGetHomeCommand, false);
end;

procedure THomeModule.Created;
begin
  inherited;
end;

procedure THomeModule.Start;
begin
  inherited;
  Schemas := THomeSchemas.Create;
  Schemas.Module := Self;
  Schemas.RegisterSchema('welcome', TWelcomeSchema);
  Schemas.RegisterSchema('assets', TAssetsSchema);
  Schemas.RegisterSchema('login', TLoginSchema);
  Schemas.RegisterSchema('ws', TWSShema);
end;

destructor THomeModule.Destroy;
begin
  FreeAndNil(Schemas);
  inherited;
end;

{ TAssetsSchema }

procedure TAssetsSchema.DoCompose;
begin
  inherited;
  Name := 'Assets';
  Route := 'assets';
  with TAssets.Create(This) do
  begin
    HomePath := Module.HomePath;
    Kind := Kind + [elFallback];
    //Name := 'document';
//    Route := '';
    with TDirectFile.Create(This) do
    begin
      Name := 'jquery';
      Route := 'jquery';
      FileName := IncludePathDelimiter(Module.HomePath) + 'jquery-3.7.1.min.js';
    end;

    with TDirectFile.Create(This) do
    begin
      Name := 'logo';
      Route := 'logo';
      FileName := IncludePathDelimiter(Module.HomePath) + 'logo.png';
    end;
  end;
end;

{ TWSEchoGetHomeCommand }

procedure TWSEchoGetHomeCommand.RespondResult(var Result: TmodRespondResult);
var
  s: string;
begin
  if Request.WebSocket then
  begin
    //Request.Path := DeleteSubPath(Name, Request.Path);
    while Respond.Stream.Connected do
    begin
      if Respond.Stream.ReadUTF8Line(s) then
      begin
        Respond.Stream.WriteUTF8Line(s);
        log(s);
      end;
    end;
  end;
  inherited;
end;

{ TLoginSchema }

procedure TLoginSchema.DoCompose;
begin
  inherited;
  Name := 'welcome';
  Route := 'welcome';
  with TDocument.Create(This) do
  begin
    //Name := 'document';
    Route := 'document';
    Title := 'MyHome';
    Direction := dirLTR;

    with Body do
    begin
      Header.Text := 'Creative Solutions';
      with TImage.Create(This) do
      begin
        Comment := 'Image from another module';
        Source := IncludeURLDelimiter(Module.HostURL)+'doc/logo.png';
      end;
      Header.RenderIt := True;
      Footer.RenderIt := True;

      with Container do
      begin
        with TParagraph.Create(This) do
        begin
          Text := 'Hello Word';
        end;

        with TCard.Create(This) do
        begin
          Caption := 'Login';

          with TForm.Create(This) do
          begin
            with TInput.Create(This) do
            begin
              ID := 'username';
              Caption := 'Username';
              PlaceHolder := 'Type user name';
            end;

            with TInputPassword.Create(This) do
            begin
              ID := 'password';
              Caption := 'Password';
            end;

            TBreak.Create(This);

          end;
        end;
      end;
    end;
  end;
end;

{ THomeSchemas }

procedure THomeSchemas.SchemaCreated(Schema: TmnwSchema);
begin
  inherited;
  if Schema is TCustomHomeSchema then
    (Schema as TCustomHomeSchema).Module := Module;
end;

{ TWSShema }

procedure TWSShema.DoCompose;
begin
  inherited;
  Name := 'ws';
  Route := 'ws';
  with TDirectFile.Create(This) do
  begin
    Route := 'echo';
    FileName := IncludePathDelimiter(Module.HomePath) + 'ws.html';
  end;
end;

end.

