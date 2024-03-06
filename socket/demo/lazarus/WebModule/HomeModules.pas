unit HomeModules;

{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils,
  mnUtils, mnStreams, mnModules, mnWebModules, mnMultipartData,
	mnWebElements, mnBootstraps;

type
  THomeModule = class;

  TCustomHomeSchema = class(TmnwBootstrap)
  public
    Module: THomeModule;
  end;

  { THomeSchema }

  THomeSchema = class(TCustomHomeSchema)
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

  { THomeModule }

  THomeModule = class(TmodBootstrapModule)
  protected
    procedure DoRegisterCommands; override;
    procedure Start; override;
    procedure Created; override;
  public
    HomeSchema: THomeSchema;
    Schemas: TmnwSchemas;
    //LoginSchema: TLoginSchema;
    destructor Destroy; override;
  end;

implementation

{ THomeSchema }

procedure THomeSchema.DoCompose;
begin
  inherited;
  Name := 'welcome';
  Route := 'welcome';
  with This.Add<TDocument> do
  begin
    //Name := 'document';
    Route := 'document';
    Title := 'MyHome';
    Direction := dirLTR;

    Header.Text := 'Creative Solutions';
    with Header.Add<TImage> do
    begin
      Comment := 'Image from another module';
      Source := IncludeURLDelimiter(Module.HostURL)+'doc/logo.png';
    end;
    Header.Active := True;
    Footer.Active := True;

    with Container do
    begin
      with This.Add<TParagraph> do
      begin
        Text := 'Hello Word';
      end;

      with This.Add<TCard>() do
      begin
        Caption := 'Login';

        with This.Add<TForm>() do
        begin
          with This.Add<TInput>('username') do
          begin
            Caption := 'Username';
            PlaceHolder := 'Type user name';
          end;

          with This.Add<TInputPassword>('password') do
          begin
            Caption := 'Password';
          end;

          This.Add<TBreak>;

          {with This.Add<TMemoryImage> do
          begin
            Name := 'logo';
            Route := 'logo';
            Data.LoadFromFile(IncludePathDelimiter(Module.HomePath) + 'logo.png');
          end;}
        end;
      end;
    end;
  end;
end;

{ TbsHttpGetHomeCommand }

procedure TbsHttpGetHomeCommand.RespondResult(var Result: TmodRespondResult);
var
  Renderer : TmnwBootstrapRenderer;
begin
  inherited;
  Respond.PutHeader('Content-Type', DocumentToContentType('html'));
  Respond.HttpResult := hrOK;
  Respond.SendHeader;

  Renderer := TmnwBootstrapRenderer.Create;
  try
    Renderer.HomeUrl := (Module as THomeModule).HomeUrl;
    (Module as THomeModule).Schemas.Respond(Request.Path, Renderer, Request, Respond.Stream);
  finally
    Renderer.Free;
  end;
end;

{ THomeModule }

procedure THomeModule.DoRegisterCommands;
begin
  inherited;
  RegisterCommand('GET', TbsHttpGetHomeCommand, true);
  RegisterCommand('PUT', TbsHttpGetHomeCommand, false);
end;

procedure THomeModule.Created;
begin
  inherited;
  Schemas := TmnwSchemas.Create;
  HomeSchema := THomeSchema.Create;
  HomeSchema.Module := Self;
  Schemas.RegisterSchema(HomeSchema.Route, HomeSchema);
end;

procedure THomeModule.Start;
begin
  inherited;
  HomeSchema.Clear;
  HomeSchema.Compose;
end;

destructor THomeModule.Destroy;
begin
  FreeAndNil(HomeSchema);
  FreeAndNil(Schemas);
  inherited;
end;

end.

