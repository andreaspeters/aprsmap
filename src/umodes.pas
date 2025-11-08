unit umodes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, utypes, RegExpr, Contnrs, fphttpclient,
  mvGpsObj, Process;

type
  { TModeSThread }
  TModeSThread = class(TThread)
  private
    FConfig: PAPRSConfig;
    Dump1090: TProcess;
    procedure RunDump190Server;
  protected
    procedure Execute; override;
  public
    ModeSMessageList: TFPHashList;
    Error: Boolean;
    procedure LoadAircraftsFromDump1090;
    procedure Stop;
    constructor Create(Config: PAPRSConfig);
  end;

implementation

{ TModeSThread }

procedure TModeSThread.Stop;
begin
  if Assigned(Dump1090) then
  begin
    if Dump1090.Running then
      Dump1090.Terminate(1);
  end;
end;

constructor TModeSThread.Create(Config: PAPRSConfig);
begin
  inherited Create(True);
  FConfig := Config;
  FreeOnTerminate := True;
  ModeSMessageList := TFPHashList.Create;
  RunDump190Server;
  Error := False;
  Start;
end;

procedure TModeSThread.Execute;
begin
  while not Terminated do
  begin
    LoadAircraftsFromDump1090;
    sleep(1000);
  end;
end;

procedure TModeSThread.RunDump190Server;
var i: Integer;
begin
  if Length(FConfig^.ModeSExecutable) <= 0 then
    Exit;

  Dump1090 := TProcess.Create(nil);
  try
    Dump1090.Executable := FConfig^.ModeSExecutable;
    Dump1090.Parameters := TStringList.Create;
    Dump1090.Parameters.Add('--net');
    Dump1090.Parameters.Add('--net-http-port');
    Dump1090.Parameters.Add(IntToStr(FConfig^.ModeSPort));

    for i := 0 to GetEnvironmentVariableCount - 1 do
      Dump1090.Environment.Add(GetEnvironmentString(i));

    Dump1090.Options := [poUsePipes, poNoConsole];
    Dump1090.Execute;
  except
    on E: Exception do
    {$IFDEF UNIX}
      Writeln('Exec Dump1090 Server Error: ' + E.Message)
    {$ENDIF}
  end;
  Sleep(200);
end;

procedure TModeSThread.LoadAircraftsFromDump1090;
var Response: String;
    Obj: TJSONObject;
    Items: TJSONArray;
    APRSMessageObject: PAPRSMessage;
    i: Integer;
    Client: TFPHTTPClient;

begin
  try
    Client := TFPHTTPClient.Create(nil);

    try
      Response := Client.Get('http://' + FConfig^.ModeSServer + ':' +
                             IntToStr(FConfig^.ModeSPort) + '/data.json');
    except
      on E: Exception do
      begin
        Error := True;
        {$IFDEF UNIX}
        writeln('HTTP connection failed: ', E.Message);
        {$ENDIF}
        Exit;
      end;
    end;

    if Client.ResponseStatusCode <> 200 then
      Exit;

    // Test Daten
    //Response := '[{"hex":"3c55c3", "flight":"TESTFLUG", "lat":53.589772, "lon":9.904902, "altitude":6850, "track":219, "speed":201},{"hex":"3c55c3", "flight":"HALLO", "lat":53.589772, "lon":9.904902, "altitude":6850, "track":219, "speed":201}]';

    if Length(Response) > 0 then
    begin
      try
        Items := TJSONArray(GetJSON(Response));
        if Items.Count <= 0 then
          Exit;

        for i := 0 to Items.Count - 1 do
        begin
          Obj := Items.Objects[i];

          new(APRSMessageObject);

          if Obj.Find('flight') <> nil then
            APRSMessageObject^.FromCall := StringReplace(Obj.Strings['flight'], ' ', '', [rfReplaceAll]);

          if Obj.Find('lat') <> nil then
            APRSMessageObject^.Latitude := Obj.Floats['lat'];

          if Obj.Find('lon') <> nil then
            APRSMessageObject^.Longitude := Obj.Floats['lon'];

          if Obj.Find('altitude') <> nil then
            APRSMessageObject^.Altitude := Round(Obj.Integers['altitude']*0.3048);

          if Obj.Find('speed') <> nil then
            APRSMessageObject^.Speed := Round(Obj.Integers['speed']*1.85);

          APRSMessageObject^.Track := TGPSTrack.Create;
          APRSMessageObject^.Track.Visible := True;
          APRSMessageObject^.Track.LineWidth := 1;
          APRSMessageObject^.Time := now();
          APRSMessageObject^.ImageIndex := 7;

          if Length(APRSMessageObject^.FromCall) > 0 then
            ModeSMessageList.Add(APRSMessageObject^.FromCall, APRSMessageObject);

        end;
      except
        on E: Exception do
        begin
        {$IFDEF UNIX}
          writeln('JSON Error: ' + E.Message);
        {$ENDIF}
        end;
      end;
    end
    else
      ModeSMessageList := TFPHashList.Create;
  finally
    Client.Free;
  end;
end;

end.

