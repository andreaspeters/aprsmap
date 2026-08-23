unit umodes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, utypes, RegExpr, Contnrs, fphttpclient,
  mvGpsObj, Process, md5, Math, umodesdecoder, urtlsdr;

type
  { TModeSThread }
  TModeSThread = class(TThread)
  private
    FConfig: PAPRSConfig;
    Dump1090: TProcess;
    NativeDevice: TRtlSdrDev;
    NativeMode: Boolean;
    NativeBuffer: array[0..262143] of Byte;
    NativeMagnitude: TModeSMagnitude;
    NativeMessages: array[0..255] of TModeSMessage;
    procedure RunDump190Server;
    procedure OpenNative;
    procedure CloseNative;
    procedure LoadAircraftsFromNative;
    procedure AddNativeMessage(const Decoded: TModeSMessage);
    function ChecksumExists(List: TFPHashList; const AChecksum: String): Boolean;
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

uses
  uaprs;

{ TModeSThread }

procedure TModeSThread.Stop;
begin
  CloseNative;
  if Assigned(Dump1090) then
  begin
    if Dump1090.Running then
      Dump1090.Terminate(1);
  end;
end;

constructor TModeSThread.Create(Config: PAPRSConfig);
begin
  inherited Create(True);
  Error := False;
  FConfig := Config;
  NativeDevice := nil;
  NativeMode := Length(Trim(FConfig^.ModeSExecutable)) = 0;
  FreeOnTerminate := True;
  ModeSMessageList := TFPHashList.Create;
  if NativeMode then
    OpenNative
  else
    RunDump190Server;
  Start;
end;

procedure TModeSThread.Execute;
begin
  if not FConfig^.ModeSEnabled then
    Exit;

  while not Terminated do
  begin
    if NativeMode then
      LoadAircraftsFromNative
    else
      LoadAircraftsFromDump1090;
    sleep(1000);
  end;
end;

procedure TModeSThread.OpenNative;
begin
  try
    if RtlSdrGetDeviceCount = 0 then
    begin
      Error := True;
      Exit;
    end;
    if RtlSdrOpen(NativeDevice, 0) <> 0 then
    begin
      Error := True;
      Exit;
    end;
    RtlSdrSetCenterFreq(NativeDevice, 1090000000);
    RtlSdrSetSampleRate(NativeDevice, 2048000);
    RtlSdrSetTunerGainMode(NativeDevice, 0);
    RtlSdrSetAgcMode(NativeDevice, 1);
    RtlSdrResetBuffer(NativeDevice);
    SetLength(NativeMagnitude, Length(NativeBuffer) div 2);
  except
    on E: Exception do
    begin
      Error := True;
      {$IFDEF UNIX}
      Writeln('Native RTL-SDR error: ', E.Message);
      {$ENDIF}
    end;
  end;
end;

procedure TModeSThread.CloseNative;
begin
  if Assigned(NativeDevice) then
  begin
    RtlSdrClose(NativeDevice);
    NativeDevice := nil;
  end;
end;

procedure TModeSThread.LoadAircraftsFromNative;
var
  ReadCount, I, Count: Integer;
begin
  if not Assigned(NativeDevice) then Exit;
  if RtlSdrReadSync(NativeDevice, NativeBuffer, Length(NativeBuffer), ReadCount) <> 0 then
  begin
    Error := True;
    Exit;
  end;
  if ReadCount < 32 then Exit;
  SetLength(NativeMagnitude, ReadCount div 2);
  for I := 0 to (ReadCount div 2) - 1 do
    NativeMagnitude[I] := Min(255, Abs(Integer(NativeBuffer[I * 2]) - 127) +
                                   Abs(Integer(NativeBuffer[I * 2 + 1]) - 127));
  Count := DemodulateModeS(NativeMagnitude, NativeMessages);
  if Count > Length(NativeMessages) then Count := Length(NativeMessages);
  for I := 0 to Count - 1 do
    AddNativeMessage(NativeMessages[I]);
end;

procedure TModeSThread.AddNativeMessage(const Decoded: TModeSMessage);
var
  APRSMessageObject: PAPRSMessage;
  Key: String;
  Latitude, Longitude: Double;
begin
  Key := IntToHex(Decoded.ICAO, 6);
  APRSMessageObject := PAPRSMessage(ModeSMessageList.Find(Key));
  if not Assigned(APRSMessageObject) then
  begin
    New(APRSMessageObject);
    FillChar(APRSMessageObject^, SizeOf(TAPRSMessage), 0);
    APRSMessageObject^.Altitude := TDoubleList.Create;
    APRSMessageObject^.Speed := TDoubleList.Create;
    APRSMessageObject^.FromCall := Key;
    ModeSMessageList.Add(Key, APRSMessageObject);
  end;
  if Length(Decoded.Flight) > 0 then
    APRSMessageObject^.FromCall := Decoded.Flight;
  if Decoded.HasAltitude then
  begin
    APRSMessageObject^.Altitude.Clear;
    APRSMessageObject^.Altitude.Add(Decoded.AltitudeFeet);
  end;
  if Decoded.HasVelocity then
  begin
    APRSMessageObject^.Speed.Clear;
    APRSMessageObject^.Speed.Add(Decoded.Velocity);
  end;
  if Decoded.HasPosition then
  begin
    if Decoded.OddCPR then
    begin
      APRSMessageObject^.ModeSOddLatitude := Decoded.RawLatitude;
      APRSMessageObject^.ModeSOddLongitude := Decoded.RawLongitude;
      APRSMessageObject^.ModeSOddValid := True;
    end
    else
    begin
      APRSMessageObject^.ModeSEvenLatitude := Decoded.RawLatitude;
      APRSMessageObject^.ModeSEvenLongitude := Decoded.RawLongitude;
      APRSMessageObject^.ModeSEvenValid := True;
    end;
    if APRSMessageObject^.ModeSEvenValid and APRSMessageObject^.ModeSOddValid and
       DecodeGlobalCPR(APRSMessageObject^.ModeSEvenLatitude,
                       APRSMessageObject^.ModeSEvenLongitude,
                       APRSMessageObject^.ModeSOddLatitude,
                       APRSMessageObject^.ModeSOddLongitude,
                       Decoded.OddCPR, Latitude, Longitude) then
    begin
      APRSMessageObject^.Latitude := Latitude;
      APRSMessageObject^.Longitude := Longitude;
    end;
  end;
  APRSMessageObject^.Time := Now;
  APRSMessageObject^.ImageIndex := 7;
  APRSMessageObject^.ModeS := True;
  APRSMessageObject^.Checksum := Key;
end;

procedure TModeSThread.RunDump190Server;
var i: Integer;
begin
  if (Length(FConfig^.ModeSExecutable) <= 0) or not FConfig^.ModeSEnabled then
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
    Root: TJSONData;
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
        Root := GetJSON(Response);
        if Root.JSONType = jtArray then
          Items := TJSONArray(Root)
        else
          Exit;

        if Items.Count <= 0 then
          Exit;

        for i := 0 to Items.Count - 1 do
        begin
          if Items[i].JSONType <> jtObject then
            Continue;
          Obj := TJSONObject(Items[i]);

          new(APRSMessageObject);
          try
            APRSMessageObject^.Altitude := TDoubleList.Create;
            APRSMessageObject^.Speed := TDoubleList.Create;

            if Obj.Find('flight') <> nil then
              APRSMessageObject^.FromCall := StringReplace(Obj.Strings['flight'], ' ', '', [rfReplaceAll]);

            if Obj.Find('lat') <> nil then
              APRSMessageObject^.Latitude := Obj.Floats['lat'];

            if Obj.Find('lon') <> nil then
              APRSMessageObject^.Longitude := Obj.Floats['lon'];

            if Obj.Find('altitude') <> nil then
              APRSMessageObject^.Altitude.Add(Round(Obj.Integers['altitude']*0.3048));

            if Obj.Find('speed') <> nil then
              APRSMessageObject^.Speed.Add(Round(Obj.Integers['speed']*1.85));

            APRSMessageObject^.Track := TGPSTrack.Create;
            APRSMessageObject^.Track.Visible := True;
            APRSMessageObject^.Track.LineWidth := 1;
            APRSMessageObject^.Time := now();
            APRSMessageObject^.ImageIndex := 7;
            if (Obj.Find('lat') <> nil) and (Obj.Find('lon') <> nil) then
              APRSMessageObject^.Checksum := MD5Print(MD5String(FloatToStr(APRSMessageObject^.Longitude)+FloatToStr(APRSMessageObject^.Latitude)));

            if (Length(APRSMessageObject^.FromCall) > 0) and
              (not ChecksumExists(APRSMessageList, APRSMessageObject^.Checksum)) then
              ModeSMessageList.Add(APRSMessageObject^.FromCall, APRSMessageObject)
            else
            begin
              APRSMessageObject^.Altitude.Free;
              APRSMessageObject^.Speed.Free;
              APRSMessageObject^.Track.Free;
              Dispose(APRSMessageObject);
            end;
          except
            Dispose(APRSMessageObject);
            raise;
          end;
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
    begin
      if ModeSMessageList = nil then
        ModeSMessageList := TFPHashList.Create
      else
        ModeSMessageList.Clear;
    end;
  finally
    Client.Free;
    Root.Free;
  end;
end;

function TModeSThread.ChecksumExists(List: TFPHashList; const AChecksum: String): Boolean;
var
  i: Integer;
  Msg: PAPRSMessage;
begin
  Result := False;
  if List = nil then Exit;

  for i := 0 to List.Count - 1 do
  begin
    Msg := PAPRSMessage(List.Items[i]);
    if Assigned(Msg) and (Msg^.Checksum = AChecksum) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.

