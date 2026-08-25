unit umodes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utypes, Contnrs, mvGpsObj, Math, umodesdecoder, urtlsdr,
  uaisdecoder, uaisreceiver;

type
  TReceiverSlot = (rsModeS, rsAIS);

  { TModeSThread }
  TModeSThread = class(TThread)
  private
    FConfig: PAPRSConfig;
    NativeDevice: TRtlSdrDev;
    NativeMagnitude: TModeSMagnitude;
    NativeMessages: array[0..255] of TModeSMessage;
    AISReceiver: TAISReceiver;
    ActiveSlot: TReceiverSlot;
    SlotBytes, SlotByteLimit: Int64;
    SlotCancelled: Boolean;


    procedure OpenNative;
    procedure CloseNative;
    procedure ProcessNativeSamples(const Buffer: PByte; const ByteCount: Integer);
    procedure AddNativeMessage(const Decoded: TModeSMessage);
    procedure AddNativeAISMessage(const Decoded: TAISMessage);
    procedure RunSlot(const Slot: TReceiverSlot);
    class procedure NativeReadCallback(Buf: PByte; Len: Cardinal; Context: Pointer); static; cdecl;

  protected
    procedure Execute; override;
  public
    ModeSMessageList: TFPHashList;
    ModeSUpdateQueue: TStringList;
    AISMessageList: TFPHashList;
    AISUpdateQueue: TStringList;
    Error: Boolean;
    procedure Stop;
    constructor Create(Config: PAPRSConfig);
    destructor Destroy; override;
  end;

implementation

uses
  uaprs;

function AppendHistoryValue(const Values: TDoubleList; const Value: Double): Boolean;
begin
  Result := Assigned(Values) and ((Values.Count = 0) or (Values.Last <> Value));
  if Result then
    Values.Add(Value);
end;

{ TModeSThread }

procedure TModeSThread.Stop;
begin
  Terminate;
  if Assigned(NativeDevice) then
    RtlSdrCancelAsync(NativeDevice);
end;

constructor TModeSThread.Create(Config: PAPRSConfig);
begin
  inherited Create(True);
  Error := False;
  FConfig := Config;
  NativeDevice := nil;

  FreeOnTerminate := False;
  ModeSMessageList := TFPHashList.Create;
  ModeSUpdateQueue := TStringList.Create;
  AISMessageList := TFPHashList.Create;
  AISUpdateQueue := TStringList.Create;
  AISReceiver := TAISReceiver.Create(@AddNativeAISMessage);
  OpenNative;
  Start;
end;

destructor TModeSThread.Destroy;
begin
  Stop;
  WaitFor;
  FreeAndNil(AISReceiver);
  FreeAndNil(AISUpdateQueue);
  FreeAndNil(AISMessageList);
  FreeAndNil(ModeSUpdateQueue);
  FreeAndNil(ModeSMessageList);
  inherited Destroy;
end;

procedure TModeSThread.Execute;
begin
  try
    while not Terminated and Assigned(NativeDevice) do
    begin
      if FConfig^.ModeSEnabled then
        RunSlot(rsModeS);
      if FConfig^.AISEnabled and not Terminated and not Error then
        RunSlot(rsAIS);
      if Error or (not FConfig^.ModeSEnabled and not FConfig^.AISEnabled) then
        Break;
    end;
  finally
    FreeAndNil(AISReceiver);
    CloseNative;
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
    RtlSdrSetTunerGainMode(NativeDevice, 0);
    RtlSdrSetAgcMode(NativeDevice, 1);

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

procedure TModeSThread.RunSlot(const Slot: TReceiverSlot);
var
  ResultCode: Integer;
begin
  if not Assigned(NativeDevice) or Terminated then Exit;
  ActiveSlot := Slot;
  SlotBytes := 0;
  SlotCancelled := False;
  case Slot of
    rsModeS:
      begin
        if (RtlSdrSetCenterFreq(NativeDevice, 1090000000) <> 0) or
           (RtlSdrSetSampleRate(NativeDevice, 2000000) <> 0) then
        begin
          Error := True;
          Exit;
        end;
        SlotByteLimit := 3200000; { 800 ms at 2 MS/s, interleaved IQ }
      end;
    rsAIS:
      begin
        if (RtlSdrSetCenterFreq(NativeDevice, 162000000) <> 0) or
           (RtlSdrSetSampleRate(NativeDevice, 240000) <> 0) then
        begin
          Error := True;
          Exit;
        end;
        SlotByteLimit := 576000; { 1200 ms at 240 kS/s, interleaved IQ }
        AISReceiver.Reset;
      end;
  end;
  if RtlSdrResetBuffer(NativeDevice) <> 0 then
  begin
    Error := True;
    Exit;
  end;
  ResultCode := RtlSdrReadAsync(NativeDevice, @NativeReadCallback, Self, 12, 32768);
  if (ResultCode <> 0) and not SlotCancelled and not Terminated then
    Error := True;
end;

procedure TModeSThread.CloseNative;
begin
  if Assigned(NativeDevice) then
  begin
    RtlSdrClose(NativeDevice);
    NativeDevice := nil;
  end;
end;

procedure TModeSThread.ProcessNativeSamples(const Buffer: PByte; const ByteCount: Integer);
var
  I, Count: Integer;
begin
  if (ByteCount < 32) or Odd(ByteCount) then Exit;
  if ActiveSlot = rsModeS then
  begin
    SetLength(NativeMagnitude, ByteCount div 2);
    for I := 0 to High(NativeMagnitude) do
      NativeMagnitude[I] := Sqr(Integer(Buffer[I * 2]) - 127) +
                            Sqr(Integer(Buffer[I * 2 + 1]) - 127);
    Count := DemodulateModeS(NativeMagnitude, NativeMessages);
    if Count > Length(NativeMessages) then Count := Length(NativeMessages);
    for I := 0 to Count - 1 do
      AddNativeMessage(NativeMessages[I]);
  end
  else if Assigned(AISReceiver) then
    AISReceiver.ProcessIQ(Buffer, ByteCount);
  SlotBytes := SlotBytes + ByteCount;
  if SlotBytes >= SlotByteLimit then
  begin
    SlotCancelled := True;
    RtlSdrCancelAsync(NativeDevice);
  end;
end;

class procedure TModeSThread.NativeReadCallback(Buf: PByte; Len: Cardinal; Context: Pointer); cdecl;
var
  Receiver: TModeSThread;
begin
  Receiver := TModeSThread(Context);
  if Assigned(Receiver) and not Receiver.Terminated then
    Receiver.ProcessNativeSamples(Buf, Len);
end;

procedure TModeSThread.AddNativeMessage(const Decoded: TModeSMessage);
var
  APRSMessageObject: PAPRSMessage;
  Key: String;
  Latitude, Longitude: Double;
  FrameTime: TDateTime;
  AltitudeChanged, VelocityChanged, CourseChanged, PositionChanged, IdentityChanged: Boolean;
  Altitude, Velocity: Double;
begin
  FrameTime := Now;
  Key := IntToHex(Decoded.ICAO, 6);
  AltitudeChanged := False;
  VelocityChanged := False;
  CourseChanged := False;
  PositionChanged := False;
  IdentityChanged := False;
  {$IFDEF UNIX}
  Writeln('[MODE-S] ICAO=', Key,
    ' flight=', Decoded.Flight,
    ' altitude=', Decoded.AltitudeFeet,
    ' altitude_valid=', Decoded.HasAltitude,
    ' speed=', FloatToStr(Decoded.Velocity),
    ' velocity_valid=', Decoded.HasVelocity,
    ' position_message=', Decoded.HasPosition,
    ' odd_cpr=', Decoded.OddCPR);
  {$ENDIF}
  APRSMessageObject := PAPRSMessage(ModeSMessageList.Find(Key));
  if not Assigned(APRSMessageObject) then
  begin
    New(APRSMessageObject);
    FillChar(APRSMessageObject^, SizeOf(TAPRSMessage), 0);
    APRSMessageObject^.Altitude := TDoubleList.Create;
    APRSMessageObject^.Speed := TDoubleList.Create;
    APRSMessageObject^.Track := TGPSTrack.Create;
    APRSMessageObject^.Track.Visible := True;
    APRSMessageObject^.Track.LineWidth := 1;
    APRSMessageObject^.FromCall := Key;
    ModeSMessageList.Add(Key, APRSMessageObject);
  end;
  if (Length(Decoded.Flight) > 0) and (APRSMessageObject^.FromCall <> Decoded.Flight) then
  begin
    APRSMessageObject^.FromCall := Decoded.Flight;
    IdentityChanged := True;
  end;
  if Decoded.HasAltitude then
  begin
    Altitude := Round(Decoded.AltitudeFeet * 0.3048);
    AltitudeChanged := AppendHistoryValue(APRSMessageObject^.Altitude, Altitude);
  end;
  if Decoded.HasVelocity then
  begin
    Velocity := Round(Decoded.Velocity * 1.852);
    VelocityChanged := AppendHistoryValue(APRSMessageObject^.Speed, Velocity);
    CourseChanged := APRSMessageObject^.Course <> Decoded.Track;
    APRSMessageObject^.Course := Decoded.Track;
  end;
  if Decoded.HasPosition then
  begin
    if Decoded.OddCPR then
    begin
      APRSMessageObject^.ModeSOddLatitude := Decoded.RawLatitude;
      APRSMessageObject^.ModeSOddLongitude := Decoded.RawLongitude;
      APRSMessageObject^.ModeSOddValid := True;
      APRSMessageObject^.ModeSOddTime := FrameTime;
    end
    else
    begin
      APRSMessageObject^.ModeSEvenLatitude := Decoded.RawLatitude;
      APRSMessageObject^.ModeSEvenLongitude := Decoded.RawLongitude;
      APRSMessageObject^.ModeSEvenValid := True;
      APRSMessageObject^.ModeSEvenTime := FrameTime;
    end;
    if APRSMessageObject^.ModeSEvenValid and APRSMessageObject^.ModeSOddValid and
       (Abs(APRSMessageObject^.ModeSEvenTime - APRSMessageObject^.ModeSOddTime) <=
        (10.0 / 86400.0)) and
       DecodeGlobalCPR(APRSMessageObject^.ModeSEvenLatitude,
                       APRSMessageObject^.ModeSEvenLongitude,
                       APRSMessageObject^.ModeSOddLatitude,
                       APRSMessageObject^.ModeSOddLongitude,
                       Decoded.OddCPR, Latitude, Longitude) then
    begin
      PositionChanged := not APRSMessageObject^.ModeSPositionValid or
        (APRSMessageObject^.Latitude <> Latitude) or
        (APRSMessageObject^.Longitude <> Longitude);
      if PositionChanged then
      begin
        APRSMessageObject^.Latitude := Latitude;
        APRSMessageObject^.Longitude := Longitude;
      end;
      APRSMessageObject^.ModeSPositionValid := True;
    end;
  end;
  APRSMessageObject^.Time := FrameTime;
  APRSMessageObject^.ImageIndex := 7;
  APRSMessageObject^.ModeS := True;
  APRSMessageObject^.Checksum := Key;
  if (AltitudeChanged or VelocityChanged or CourseChanged or PositionChanged or IdentityChanged) and
     (ModeSUpdateQueue.IndexOf(Key) < 0) then
    ModeSUpdateQueue.Add(Key);
end;

procedure TModeSThread.AddNativeAISMessage(const Decoded: TAISMessage);
var
  APRSMessageObject: PAPRSMessage;
  Key: String;
  PositionChanged, SpeedChanged, CourseChanged, NameChanged: Boolean;
  Speed: Double;
begin
  Key := IntToStr(Decoded.MMSI);
  PositionChanged := False;
  SpeedChanged := False;
  CourseChanged := False;
  NameChanged := False;
  APRSMessageObject := PAPRSMessage(AISMessageList.Find(Key));
  if not Assigned(APRSMessageObject) then
  begin
    New(APRSMessageObject);
    FillChar(APRSMessageObject^, SizeOf(TAPRSMessage), 0);
    APRSMessageObject^.Speed := TDoubleList.Create;
    APRSMessageObject^.Track := TGPSTrack.Create;
    APRSMessageObject^.Track.Visible := True;
    APRSMessageObject^.Track.LineWidth := 1;
    APRSMessageObject^.FromCall := Key;
    APRSMessageObject^.Checksum := Key;
    AISMessageList.Add(Key, APRSMessageObject);
  end;
  if Decoded.HasName and (APRSMessageObject^.FromCall <> Decoded.ShipName) then
  begin
    APRSMessageObject^.FromCall := Decoded.ShipName;
    NameChanged := True;
  end;
  if Decoded.HasPosition then
  begin
    PositionChanged := not APRSMessageObject^.AISPositionValid or
      (APRSMessageObject^.Latitude <> Decoded.Latitude) or
      (APRSMessageObject^.Longitude <> Decoded.Longitude);
    if PositionChanged then
    begin
      APRSMessageObject^.Latitude := Decoded.Latitude;
      APRSMessageObject^.Longitude := Decoded.Longitude;
      if Assigned(APRSMessageObject^.Track) then
        APRSMessageObject^.Track.Points.Add(TGPSPoint.Create(Decoded.Longitude,
          Decoded.Latitude, 0));
    end;
    APRSMessageObject^.AISPositionValid := True;
  end;
  if Decoded.HasPosition then
  begin
    Speed := Decoded.SOG * 1.852;
    SpeedChanged := AppendHistoryValue(APRSMessageObject^.Speed, Speed);
    CourseChanged := APRSMessageObject^.Course <> Decoded.COG;
    APRSMessageObject^.Course := Decoded.COG;
    APRSMessageObject^.AISHeading := Decoded.Heading;
  end;
  APRSMessageObject^.Time := Now;
  if (PositionChanged or SpeedChanged or CourseChanged or NameChanged) and
     (AISUpdateQueue.IndexOf(Key) < 0) then
    AISUpdateQueue.Add(Key);
end;

end.

