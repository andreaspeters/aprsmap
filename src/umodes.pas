unit umodes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utypes, Contnrs, mvGpsObj, Math, umodesdecoder, urtlsdr;

type
  { TModeSThread }
  TModeSThread = class(TThread)
  private
    FConfig: PAPRSConfig;
    NativeDevice: TRtlSdrDev;
    NativeMagnitude: TModeSMagnitude;
    NativeMessages: array[0..255] of TModeSMessage;

    procedure OpenNative;
    procedure CloseNative;
    procedure ProcessNativeSamples(const Buffer: PByte; const ByteCount: Integer);
    procedure AddNativeMessage(const Decoded: TModeSMessage);
    class procedure NativeReadCallback(Buf: PByte; Len: Cardinal; Context: Pointer); static; cdecl;

  protected
    procedure Execute; override;
  public
    ModeSMessageList: TFPHashList;
    Error: Boolean;
    procedure Stop;
    constructor Create(Config: PAPRSConfig);
  end;

implementation

uses
  uaprs;

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

  FreeOnTerminate := True;
  ModeSMessageList := TFPHashList.Create;
  OpenNative;
  Start;
end;

procedure TModeSThread.Execute;
begin
  try
    if FConfig^.ModeSEnabled and Assigned(NativeDevice) then
      if RtlSdrReadAsync(NativeDevice, @NativeReadCallback, Self, 12, 262144) <> 0 then
        Error := True;
  finally
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
    RtlSdrSetCenterFreq(NativeDevice, 1090000000);
    RtlSdrSetSampleRate(NativeDevice, 2000000);
    RtlSdrSetTunerGainMode(NativeDevice, 0);
    RtlSdrSetAgcMode(NativeDevice, 1);
    RtlSdrResetBuffer(NativeDevice);

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

procedure TModeSThread.ProcessNativeSamples(const Buffer: PByte; const ByteCount: Integer);
var
  I, Count: Integer;
begin
  if (ByteCount < 32) or Odd(ByteCount) then Exit;
  SetLength(NativeMagnitude, ByteCount div 2);
  for I := 0 to High(NativeMagnitude) do
    NativeMagnitude[I] := Sqr(Integer(Buffer[I * 2]) - 127) +
                          Sqr(Integer(Buffer[I * 2 + 1]) - 127);
  Count := DemodulateModeS(NativeMagnitude, NativeMessages);
  if Count > Length(NativeMessages) then Count := Length(NativeMessages);
  for I := 0 to Count - 1 do
    AddNativeMessage(NativeMessages[I]);
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
begin
  Key := IntToHex(Decoded.ICAO, 6);
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
      APRSMessageObject^.ModeSPositionValid := True;
    end;
  end;
  APRSMessageObject^.Time := Now;
  APRSMessageObject^.ImageIndex := 7;
  APRSMessageObject^.ModeS := True;
  APRSMessageObject^.Checksum := Key;
end;

end.

