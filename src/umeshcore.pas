unit umeshcore;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils, ubluetoothlegatt;

const
  MESHCORE_UART_SERVICE_UUID = '6e400001-b5a3-f393-e0a9-e50e24dcca9e';
  MESHCORE_UART_RX_UUID = '6e400002-b5a3-f393-e0a9-e50e24dcca9e';
  MESHCORE_UART_TX_UUID = '6e400003-b5a3-f393-e0a9-e50e24dcca9e';

type
  TMeshCoreMessageKind = (mcmPrivate, mcmChannel);
  TMeshCoreNodeKind = (mcnUnknown, mcnUser, mcnRepeater, mcnRoom, mcnSensor);

  TMeshCoreMessage = record
    Kind: TMeshCoreMessageKind;
    Sender: String;
    Channel: Byte;
    TimeStamp: Cardinal;
    Text: String;
  end;
  PMeshCoreMessage = ^TMeshCoreMessage;

  TMeshCoreNode = record
    Kind: TMeshCoreNodeKind;
    PublicKeyPrefix: String;
    Name: String;
    Latitude: Double;
    Longitude: Double;
    LastAdvert: Cardinal;
  end;
  PMeshCoreNode = ^TMeshCoreNode;

  TMeshCoreWeather = record
    PublicKeyPrefix: String;
    Sender: String;
    HasTemperature: Boolean;
    Temperature: Double;
    HasHumidity: Boolean;
    Humidity: Double;
    HasPressure: Boolean;
    Pressure: Double;
    HasIlluminance: Boolean;
    Illuminance: Double;
  end;
  PMeshCoreWeather = ^TMeshCoreWeather;

  { TMeshCoreClient

    BLE transport using BluetoothLaz plus the native BlueZ D-Bus GATT extension.
    The worker owns the connection and queues decoded data; callers consume it
    from the GUI thread. }
  TMeshCoreClient = class(TThread)
  private
    FAddress: String;
    FChannel: Byte;
    FTransport: TBlueZGattClient;
    FLock: TCriticalSection;
    FAPRSQueue: TStringList;
    FMessageQueue: TList;
    FNodeQueue: TList;
    FWeatherQueue: TList;
    FContactNames: TStringList;
    FSendQueue: TStringList;
    FConnected: Boolean;
    FLastError: String;
    procedure QueueReceived(const Msg: TMeshCoreMessage);
    procedure QueueNode(const Node: TMeshCoreNode);
    procedure QueueWeather(const Weather: TMeshCoreWeather);
    procedure ProcessPacket(const Packet: TBytes);
    procedure SendRaw(const Data: TBytes);
    procedure DrainSendQueue;
    procedure WaitForRetry(DelayMs: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(const Address: String; Channel: Byte);
    destructor Destroy; override;
    procedure Stop;
    procedure SendChannelText(const Text: String);
    function TryDequeueAPRS(out APRSLine: String): Boolean;
    function TryDequeueMessage(out Msg: TMeshCoreMessage): Boolean;
    function TryDequeueNode(out Node: TMeshCoreNode): Boolean;
    function TryDequeueWeather(out Weather: TMeshCoreWeather): Boolean;
    property Connected: Boolean read FConnected;
    property LastError: String read FLastError;
  end;

function BuildMeshCoreAppStart(const AppName: String): TBytes;
function BuildMeshCoreGetContacts: TBytes;
function BuildMeshCoreSyncNextMessage: TBytes;
function MeshCoreResponseNeedsNextMessage(PacketType: Byte): Boolean;
function MeshCoreResponseStartsMessageSync(PacketType: Byte): Boolean;
function BuildMeshCoreChannelMessage(Channel: Byte; const Text: String;
  TimeStamp: Cardinal = 0): TBytes;
function MeshCoreBytesToHex(const Data: TBytes): String;
function MeshCoreHexToBytes(const Text: String): TBytes;
function ParseMeshCorePacket(const Packet: TBytes; out Msg: TMeshCoreMessage): Boolean;
function MeshCoreTextToAPRS(const Text: String; out APRSLine: String): Boolean;
function MeshCoreMailType(Kind: TMeshCoreMessageKind): String;
function MeshCoreMailRecipient(const Msg: TMeshCoreMessage;
  const OwnCall: String): String;
function ParseMeshCoreNode(const Packet: TBytes; out Node: TMeshCoreNode): Boolean;
function MeshCoreNodeTypeName(Kind: TMeshCoreNodeKind): String;
function ParseMeshCoreWeather(const Packet: TBytes;
  out Weather: TMeshCoreWeather): Boolean;

implementation

function ReadLE32(const Data: TBytes; Index: Integer): Cardinal;
begin
  Result := Cardinal(Data[Index]) or (Cardinal(Data[Index + 1]) shl 8) or
    (Cardinal(Data[Index + 2]) shl 16) or (Cardinal(Data[Index + 3]) shl 24);
end;

procedure AppendByte(var Data: TBytes; Value: Byte);
var
  N: Integer;
begin
  N := Length(Data);
  SetLength(Data, N + 1);
  Data[N] := Value;
end;

procedure AppendString(var Data: TBytes; const Value: UTF8String);
var
  I, N: Integer;
begin
  N := Length(Data);
  SetLength(Data, N + Length(Value));
  for I := 1 to Length(Value) do
    Data[N + I - 1] := Ord(Value[I]);
end;

function BuildMeshCoreAppStart(const AppName: String): TBytes;
var
  Name: UTF8String;
  I: Integer;
  Data: TBytes;
begin
  SetLength(Data, 8);
  Data[0] := $01;
  Data[1] := $03;
  for I := 2 to 7 do
    Data[I] := Ord(' ');
  Name := UTF8Encode(Copy(AppName, 1, 15));
  AppendString(Data, Name);
  Result := Data;
end;

function BuildMeshCoreGetContacts: TBytes;
begin
  Result := TBytes.Create($04);
end;

function BuildMeshCoreSyncNextMessage: TBytes;
begin
  Result := TBytes.Create($0A);
end;

function MeshCoreResponseNeedsNextMessage(PacketType: Byte): Boolean;
begin
  Result := PacketType in [$07, $08, $10, $11, $1B];
end;

function MeshCoreResponseStartsMessageSync(PacketType: Byte): Boolean;
begin
  Result := PacketType in [$04, $83];
end;

function BuildMeshCoreChannelMessage(Channel: Byte; const Text: String;
  TimeStamp: Cardinal): TBytes;
var
  Payload: UTF8String;
  Data: TBytes;
begin
  if TimeStamp = 0 then
    TimeStamp := DateTimeToUnix(Now);
  SetLength(Data, 7);
  Data[0] := $03;
  Data[1] := $00;
  Data[2] := Channel;
  Data[3] := TimeStamp and $FF;
  Data[4] := (TimeStamp shr 8) and $FF;
  Data[5] := (TimeStamp shr 16) and $FF;
  Data[6] := (TimeStamp shr 24) and $FF;
  Payload := UTF8Encode(Text);
  AppendString(Data, Payload);
  Result := Data;
end;

function MeshCoreBytesToHex(const Data: TBytes): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Data) do
  begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + LowerCase(IntToHex(Data[I], 2));
  end;
end;

function IsHexDigit(C: Char): Boolean;
begin
  Result := C in ['0'..'9', 'a'..'f', 'A'..'F'];
end;

function MeshCoreHexToBytes(const Text: String): TBytes;
var
  I: Integer;
  Pair: String;
  Data: TBytes;
begin
  SetLength(Data, 0);
  I := 1;
  while I <= Length(Text) do
  begin
    while (I <= Length(Text)) and not IsHexDigit(Text[I]) do
      Inc(I);
    if (I + 1 <= Length(Text)) and IsHexDigit(Text[I]) and
      IsHexDigit(Text[I + 1]) then
    begin
      Pair := Copy(Text, I, 2);
      AppendByte(Data, StrToInt('$' + Pair));
      Inc(I, 2);
    end
    else
      Break;
  end;
  Result := Data;
end;

function BytesToUTF8(const Data: TBytes; StartIndex: Integer): String;
var
  S: UTF8String;
  I, N: Integer;
begin
  if StartIndex > High(Data) then
    Exit('');
  N := Length(Data) - StartIndex;
  SetLength(S, N);
  for I := 0 to N - 1 do
    S[I + 1] := AnsiChar(Data[StartIndex + I]);
  while (Length(S) > 0) and (S[Length(S)] = #0) do
    Delete(S, Length(S), 1);
  Result := S;
end;

function PrefixToHex(const Data: TBytes; StartIndex: Integer): String;
var
  I: Integer;
begin
  Result := '';
  for I := StartIndex to StartIndex + 5 do
    Result := Result + LowerCase(IntToHex(Data[I], 2));
end;

function BytesRangeToUTF8(const Data: TBytes; StartIndex, Count: Integer): String;
var
  S: UTF8String;
  I, N: Integer;
begin
  N := 0;
  while (N < Count) and (StartIndex + N < Length(Data)) and
    (Data[StartIndex + N] <> 0) do Inc(N);
  SetLength(S, N);
  for I := 0 to N - 1 do S[I + 1] := AnsiChar(Data[StartIndex + I]);
  Result := String(S);
end;

function MeshCoreNodeKindFromByte(Value: Byte): TMeshCoreNodeKind;
begin
  case Value of
    1: Result := mcnUser;
    2: Result := mcnRepeater;
    3: Result := mcnRoom;
    4: Result := mcnSensor;
    else Result := mcnUnknown;
  end;
end;

function MeshCoreNodeTypeName(Kind: TMeshCoreNodeKind): String;
begin
  case Kind of
    mcnUser: Result := 'User';
    mcnRepeater: Result := 'Repeater';
    mcnRoom: Result := 'Room Server';
    mcnSensor: Result := 'Sensor';
    else Result := 'Node';
  end;
end;

function ParseMeshCoreNode(const Packet: TBytes; out Node: TMeshCoreNode): Boolean;
var
  LatRaw, LonRaw: LongInt;
begin
  Result := False;
  Node := Default(TMeshCoreNode);
  if (Length(Packet) < 148) or not (Packet[0] in [$03, $8A]) then Exit;
  Node.PublicKeyPrefix := PrefixToHex(Packet, 1);
  Node.Kind := MeshCoreNodeKindFromByte(Packet[33]);
  Node.Name := Trim(BytesRangeToUTF8(Packet, 100, 32));
  if Node.Name = '' then Node.Name := 'MeshCore ' + Node.PublicKeyPrefix;
  Node.LastAdvert := ReadLE32(Packet, 132);
  LatRaw := LongInt(ReadLE32(Packet, 136));
  LonRaw := LongInt(ReadLE32(Packet, 140));
  Node.Latitude := LatRaw / 1000000.0;
  Node.Longitude := LonRaw / 1000000.0;
  Result := ((Node.Latitude <> 0.0) or (Node.Longitude <> 0.0)) and
    (Abs(Node.Latitude) <= 90.0) and (Abs(Node.Longitude) <= 180.0);
end;

function LPPValueSize(ValueType: Byte): Integer;
begin
  case ValueType of
    0, 1, 102, 104, 120, 142: Result := 1;
    2, 3, 101, 103, 115, 116, 117, 121, 125, 128, 132: Result := 2;
    122: Result := 3;
    100, 118, 130, 131, 133: Result := 4;
    113, 134: Result := 6;
    135: Result := 3;
    136: Result := 9;
    else Result := 0;
  end;
end;

function ReadBE16(const Data: TBytes; Index: Integer): Word;
begin
  Result := (Word(Data[Index]) shl 8) or Word(Data[Index + 1]);
end;

function ParseMeshCoreWeather(const Packet: TBytes;
  out Weather: TMeshCoreWeather): Boolean;
var
  Index, ValueSize: Integer;
  ValueType: Byte;
  SignedValue: SmallInt;
begin
  Result := False;
  Weather := Default(TMeshCoreWeather);
  if (Length(Packet) < 11) or (Packet[0] <> $8B) then Exit;
  Weather.PublicKeyPrefix := PrefixToHex(Packet, 2);
  Weather.Sender := Weather.PublicKeyPrefix;
  Index := 8;
  while (Index + 1 < Length(Packet)) and (Packet[Index] <> 0) do
  begin
    ValueType := Packet[Index + 1];
    ValueSize := LPPValueSize(ValueType);
    if (ValueSize = 0) or (Index + 2 + ValueSize > Length(Packet)) then Break;
    case ValueType of
      101: begin
        Weather.Illuminance := ReadBE16(Packet, Index + 2);
        Weather.HasIlluminance := True;
      end;
      103: begin
        SignedValue := SmallInt(ReadBE16(Packet, Index + 2));
        Weather.Temperature := SignedValue / 10.0;
        Weather.HasTemperature := True;
      end;
      104: begin
        Weather.Humidity := Packet[Index + 2] / 2.0;
        Weather.HasHumidity := True;
      end;
      115: begin
        Weather.Pressure := ReadBE16(Packet, Index + 2) / 10.0;
        Weather.HasPressure := True;
      end;
    end;
    Inc(Index, 2 + ValueSize);
  end;
  Result := Weather.HasTemperature or Weather.HasHumidity or
    Weather.HasPressure or Weather.HasIlluminance;
end;

function ParseMeshCorePacket(const Packet: TBytes; out Msg: TMeshCoreMessage): Boolean;
var
  TextIndex, TxtType: Integer;
begin
  Result := False;
  Msg := Default(TMeshCoreMessage);
  if Length(Packet) = 0 then
    Exit;

  case Packet[0] of
    $07: begin
      if Length(Packet) < 13 then Exit;
      Msg.Kind := mcmPrivate;
      Msg.Sender := PrefixToHex(Packet, 1);
      TxtType := Packet[8];
      Msg.TimeStamp := ReadLE32(Packet, 9);
      TextIndex := 13;
      if TxtType = 2 then Inc(TextIndex, 4);
    end;
    $10: begin
      if Length(Packet) < 16 then Exit;
      Msg.Kind := mcmPrivate;
      Msg.Sender := PrefixToHex(Packet, 4);
      TxtType := Packet[11];
      Msg.TimeStamp := ReadLE32(Packet, 12);
      TextIndex := 16;
      if TxtType = 2 then Inc(TextIndex, 4);
    end;
    $08: begin
      if Length(Packet) < 8 then Exit;
      Msg.Kind := mcmChannel;
      Msg.Channel := Packet[1];
      Msg.Sender := 'MeshCore CH' + IntToStr(Msg.Channel);
      Msg.TimeStamp := ReadLE32(Packet, 4);
      TextIndex := 8;
    end;
    $11: begin
      if Length(Packet) < 11 then Exit;
      Msg.Kind := mcmChannel;
      Msg.Channel := Packet[4];
      Msg.Sender := 'MeshCore CH' + IntToStr(Msg.Channel);
      Msg.TimeStamp := ReadLE32(Packet, 7);
      TextIndex := 11;
    end;
    else Exit;
  end;

  if TextIndex > Length(Packet) then
    Exit;
  Msg.Text := BytesToUTF8(Packet, TextIndex);
  Result := Msg.Text <> '';
end;

function MeshCoreTextToAPRS(const Text: String; out APRSLine: String): Boolean;
var
  PArrow, PColon: Integer;
  DataType: Char;
begin
  APRSLine := Trim(Text);
  PArrow := Pos('>', APRSLine);
  PColon := Pos(':', APRSLine);
  Result := (PArrow > 1) and (PColon > PArrow + 1) and
    (PColon < Length(APRSLine));
  if Result then
  begin
    DataType := APRSLine[PColon + 1];
    Result := DataType in ['!', '=', '/', '@'];
  end;
  if not Result then
    APRSLine := '';
end;

function MeshCoreMailType(Kind: TMeshCoreMessageKind): String;
begin
  if Kind = mcmChannel then
    Result := 'BLN'
  else
    Result := 'MSG';
end;

function MeshCoreMailRecipient(const Msg: TMeshCoreMessage;
  const OwnCall: String): String;
begin
  if Msg.Kind = mcmChannel then
    Result := 'BLN' + IntToStr(Msg.Channel)
  else
    Result := OwnCall;
end;

constructor TMeshCoreClient.Create(const Address: String; Channel: Byte);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FAddress := Trim(Address);
  FChannel := Channel;
  FLock := TCriticalSection.Create;
  FAPRSQueue := TStringList.Create;
  FMessageQueue := TList.Create;
  FNodeQueue := TList.Create;
  FWeatherQueue := TList.Create;
  FContactNames := TStringList.Create;
  FContactNames.NameValueSeparator := '=';
  FSendQueue := TStringList.Create;
  Start;
end;

destructor TMeshCoreClient.Destroy;
var
  MessageItem: PMeshCoreMessage;
  NodeItem: PMeshCoreNode;
  WeatherItem: PMeshCoreWeather;
begin
  Stop;
  while FMessageQueue.Count > 0 do
  begin
    MessageItem := PMeshCoreMessage(FMessageQueue[0]);
    FMessageQueue.Delete(0);
    Dispose(MessageItem);
  end;
  while FNodeQueue.Count > 0 do
  begin
    NodeItem := PMeshCoreNode(FNodeQueue[0]);
    FNodeQueue.Delete(0);
    Dispose(NodeItem);
  end;
  while FWeatherQueue.Count > 0 do
  begin
    WeatherItem := PMeshCoreWeather(FWeatherQueue[0]);
    FWeatherQueue.Delete(0);
    Dispose(WeatherItem);
  end;
  FSendQueue.Free;
  FContactNames.Free;
  FWeatherQueue.Free;
  FNodeQueue.Free;
  FMessageQueue.Free;
  FAPRSQueue.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TMeshCoreClient.Stop;
begin
  if not Terminated then
    Terminate;
  if not Finished then
    WaitFor;
end;

procedure TMeshCoreClient.SendRaw(const Data: TBytes);
begin
  if not Assigned(FTransport) then Exit;
  if not FTransport.WriteValue(Data) then
    FLastError := FTransport.LastError;
end;

procedure TMeshCoreClient.SendChannelText(const Text: String);
begin
  if Text = '' then Exit;
  FLock.Acquire;
  try
    FSendQueue.Add(Text);
  finally
    FLock.Release;
  end;
end;

procedure TMeshCoreClient.DrainSendQueue;
var
  Text: String;
begin
  Text := '';
  FLock.Acquire;
  try
    if FSendQueue.Count > 0 then
    begin
      Text := FSendQueue[0];
      FSendQueue.Delete(0);
    end;
  finally
    FLock.Release;
  end;
  if Text <> '' then
    SendRaw(BuildMeshCoreChannelMessage(FChannel, Text));
end;

procedure TMeshCoreClient.WaitForRetry(DelayMs: Integer);
const
  RETRY_SLICE_MS = 100;
begin
  while (DelayMs > 0) and not Terminated do
  begin
    if DelayMs < RETRY_SLICE_MS then
      Sleep(DelayMs)
    else
      Sleep(RETRY_SLICE_MS);
    Dec(DelayMs, RETRY_SLICE_MS);
  end;
end;

procedure TMeshCoreClient.QueueReceived(const Msg: TMeshCoreMessage);
var
  APRSLine: String;
  Item: PMeshCoreMessage;
begin
  New(Item);
  Item^ := Msg;
  FLock.Acquire;
  try
    if (Item^.Kind = mcmPrivate) and
      (FContactNames.Values[Item^.Sender] <> '') then
      Item^.Sender := FContactNames.Values[Item^.Sender];
    FMessageQueue.Add(Item);
    if MeshCoreTextToAPRS(Msg.Text, APRSLine) then
      FAPRSQueue.Add(APRSLine);
    Item := nil;
  finally
    FLock.Release;
    if Assigned(Item) then Dispose(Item);
  end;
end;

procedure TMeshCoreClient.QueueNode(const Node: TMeshCoreNode);
var
  Item: PMeshCoreNode;
begin
  New(Item);
  Item^ := Node;
  FLock.Acquire;
  try
    FNodeQueue.Add(Item);
    FContactNames.Values[Node.PublicKeyPrefix] := Node.Name;
    Item := nil;
  finally
    FLock.Release;
    if Assigned(Item) then Dispose(Item);
  end;
end;

procedure TMeshCoreClient.QueueWeather(const Weather: TMeshCoreWeather);
var
  Item: PMeshCoreWeather;
begin
  New(Item);
  Item^ := Weather;
  FLock.Acquire;
  try
    if FContactNames.Values[Item^.PublicKeyPrefix] <> '' then
      Item^.Sender := FContactNames.Values[Item^.PublicKeyPrefix];
    FWeatherQueue.Add(Item);
    Item := nil;
  finally
    FLock.Release;
    if Assigned(Item) then Dispose(Item);
  end;
end;

function TMeshCoreClient.TryDequeueAPRS(out APRSLine: String): Boolean;
begin
  APRSLine := '';
  FLock.Acquire;
  try
    Result := FAPRSQueue.Count > 0;
    if Result then
    begin
      APRSLine := FAPRSQueue[0];
      FAPRSQueue.Delete(0);
    end;
  finally
    FLock.Release;
  end;
end;

function TMeshCoreClient.TryDequeueMessage(out Msg: TMeshCoreMessage): Boolean;
var
  Item: PMeshCoreMessage;
begin
  Msg := Default(TMeshCoreMessage);
  Item := nil;
  FLock.Acquire;
  try
    Result := FMessageQueue.Count > 0;
    if Result then
    begin
      Item := PMeshCoreMessage(FMessageQueue[0]);
      FMessageQueue.Delete(0);
      Msg := Item^;
    end;
  finally
    FLock.Release;
  end;
  if Assigned(Item) then Dispose(Item);
end;

function TMeshCoreClient.TryDequeueNode(out Node: TMeshCoreNode): Boolean;
var
  Item: PMeshCoreNode;
begin
  Node := Default(TMeshCoreNode);
  Item := nil;
  FLock.Acquire;
  try
    Result := FNodeQueue.Count > 0;
    if Result then
    begin
      Item := PMeshCoreNode(FNodeQueue[0]);
      FNodeQueue.Delete(0);
      Node := Item^;
    end;
  finally
    FLock.Release;
  end;
  if Assigned(Item) then Dispose(Item);
end;

function TMeshCoreClient.TryDequeueWeather(out Weather: TMeshCoreWeather): Boolean;
var
  Item: PMeshCoreWeather;
begin
  Weather := Default(TMeshCoreWeather);
  Item := nil;
  FLock.Acquire;
  try
    Result := FWeatherQueue.Count > 0;
    if Result then
    begin
      Item := PMeshCoreWeather(FWeatherQueue[0]);
      FWeatherQueue.Delete(0);
      Weather := Item^;
    end;
  finally
    FLock.Release;
  end;
  if Assigned(Item) then Dispose(Item);
end;

procedure TMeshCoreClient.ProcessPacket(const Packet: TBytes);
var
  Msg: TMeshCoreMessage;
  Node: TMeshCoreNode;
  Weather: TMeshCoreWeather;
begin
  if Length(Packet) = 0 then Exit;
  if Packet[0] = $05 then
  begin
    FConnected := True;
    SendRaw(BuildMeshCoreGetContacts);
    Exit;
  end;
  if MeshCoreResponseStartsMessageSync(Packet[0]) then
  begin
    SendRaw(BuildMeshCoreSyncNextMessage);
    Exit;
  end;
  if MeshCoreResponseNeedsNextMessage(Packet[0]) then
  begin
    if ParseMeshCorePacket(Packet, Msg) then
      QueueReceived(Msg);
    SendRaw(BuildMeshCoreSyncNextMessage);
    Exit;
  end;
  if ParseMeshCoreNode(Packet, Node) then
  begin
    QueueNode(Node);
    Exit;
  end;
  if ParseMeshCoreWeather(Packet, Weather) then
  begin
    QueueWeather(Weather);
    Exit;
  end;
  if ParseMeshCorePacket(Packet, Msg) then
    QueueReceived(Msg);
end;

procedure TMeshCoreClient.Execute;
var
  Packet: TBytes;
  Ready: Boolean;
begin
  if FAddress = '' then Exit;
  while not Terminated do
  begin
    FTransport := TBlueZGattClient.Create(FAddress);
    try
      try
        Ready := FTransport.Connect(MESHCORE_UART_RX_UUID,
          MESHCORE_UART_TX_UUID);
        if not Ready then
          FLastError := FTransport.LastError;

        if Ready then
        begin
          FConnected := True;
          while not Terminated and FTransport.Connected and
            not FTransport.WriteValue(BuildMeshCoreAppStart('aprsmap')) do
          begin
            FLastError := FTransport.LastError;
            if not IsBlueZInProgressError(FLastError) then
            begin
              Ready := False;
              Break;
            end;
            WaitForRetry(50);
          end;
          if Terminated or not FTransport.Connected then
            Ready := False;

          if Ready then
          begin
            FLastError := '';
            while not Terminated and FTransport.Connected do
            begin
              if FTransport.PollNotification(25, Packet) then
                ProcessPacket(Packet);
              DrainSendQueue;
            end;
          end;
        end;
      except
        on E: Exception do FLastError := E.Message;
      end;
    finally
      FConnected := False;
      FTransport.Free;
      FTransport := nil;
    end;

    if not Terminated then
      WaitForRetry(3000);
  end;
end;

end.
