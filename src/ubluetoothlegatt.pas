unit ubluetoothlegatt;

{$mode ObjFPC}{$H+}
{$linklib bluetooth}

interface

uses
  Classes, SysUtils, ctypes, dbus, Bluetooth;

const
  BLUEZ_SERVICE = 'org.bluez';
  BLUEZ_DEVICE_IFACE = 'org.bluez.Device1';
  BLUEZ_GATT_CHARACTERISTIC_IFACE = 'org.bluez.GattCharacteristic1';
  DBUS_OBJECT_MANAGER_IFACE = 'org.freedesktop.DBus.ObjectManager';
  DBUS_PROPERTIES_IFACE = 'org.freedesktop.DBus.Properties';

type
  TBlueZGattWriteResult = (bgwrSuccess, bgwrInProgress, bgwrFailure);

  { Native BlueZ D-Bus GATT extension for BluetoothLaz.
    BluetoothLaz supplies the BlueZ address bindings but has no BLE/GATT API. }
  TBlueZGattClient = class
  private
    FConnection: PDBusConnection;
    FAddress: String;
    FDevicePath: String;
    FRxPath: String;
    FTxPath: String;
    FConnected: Boolean;
    FLastError: String;
    function ErrorText(var Error: DBusError): String;
    function CallNoArgs(const Path, InterfaceName, MemberName: String;
      IgnoreAlreadyConnected: Boolean = False): Boolean;
    function FindCharacteristics(const RxUUID, TxUUID: String): Boolean;
    function ExtractNotification(Message_: PDBusMessage; out Data: TBytes): Boolean;
  protected
    function IsReadyToWrite: Boolean; virtual;
    function WriteValueOnce(const Data: TBytes): TBlueZGattWriteResult; virtual;
  public
    constructor Create(const Address: String);
    destructor Destroy; override;
    function Connect(const RxUUID, TxUUID: String): Boolean;
    procedure Disconnect;
    function WriteValue(const Data: TBytes): Boolean;
    function PollNotification(TimeoutMs: Integer; out Data: TBytes): Boolean;
    property Connected: Boolean read FConnected;
    property LastError: String read FLastError;
    property RxPath: String read FRxPath;
    property TxPath: String read FTxPath;
  end;

function IsBlueZInProgressError(const ErrorText: String): Boolean;
function BluetoothAddressToBlueZPath(const Address: String): String; overload;
function BluetoothAddressToBlueZPath(const Address: String;
  AdapterID: Integer): String; overload;

implementation

function IsBlueZInProgressError(const ErrorText: String): Boolean;
var
  Normalized: String;
begin
  Normalized := LowerCase(ErrorText);
  Result := (Pos('org.bluez.error.inprogress', Normalized) > 0) or
    (Pos('in progress', Normalized) > 0);
end;

function BluetoothAddressToBlueZPath(const Address: String;
  AdapterID: Integer): String;
var
  A: String;
begin
  A := UpperCase(Trim(Address));
  if bachk(pcchar(PAnsiChar(A))) <> 0 then
    raise EConvertError.CreateFmt('Invalid Bluetooth address: %s', [Address]);
  if AdapterID < 0 then
    raise EConvertError.Create('No Bluetooth adapter available');
  Result := Format('/org/bluez/hci%d/dev_%s',
    [AdapterID, StringReplace(A, ':', '_', [rfReplaceAll])]);
end;

function BluetoothAddressToBlueZPath(const Address: String): String;
var
  AdapterID: Integer;
begin
  AdapterID := hci_get_route(nil);
  if AdapterID < 0 then AdapterID := 0;
  Result := BluetoothAddressToBlueZPath(Address, AdapterID);
end;

constructor TBlueZGattClient.Create(const Address: String);
begin
  inherited Create;
  FAddress := UpperCase(Trim(Address));
  FDevicePath := BluetoothAddressToBlueZPath(FAddress);
end;

function TBlueZGattClient.ErrorText(var Error: DBusError): String;
begin
  if Assigned(Error.message) then
    Result := String(Error.message)
  else if Assigned(Error.name) then
    Result := String(Error.name)
  else
    Result := 'unknown D-Bus error';
end;

function TBlueZGattClient.CallNoArgs(const Path, InterfaceName,
  MemberName: String; IgnoreAlreadyConnected: Boolean): Boolean;
var
  Request, Reply: PDBusMessage;
  Error: DBusError;
  ErrorName: String;
begin
  Result := False;
  Request := dbus_message_new_method_call(BLUEZ_SERVICE, PChar(Path),
    PChar(InterfaceName), PChar(MemberName));
  if not Assigned(Request) then
  begin
    FLastError := 'Cannot allocate D-Bus request';
    Exit;
  end;
  dbus_error_init(@Error);
  Reply := dbus_connection_send_with_reply_and_block(FConnection, Request,
    10000, @Error);
  dbus_message_unref(Request);
  if not Assigned(Reply) then
  begin
    ErrorName := '';
    if Assigned(Error.name) then ErrorName := String(Error.name);
    if IgnoreAlreadyConnected and
      ((ErrorName = 'org.bluez.Error.AlreadyConnected') or
       (Pos('Already Connected', ErrorText(Error)) > 0)) then
      Result := True
    else
      FLastError := MemberName + ': ' + ErrorText(Error);
    dbus_error_free(@Error);
    Exit;
  end;
  dbus_message_unref(Reply);
  dbus_error_free(@Error);
  Result := True;
end;

function ReadBasicString(var Iter: DBusMessageIter): String;
var
  Value: PAnsiChar;
begin
  Value := nil;
  dbus_message_iter_get_basic(@Iter, @Value);
  if Assigned(Value) then Result := String(Value) else Result := '';
end;

function FindStringProperty(var PropertiesIter: DBusMessageIter;
  const WantedName: String; out Value: String): Boolean;
var
  PropertyEntry, PropertyValue: DBusMessageIter;
  Name: String;
begin
  Result := False;
  Value := '';
  while dbus_message_iter_get_arg_type(@PropertiesIter) <> DBUS_TYPE_INVALID do
  begin
    if dbus_message_iter_get_arg_type(@PropertiesIter) = DBUS_TYPE_DICT_ENTRY then
    begin
      dbus_message_iter_recurse(@PropertiesIter, @PropertyEntry);
      Name := ReadBasicString(PropertyEntry);
      if (dbus_message_iter_next(@PropertyEntry) <> 0) and (Name = WantedName) and
        (dbus_message_iter_get_arg_type(@PropertyEntry) = DBUS_TYPE_VARIANT) then
      begin
        dbus_message_iter_recurse(@PropertyEntry, @PropertyValue);
        if dbus_message_iter_get_arg_type(@PropertyValue) in
          [DBUS_TYPE_STRING, DBUS_TYPE_OBJECT_PATH] then
        begin
          Value := ReadBasicString(PropertyValue);
          Exit(True);
        end;
      end;
    end;
    if dbus_message_iter_next(@PropertiesIter) = 0 then Break;
  end;
end;

function TBlueZGattClient.FindCharacteristics(const RxUUID, TxUUID: String): Boolean;
var
  Request, Reply: PDBusMessage;
  Error: DBusError;
  Root, Objects, ObjectEntry, Interfaces, InterfaceEntry, Properties: DBusMessageIter;
  ObjectPath, InterfaceName, UUID: String;
begin
  Result := False;
  FRxPath := '';
  FTxPath := '';
  Request := dbus_message_new_method_call(BLUEZ_SERVICE, '/',
    DBUS_OBJECT_MANAGER_IFACE, 'GetManagedObjects');
  if not Assigned(Request) then Exit;
  dbus_error_init(@Error);
  Reply := dbus_connection_send_with_reply_and_block(FConnection, Request,
    10000, @Error);
  dbus_message_unref(Request);
  if not Assigned(Reply) then
  begin
    FLastError := 'GetManagedObjects: ' + ErrorText(Error);
    dbus_error_free(@Error);
    Exit;
  end;
  dbus_error_free(@Error);
  try
    if (dbus_message_iter_init(Reply, @Root) = 0) or
      (dbus_message_iter_get_arg_type(@Root) <> DBUS_TYPE_ARRAY) then Exit;
    dbus_message_iter_recurse(@Root, @Objects);
    while dbus_message_iter_get_arg_type(@Objects) <> DBUS_TYPE_INVALID do
    begin
      if dbus_message_iter_get_arg_type(@Objects) = DBUS_TYPE_DICT_ENTRY then
      begin
        dbus_message_iter_recurse(@Objects, @ObjectEntry);
        ObjectPath := ReadBasicString(ObjectEntry);
        if (Pos(FDevicePath + '/', ObjectPath) = 1) and
          (dbus_message_iter_next(@ObjectEntry) <> 0) and
          (dbus_message_iter_get_arg_type(@ObjectEntry) = DBUS_TYPE_ARRAY) then
        begin
          dbus_message_iter_recurse(@ObjectEntry, @Interfaces);
          while dbus_message_iter_get_arg_type(@Interfaces) <> DBUS_TYPE_INVALID do
          begin
            if dbus_message_iter_get_arg_type(@Interfaces) = DBUS_TYPE_DICT_ENTRY then
            begin
              dbus_message_iter_recurse(@Interfaces, @InterfaceEntry);
              InterfaceName := ReadBasicString(InterfaceEntry);
              if (InterfaceName = BLUEZ_GATT_CHARACTERISTIC_IFACE) and
                (dbus_message_iter_next(@InterfaceEntry) <> 0) and
                (dbus_message_iter_get_arg_type(@InterfaceEntry) = DBUS_TYPE_ARRAY) then
              begin
                dbus_message_iter_recurse(@InterfaceEntry, @Properties);
                if FindStringProperty(Properties, 'UUID', UUID) then
                begin
                  if SameText(UUID, RxUUID) then FRxPath := ObjectPath;
                  if SameText(UUID, TxUUID) then FTxPath := ObjectPath;
                end;
              end;
            end;
            if dbus_message_iter_next(@Interfaces) = 0 then Break;
          end;
        end;
      end;
      if dbus_message_iter_next(@Objects) = 0 then Break;
    end;
    Result := (FRxPath <> '') and (FTxPath <> '');
    if not Result then
      FLastError := 'Nordic UART GATT characteristics were not found';
  finally
    dbus_message_unref(Reply);
  end;
end;

function TBlueZGattClient.Connect(const RxUUID, TxUUID: String): Boolean;
var
  Error: DBusError;
  I: Integer;
  MatchRule: String;
begin
  Result := False;
  FLastError := '';
  dbus_error_init(@Error);
  FConnection := dbus_bus_get(DBUS_BUS_SYSTEM, @Error);
  if not Assigned(FConnection) then
  begin
    FLastError := 'System D-Bus: ' + ErrorText(Error);
    dbus_error_free(@Error);
    Exit;
  end;
  dbus_error_free(@Error);

  if not CallNoArgs(FDevicePath, BLUEZ_DEVICE_IFACE, 'Connect', True) then Exit;
  for I := 1 to 40 do
  begin
    if FindCharacteristics(RxUUID, TxUUID) then Break;
    Sleep(250);
  end;
  if (FRxPath = '') or (FTxPath = '') then Exit;

  if not CallNoArgs(FTxPath, BLUEZ_GATT_CHARACTERISTIC_IFACE, 'StartNotify') then
    Exit;
  MatchRule := 'type=''signal'',sender=''org.bluez'',interface=''' +
    DBUS_PROPERTIES_IFACE + ''',member=''PropertiesChanged'',path=''' + FTxPath + '''';
  dbus_error_init(@Error);
  dbus_bus_add_match(FConnection, PChar(MatchRule), @Error);
  dbus_connection_flush(FConnection);
  if dbus_error_is_set(@Error) <> 0 then
  begin
    FLastError := 'Add notification match: ' + ErrorText(Error);
    dbus_error_free(@Error);
    Exit;
  end;
  dbus_error_free(@Error);
  FConnected := True;
  Result := True;
end;

procedure TBlueZGattClient.Disconnect;
begin
  if Assigned(FConnection) then
  begin
    if FTxPath <> '' then
      CallNoArgs(FTxPath, BLUEZ_GATT_CHARACTERISTIC_IFACE, 'StopNotify');
    if FConnected then
      CallNoArgs(FDevicePath, BLUEZ_DEVICE_IFACE, 'Disconnect');
    dbus_connection_unref(FConnection);
    FConnection := nil;
  end;
  FConnected := False;
end;

destructor TBlueZGattClient.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TBlueZGattClient.IsReadyToWrite: Boolean;
begin
  Result := FConnected;
end;

function TBlueZGattClient.WriteValueOnce(const Data: TBytes): TBlueZGattWriteResult;
var
  Request, Reply: PDBusMessage;
  Root, ByteArray, Options: DBusMessageIter;
  Error: DBusError;
  DataPtr: Pointer;
  ErrorName, ErrorMessage: String;
begin
  Result := bgwrFailure;
  Request := dbus_message_new_method_call(BLUEZ_SERVICE, PChar(FRxPath),
    BLUEZ_GATT_CHARACTERISTIC_IFACE, 'WriteValue');
  if not Assigned(Request) then Exit;
  dbus_message_iter_init_append(Request, @Root);
  if dbus_message_iter_open_container(@Root, DBUS_TYPE_ARRAY, 'y', @ByteArray) = 0 then
  begin
    dbus_message_unref(Request);
    Exit;
  end;
  DataPtr := @Data[0];
  dbus_message_iter_append_fixed_array(@ByteArray, DBUS_TYPE_BYTE, @DataPtr,
    Length(Data));
  dbus_message_iter_close_container(@Root, @ByteArray);
  dbus_message_iter_open_container(@Root, DBUS_TYPE_ARRAY, '{sv}', @Options);
  dbus_message_iter_close_container(@Root, @Options);

  dbus_error_init(@Error);
  Reply := dbus_connection_send_with_reply_and_block(FConnection, Request,
    10000, @Error);
  dbus_message_unref(Request);
  if not Assigned(Reply) then
  begin
    ErrorName := '';
    ErrorMessage := ErrorText(Error);
    if Assigned(Error.name) then ErrorName := String(Error.name);
    FLastError := 'WriteValue: ' + ErrorMessage;
    if IsBlueZInProgressError(ErrorName + ': ' + ErrorMessage) then
      Result := bgwrInProgress;
    dbus_error_free(@Error);
    Exit;
  end;
  dbus_message_unref(Reply);
  dbus_error_free(@Error);
  Result := bgwrSuccess;
end;

function TBlueZGattClient.WriteValue(const Data: TBytes): Boolean;
const
  MAX_IN_PROGRESS_RETRIES = 100;
  IN_PROGRESS_RETRY_DELAY_MS = 50;
var
  Attempt: Integer;
  WriteResult: TBlueZGattWriteResult;
begin
  Result := False;
  if not IsReadyToWrite or (Length(Data) = 0) then Exit;
  for Attempt := 1 to MAX_IN_PROGRESS_RETRIES do
  begin
    WriteResult := WriteValueOnce(Data);
    case WriteResult of
      bgwrSuccess:
        begin
          FLastError := '';
          Exit(True);
        end;
      bgwrFailure: Exit(False);
      bgwrInProgress:
        if Attempt < MAX_IN_PROGRESS_RETRIES then
          Sleep(IN_PROGRESS_RETRY_DELAY_MS);
    end;
  end;
end;

function TBlueZGattClient.ExtractNotification(Message_: PDBusMessage;
  out Data: TBytes): Boolean;
var
  Root, Changed, Entry, VariantValue, ByteArray: DBusMessageIter;
  InterfaceName, PropertyName: String;
  BytesPtr: PByte;
  Count, I: Integer;
begin
  Result := False;
  SetLength(Data, 0);
  if dbus_message_is_signal(Message_, DBUS_PROPERTIES_IFACE,
    'PropertiesChanged') = 0 then Exit;
  if String(dbus_message_get_path(Message_)) <> FTxPath then Exit;
  if dbus_message_iter_init(Message_, @Root) = 0 then Exit;
  InterfaceName := ReadBasicString(Root);
  if (InterfaceName <> BLUEZ_GATT_CHARACTERISTIC_IFACE) or
    (dbus_message_iter_next(@Root) = 0) or
    (dbus_message_iter_get_arg_type(@Root) <> DBUS_TYPE_ARRAY) then Exit;
  dbus_message_iter_recurse(@Root, @Changed);
  while dbus_message_iter_get_arg_type(@Changed) <> DBUS_TYPE_INVALID do
  begin
    if dbus_message_iter_get_arg_type(@Changed) = DBUS_TYPE_DICT_ENTRY then
    begin
      dbus_message_iter_recurse(@Changed, @Entry);
      PropertyName := ReadBasicString(Entry);
      if (PropertyName = 'Value') and (dbus_message_iter_next(@Entry) <> 0) and
        (dbus_message_iter_get_arg_type(@Entry) = DBUS_TYPE_VARIANT) then
      begin
        dbus_message_iter_recurse(@Entry, @VariantValue);
        if dbus_message_iter_get_arg_type(@VariantValue) = DBUS_TYPE_ARRAY then
        begin
          dbus_message_iter_recurse(@VariantValue, @ByteArray);
          BytesPtr := nil;
          Count := 0;
          dbus_message_iter_get_fixed_array(@ByteArray, @BytesPtr, @Count);
          if Count > 0 then
          begin
            SetLength(Data, Count);
            for I := 0 to Count - 1 do Data[I] := BytesPtr[I];
            Exit(True);
          end;
        end;
      end;
    end;
    if dbus_message_iter_next(@Changed) = 0 then Break;
  end;
end;

function TBlueZGattClient.PollNotification(TimeoutMs: Integer;
  out Data: TBytes): Boolean;
var
  Message_: PDBusMessage;
begin
  Result := False;
  SetLength(Data, 0);
  if not Assigned(FConnection) then Exit;
  dbus_connection_read_write(FConnection, TimeoutMs);
  repeat
    Message_ := dbus_connection_pop_message(FConnection);
    if not Assigned(Message_) then Break;
    try
      if ExtractNotification(Message_, Data) then Exit(True);
    finally
      dbus_message_unref(Message_);
    end;
  until False;
end;

end.
