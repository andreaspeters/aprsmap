unit usettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  Buttons, StdCtrls, ComboEx, Spin, utypes, uini, ugps, uaprs, uigate, umodes,
  umeshcore, Bluetooth, ctypes;

type

  { TFSettings }

  TFSettings = class(TForm)
    BPDefaultButtons: TButtonPanel;
    CBMeshCoreDevices: TComboBox;
    CBMeshCoreEnable: TCheckBox;
    CBMeshCoreSendPosition: TCheckBox;
    CBESymbol: TComboBoxEx;
    cbModeSEnable: TCheckBox;
    cbIGateEnable: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBoxMeshCore: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    LabelMeshCoreDevice: TLabel;
    leAPRSMessage: TLabeledEdit;
    LECleanupTime: TLabeledEdit;
    LECallsign: TLabeledEdit;
    LEIgatePassword: TLabeledEdit;
    LEIgateFilter: TLabeledEdit;

    LEIGateServer: TLabeledEdit;
    LEIGatePort: TLabeledEdit;

    LELatitude: TLabeledEdit;
    LELongitude: TLabeledEdit;
    LEMapCache: TLabeledEdit;
    LEMapLocalDirectory: TLabeledEdit;

    SDDCacheDirectory: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;

    sbGetGPSPosition: TSpeedButton;
    SBScanMeshCoreDevices: TSpeedButton;
    spUpdateInterval: TSpinEdit;
    procedure BBOSMMapCacheClick(Sender: TObject);
    procedure BBOSMLocalTilesClick(Sender: TObject);

    procedure CancelButtonClick(Sender: TObject);
    procedure CBMeshCoreEnableChange(Sender: TObject);

    procedure cbIGateEnableChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ScanMeshCoreDevicesClick(Sender: TObject);
    procedure sbGetGPSPositionClick(Sender: TObject);
  public
    procedure SetConfig(Config: PAPRSConfig);
  end;

var
  FSettings: TFSettings;
  FConfig: PAPRSConfig;

implementation

Uses
  UMain;

{$R *.lfm}

{ TFSettings }

function MeshCoreAddressFromSelection(const Selection: String): String;
var
  DelimiterPosition: Integer;
begin
  DelimiterPosition := LastDelimiter(',', Selection);
  if DelimiterPosition > 0 then
    Result := Trim(Copy(Selection, DelimiterPosition + 1, MaxInt))
  else
    Result := Trim(Selection);
end;

procedure TFSettings.BBOSMMapCacheClick(Sender: TObject);
begin
  if SDDCacheDirectory.Execute then
    LEMapCache.Caption := SDDCacheDirectory.FileName;
end;

procedure TFSettings.BBOSMLocalTilesClick(Sender: TObject);
begin
  if SDDCacheDirectory.Execute then
    LEMapLocalDirectory.Caption := SDDCacheDirectory.FileName;
end;


procedure TFSettings.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFSettings.CBMeshCoreEnableChange(Sender: TObject);
begin
  CBMeshCoreSendPosition.Enabled := CBMeshCoreEnable.Checked;
  LabelMeshCoreDevice.Enabled := CBMeshCoreEnable.Checked;
  CBMeshCoreDevices.Enabled := CBMeshCoreEnable.Checked;
  SBScanMeshCoreDevices.Enabled := CBMeshCoreEnable.Checked;
end;


procedure TFSettings.cbIGateEnableChange(Sender: TObject);
begin
  LEIgateServer.Enabled := cbIGateEnable.Checked;
  LEIgatePassword.Enabled := cbIGateEnable.Checked;
  LEIgatePort.Enabled := cbIGateEnable.Checked;
  LEIgateFilter.Enabled := cbIGateEnable.Checked;
end;

procedure TFSettings.FormShow(Sender: TObject);
var i, count: Byte;
begin
  CBESymbol.Clear;

  // Primary Icons
  count := Length(APRSPrimarySymbolTable);
  for i := 1 to count do
    CBESymbol.ItemsEx.AddItem(APRSPrimarySymbolTable[i].Description, i, 0, 0, 0, nil);

  // Alternate Icons
  count := Length(APRSAlternateSymbolTable);
  for i := 1 to count do
    CBESymbol.ItemsEx.AddItem(APRSAlternateSymbolTable[i].Description, i+96, 0, 0, 0, nil);

  CBESymbol.ItemIndex := FConfig^.AprsSymbol;

  if FGPS.IsEnabled then
    sbGetGPSPosition.Enabled := True
  else
    sbGetGPSPosition.Enabled := False;

  cbModeSEnable.Checked := FConfig^.ModeSEnabled;
  cbIgateEnable.Checked := FConfig^.IGateEnabled;
  CBMeshCoreEnable.Checked := FConfig^.MeshCoreEnabled;
  CBMeshCoreDevices.Clear;
  if Trim(FConfig^.MeshCoreAddress) <> '' then
  begin
    CBMeshCoreDevices.Items.Add(FConfig^.MeshCoreAddress);
    CBMeshCoreDevices.ItemIndex := 0;
  end;
  CBMeshCoreSendPosition.Checked := FConfig^.MeshCoreSendPosition;

  cbIGateEnableChange(Self);
end;

procedure TFSettings.OKButtonClick(Sender: TObject);
begin
  FConfig^.MAPCache := LEMapCache.Caption;
  FConfig^.Callsign := LECallsign.Caption;
  FConfig^.Latitude := StrToFloat(LELatitude.Caption);
  FConfig^.Longitude := StrToFloat(LELongitude.Caption);
  FConfig^.IGateServer := LEIGateServer.Caption;
  FConfig^.IGatePort := StrToInt(LEIGatePort.Caption);
  FConfig^.IGatePassword := LEIGatePassword.Caption;
  FConfig^.IGateFilter := LEIGateFilter.Caption;
  FConfig^.IGateEnabled := cbIgateEnable.Checked;
  FConfig^.CleanupTime := StrToInt(LECleanupTime.Caption);
  FConfig^.LocalTilesDirectory := LEMapLocalDirectory.Caption;


  FConfig^.AprsSymbol := CBESymbol.ItemIndex;
  FConfig^.ModeSEnabled := cbModeSEnable.Checked;
  FConfig^.AprsMessage := leAprsMessage.Caption;
  FConfig^.AprsUpdateInterval := spUpdateInterval.Value;
  FConfig^.MeshCoreEnabled := CBMeshCoreEnable.Checked;
  FConfig^.MeshCoreAddress := MeshCoreAddressFromSelection(CBMeshCoreDevices.Text);
  FConfig^.MeshCoreSendPosition := CBMeshCoreSendPosition.Checked;


  if Assigned(FMain.ModeS) then
    FMain.ModeS.Free;
  if FConfig^.ModeSEnabled then
    FMain.ModeS := TModeSThread.Create(@APRSConfig);

  if Assigned(FMain.IGate) then
    FMain.IGate.Free;
  if FConfig^.IGateEnabled then
    FMain.IGate := TIGateThread.Create(@APRSConfig);

  FreeAndNil(FMain.MeshCore);
  if FConfig^.MeshCoreEnabled and (FConfig^.MeshCoreAddress <> '') then
    FMain.MeshCore := TMeshCoreClient.Create(FConfig^.MeshCoreAddress);

  SaveConfigToFile(FConfig);

  SetPoi(FMain.PoILayer, FConfig^.Latitude, FConfig^.Longitude, FConfig^.Callsign, True, FConfig^.AprsSymbol+1, FMain.MVMap.GPSItems);

  Close;
end;

procedure TFSettings.ScanMeshCoreDevicesClick(Sender: TObject);
const
  InquiryDuration = 5;
  RemoteNameTimeout = 5000;
var
  DeviceID, DeviceSocket: cint;
  ScanInfo: array[0..127] of inquiry_info;
  ScanInfoPtr: Pinquiry_info;
  FoundDevices: cint;
  DeviceAddress: array[0..255] of Char;
  RemoteName: array[0..255] of Char;
  I: Integer;
  Entry, CurrentAddress: String;
begin
  CurrentAddress := MeshCoreAddressFromSelection(CBMeshCoreDevices.Text);
  DeviceID := hci_get_route(nil);
  if DeviceID < 0 then
    raise Exception.Create('Bluetooth scan: no adapter found');

  DeviceSocket := hci_open_dev(DeviceID);
  if DeviceSocket < 0 then
    raise Exception.Create('Bluetooth scan: unable to open adapter');

  try
    ScanInfoPtr := @ScanInfo[0];
    FillChar(ScanInfo, SizeOf(ScanInfo), 0);
    FoundDevices := hci_inquiry_1(DeviceID, InquiryDuration, Length(ScanInfo),
      nil, @ScanInfoPtr, IREQ_CACHE_FLUSH);

    CBMeshCoreDevices.Items.BeginUpdate;
    try
      CBMeshCoreDevices.Clear;
      if CurrentAddress <> '' then
        CBMeshCoreDevices.Items.Add(CurrentAddress);

      if FoundDevices > 0 then
        for I := 0 to FoundDevices - 1 do
        begin
          FillChar(DeviceAddress, SizeOf(DeviceAddress), 0);
          FillChar(RemoteName, SizeOf(RemoteName), 0);
          ba2str(@ScanInfo[I].bdaddr, @DeviceAddress[0]);
          if hci_read_remote_name(DeviceSocket, @ScanInfo[I].bdaddr,
            High(RemoteName), @RemoteName[0], RemoteNameTimeout) = 0 then
            Entry := Format('%s ,%s', [PChar(@RemoteName[0]),
              PChar(@DeviceAddress[0])])
          else
            Entry := String(PChar(@DeviceAddress[0]));
          if CBMeshCoreDevices.Items.IndexOf(Entry) < 0 then
            CBMeshCoreDevices.Items.Add(Entry);
        end;

      if CBMeshCoreDevices.Items.Count > 0 then
        CBMeshCoreDevices.ItemIndex := 0;
    finally
      CBMeshCoreDevices.Items.EndUpdate;
    end;
  finally
    hci_close_dev(DeviceSocket);
  end;
end;

procedure TFSettings.sbGetGPSPositionClick(Sender: TObject);
begin
  LELatitude.Text := FloatToStrF(FGPS.GetLat, ffFixed, 3, 6);
  LELongitude.Text := FloatToStrF(FGPS.GetLon, ffFixed, 3, 5);
end;

procedure TFSettings.SetConfig(Config: PAPRSConfig);
begin
  FConfig := Config;
  LEMapCache.Caption := FConfig^.MAPCache;
  LECallsign.Caption := FConfig^.Callsign;
  LELatitude.Caption := FloatToStr(FConfig^.Latitude);
  LELongitude.Caption := FloatToStr(FConfig^.Longitude);
  LEIGateServer.Caption := FConfig^.IGateServer;
  LEIGatePort.Caption := IntToStr(FConfig^.IGatePort);
  LEIGatePassword.Caption := FConfig^.IGatePassword;
  LEIGateFilter.Caption := FConfig^.IGateFilter;
  LECleanupTime.Caption := IntToStr(FConfig^.CleanupTime);
  LEMapLocalDirectory.Caption := FConfig^.LocalTilesDirectory;
  leAprsMessage.Caption := FConfig^.AprsMessage;
  spUpdateInterval.Value := FConfig^.AprsUpdateInterval;
  FMain.tBake.Interval := FConfig^.AprsUpdateInterval * 60 * 1000;
  if FMain.tBake.Enabled then
  begin
    FMain.tBake.Enabled := False;
    FMain.tBake.Enabled := True;
  end;
end;

end.

