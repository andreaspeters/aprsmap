unit usettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  Buttons, StdCtrls, ComboEx, Spin, utypes, uini, ugps, uaprs, uigate, umodes,
  umeshcore;

type

  { TFSettings }

  TFSettings = class(TForm)
    BPDefaultButtons: TButtonPanel;
    CBESymbol: TComboBoxEx;
    cbModeSEnable: TCheckBox;
    cbIGateEnable: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    leAPRSMessage: TLabeledEdit;
    LECleanupTime: TLabeledEdit;
    LECallsign: TLabeledEdit;
    LEIgatePassword: TLabeledEdit;
    LEIgateFilter: TLabeledEdit;
    LEModeSExecutable: TLabeledEdit;
    LEModeSPort: TLabeledEdit;
    LEIGateServer: TLabeledEdit;
    LEIGatePort: TLabeledEdit;
    LEModeSServer: TLabeledEdit;
    LELatitude: TLabeledEdit;
    LELongitude: TLabeledEdit;
    LEMapCache: TLabeledEdit;
    LEMapLocalDirectory: TLabeledEdit;
    ODSelectFile: TOpenDialog;
    SDDCacheDirectory: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    sbGetGPSPosition: TSpeedButton;
    spUpdateInterval: TSpinEdit;
    procedure BBOSMMapCacheClick(Sender: TObject);
    procedure BBOSMLocalTilesClick(Sender: TObject);
    procedure BBSetDump1090(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure cbModeSEnableChange(Sender: TObject);
    procedure cbIGateEnableChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure sbGetGPSPositionClick(Sender: TObject);
  private
    FMeshCoreGroup: TGroupBox;
    CBMeshCoreEnable: TCheckBox;
    CBMeshCoreSendPosition: TCheckBox;
    LEMeshCoreAddress: TLabeledEdit;
    SEMeshCoreChannel: TSpinEdit;
    procedure EnsureMeshCoreControls;
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

procedure TFSettings.EnsureMeshCoreControls;
begin
  if Assigned(FMeshCoreGroup) then Exit;

  Height := 755;
  BPDefaultButtons.Top := 704;
  FMeshCoreGroup := TGroupBox.Create(Self);
  FMeshCoreGroup.Parent := Self;
  FMeshCoreGroup.SetBounds(6, 536, 1035, 160);
  FMeshCoreGroup.Caption := 'MeshCore (Bluetooth LE)';

  CBMeshCoreEnable := TCheckBox.Create(Self);
  CBMeshCoreEnable.Parent := FMeshCoreGroup;
  CBMeshCoreEnable.SetBounds(16, 28, 100, 28);
  CBMeshCoreEnable.Caption := 'Enable';

  LEMeshCoreAddress := TLabeledEdit.Create(Self);
  LEMeshCoreAddress.Parent := FMeshCoreGroup;
  LEMeshCoreAddress.SetBounds(260, 24, 250, 36);
  LEMeshCoreAddress.EditLabel.Caption := 'Bluetooth address';
  LEMeshCoreAddress.LabelPosition := lpLeft;

  SEMeshCoreChannel := TSpinEdit.Create(Self);
  SEMeshCoreChannel.Parent := FMeshCoreGroup;
  SEMeshCoreChannel.SetBounds(650, 24, 72, 36);
  SEMeshCoreChannel.MinValue := 0;
  SEMeshCoreChannel.MaxValue := 255;

  CBMeshCoreSendPosition := TCheckBox.Create(Self);
  CBMeshCoreSendPosition.Parent := FMeshCoreGroup;
  CBMeshCoreSendPosition.SetBounds(16, 88, 300, 28);
  CBMeshCoreSendPosition.Caption := 'Send APRS position on MeshCore channel';
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

procedure TFSettings.BBSetDump1090(Sender: TObject);
begin
  if ODSelectFile.Execute then
    LEModeSExecutable.Caption := ODSelectFile.FileName;
end;

procedure TFSettings.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFSettings.cbModeSEnableChange(Sender: TObject);
begin
  LEModeSServer.Enabled := cbModeSEnable.Checked;
  LEModeSPort.Enabled := cbModeSEnable.Checked;
  LEModeSExecutable.Enabled := cbModeSEnable.Checked;
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
  EnsureMeshCoreControls;
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
  LEMeshCoreAddress.Text := FConfig^.MeshCoreAddress;
  SEMeshCoreChannel.Value := FConfig^.MeshCoreChannel;
  CBMeshCoreSendPosition.Checked := FConfig^.MeshCoreSendPosition;

  cbModeSEnableChange(Self);
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

  FConfig^.ModeSServer := LEModeSServer.Caption;
  FConfig^.ModeSPort := StrToInt(LEModeSPort.Caption);
  FConfig^.ModeSExecutable := LEModeSExecutable.Caption;
  FConfig^.AprsSymbol := CBESymbol.ItemIndex;
  FConfig^.ModeSEnabled := LEModeSServer.Enabled;
  FConfig^.AprsMessage := leAprsMessage.Caption;
  FConfig^.AprsUpdateInterval := spUpdateInterval.Value;
  FConfig^.MeshCoreEnabled := CBMeshCoreEnable.Checked;
  FConfig^.MeshCoreAddress := Trim(LEMeshCoreAddress.Text);
  FConfig^.MeshCoreChannel := SEMeshCoreChannel.Value;
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
    FMain.MeshCore := TMeshCoreClient.Create(FConfig^.MeshCoreAddress,
      Byte(FConfig^.MeshCoreChannel));

  SaveConfigToFile(FConfig);

  SetPoi(FMain.PoILayer, FConfig^.Latitude, FConfig^.Longitude, FConfig^.Callsign, True, FConfig^.AprsSymbol+1, FMain.MVMap.GPSItems);

  Close;
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
  LEModeSServer.Caption := FConfig^.ModeSServer;
  LEModeSPort.Caption := IntToStr(FConfig^.ModeSPort);
  LEModeSExecutable.Caption := FConfig^.ModeSExecutable;
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

