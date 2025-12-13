unit usettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  Buttons, StdCtrls, ComboEx, utypes, uini, ugps, uaprs, mvGPSObj;

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
    Label3: TLabel;
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
end;

end.

