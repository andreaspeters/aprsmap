unit usettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  Buttons, StdCtrls, utypes, uini;

type

  { TFSettings }

  TFSettings = class(TForm)
    BPDefaultButtons: TButtonPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    ImageList1: TImageList;
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
    procedure BBOSMMapCacheClick(Sender: TObject);
    procedure BBOSMLocalTilesClick(Sender: TObject);
    procedure BBSetDump1090(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public
    procedure SetConfig(Config: PAPRSConfig);
  end;

var
  FSettings: TFSettings;
  FConfig: PAPRSConfig;

implementation

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
  FConfig^.CleanupTime := StrToInt(LECleanupTime.Caption);
  FConfig^.LocalTilesDirectory := LEMapLocalDirectory.Caption;

  FConfig^.ModeSServer := LEModeSServer.Caption;
  FConfig^.ModeSPort := StrToInt(LEModeSPort.Caption);
  FConfig^.ModeSExecutable := LEModeSExecutable.Caption;

  if Length(FConfig^.IGatePassword) > 0 then
    FConfig^.IGateEnabled := True;

  SaveConfigToFile(FConfig);
  if MessageDlg('To apply the configuration, we have to restart APRSMap.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;

  Close;
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

