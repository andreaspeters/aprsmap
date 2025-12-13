unit ugps;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ButtonPanel, IdTCPClient, fpjson, jsonparser, ugpsd, utypes;

type

  { TFGPS }

  TFGPS = class(TForm)
    BPDefaultButtons: TButtonPanel;
    cbGPSdEnable: TCheckBox;
    GroupBox1: TGroupBox;
    leGPSdHost: TLabeledEdit;
    leGPSdPort: TLabeledEdit;
    procedure BPDefaultButtonsClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure cbGPSdEnableChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Start(Config: PTAPRSConfig);
  private
  public
    function GetLat: Double;
    function GetLon: Double;
    function IsEnabled: Boolean;
  end;

var
  FGPS: TFGPS;
  GPSd: TGpsThread;
  FConfig: PTAPRSConfig;

implementation

{$R *.lfm}

{ TFGPS }

procedure TFGPS.Start(Config: PTAPRSConfig);
begin
  FConfig := Config;
  if FConfig^.GPSdEnabled then
  begin
    GPSd := TGpsThread.Create(FConfig);
    GPSd.Start;
  end;
end;

procedure TFGPS.cbGPSdEnableChange(Sender: TObject);
begin
  leGPSdHost.Enabled := cbGPSdEnable.Checked;
  leGPSdPort.Enabled := cbGPSdEnable.Checked;

  if leGPSdHost.Enabled then
  begin
    if Assigned(GPSd) then
      GPSd.Free;

    GPSd := TGpsThread.Create(FConfig);
    GPSd.Start
  end
  else
    GPSd.Terminate
end;

procedure TFGPS.BPDefaultButtonsClick(Sender: TObject);
begin
  FConfig^.GPSdEnabled := cbGPSdEnable.Checked;
  if Length(leGPSdPort.Text) > 0 then
    FConfig^.GPSdHost := leGPSdHost.Text;
  if Length(leGPSdPort.Text) > 0 then
    FConfig^.GPSdPort := StrToInt(leGPSdPort.Text);

  Close;
end;

procedure TFGPS.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFGPS.FormShow(Sender: TObject);
begin
  cbGPSdEnable.Checked := FConfig^.GPSdEnabled;
  cbGPSdEnable.Enabled := True;
  leGPSdHost.Text := FConfig^.GPSdHost;
  leGPSdPort.Text := IntToStr(FConfig^.GPSdPort);
end;

function TFGPS.GetLat: Double;
begin
  Result := 0;
  if FConfig^.GPSdEnabled and Assigned(GPSd) then
    Result := GPSd.Latitude;
end;

function TFGPS.GetLon: Double;
begin
  Result := 0;
  if FConfig^.GPSdEnabled and Assigned(GPSd) then
    Result := GPSd.Longitude;
end;

function TFGPS.IsEnabled: Boolean;
begin
  Result := False;
  if FConfig^.GPSdEnabled then
    Result := True;
end;

end.
