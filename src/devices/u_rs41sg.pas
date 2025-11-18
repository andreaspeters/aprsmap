unit u_rs41sg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, utypes, Controls;

procedure RS41SGP(msg: PAPRSMessage);
procedure RS41SGPUpdate(newMsg, oldMsg: PAPRSMessage);
procedure RS41SGPChart(msg: PAPRSMessage; chart: TWinControl);
function GetTemperature(const Text: String):Double;
function GetHumidity(const Text: String):Double;
function GetPressure(const Text: String):Double;
function GetClimbrate(const Text: String):Double;
function GetSats(const Text: String):Double;
function GetRSSI(const Text: String):Double;
function GetBatterie(const Text: String):Double;

implementation

uses
  umain;


procedure RS41SGP(msg: PAPRSMessage);
begin
  try
    if (Pos('Type=RS41', msg^.Message) > 0) or (Pos('Type=DFM', msg^.Message) > 0) then
    begin
      if not msg^.Devices.RS41.Enabled then
      begin
        with msg^.Devices.RS41 do
        begin
          Enabled := True;
          clb := TDoubleList.Create;
          o3 := TDoubleList.Create;
          t := TDoubleList.Create;
          ti := TDoubleList.Create;
          pump := TDoubleList.Create;
          batt := TDoubleList.Create;
          dist := TDoubleList.Create;
          rssi := TDoubleList.Create;
          p := TDoubleList.Create;
          h := TDoubleList.Create;
          sats := TDoubleList.Create;
        end;
      end;

      with msg^.Devices.RS41 do
      begin
        if GetTemperature(msg^.Message) <> -999999 then
        begin
          t.Add(GetTemperature(msg^.Message));
          msg^.WXTemperature.Add(GetTemperature(msg^.Message));
        end;

        if GetPressure(msg^.Message) <> -999999 then
        begin
          p.Add(GetPressure(msg^.Message));
          msg^.WXPressure.Add(GetPressure(msg^.Message));
        end;

        if GetHumidity(msg^.Message) <> -999999 then
        begin
          h.Add(GetHumidity(msg^.Message));
          msg^.WXHumidity.Add(GetHumidity(msg^.Message));
        end;
        if GetClimbrate(msg^.Message) <> -999999 then
          clb.Add(GetClimbrate(msg^.Message));
        if GetSats(msg^.Message) <> -999999 then
          sats.Add(GetSats(msg^.Message));
        if GetRSSI(msg^.Message) <> -999999 then
          rssi.Add(GetRSSI(msg^.Message));
        if GetBatterie(msg^.Message) <> -999999 then
          batt.Add(GetBatterie(msg^.Message));
      end;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln(Format('RS41SGP Error %s: %s ', [msg^.FromCall, E.Message]));
      {$ENDIF}
    end;
  end;
end;

procedure RS41SGPUpdate(newMsg, oldMsg: PAPRSMessage);
begin
  PrependDoubleList(newMsg^.Devices.RS41.clb, oldMsg^.Devices.RS41.clb);
  PrependDoubleList(newMsg^.Devices.RS41.t, oldMsg^.Devices.RS41.t);
  PrependDoubleList(newMsg^.Devices.RS41.p, oldMsg^.Devices.RS41.p);
  PrependDoubleList(newMsg^.Devices.RS41.h, oldMsg^.Devices.RS41.h);
  PrependDoubleList(newMsg^.Devices.RS41.batt, oldMsg^.Devices.RS41.batt);
  PrependDoubleList(newMsg^.Devices.RS41.sats, oldMsg^.Devices.RS41.sats);
  PrependDoubleList(newMsg^.Devices.RS41.rssi, oldMsg^.Devices.RS41.rssi);
end;

procedure RS41SGPChart(msg: PAPRSMessage; chart: TWinControl);
begin
  try
    FMain.WriteChart(msg^.Devices.RS41.t, 'Temperature',  'Time', 'Temperature (°C)', chart);
    FMain.WriteChart(msg^.Devices.RS41.p, 'Pressure',  'Time', 'Pressure (hPa)', chart);
    FMain.WriteChart(msg^.Devices.RS41.h, 'Humidity',  'Time', 'Humidity (%)', chart);
    FMain.WriteChart(msg^.Devices.RS41.clb, 'Climbrate',  'Time', 'Climbrate (m/s)', chart);
    FMain.WriteChart(msg^.Devices.RS41.batt, 'Batterie',  'Time', 'Batterie (V)', chart);
    FMain.WriteChart(msg^.Devices.RS41.sats, 'Satellites',  'Time', 'Satellites', chart);
    FMain.WriteChart(msg^.Devices.RS41.rssi, 'RSSI',  'Time', 'RSSI (dB)', chart);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln(Format('RS41SGPChart Error %s: %s ', [msg^.FromCall, E.Message]));
      {$ENDIF}
    end;
  end;
end;

function GetTemperature(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 't=(-?\d+(?:\.\d+))C';

    Regex.ModifierI := false;
    if Regex.Exec(Text) then
    begin
       if Regex.SubExprMatchCount >= 2 then
       begin
         Result := StrToFloat(Regex.Match[1]);

         if Regex.Match[2] = 'F' then
           Result := Round((Result - 32)*5/9);
       end;
       Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetHumidity(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'h=(\d+(?:\.\d+))%';

    Regex.ModifierI := false;
    if Regex.Exec(Text) then
    begin
      if Regex.SubExprMatchCount >= 1 then
       Result := StrToFloat(Regex.Match[1]);

       Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetPressure(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'p=(\d+(?:\.\d+))hPa';

    Regex.ModifierI := false;
    if Regex.Exec(Text) then
    begin
      if Regex.SubExprMatchCount >= 1 then
        Result := StrToFloat(Regex.Match[1]);

      Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetClimbrate(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'Clb=(-?\d+(?:\.\d+))m/s';

    Regex.ModifierI := false;
    if Regex.Exec(Text) then
    begin
      if Regex.SubExprMatchCount >= 1 then
        Result := StrToFloat(Regex.Match[1]);

      Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetBatterie(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'batt=(\d+(?:\.\d+))V';

    Regex.ModifierI := false;
    if Regex.Exec(Text) then
    begin
      if Regex.SubExprMatchCount >= 1 then
        Result := StrToFloat(Regex.Match[1]);

      Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetSats(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'Sats=(\d+)\s';

    Regex.ModifierI := false;
    if Regex.Exec(Text) then
    begin
      if Regex.SubExprMatchCount >= 1 then
        Result := StrToFloat(Regex.Match[1]);

      Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetRSSI(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'rssi=(\d+(?:\.\d+))dB';

    Regex.ModifierI := false;
    if Regex.Exec(Text) then
    begin
      if Regex.SubExprMatchCount >= 1 then
        Result := StrToFloat(Regex.Match[1]);

      Exit;
    end;
  finally
    Regex.Free;
  end;
end;

end.

