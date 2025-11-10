unit u_rs41sg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utypes, RegExpr;

procedure RS41SGP(msg: PAPRSMessage);
function GetTemperature(const Text: String):Integer;
function GetHumidity(const Text: String):Integer;
function GetPressure(const Text: String):Integer;

implementation

procedure RS41SGP(msg: PAPRSMessage);
begin
  if (Pos('Type=RS41-SGP', msg^.Message) > 0) or (Pos('Type=DFM', msg^.Message) > 0) then
  begin
    msg^.WXTemperature.Add(GetTemperature(msg^.Message));
    msg^.WXHumidity := GetTemperature(msg^.Message);
    msg^.WXPressure.Add(GetPressure(msg^.Message));
  end;
end;

function GetTemperature(const Text: String):Integer;
var Regex: TRegExpr;
begin
  Result := 0;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 't=(-?\d+(?:\.\d+))(\S)';

    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
       Result := StrToInt(Regex.Match[1]);

       if Regex.Match[2] = 'F' then
         Result := Round((Result - 32)*5/9);

       Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetHumidity(const Text: String):Integer;
var Regex: TRegExpr;
begin
  Result := 0;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 't=(\d+(?:\.\d+))%';

    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
       Result := StrToInt(Regex.Match[1]);

       Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetPressure(const Text: String):Integer;
var Regex: TRegExpr;
begin
  Result := 0;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'p=(\d+(?:\.\d+))hPa';

    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
       Result := StrToInt(Regex.Match[1]);

       Exit;
    end;
  finally
    Regex.Free;
  end;
end;

end.

