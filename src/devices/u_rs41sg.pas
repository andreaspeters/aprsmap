unit u_rs41sg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utypes, RegExpr;

procedure RS41SGP(msg: PAPRSMessage);
function GetTemperature(const Text: String):Double;
function GetHumidity(const Text: String):Double;
function GetPressure(const Text: String):Double;

implementation

procedure RS41SGP(msg: PAPRSMessage);
begin
  if (Pos('Type=RS41', msg^.Message) > 0) or (Pos('Type=DFM', msg^.Message) > 0) then
  begin
    msg^.WXTemperature.Add(GetTemperature(msg^.Message));
    msg^.WXHumidity.Add(GetTemperature(msg^.Message));
    msg^.WXPressure.Add(GetPressure(msg^.Message));
  end;
end;

function GetTemperature(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := 0;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 't=(-?\d+(?:\.\d+))(\S)';

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
  Result := 0;
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
  Result := 0;
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

end.

