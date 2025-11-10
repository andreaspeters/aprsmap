unit u_rs41sg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utypes, RegExpr;

procedure GetData(msg: PAPRSMessage);
function GetTemperature(const Text: String):Integer;

implementation

procedure RS41-SGP(msg: PAPRSMessage);
begin
  if Pos('Type=RS41-SGP', msg^.Message) > 0 then
  begin
    msg^.WXTemperature := GetTemperature(msg^.Message);
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

end.

