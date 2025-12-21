unit ucompressed;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utypes;

procedure DecodeCompressedLatLon(LatStr, LonStr:String; APRSMessage: PAPRSMessage);
function Base91ToInt(const S: String): Integer;

implementation

function Base91ToInt(const S: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
  begin
    if (Ord(S[i]) < 33) or (Ord(S[i]) > 123) then
      Exit(-1);
    Result := Result * 91 + (Ord(S[i]) - 33);
  end;
end;

procedure DecodeCompressedLatLon(LatStr, LonStr:String; APRSMessage: PAPRSMessage);
var Data: String;
    Lat, Lon: Double;
    LatVal, LonVal: Integer;
begin
  Lat := 0.0;
  Lon := 0.0;

  if (Length(LatStr) < 4) or (Length(LonStr) < 4) then
    Exit;

  // Position der Base-91-Daten:
  // Info[4..7] = Latitude
  // Info[8..11] = Longitude
  LatVal := Base91ToInt(LatStr);
  LonVal := Base91ToInt(LonStr);

  if (LatVal < 0) or (LonVal < 0) then
    Exit;

  // APRS-Formeln laut Spec
  Lat := 90.0  - (LatVal / 380926.0);
  Lon := -180.0 + (LonVal / 190463.0);

  // Plausibilitätsprüfung
  if (Lat < -90.0) or (Lat > 90.0) then Exit;
  if (Lon < -180.0) or (Lon > 180.0) then Exit;

  APRSMessage^.Latitude := Lat;
  APRSMessage^.Longitude := Lon;
end;


end.

