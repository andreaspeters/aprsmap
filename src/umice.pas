unit umice;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utypes;

procedure DecodeMicELonSpeedCourse(const Data: String; APRSMessage: PAPRSMessage);
procedure DecodeMicELat(APRSMessage: PAPRSMessage);
function MicECharValue(C: Char): Integer;

implementation

function MicECharValue(C: Char): Integer;
begin
  Result := Ord(C) - 28;
end;

procedure DecodeMicELonSpeedCourse(const Data: String; APRSMessage: PAPRSMessage);
var Lon, Speed, Course: Double;
    SymTab, SymCode: Char;
    Deg, Min, Hun: Integer;
begin
  Lon := 0;
  Speed := 0;
  Course := 0;
  SymTab := '/';
  SymCode := '>';

  if (Length(Data) < 9) or (Length(APRSMessage^.ToCall) <> 6) then
    Exit;

  // Longitude
  Deg := MicECharValue(Data[2]);
  Min := MicECharValue(Data[3]);
  Hun := MicECharValue(Data[4]);

  // Degree-Korrektur laut Spec
  if Deg >= 180 then
    Deg := Deg - 80
  else if Deg >= 100 then
    Deg := Deg - 100;

  Lon := Deg + (Min / 60.0) + (Hun / 6000.0);

  if APRSMessage^.ToCall[6] in ['P'..'Y'] then
    Lon := -Lon;

  // Speed (knots)
  Speed := MicECharValue(Data[5]) * 10 + (MicECharValue(Data[6]) / 10.0) * 1.852;

  // Course
  Course := MicECharValue(Data[7]) * 4;

  APRSMessage^.Longitude := Lon;
  APRSMessage^.Speed.Add(Speed);
  APRSMessage^.Course := Course;
  APRSMessage^.IconPrimary := Data[8];
  APRSMessage^.Icon := Data[9];
end;

procedure DecodeMicELat(APRSMessage: PAPRSMessage);
var d: array[1..6] of Integer;
    i: Integer;
    Lat: Double;
    NS: Char;
begin
  NS := 'N';

  if Length(APRSMessage^.ToCall) < 6 then
    Exit;

  for i := 1 to 6 do
  begin
    case APRSMessage^.ToCall[i] of
      '0'..'9': d[i] := Ord(APRSMessage^.ToCall[i]) - Ord('0');
      'A'..'J': d[i] := Ord(APRSMessage^.ToCall[i]) - Ord('A');
      'P'..'Y': d[i] := Ord(APRSMessage^.ToCall[i]) - Ord('P');
      else Exit;
    end;
  end;

  // N / S
  if APRSMessage^.ToCall[4] in ['P'..'Y'] then
    NS := 'S';

  Lat := (d[1] * 10 + d[2]) +
         (d[3] * 10 + d[4]) / 60.0 +
         (d[5] * 10 + d[6]) / 6000.0;

  if NS = 'S' then
    Lat := -Lat;

  APRSMessage^.Latitude := Lat;
end;


end.

