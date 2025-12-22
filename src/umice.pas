unit umice;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utypes;


type
  TMicEDestination = record
    Lat: Double;
    Message: String;
    NS: Char;
    Offset: Byte;
    WE: Char;
  end;


procedure DecodeMicELonSpeedCourse(const Data: String; APRSMessage: PAPRSMessage);
procedure DecodeMicELat(APRSMessage: PAPRSMessage);

implementation

procedure DecodeMicELonSpeedCourse(const Data: String; APRSMessage: PAPRSMessage);
var Lon, Speed, Course: Double;
    Deg, Min, Sec: Double;
    WE: Char;
begin
  Lon := 0;
  Speed := 0;
  Course := 0;
  WE := 'W';

  if (Length(Data) < 9) or (Length(APRSMessage^.ToCall) <> 6) then
    Exit;

  // Longitude
  Deg := Ord(Data[2]) - 28;
  Min := Ord(Data[3]) - 28;
  Sec := Ord(Data[4]) - 28;

  if APRSMessage^.ToCall[3] in ['P'..'Y'] then
  begin
    Deg := Deg + 100;
    WE := 'E';
  end;

  if Min >= 60 then
    Min := Min - 60;

  if Deg >= 190 then
    Deg := Deg - 190
  else if Deg >= 180 then
    Deg := Deg - 80
  else
    Deg := Deg;

  Lon := (Deg + (Min / 60.0)) + (Sec / 6000.0);
  if WE = 'W' then
    Lon := -Lon;

  // Speed (knots)
  Speed := (Ord(Data[5]) - 28) * 10 + ((Ord(Data[6]) - 28) / 10.0) * 1.852;

  // Course
  Course := (Ord(Data[7]) - 28)* 4;

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
    Deg, Min, Sec: Double;
begin
  NS := 'S';

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
  if APRSMessage^.ToCall[3] in ['P'..'Y'] then
    NS := 'N';

  Deg := (d[1] * 10 + d[2]);
  Min := (d[3] * 10 + d[4]);
  Sec := (d[5] * 10 + d[6]);

  Lat := (Deg + (Min / 60.0)) + (Sec / 6000.0);
  if NS = 'S' then
    Lat := Lat * -1;

  APRSMessage^.Latitude := Lat;
end;


end.

