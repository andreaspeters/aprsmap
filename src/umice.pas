unit umice;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utypes;

type
  TMicEMsgs = record
    Messages : array[0..15] of String[30];
  end;

var
  MicEMsgs : TMicEMsgs = (
    Messages: (
      'EMERGENCY',   //  0
      'Priority',    //  1
      'Special',     //  2
      'Committed',   //  3
      'Returning',   //  4
      'In Service',  //  5
      'En Route',    //  6
      'Off Duty',    //  7
      '',            //  8
      'Custom 6',    //  9
      'Custom 5',    // 10
      'Custom 4',    // 11
      'Custom 3',    // 12
      'Custom 2',    // 13
      'Custom 1',    // 14
      'Custom 0'     // 15
    )
  );

procedure DecodeMicELonSpeedCourse(const Data: String; APRSMessage: PAPRSMessage);
procedure DecodeMicELat(APRSMessage: PAPRSMessage);
function DecodeMicEMessage(Const ToCall: String): Integer;
function GetMicEMessageBit(Const ToCall: String; Index: Integer): Integer;

implementation

function GetMicEMessageBit(Const ToCall: String; Index: Integer): Integer;
var c: Char;
begin
  Result := -1;

  if (Index < 1) or (Index > Length(ToCall)) then
    Exit;

  c := ToCall[Index];

  if (c in ['0'..'9']) or (c in ['A'..'K']) then
    Result := 0
  else if (c in ['P'..'Z']) then
    Result := 1;
end;

function DecodeMicEMessage(Const ToCall: String): Integer;
var j, bit: Integer;
    MicMsg: Integer;
begin
  Result := -1;
  MicMsg := 0;

  if Length(ToCall) < 3 then Exit;

  for j := 1 to 3 do
  begin
    bit := GetMicEMessageBit(ToCall, j);
    if bit < 0 then Exit;

    MicMsg := MicMsg shl 1;
    MicMsg := MicMsg or bit;
  end;

  // Custom message handling
  if MicMsg > 7 then
  begin
    MicMsg := MicMsg and $07; // lower 3 bits
    MicMsg := MicMsg or $08;  // custom flag
  end;

  Result := MicMsg;
end;

procedure DecodeMicELonSpeedCourse(const Data: String; APRSMessage: PAPRSMessage);
var Lon, Speed, M, Course: Double;
    Deg, Min, Sec: Double;
    WE: Char;
    i: Integer;
begin
  Lon := 0;
  Speed := 0;
  Course := 0;
  WE := 'E';

  if (Length(Data) < 9) or (Length(APRSMessage^.ToCall) <> 6) then
    Exit;

  // Longitude
  Deg := Ord(Data[2]) - 28;
  Min := Ord(Data[3]) - 28;
  Sec := Ord(Data[4]) - 28;

  if APRSMessage^.ToCall[5] in ['P'..'Y'] then
    Deg := Deg + 100;

  if APRSMessage^.ToCall[6] in ['P'..'Y'] then
    WE := 'W';

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

  // Speed
  M := (Ord(Data[6]) - 28);
  Speed := (Ord(Data[5]) - 28);

  if (M > 0) and (M < 97) then
    if (Speed > 0) and (Speed < 99) then
    begin
      Speed := (Speed * 10) + (M / 10);
      if (Speed >= 800) then
        Speed := Speed - 800;

      // knots -> km/h
      APRSMessage^.Speed.Add(Speed * 1.85);

      Course := (Ord(Data[7]) - 28);
      if (Course > 0) and (Course < 99) then
      begin
        Course := ((M / 10) * 100) + Course;
        if (Course >= 400) then
          Course := Course - 400;

        if (Course > 0) then
          APRSMessage^.Course := Course;
      end;
    end;

  i := DecodeMicEMessage(APRSMessage^.ToCall);
  if (i >= 0) and (i < Length(MicEMsgs.Messages)) then
    APRSMessage^.MicEMessage := MicEMsgs.Messages[i];

  APRSMessage^.Longitude := Lon;
  APRSMessage^.IconPrimary := Data[9];
  APRSMessage^.Icon := Data[8];
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
  if APRSMessage^.ToCall[4] in ['P'..'Y'] then
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

