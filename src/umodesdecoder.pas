unit umodesdecoder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TModeSMessage = record
    ICAO: Cardinal;
    Flight: String;
    MessageType: Integer;
    AltitudeFeet: Integer;
    RawLatitude: Integer;
    RawLongitude: Integer;
    OddCPR: Boolean;
    Velocity: Integer;
    Track: Integer;
    HasAltitude: Boolean;
    HasPosition: Boolean;
    HasVelocity: Boolean;
  end;

  { Squared IQ magnitude is in the 0..32258 range. Keeping it as Word avoids
    destroying the amplitude ordering needed by the Manchester decoder. }
  TModeSMagnitude = array of Word;
  TModeSBytes = array of Byte;

function ModeSCRC(const Data: TModeSBytes; BitCount: Integer): Cardinal;
function DecodeModeSMessage(const Data: TModeSBytes; out Message: TModeSMessage): Boolean;
function DecodeGlobalCPR(const EvenLatitude, EvenLongitude, OddLatitude,
  OddLongitude: Integer; const UseOdd: Boolean; out Latitude, Longitude: Double): Boolean;
function DemodulateModeS(const Magnitude: TModeSMagnitude; out Messages: array of TModeSMessage): Integer;
function ModeSFlightCharacter(const Value: Integer): Char;

implementation

const
  MODES_CRC_POLY = $FFF409;
  MODES_LONG_BITS = 112;
  MODES_PREAMBLE_SAMPLES = 16;
  MODES_BIT_SAMPLES = 2;

function GetBit(const Data: TModeSBytes; const Bit: Integer): Integer; inline;
begin
  Result := (Data[Bit div 8] shr (7 - (Bit mod 8))) and 1;
end;

function ModeSCRC(const Data: TModeSBytes; BitCount: Integer): Cardinal;
var
  I, Feedback: Integer;
  CRC: Cardinal;
begin
  CRC := 0;
  for I := 0 to BitCount - 1 do
  begin
    Feedback := GetBit(Data, I) xor Integer((CRC shr 23) and 1);
    CRC := (CRC shl 1) and $FFFFFF;
    if Feedback <> 0 then
      CRC := CRC xor MODES_CRC_POLY;
  end;
  Result := CRC;
end;

function ModeSFlightCharacter(const Value: Integer): Char;
begin
  if (Value >= 1) and (Value <= 26) then
    Result := Chr(Ord('A') + Value - 1)
  else if (Value >= 48) and (Value <= 57) then
    Result := Chr(Ord('0') + Value - 48)
  else
    Result := ' ';
end;

function DecodeFlight(const Data: TModeSBytes): String;
var
  I, V: Integer;
begin
  Result := '';
  for I := 0 to 7 do
  begin
    case I of
      0: V := (Data[5] shr 2) and $3F;
      1: V := ((Data[5] and 3) shl 4) or (Data[6] shr 4);
      2: V := ((Data[6] and $0F) shl 2) or (Data[7] shr 6);
      3: V := Data[7] and $3F;
      4: V := (Data[8] shr 2) and $3F;
      5: V := ((Data[8] and 3) shl 4) or (Data[9] shr 4);
      6: V := ((Data[9] and $0F) shl 2) or (Data[10] shr 6);
      else V := Data[10] and $3F;
    end;
    Result := Result + ModeSFlightCharacter(V);
  end;
  Result := TrimRight(Result);
end;

function DecodeModeSMessage(const Data: TModeSBytes; out Message: TModeSMessage): Boolean;
var
  DF, TypeCode, SubType, Parity: Integer;
  AltitudeCode, AltitudeN, EastWest, NorthSouth, SpeedMultiplier: Integer;
  EastWestWest, NorthSouthSouth: Boolean;
begin
  FillChar(Message, SizeOf(Message), 0);
  Result := False;
  if Length(Data) <> 14 then Exit;
  DF := Data[0] shr 3;
  if DF <> 17 then Exit;
  Parity := (Data[11] shl 16) or (Data[12] shl 8) or Data[13];
  if ModeSCRC(Data, 88) <> Cardinal(Parity) then Exit;

  Message.ICAO := (Data[1] shl 16) or (Data[2] shl 8) or Data[3];
  TypeCode := Data[4] shr 3;
  Message.MessageType := TypeCode;

  if (TypeCode >= 1) and (TypeCode <= 4) then
    Message.Flight := DecodeFlight(Data)
  else if (TypeCode >= 9) and (TypeCode <= 18) then
  begin
    AltitudeCode := (Data[5] shl 4) or (Data[6] shr 4);
    { AC12 bit 4 is the Q bit. Only Q=1 carries a 25 ft barometric altitude. }
    if (AltitudeCode and $10) <> 0 then
    begin
      AltitudeN := ((AltitudeCode and $FE0) shr 1) or (AltitudeCode and $0F);
      Message.AltitudeFeet := AltitudeN * 25 - 1000;
      Message.HasAltitude := True;
    end;
    Message.RawLatitude := ((Data[6] and 3) shl 15) or (Data[7] shl 7) or (Data[8] shr 1);
    Message.RawLongitude := ((Data[8] and 1) shl 16) or (Data[9] shl 8) or Data[10];
    Message.OddCPR := (Data[6] and 4) <> 0;
    Message.HasPosition := True;
  end
  else if TypeCode = 19 then
  begin
    SubType := Data[4] and 7;
    if (SubType = 1) or (SubType = 2) then
    begin
      EastWestWest := (Data[5] and $04) <> 0;
      EastWest := ((Data[5] and $03) shl 8) or Data[6];
      NorthSouthSouth := (Data[7] and $80) <> 0;
      NorthSouth := ((Data[7] and $7F) shl 3) or (Data[8] shr 5);
      if (EastWest <> 0) and (NorthSouth <> 0) then
      begin
        Dec(EastWest);
        Dec(NorthSouth);
        SpeedMultiplier := 1;
        if SubType = 2 then SpeedMultiplier := 4;
        EastWest := EastWest * SpeedMultiplier;
        NorthSouth := NorthSouth * SpeedMultiplier;
        if EastWestWest then EastWest := -EastWest;
        if NorthSouthSouth then NorthSouth := -NorthSouth;
        Message.Velocity := Round(Sqrt(Sqr(EastWest) + Sqr(NorthSouth)));
        Message.Track := Round(ArcTan2(EastWest, NorthSouth) * 180.0 / Pi);
        if Message.Track < 0 then Inc(Message.Track, 360);
        Message.HasVelocity := True;
      end;
    end;
  end;
  Result := True;
end;

function CPRMod(const Value, Divisor: Integer): Integer; inline;
begin
  Result := Value mod Divisor;
  if Result < 0 then Inc(Result, Divisor);
end;

function CPRNl(const Latitude: Double): Integer;
const
  LIMITS: array[0..58] of Double = (
    10.47047130, 14.82817437, 18.18626357, 21.02939493, 23.54504487,
    25.82924707, 27.93898710, 29.91135686, 31.77209708, 33.53993436,
    35.22899598, 36.85025108, 38.41241892, 39.92256684, 41.38651832,
    42.80914012, 44.19454951, 45.54626723, 46.86733252, 48.16039128,
    49.42776439, 50.67150166, 51.89342470, 53.09516153, 54.27817472,
    55.44378444, 56.59318756, 57.72747354, 58.84763776, 59.95459277,
    61.04917774, 62.13216659, 63.21428092, 64.29518780, 65.37455310,
    66.45204567, 67.52734399, 68.59999999, 69.66965557, 70.73600000,
    71.79899999, 72.86100000, 73.92300000, 74.98500000, 76.04700000,
    77.10900000, 78.17100000, 79.23300000, 80.29500000, 81.35700000,
    82.41900000, 83.48100000, 84.54300000, 85.60500000, 86.66700000,
    87.72900000, 88.79100000, 89.85300000, 90.00000000);
var
  A: Double;
  I: Integer;
begin
  A := Abs(Latitude);
  Result := 59;
  for I := 0 to High(LIMITS) do
    if A < LIMITS[I] then
    begin
      Result := 59 - I;
      Exit;
    end;
  Result := 1;
end;

function DecodeGlobalCPR(const EvenLatitude, EvenLongitude, OddLatitude,
  OddLongitude: Integer; const UseOdd: Boolean; out Latitude, Longitude: Double): Boolean;
var
  J, M, NL, NLI: Integer;
  LatEven, LatOdd, DLatEven, DLatOdd, DLong: Double;
begin
  Result := False;
  if (EvenLatitude < 0) or (EvenLatitude >= 131072) or
     (OddLatitude < 0) or (OddLatitude >= 131072) or
     (EvenLongitude < 0) or (EvenLongitude >= 131072) or
     (OddLongitude < 0) or (OddLongitude >= 131072) then Exit;
  DLatEven := 360.0 / 60.0;
  DLatOdd := 360.0 / 59.0;
  J := Trunc((59.0 * EvenLatitude - 60.0 * OddLatitude) / 131072.0 + 0.5);
  LatEven := DLatEven * (CPRMod(J, 60) + EvenLatitude / 131072.0);
  LatOdd := DLatOdd * (CPRMod(J, 59) + OddLatitude / 131072.0);
  if LatEven >= 270 then LatEven := LatEven - 360;
  if LatOdd >= 270 then LatOdd := LatOdd - 360;
  if CPRNl(LatEven) <> CPRNl(LatOdd) then Exit;
  if UseOdd then
  begin
    Latitude := LatOdd;
    NL := CPRNl(LatOdd);
    NLI := Max(NL - 1, 1);
    M := Trunc((EvenLongitude * (NL - 1) - OddLongitude * NL) / 131072.0 + 0.5);
    DLong := 360.0 / NLI;
    Longitude := DLong * (CPRMod(M, NLI) + OddLongitude / 131072.0);
  end
  else
  begin
    Latitude := LatEven;
    NL := CPRNl(LatEven);
    NLI := Max(NL, 1);
    M := Trunc((EvenLongitude * (NL - 1) - OddLongitude * NL) / 131072.0 + 0.5);
    DLong := 360.0 / NLI;
    Longitude := DLong * (CPRMod(M, NLI) + EvenLongitude / 131072.0);
  end;
  if Longitude > 180 then Longitude := Longitude - 360;
  Result := (Latitude >= -90) and (Latitude <= 90) and
            (Longitude >= -180) and (Longitude <= 180);
end;

function HasModeSPreamble(const Magnitude: TModeSMagnitude; const Start: Integer): Boolean;
const
  HighSamples: array[0..3] of Integer = (0, 2, 7, 9);
var
  I, HighIndex, HighValue, LowValue: Integer;
begin
  HighIndex := 0;
  HighValue := High(Magnitude[0]) + 1;
  LowValue := 0;
  for I := 0 to MODES_PREAMBLE_SAMPLES - 1 do
  begin
    if (HighIndex <= High(HighSamples)) and (I = HighSamples[HighIndex]) then
    begin
      HighValue := Magnitude[Start + I];
      Inc(HighIndex);
    end
    else
      LowValue := Magnitude[Start + I];
    if HighValue <= LowValue then Exit(False);
  end;
  Result := True;
end;

function DecodeManchesterBit(const A, B, C, D: Word; out Bit: Integer): Boolean;
var
  PreviousBit, CurrentBit: Boolean;
begin
  PreviousBit := A > B;
  CurrentBit := C > D;
  Bit := Ord(CurrentBit);
  if CurrentBit and PreviousBit then
    Result := C > B
  else if CurrentBit then
    Result := D < B
  else if PreviousBit then
    Result := D > B
  else
    Result := C < B;
end;

function DemodulateModeS(const Magnitude: TModeSMagnitude; out Messages: array of TModeSMessage): Integer;
const
  AllowedManchesterErrors = 5;
var
  I, BitIndex, BitValue, ErrorCount, FrameBits: Integer;
  A, B, C, D: Word;
  Data: TModeSBytes;
  Candidate: TModeSMessage;
begin
  Result := 0;
  I := 0;
  while I + MODES_PREAMBLE_SAMPLES + MODES_LONG_BITS * MODES_BIT_SAMPLES <= Length(Magnitude) do
  begin
    if not HasModeSPreamble(Magnitude, I) then
    begin
      Inc(I);
      Continue;
    end;

    SetLength(Data, 14);
    FillChar(Data[0], Length(Data), 0);
    A := Magnitude[I];
    B := Magnitude[I + 1];
    ErrorCount := 0;
    FrameBits := MODES_LONG_BITS;
    for BitIndex := 0 to MODES_LONG_BITS - 1 do
    begin
      C := Magnitude[I + MODES_PREAMBLE_SAMPLES + BitIndex * 2];
      D := Magnitude[I + MODES_PREAMBLE_SAMPLES + BitIndex * 2 + 1];
      if not DecodeManchesterBit(A, B, C, D, BitValue) then
      begin
        Inc(ErrorCount);
        if ErrorCount > AllowedManchesterErrors then Break;
        BitValue := Ord(C > D);
        A := 0;
        B := High(Word);
      end
      else
      begin
        A := C;
        B := D;
      end;
      if BitValue <> 0 then
        Data[BitIndex div 8] := Data[BitIndex div 8] or (1 shl (7 - (BitIndex mod 8)));
      if BitIndex = 7 then
        if (Data[0] and $80) = 0 then
          FrameBits := 56;
      if BitIndex + 1 = FrameBits then Break;
    end;

    if (ErrorCount <= AllowedManchesterErrors) and (FrameBits = MODES_LONG_BITS) and
       DecodeModeSMessage(Data, Candidate) then
    begin
      if Result < Length(Messages) then Messages[Result] := Candidate;
      Inc(Result);
      I := I + MODES_PREAMBLE_SAMPLES + MODES_LONG_BITS * MODES_BIT_SAMPLES;
    end
    else
      Inc(I);
  end;
end;

end.
