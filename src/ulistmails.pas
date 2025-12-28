unit ulistmails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Grids,
  PairSplitter, Menus, ComCtrls, ActnList, RichMemo, utypes, RegExpr, FileUtil,
  LConvEncoding, Types, ueditor;

type

  { TFListMails }

  TMessageHeader = record
    MType: String;
    DateStr: String;
    TimeStr: String;
    Subject: String;
    FromCall: String;
    ToCall: String;
    Message: String;
  end;

  TFListMails = class(TForm)
    actDeleteMail: TAction;
    actClose: TAction;
    actNewMessage: TAction;
    actList: TActionList;
    MenuItem1: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    pmMailList: TPopupMenu;
    sdSaveAs: TSaveDialog;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton7: TToolButton;
    trmShowMail: TRichMemo;
    sgMailList: TStringGrid;
    procedure actCloseExecute(Sender: TObject);
    procedure actDeleteMailExecute(Sender: TObject);
    procedure actNewMessageExecute(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListFilesToGrid;
    procedure AutoSizeStringGridColumns;
    procedure sgMailListClick(Sender: TObject);
    procedure sgMailListDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SortGridByDate;
    function ParseMessageHeader(const FileName: String): TMessageHeader;
    function ParseDateTimeString(const S: String): TDateTime;
    function ExpandTabs(const S: String; TabWidth: Integer): String;
    function LoadFileAsRawByteString(const FileName: String): RawByteString;
    function RemoveANSICodes(const S: AnsiString): String;
    function GetField(const FieldText, Field: String): String;
  private

  public
    procedure SetConfig(Config: PAPRSConfig);
  end;

var
  FListMails: TFListMails;
  APRSConfig: PAPRSConfig;

implementation

{$R *.lfm}

{ TFListMails }

procedure TFListMails.SetConfig(Config: PAPRSConfig);
begin
  APRSConfig := Config;
end;

procedure TFListMails.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFListMails.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

function TFListMails.ExpandTabs(const S: string; TabWidth: Integer): string;
var
  i, Col: Integer;
  ResultStr: string;
begin
  Col := 0;
  ResultStr := '';
  for i := 1 to Length(S) do
  begin
    if S[i] = #9 then
    begin
      repeat
        ResultStr := ResultStr + ' ';
        Inc(Col);
      until (Col mod TabWidth = 0);
    end
    else
    begin
      ResultStr := ResultStr + S[i];
      Inc(Col);
    end;
  end;
  Result := ResultStr;
end;


procedure TFListMails.FormResize(Sender: TObject);
begin
end;

procedure TFListMails.FormShow(Sender: TObject);
begin
  sgMailList.FixedCols := 0;
  ListFilesToGrid;
  SortGridByDate;
  PairSplitter1.Position := FListMails.Height div 2;
end;

procedure TFListMails.actDeleteMailExecute(Sender: TObject);
var Row: Integer;
    FileName: String;
    RowsToDelete: TList;
    i: Integer;
begin
  if sgMailList.Selection.Top <= 0 then
    Exit;

  if MessageDlg('Sure you want to delete the selected Mails?', mtConfirmation, [mbOK, mbCancel], 0) <> mrOK then
    Exit;

  RowsToDelete := TList.Create;
  try
    // get all selected rows
    for Row := sgMailList.Selection.Top to sgMailList.Selection.Bottom do
      if Row > 0 then // ignore header line
        RowsToDelete.Add(Pointer(Row));

    // delete from down to top to preserve index
    for i := RowsToDelete.Count -1 downto 0 do
    begin
      Row := Integer(RowsToDelete[i]);

      FileName := APRSConfig^.MailDirectory + DirectorySeparator + sgMailList.Cells[8, Row];

      if FileExists(FileName) then
        if not DeleteFile(FileName) then
          ShowMessage('Could not delete file: ' + FileName);

      sgMailList.DeleteRow(Row);
    end;

    ListFilesToGrid;
    SortGridByDate;
  finally
    RowsToDelete.Free;
  end;
end;

procedure TFListMails.actNewMessageExecute(Sender: TObject);
begin
  TFEditor.SetConfig(APRSConfig);
  TFEditor.Show;
end;

procedure TFListMails.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFListMails.ListFilesToGrid;
var
  SR: TSearchRec;
  Row: Integer;
  Path: String;
  Header: TMessageHeader;
begin
  Path := APRSConfig^.MailDirectory;

  sgMailList.Clear;
  sgMailList.RowCount := 1;
  sgMailList.ColCount := 9;

  sgMailList.Cells[0, 0] := 'Nr';
  sgMailList.Cells[1, 0] := 'T';
  sgMailList.Cells[2, 0] := 'Date';
  sgMailList.Cells[3, 0] := 'Time';
  sgMailList.Cells[4, 0] := 'From';
  sgMailList.Cells[5, 0] := 'To';
  sgMailList.Cells[6, 0] := 'Message';
  sgMailList.Cells[7, 0] := 'Size (Bytes)';
  sgMailList.Cells[8, 0] := 'Filename';

  Row := 1;

  if FindFirst(Path + DirectorySeparator + '*', faAnyFile and not faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory) = 0 then
      begin
        Header := ParseMessageHeader(Path + DirectorySeparator + SR.Name);

        sgMailList.RowCount := Row + 1;
        sgMailList.Cells[1, Row] := Header.MType;
        sgMailList.Cells[2, Row] := Header.DateStr;
        sgMailList.Cells[3, Row] := Header.TimeStr;
        sgMailList.Cells[4, Row] := Header.FromCall;
        sgMailList.Cells[5, Row] := Header.ToCall;
        sgMailList.Cells[6, Row] := Header.Message;
        sgMailList.Cells[7, Row] := IntToStr(SR.Size);
        sgMailList.Cells[8, Row] := SR.Name;

        Inc(Row);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  AutoSizeStringGridColumns;
end;

procedure TFListMails.SortGridByDate;
var  i, j, Col, RowCount: Integer;
     Date1, Date2: TDateTime;
     Temp: String;
begin
  RowCount := sgMailList.RowCount;
  Col := sgMailList.ColCount;

  // BubbleSort
  for i := 1 to RowCount - 2 do
    for j := i + 1 to RowCount - 1 do
    begin
      Date1 := ParseDateTimeString(sgMailList.Cells[2, i] + ' ' + sgMailList.Cells[3, i]);
      Date2 := ParseDateTimeString(sgMailList.Cells[2, j] + ' ' + sgMailList.Cells[3, j]);

      if Date1 < Date2 then
      begin
        for Col := 0 to sgMailList.ColCount - 1 do
        begin
          Temp := sgMailList.Cells[Col, i];
          sgMailList.Cells[Col, i] := sgMailList.Cells[Col, j];
          sgMailList.Cells[Col, j] := Temp;
        end;
      end;
    end;
  for i := 1 to RowCount -1 do
    sgMailList.Cells[0, i] := IntToStr(i);
end;

function TFListMails.ParseDateTimeString(const S: String): TDateTime;
var FS: TFormatSettings;
    CleanStr: string;
    Regex: TRegExpr;
    FBDate: TDateTime;
begin
  // FormatSettings konfigurieren
  FS := DefaultFormatSettings;
  FS.DateSeparator := '.';
  FS.TimeSeparator := ':';
  FS.ShortDateFormat := 'dd.mm.yy';
  FS.ShortTimeFormat := 'hh:nn';

  // das 'z' entfernen
  Regex := TRegExpr.Create;
  Regex.Expression := '\b\d{2}\.\d{2}\.\d{2} \d{2}:\d{2}z\b';
  Regex.ModifierI := True;
  if Regex.Exec(S) then
  begin
    CleanStr := StringReplace(S, 'z', '', [rfIgnoreCase]);
    Result := StrToDateTime(CleanStr, FS);
  end
  else
    Result := EncodeDate(1970, 1, 1);
end;


function TFListMails.ParseMessageHeader(const FileName: String): TMessageHeader;
var sl: TStringList;
    i: Integer;
    Message: Boolean;
begin
  Message := False;
  FillChar(Result, SizeOf(Result), 0);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);

    if sl.Count > 0 then
    begin
      for i := 0 to sl.Count-1 do
      begin
        if Pos('MType:',sl.Strings[i]) > 0 then
          Result.MType := GetField('MType:', sl.Strings[i]);
        if Pos('FromCall:',sl.Strings[i]) > 0 then
          Result.FromCall := GetField('FromCall:', sl.Strings[i]);
        if Pos('ToCall:',sl.Strings[i]) > 0 then
          Result.ToCall := GetField('ToCall:', sl.Strings[i]);
        if Pos('DateStr:',sl.Strings[i]) > 0 then
          Result.DateStr := GetField('DateStr:', sl.Strings[i]);
        if Pos('TimeStr:',sl.Strings[i]) > 0 then
          Result.TimeStr := GetField('TimeStr:', sl.Strings[i]);
        if Pos('Message:',sl.Strings[i]) > 0 then
        begin
          Message := True;
          Continue;
        end;

        if Message then
          Result.Message := Result.Message + sl.Strings[i];
      end;
    end;

  finally
    sl.Free;
  end;
end;

function TFListMails.GetField(const FieldText, Field: String): String;
begin
  Result := '';
  if Pos(FieldText, Field) > 0 then
    Result := Trim(StringReplace(Field, FieldText, '', [rfReplaceAll]));
end;

procedure TFListMails.sgMailListClick(Sender: TObject);
var raw: RawByteString;
    utf8Text: String;
    FileName: String;
begin
  fileName := APRSConfig^.MailDirectory + DirectorySeparator + sgMailList.Cells[8, sgMailList.Row];

  if not FileExists(FileName) then
     Exit;

  raw := LoadFileAsRawByteString(fileName);
  utf8Text := CP437ToUTF8(raw);

  trmShowMail.Lines.Text := RemoveANSICodes(utf8Text);
end;

function TFListMails.RemoveANSICodes(const S: AnsiString): String;
var
  i, Len: Integer;
  Buf: String;
  InEscSeq: Boolean;
begin
  SetLength(Buf, Length(S));
  Len := 0;
  InEscSeq := False;

  for i := 1 to Length(S) do
  begin
    if not InEscSeq then
    begin
      if S[i] = #27 then      // ESC beginnt eine ANSI-Sequenz
        InEscSeq := True
      else
      begin
        Inc(Len);
        Buf[Len] := S[i];
      end;
    end
    else
    begin
      if (S[i] >= '@') and (S[i] <= '~') then
        InEscSeq := False;
      // alle Zeichen der Sequenz werden übersprungen
    end;
  end;

  SetLength(Buf, Len);
  Result := Buf;
end;

function TFListMails.LoadFileAsRawByteString(const FileName: String): RawByteString;
var Stream: TFileStream;
    Buffer: RawByteString;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Buffer, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Buffer[1], Stream.Size);
  finally
    Stream.Free;
  end;
  Result := Buffer;
end;

procedure TFListMails.sgMailListDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  with sgMailList do
  begin
    if ARow = 0 then
      Canvas.Font.Style := [fsBold]
    else
      Canvas.Font.Style := [];

    Canvas.FillRect(ARect);

    Canvas.TextRect(ARect, ARect.Left + 2, ARect.Top + 2, Cells[ACol, ARow]);
  end;
end;

procedure TFListMails.AutoSizeStringGridColumns;
var Col, Row, W, MaxWidth: Integer;
    CellText: string;
    ACanvas: TCanvas;
begin
  ACanvas := sgMailList.Canvas;

  for Col := 0 to sgMailList.ColCount - 1 do
  begin
    MaxWidth := 0;
    for Row := 0 to sgMailList.RowCount - 1 do
    begin
      CellText := sgMailList.Cells[Col, Row];
      W := ACanvas.TextWidth(CellText) + 10; // +10 für Abstand/Padding
      if W > MaxWidth then
        MaxWidth := W;
    end;
    sgMailList.ColWidths[Col] := MaxWidth;
  end;
  sgMailList.ColWidths[8] := 0; // hide filename col
end;

end.

