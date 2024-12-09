unit uinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, LCLIntf, ComCtrls;

type

  { TFInfo }

  TFInfo = class(TForm)
    ButtonPanel1: TButtonPanel;
    IGithubDonation: TImage;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LFPSourceCode: TLabel;
    LGithubDonation: TLabel;
    Memo1: TMemo;
    procedure ButtonPanel1Click(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure LFPSourceCodeClick(Sender: TObject);
    procedure LGithubDonationClick(Sender: TObject);
  private

  public

  end;

var
  FInfo: TFInfo;

implementation

{$R *.lfm}

{ TFInfo }

procedure TFInfo.CloseButtonClick(Sender: TObject);
begin
  Close
end;

procedure TFInfo.ButtonPanel1Click(Sender: TObject);
begin
  Close
end;

procedure TFInfo.LFPSourceCodeClick(Sender: TObject);
begin
  if not OpenURL('https://github.com/andreaspeters/aprsmap') then
    ShowMessage('Could not open URL: https://github.com/andreaspeters/aprsmap');
end;

procedure TFInfo.LGithubDonationClick(Sender: TObject);
begin
  if not OpenURL('https://github.com/sponsors/AVENTER-UG') then
    ShowMessage('Could not open URL: https://github.com/sponsors/AVENTER-UG');
end;

end.

