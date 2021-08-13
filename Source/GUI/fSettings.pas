unit fSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Menus,
  Vcl.StdCtrls, frSettings;

type
  TFormSettings = class(TForm)
    frSettings: TFrameSettings;
    btOk: TButton;
    btCancel: TButton;
    procedure btOkClick(Sender: TObject);
  private
  public
    class procedure ShowSettings;
  end;

implementation

uses PLSQLDev_Formatter;

{$R *.dfm}

{ TFormSettings }

procedure TFormSettings.btOkClick(Sender: TObject);
begin
  frSettings.UpdateSettings;
end;

class procedure TFormSettings.ShowSettings;
begin
  with TFormSettings.Create(nil) do
  try
    frSettings.Settings := TPLSQLDevPlugIn.GetInstance.Settings;
    frSettings.UpdateEdits;
    if ShowModal = mrOk then TPLSQLDevPlugIn.GetInstance.SaveSettings;
  finally
    Free;
  end;
end;

end.
