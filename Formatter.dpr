program Formatter;

uses
  Vcl.Forms,
  fMain in 'C:\Users\Softwarer\Documents\RAD Studio\Projects\Formatter\fMain.pas' {FormMain},
  Base in 'C:\Users\Softwarer\Documents\RAD Studio\Projects\Formatter\Base.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

