unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    edSrc: TMemo;
    edDest: TMemo;
    procedure FormResize(Sender: TObject);
    procedure edSrcChange(Sender: TObject);
  private
    procedure Process(const AText: string);
  public
  end;

var
  FormMain: TFormMain;

implementation

uses Streams, Tokens, Printer, Tokenizer;

{$R *.dfm}

procedure TFormMain.edSrcChange(Sender: TObject);
begin
  Process(edSrc.Text);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  edSrc.Width := Self.ClientWidth div 2;
end;

procedure TFormMain.Process(const AText: string);
var
  P: TPrinter;
  S: TBufferedStream<TToken>;
begin
  S := TCommentProcessor.Create(TWhitespaceSkipper.Create(TTokenizer.Create(TPositionStream.Create(TStringStream.Create(AText)))));
  P := TPrinter.Create;
  try
    { прокрутим список, чтобы комментарии привязались до печати }
    S.SaveMark;
    while not S.Eof do S.Next;
    S.Restore;
    while not S.Eof do S.Next.Describe(P);
  finally
    edDest.Text := P.Data;
    FreeAndNil(S);
    FreeAndNil(P);
  end;
end;

end.

