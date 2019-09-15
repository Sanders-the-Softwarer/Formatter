unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TFormMain = class(TForm)
    edSrc: TMemo;
    Pages: TPageControl;
    tabTokenizer: TTabSheet;
    edTokenizer: TMemo;
    tabParser: TTabSheet;
    treeParser: TTreeView;
    procedure FormResize(Sender: TObject);
    procedure edSrcChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure Process(const AText: string);
  public
  end;

var
  FormMain: TFormMain;

implementation

uses Streams, Tokens, Printer, TreePrinter, Tokenizer, Parser;

{$R *.dfm}

procedure TFormMain.edSrcChange(Sender: TObject);
begin
  Process(edSrc.Text);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  edSrc.Width := Self.ClientWidth div 2;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  Self.WindowState := wsMaximized;
end;

procedure TFormMain.Process(const AText: string);
var
  P: TPrinter;
  TP: TTreePrinter;
  Tokens: TBufferedStream<TToken>;
  Statements: TBufferedStream<TStatement>;
begin
  Tokens := TCommentProcessor.Create(TWhitespaceSkipper.Create(TTokenizer.Create(TPositionStream.Create(TStringStream.Create(AText)))));
  Statements := TParser.Create(Tokens);
  P := TPrinter.Create;
  TP := TTreePrinter.Create(treeParser);
  try
    treeParser.Items.BeginUpdate;
    Tokens.SaveMark;
    while not Statements.Eof do Statements.Next.Describe(TP);
    Tokens.Restore;
    while not Tokens.Eof do Tokens.Next.Describe(P);
  finally
    edTokenizer.Text := P.Data;
    treeParser.Items.EndUpdate;
    FreeAndNil(Statements);
    FreeAndNil(P);
    FreeAndNil(TP);
  end;
end;

end.

