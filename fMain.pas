unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Printers_;

type
  TFormMain = class(TForm)
    edSrc: TMemo;
    Pages: TPageControl;
    tabTokenizer: TTabSheet;
    edTokenizer: TMemo;
    tabParser: TTabSheet;
    treeParser: TTreeView;
    tabResult: TTabSheet;
    edResult: TMemo;
    procedure FormResize(Sender: TObject);
    procedure edSrcChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PagesChange(Sender: TObject);
  private
    TokenizerPrinter, SyntaxTreePrinter, ResultPrinter: TPrinter;
    function CurrentPrinter: TPrinter;
    procedure UpdateData;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses Streams, Tokens, Tokenizer, Parser;

{$R *.dfm}

procedure TFormMain.FormShow(Sender: TObject);
begin
  TokenizerPrinter  := TPrinter.CreateTokenizerPrinter(edTokenizer.Lines);
  SyntaxTreePrinter := TPrinter.CreateSyntaxTreePrinter(treeParser);
  ResultPrinter     := TPrinter.CreateFormatterPrinter(edResult.Lines);
  Self.WindowState  := wsMaximized;
end;

procedure TFormMain.PagesChange(Sender: TObject);
begin
  UpdateData;
end;

procedure TFormMain.edSrcChange(Sender: TObject);
begin
  UpdateData;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TokenizerPrinter);
  FreeAndNil(SyntaxTreePrinter);
  FreeAndNil(ResultPrinter);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  edSrc.Width := Self.ClientWidth div 2;
end;

function TFormMain.CurrentPrinter: TPrinter;
begin
  if Pages.ActivePage = tabTokenizer then
    Result := TokenizerPrinter
  else if Pages.ActivePage = tabParser then
    Result := SyntaxTreePrinter
  else
    Result := ResultPrinter;
end;

procedure TFormMain.UpdateData;
var Statements: TBufferedStream<TStatement>;
begin
  Statements := TParser.Create(TCommentProcessor.Create(TWhitespaceSkipper.Create(TTokenizer.Create(TPositionStream.Create(TStringStream.Create(edSrc.Text))))));
  try
    CurrentPrinter.BeginPrint;
    while not Statements.Eof do CurrentPrinter.PrintItem(Statements.Next);
  finally
    CurrentPrinter.EndPrint;
    FreeAndNil(Statements);
  end;
end;

end.

