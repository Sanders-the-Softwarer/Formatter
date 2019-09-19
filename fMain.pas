unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Printers_,
  Vcl.ExtCtrls, Vcl.Samples.Spin, Parser;

type
  TFormMain = class(TForm)
    pgDest: TPageControl;
    tabTokenizer: TTabSheet;
    edTokenizer: TMemo;
    tabParser: TTabSheet;
    treeParser: TTreeView;
    tabResult: TTabSheet;
    edResult: TMemo;
    panSrc: TPanel;
    pgSrc: TPageControl;
    tabSrc: TTabSheet;
    edSrc: TMemo;
    pgSettings: TPageControl;
    tabSettings: TTabSheet;
    spVert: TSplitter;
    edDeclarationSingleLineParamLimit: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    edArgumentSingleLineParamLimit: TSpinEdit;
    procedure FormResize(Sender: TObject);
    procedure UpdateRequired(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    TokenizerPrinter, SyntaxTreePrinter, ResultPrinter: TPrinter;
    Settings: TParserSettings;
    function CurrentPrinter: TPrinter;
    procedure UpdateData;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses Streams, Tokens, Tokenizer;

{$R *.dfm}

procedure TFormMain.FormShow(Sender: TObject);
begin
  TokenizerPrinter  := TPrinter.CreateTokenizerPrinter(edTokenizer.Lines);
  SyntaxTreePrinter := TPrinter.CreateSyntaxTreePrinter(treeParser);
  ResultPrinter     := TPrinter.CreateFormatterPrinter(edResult.Lines);
  Self.WindowState  := wsMaximized;
  Settings := TParserSettings.Create;
end;

procedure TFormMain.UpdateRequired(Sender: TObject);
begin
  UpdateData;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TokenizerPrinter);
  FreeAndNil(SyntaxTreePrinter);
  FreeAndNil(ResultPrinter);
  FreeAndNil(Settings);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  panSrc.Width := (Self.ClientWidth - spVert.Width) div 2;
end;

function TFormMain.CurrentPrinter: TPrinter;
begin
  if pgDest.ActivePage = tabTokenizer then
    Result := TokenizerPrinter
  else if pgDest.ActivePage = tabParser then
    Result := SyntaxTreePrinter
  else
    Result := ResultPrinter;
end;

procedure TFormMain.UpdateData;
var Statements: TBufferedStream<TStatement>;
//var Statements: TBufferedStream<TToken>;
begin
  Settings.DeclarationSingleLineParamLimit := edDeclarationSingleLineParamLimit.Value;
  Settings.ArgumentSingleLineParamLimit    := edArgumentSingleLineParamLimit.Value;
  Statements := TParser.Create(TCommentProcessor.Create(TMerger.Create(TWhitespaceSkipper.Create(TTokenizer.Create(TPositionStream.Create(TStringStream.Create(edSrc.Text)))))), Settings);
  try
    CurrentPrinter.BeginPrint;
    while not Statements.Eof do CurrentPrinter.PrintItem(Statements.Next);
  finally
    CurrentPrinter.EndPrint;
    FreeAndNil(Statements);
  end;
end;

end.

