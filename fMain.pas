////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//              ������� � ������������ ����� ��������� ����������             //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit fMain;

{ ----- ���������� -------------------------------------------------------------

  ����� �������������� ��������� ������� � ����� �������� ������ � ����������
  �������������� ��������� ��������� ���, ����� ��� �� ����������� ���������
  �� ���� � �� �� �������. ��� ����� ����� � �������� ��������� ��������
  SyncNotification.

  ��������� TMemo �� ����� ��������� � ��������� �� ������, ������� �����
  � ������� ��������� ��������� �������, � ���� ��� ���������� - ����������
  �������������.

------------------------------------------------------------------------------ }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Printers_,
  Vcl.ExtCtrls, Vcl.Samples.Spin, Parser, Streams, Tokens, Statements, Tokenizer;

type
  TFormMain = class(TForm)
    pgDest: TPageControl;
    tabTokenizer: TTabSheet;
    edTokenizer: TListBox;
    tabParser: TTabSheet;
    treeParser: TTreeView;
    tabResult: TTabSheet;
    edResult: TMemo;
    panSrc: TPanel;
    pgSrc: TPageControl;
    tabSrc: TTabSheet;
    edSrc: TMemo;
    tabSettings: TTabSheet;
    spVert: TSplitter;
    edDeclarationSingleLineParamLimit: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    edArgumentSingleLineParamLimit: TSpinEdit;
    GroupBox1: TGroupBox;
    checkAlignFields: TCheckBox;
    checkAlignVariables: TCheckBox;
    checkAlignSpecialComments: TCheckBox;
    GroupBox3: TGroupBox;
    checkReplaceDefault: TCheckBox;
    Label3: TLabel;
    edMatchParamLimit: TSpinEdit;
    checkReplaceAsIs: TCheckBox;
    tabAlarm: TTabSheet;
    edAlarm: TListBox;
    tmMemo: TTimer;
    procedure FormResize(Sender: TObject);
    procedure UpdateRequired(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edTokenizerClick(Sender: TObject);
    procedure edAlarmClick(Sender: TObject);
    procedure treeParserChange(Sender: TObject; Node: TTreeNode);
    procedure tmMemoTimer(Sender: TObject);
    procedure pgDestChange(Sender: TObject);
  private
    TokenizerPrinter, SyntaxTreePrinter, ResultPrinter, AlarmPrinter: TPrinter;
    TokenStream: TBufferedStream<TToken>;
    StatementStream: TBufferedStream<TStatement>;
    Settings: TFormatSettings;
    PrevSrcCaret, PrevResultCaret: integer;
    IntoSync: boolean;
    procedure UpdateData;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
    procedure CoordsToCaret(Memo: TMemo; const Line, Col: integer; out Pos: integer);
    procedure CaretToCoords(Memo: TMemo; out Line, Col: integer; Pos: integer);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{ ���������� � ���������� ���������������� ������ }
procedure TFormMain.UpdateData;
begin
  FreeAndNil(StatementStream);
  { ��������� ��������� }
  Settings.DeclarationSingleLineParamLimit := edDeclarationSingleLineParamLimit.Value;
  Settings.ArgumentSingleLineParamLimit    := edArgumentSingleLineParamLimit.Value;
  Settings.MatchParamLimit                 := edMatchParamLimit.Value;
  Settings.AlignVariables                  := checkAlignVariables.Checked;
  Settings.AlignFields                     := checkAlignFields.Checked;
  Settings.AlignSpecialComments            := checkAlignSpecialComments.Checked;
  Settings.ReplaceDefault                  := checkReplaceDefault.Checked;
  Settings.ReplaceAsIs                     := checkReplaceAsIs.Checked;
  { �������� ������ }
  TokenStream     := TMerger.Create(TProcedureDeleteStream.Create(TWhitespaceSkipper.Create(TTokenizer.Create(TPositionStream.Create(TStringStream.Create(edSrc.Text))))));
  StatementStream := TParser.Create(TCommentProcessor.Create(TokenStream), Settings);
  { ���������� ������ }
  StatementStream.PrintAll(ResultPrinter);
  StatementStream.PrintAll(SyntaxTreePrinter);
  { ���� ����� �������� � �� StatementStream, �� ����� �� ����� ����� �������� ��������������� ������� }
  TokenStream.PrintAll(TokenizerPrinter);
  TokenStream.PrintAll(AlarmPrinter);
  { ������� ������������� }
  PrevSrcCaret := -1;
end;

{ �������� ���������� ��� ������������� �������� �� ��������� �������������� ������ }
procedure TFormMain.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
var Caret, i: integer;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    { ���� ������� ������� - ������ �� ��������, �������������� �������� }
    if Assigned(AToken) then
    begin
      CoordsToCaret(edSrc, AToken.Line, AToken.Col, Caret);
      edSrc.SelStart := Caret;
      edSrc.SelLength := AToken.Value.Length;
    end;
    { �������� �������� �������, ������ ��� �������� ������� }
    if pgDest.ActivePage = tabTokenizer then TokenizerPrinter.SyncNotification(AToken, ALine, ACol, ALen);
    if pgDest.ActivePage = tabParser then SyntaxTreePrinter.SyncNotification(AToken, ALine, ACol, ALen);
    if pgDest.ActivePage = tabResult then ResultPrinter.SyncNotification(AToken, ALine, ACol, ALen);
    if pgDest.ActivePage = tabAlarm then AlarmPrinter.SyncNotification(AToken, ALine, ACol, ALen);
  finally
    IntoSync := false;
  end;
end;

{ ��� �������� ����� ��������� �������� � ���������, � ����� ������������� ���������� }
procedure TFormMain.FormShow(Sender: TObject);
begin
  TokenizerPrinter  := TPrinter.CreateTokenizerPrinter(edTokenizer);
  SyntaxTreePrinter := TPrinter.CreateSyntaxTreePrinter(treeParser);
  ResultPrinter     := TPrinter.CreateFormatterPrinter(edResult);
  AlarmPrinter      := TPrinter.CreateAlarmPrinter(edAlarm, tabAlarm);
  Self.WindowState  := wsMaximized;
  Settings := TFormatSettings.Create;
  ResultPrinter.Settings := Settings;
  Printers_.SyncNotification := Self.SyncNotification;
end;

{ ��� ������������� ����������� ������ �������� ��������, ������� ��� ����� ��������
  ������������� ���������� ������������� }
procedure TFormMain.pgDestChange(Sender: TObject);
begin
  PrevSrcCaret := -1;
end;

{ ������� �� ��������, ��������� ������ ���������� ����� }
procedure TFormMain.UpdateRequired(Sender: TObject);
begin
  UpdateData;
end;

{ ���������� �������� � �������� ������������ �� ������ ��������������� ������ }
procedure TFormMain.edAlarmClick(Sender: TObject);
begin
  AlarmPrinter.ControlChanged;
end;

{ ���������� �������� � �������� ������������ �� ������ ������ }
procedure TFormMain.edTokenizerClick(Sender: TObject);
begin
  TokenizerPrinter.ControlChanged;
end;

{ ���������� �������� � �������� ������������ �� ��������������� ������ }
procedure TFormMain.treeParserChange(Sender: TObject; Node: TTreeNode);
begin
  SyntaxTreePrinter.ControlChanged;
end;

{ ���������� � �������� ������������ �� ���������� ���� ���������������� ������ }
procedure TFormMain.tmMemoTimer(Sender: TObject);
var Line, Col: integer;
begin
  if edSrc.SelStart <> PrevSrcCaret then
  begin
    PrevSrcCaret := edSrc.SelStart;
    CaretToCoords(edSrc, Line, Col, edSrc.SelStart);
    SyncNotification(nil, Line, Col, 1);
  end;
  if edResult.SelStart <> PrevResultCaret then
  begin
    PrevResultCaret := edResult.SelStart;
    ResultPrinter.ControlChanged;
  end;
end;

{ ����������� ��������, ��������� ������, ��� ���������� ���������� }
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TokenizerPrinter);
  FreeAndNil(SyntaxTreePrinter);
  FreeAndNil(ResultPrinter);
  FreeAndNil(AlarmPrinter);
  FreeAndNil(StatementStream);
  FreeAndNil(Settings);
end;

{ ��������� �������� ��� ������� ���� }
procedure TFormMain.FormResize(Sender: TObject);
begin
  panSrc.Width := (Self.ClientWidth - spVert.Width) div 2;
end;

{ �������� ��������� ������-������� � ��������� ������� }
procedure TFormMain.CoordsToCaret(Memo: TMemo; const Line, Col: integer; out Pos: integer);
var i: integer;
begin
  Pos := 0;
  for i := 0 to Line - 2 do Inc(Pos, Memo.Lines[i].Length + 2);
  Inc(Pos, Col - 1);
end;

{ �������� ��������� ������� � ���������� ������-������� }
procedure TFormMain.CaretToCoords(Memo: TMemo; out Line, Col: integer; Pos: integer);
begin
  Line := 1;
  while Pos > Memo.Lines[Line - 1].Length do
  begin
    Dec(Pos, Memo.Lines[Line - 1].Length + 2);
    Inc(Line);
  end;
  Col := Pos + 1;
end;

end.

