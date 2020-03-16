////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//              Главная и единственная форма тестового приложения             //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit fMain;

{ ----- Примечания -------------------------------------------------------------

  Форма синхронизирует положение курсора в полях входного текста и визуальных
  представлениях различных принтеров так, чтобы они по возможности указывали
  на одну и ту же лексему. Для этого форму и принтеры связывает механизм
  SyncNotification.

  Компонент TMemo не умеет оповещать о навигации по тексту, поэтому форма
  в таймере проверяет положение каретки, и если оно изменилось - инициирует
  синхронизацию.

------------------------------------------------------------------------------ }

interface

uses
  Tokens { должен идти до StdCtrls, чтобы TLabel не мешала загрузке формы },
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, PrinterIntf,
  Vcl.ExtCtrls, Vcl.Samples.Spin, Controller, Streams, Statements, Tokenizer;

type
  TFormMain = class(TForm)
    pgDest: TPageControl;
    tabTokenizer: TTabSheet;
    edTokenizer: TListBox;
    edAlarmToken: TListBox;
    edAlarmStatement: TListBox;
    tabParser: TTabSheet;
    treeParser: TTreeView;
    tabResult: TTabSheet;
    tabAlarmToken: TTabSheet;
    tabAlarmStatement: TTabSheet;
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
    edNamedArgumentSingleLineParamLimit: TSpinEdit;
    GroupBox1: TGroupBox;
    checkAlignFields: TCheckBox;
    checkAlignVariables: TCheckBox;
    checkAlignSpecialComments: TCheckBox;
    GroupBox3: TGroupBox;
    checkReplaceDefault: TCheckBox;
    Label3: TLabel;
    edMatchParamLimit: TSpinEdit;
    tmMemo: TTimer;
    checkAlignTableColumnComments: TCheckBox;
    checkReplaceAsIs: TCheckBox;
    edPreferredExpressionLength: TSpinEdit;
    Label4: TLabel;
    checkAlignExpressions: TCheckBox;
    checkAlignColumns: TCheckBox;
    GroupBox2: TGroupBox;
    checkAddInAccessSpecificator: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    edPositionalArgumentSingleLineParamLimit: TSpinEdit;
    GroupBox4: TGroupBox;
    checkIndentBrackets: TCheckBox;
    procedure FormResize(Sender: TObject);
    procedure UpdateRequired(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edTokenizerClick(Sender: TObject);
    procedure edAlarmTokenClick(Sender: TObject);
    procedure edAlarmStatementClick(Sender: TObject);
    procedure treeParserChange(Sender: TObject; Node: TTreeNode);
    procedure tmMemoTimer(Sender: TObject);
    procedure pgDestChange(Sender: TObject);
  private
    TokenizerPrinter, SyntaxTreePrinter, ResultPrinter, AlarmTokenPrinter, AlarmStatementPrinter: TPrinter;
    MinTokenStream, AdvTokenStream: TBufferedStream<TToken>;
    StatementStream: TBufferedStream<TStatement>;
    Settings: TFormatSettings;
    PrevSrcCaret, PrevResultCaret: integer;
    IntoSync, IntoUpdateSettings: boolean;
    function CorrectCRLF: boolean;
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

uses GUIPrinters;

{ Приведение переносов строк к стандартному виду }
function TFormMain.CorrectCRLF: boolean;
var Src, Dest: string;
begin
  Src := edSrc.Text;
  Dest := StringReplace(Src,  #13#10, #10, [rfReplaceAll]);
  Dest := StringReplace(Dest, #13,    #10, [rfReplaceAll]);
  Dest := StringReplace(Dest, #10, #13#10, [rfReplaceAll]);
  Result := Dest <> Src;
  if Result then edSrc.Text := Dest;
end;

{ Подготовка и распечатка форматированного текста }
procedure TFormMain.UpdateData;
begin
  FreeAndNil(StatementStream);
  { Скопируем настройки }
  if not IntoUpdateSettings then
  begin
    Settings.DeclarationSingleLineParamLimit := edDeclarationSingleLineParamLimit.Value;
    Settings.NamedArgumentSingleLineParamLimit := edNamedArgumentSingleLineParamLimit.Value;
    Settings.PositionalArgumentSingleLineParamLimit := edPositionalArgumentSingleLineParamLimit.Value;
    Settings.MatchParamLimit                 := edMatchParamLimit.Value;
    Settings.AlignVariables                  := checkAlignVariables.Checked;
    Settings.AlignFields                     := checkAlignFields.Checked;
    Settings.AlignColumns                    := checkAlignColumns.Checked;
    Settings.AlignExpressions                := checkAlignExpressions.Checked;
    Settings.AlignSpecialComments            := checkAlignSpecialComments.Checked;
    Settings.AlignTableColumnComments        := checkAlignTableColumnComments.Checked;
    Settings.ReplaceDefault                  := checkReplaceDefault.Checked;
    Settings.ReplaceAsIs                     := checkReplaceAsIs.Checked;
    Settings.AddInAccessSpecificator         := checkAddInAccessSpecificator.Checked;
    Settings.PreferredExpressionLength       := edPreferredExpressionLength.Value;
    Settings.IndentBrackets                  := checkIndentBrackets.Checked;
  end;
  { Создадим потоки }
  MinTokenStream  := Controller.MakeMinimalTokenStream(edSrc.Text);
  AdvTokenStream  := Controller.MakeAdvancedTokenStream(MinTokenStream);
  StatementStream := Controller.MakeStatementStream(AdvTokenStream, Settings);
  { Напечатаем данные }
  StatementStream.PrintAll(ResultPrinter);
  StatementStream.PrintAll(SyntaxTreePrinter);
  StatementStream.PrintAll(AlarmStatementPrinter);
  { Сюда печатаем из MinTokenStream, чтобы увидеть лексемы, выпавшие при печати из синтаксического анализа }
  MinTokenStream.PrintAll(TokenizerPrinter);
  MinTokenStream.PrintAll(AlarmTokenPrinter);
  { Толкнём синхронизацию }
  PrevSrcCaret := -1;
end;

{ Рассылка оповещений для синхронизации движения по различным представлениям текста }
procedure TFormMain.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
var Caret: integer;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    { Если указана лексема - сигнал от принтера, синхронизируем исходник }
    if Assigned(AToken) then
    begin
      CoordsToCaret(edSrc, AToken.Line, AToken.Col, Caret);
      edSrc.SelStart := Caret;
      edSrc.SelLength := AToken.Value.Length;
      PrevSrcCaret := Caret;
    end;
    { Известим активный принтер, прочие для скорости оставим }
    if pgDest.ActivePage = tabTokenizer then TokenizerPrinter.SyncNotification(AToken, ALine, ACol, ALen);
    if pgDest.ActivePage = tabParser then SyntaxTreePrinter.SyncNotification(AToken, ALine, ACol, ALen);
    if pgDest.ActivePage = tabResult then ResultPrinter.SyncNotification(AToken, ALine, ACol, ALen);
    if pgDest.ActivePage = tabAlarmToken then AlarmTokenPrinter.SyncNotification(AToken, ALine, ACol, ALen);
    if pgDest.ActivePage = tabAlarmStatement then AlarmStatementPrinter.SyncNotification(AToken, ALine, ACol, ALen);
  finally
    IntoSync := false;
  end;
end;

{ При создании формы создаются принтеры и настройки, а также настраиваются оповещения }
procedure TFormMain.FormShow(Sender: TObject);
begin
  TokenizerPrinter  := GUIPrinters.CreateTokenizerPrinter(edTokenizer);
  SyntaxTreePrinter := GUIPrinters.CreateSyntaxTreePrinter(treeParser);
  ResultPrinter     := GUIPrinters.CreateFormatterPrinter(edResult);
  AlarmTokenPrinter := GUIPrinters.CreateAlarmTokenPrinter(edAlarmToken, tabAlarmToken);
  AlarmStatementPrinter := GUIPrinters.CreateAlarmStatementPrinter(edAlarmStatement, tabAlarmStatement);
  Self.WindowState  := wsMaximized;
  Settings := TFormatSettings.Default;
  ResultPrinter.Settings := Settings;
  PrinterIntf.SyncNotification := Self.SyncNotification;
  try
    IntoUpdateSettings := true;
    edDeclarationSingleLineParamLimit.Value := Settings.DeclarationSingleLineParamLimit;
    edNamedArgumentSingleLineParamLimit.Value := Settings.NamedArgumentSingleLineParamLimit;
    edPositionalArgumentSingleLineParamLimit.Value := Settings.PositionalArgumentSingleLineParamLimit;
    edMatchParamLimit.Value                 := Settings.MatchParamLimit;
    edPreferredExpressionLength.Value       := Settings.PreferredExpressionLength;
    checkAlignFields.Checked                := Settings.AlignFields;
    checkAlignColumns.Checked               := Settings.AlignColumns;
    checkAlignVariables.Checked             := Settings.AlignVariables;
    checkAlignSpecialComments.Checked       := Settings.AlignSpecialComments;
    checkAlignTableColumnComments.Checked   := Settings.AlignTableColumnComments;
    checkAlignExpressions.Checked           := Settings.AlignExpressions;
    checkReplaceDefault.Checked             := Settings.ReplaceDefault;
    checkReplaceAsIs.Checked                := Settings.ReplaceAsIs;
    checkAddInAccessSpecificator.Checked    := Settings.AddInAccessSpecificator;
    checkIndentBrackets.Checked             := Settings.IndentBrackets;
  finally
    IntoUpdateSettings := false;
  end;
end;

{ При синхронизации обновляется только активная страница, поэтому при смене страницы
  принудительно инициируем синхронизацию }
procedure TFormMain.pgDestChange(Sender: TObject);
begin
  PrevSrcCaret := -1;
end;

{ Реакция на действия, требующие заново напечатать текст }
procedure TFormMain.UpdateRequired(Sender: TObject);
begin
  if not CorrectCRLF then UpdateData;
end;

{ Оповещение принтера о движении пользователя по списку непропечатанных лексем }
procedure TFormMain.edAlarmTokenClick(Sender: TObject);
begin
  AlarmTokenPrinter.ControlChanged;
end;

{ Оповещение принтера о движении пользователя по списку непропечатанных лексем }
procedure TFormMain.edAlarmStatementClick(Sender: TObject);
begin
  AlarmStatementPrinter.ControlChanged;
end;

{ Оповещение принтера о движении пользователя по списку лексем }
procedure TFormMain.edTokenizerClick(Sender: TObject);
begin
  TokenizerPrinter.ControlChanged;
end;

{ Оповещение принтера о движении пользователя по синтаксическому дереву }
procedure TFormMain.treeParserChange(Sender: TObject; Node: TTreeNode);
begin
  SyntaxTreePrinter.ControlChanged;
end;

{ Оповещение о движении пользователя по исходникам либо форматированному выводу }
procedure TFormMain.tmMemoTimer(Sender: TObject);
var
  Line, Col: integer;
  SrcChanged: boolean;
begin
  SrcChanged := (edSrc.SelStart <> PrevSrcCaret);
  if SrcChanged then
  begin
    PrevSrcCaret := edSrc.SelStart;
    CaretToCoords(edSrc, Line, Col, edSrc.SelStart);
    SyncNotification(nil, Line, Col, 1);
  end;
  if edResult.SelStart <> PrevResultCaret then
  begin
    PrevResultCaret := edResult.SelStart;
    if not SrcChanged then ResultPrinter.ControlChanged;
  end;
end;

{ Уничтожение объектов, созданных формой, при завершении приложения }
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TokenizerPrinter);
  FreeAndNil(SyntaxTreePrinter);
  FreeAndNil(ResultPrinter);
  FreeAndNil(AlarmTokenPrinter);
  FreeAndNil(AlarmStatementPrinter);
  FreeAndNil(StatementStream);
  FreeAndNil(Settings);
end;

{ Настройка размеров при ресайзе окна }
procedure TFormMain.FormResize(Sender: TObject);
begin
  panSrc.Width := (Self.ClientWidth - spVert.Width) div 2;
end;

{ Пересчёт координат строка-колонка в положение каретки }
procedure TFormMain.CoordsToCaret(Memo: TMemo; const Line, Col: integer; out Pos: integer);
var i: integer;
begin
  Pos := 0;
  for i := 0 to Line - 2 do Inc(Pos, Memo.Lines[i].Length + 2);
  Inc(Pos, Col - 1);
end;

{ Пересчёт положения каретки в координаты строка-колонка }
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

