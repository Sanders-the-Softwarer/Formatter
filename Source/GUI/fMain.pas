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

{$IFNDEF DEBUG}
  {$MESSAGE Fatal 'DebugTool.exe нужно компилировать в режиме Debug' }
{$ENDIF}

uses
  Tokens { должен идти до StdCtrls, чтобы TLabel не мешала загрузке формы },
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Printer,
  Vcl.ExtCtrls, Vcl.Samples.Spin, Controller, Streams, Statements, Tokenizer,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, cxButtons;

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
    checkAlignCommands: TCheckBox;
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
    tabCompareAutoTestResult: TTabSheet;
    edCompareAutoTestResult: TMemo;
    GroupBox4: TGroupBox;
    checkChangeCommentType: TCheckBox;
    checkUseSpace: TCheckBox;
    GroupBox5: TGroupBox;
    checkRemovePasswords: TCheckBox;
    checkLongOperands: TCheckBox;
    checkShowTransparent: TCheckBox;
    edDebugInfo: TMemo;
    spDebugInfo: TSplitter;
    checkShowDebugInfo: TCheckBox;
    checkAlignRightComments: TCheckBox;
    checkCommentCorrectSpaces: TCheckBox;
    checkAlignFrom: TCheckBox;
    GroupBox6: TGroupBox;
    checkShiftPackageHeader: TCheckBox;
    checkShiftPackageBody: TCheckBox;
    btnSaveToFile: TcxButton;
    OpenDialog: TOpenDialog;
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
    procedure edCompareAutoTestResultChange(Sender: TObject);
    procedure checkShowTransparentClick(Sender: TObject);
    procedure checkShowDebugInfoClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
  private
    TokenizerPrinter, SyntaxTreePrinter, ResultPrinter, AlarmTokenPrinter, AlarmStatementPrinter: TPrinter;
    MinTokenStream, AdvTokenStream: TBufferedStream<TToken>;
    StatementStream: TBufferedStream<TStatement>;
    Settings: TFormatSettings;
    PrevSrcCaret, PrevResultCaret: integer;
    IntoSync, IntoUpdateSettings: boolean;
    function CorrectCRLF: boolean;
    procedure UpdateSettings;
    procedure UpdateData;
    procedure SyncNotification(AObject: TObject; ALine, ACol, ALen: integer);
    procedure CoordsToCaret(Memo: TMemo; const Line, Col: integer; out Pos: integer);
    procedure CaretToCoords(Memo: TMemo; out Line, Col: integer; Pos: integer);
    procedure CheckReformatAutoTestResult;
    procedure UpdateDebugInfo(AToken: TToken); overload;
    procedure UpdateDebugInfo(AStatement: TStatement); overload;
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

{ Обновление настроек из интерфейса }
procedure TFormMain.UpdateSettings;
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
  Settings.AlignCommands                   := checkAlignCommands.Checked;
  Settings.AlignUseSpace                   := checkUseSpace.Checked;
  Settings.AlignRightComments              := checkAlignRightComments.Checked;
  Settings.AlignFrom                       := checkAlignFrom.Checked;
  Settings.ReplaceDefault                  := checkReplaceDefault.Checked;
  Settings.ReplaceAsIs                     := checkReplaceAsIs.Checked;
  Settings.AddInAccessSpecificator         := checkAddInAccessSpecificator.Checked;
  Settings.ChangeCommentType               := checkChangeCommentType.Checked;
  Settings.CorrectCommentSpaces            := checkCommentCorrectSpaces.Checked;
  Settings.RemoveConnectPasswords          := checkRemovePasswords.Checked;
  Settings.BeautifyLongOperands            := checkLongOperands.Checked;
  Settings.ShiftPackageHeader              := checkShiftPackageHeader.Checked;
  Settings.ShiftPackageBody                := checkShiftPackageBody.Checked;
  Settings.PreferredExpressionLength       := edPreferredExpressionLength.Value;
end;

{ Подготовка и распечатка форматированного текста }
procedure TFormMain.UpdateData;
begin
  FreeAndNil(StatementStream);
  { Скопируем настройки }
  if not IntoUpdateSettings then UpdateSettings;
  { Создадим потоки }
  MinTokenStream  := Controller.MakeMinimalTokenStream(edSrc.Text);
  AdvTokenStream  := Controller.MakeAdvancedTokenStream(MinTokenStream);
  StatementStream := Controller.MakeStatementStream(AdvTokenStream, Settings);
  { Напечатаем данные }
  try
    StatementStream.PrintAll(ResultPrinter);
  except
    on E: Exception do Application.ShowException(E);
  end;
  { Даже если разбор завершается ошибкой, выведем сформированные части данных в другие принтеры }
  try
    StatementStream.PrintAll(SyntaxTreePrinter);
  except
  end;
  try
    StatementStream.PrintAll(AlarmStatementPrinter);
  except
  end;
  { Сюда печатаем из MinTokenStream, чтобы увидеть лексемы, выпавшие при печати из синтаксического анализа }
  MinTokenStream.PrintAll(TokenizerPrinter);
  MinTokenStream.PrintAll(AlarmTokenPrinter);
  { Толкнём синхронизацию }
  PrevSrcCaret := -1;
end;

{ Рассылка оповещений для синхронизации движения по различным представлениям текста }
procedure TFormMain.SyncNotification(AObject: TObject; ALine, ACol, ALen: integer);
var
  Caret: integer;
  Token: TToken absolute AObject;
  Statement: TStatement absolute AObject;
begin
  { Заблокируем возможность зацикливания на оповещениях }
  if IntoSync then exit;
  { Обновим панель отладочной информации }
  if AObject is TToken then
    UpdateDebugInfo(Token)
  else if AObject is TStatement then
    UpdateDebugInfo(Statement)
  else
    edDebugInfo.Text := '';
  try
    IntoSync := true;
    { Если указана лексема - сигнал от принтера, синхронизируем исходник }
    if AObject is TToken then
    begin
      CoordsToCaret(edSrc, Token.Line, Token.Col, Caret);
      edSrc.SelStart := Caret;
      edSrc.SelLength := Token.Value.Length;
      PrevSrcCaret := Caret;
    end;
    { Принтер лексем известим всегда - для срабатывания обновления отладочной информации }
    TokenizerPrinter.SyncNotification(AObject, ALine, ACol, ALen);
    { Известим активный принтер, прочие для скорости оставим }
    if pgDest.ActivePage = tabParser then SyntaxTreePrinter.SyncNotification(AObject, ALine, ACol, ALen);
    if pgDest.ActivePage = tabResult then ResultPrinter.SyncNotification(AObject, ALine, ACol, ALen);
    if pgDest.ActivePage = tabAlarmToken then AlarmTokenPrinter.SyncNotification(AObject, ALine, ACol, ALen);
    if pgDest.ActivePage = tabAlarmStatement then AlarmStatementPrinter.SyncNotification(AObject, ALine, ACol, ALen);
  finally
    IntoSync := false;
  end;
end;

{ При создании формы создаются принтеры и настройки, а также настраиваются оповещения }
procedure TFormMain.FormShow(Sender: TObject);
begin
  Self.WindowState  := wsMaximized;
  Settings          := TFormatSettings.Default;
  TokenizerPrinter  := GUIPrinters.CreateTokenizerPrinter(edTokenizer);
  SyntaxTreePrinter := GUIPrinters.CreateSyntaxTreePrinter(treeParser, checkShowTransparent);
  ResultPrinter     := GUIPrinters.CreateFormatterPrinter(Settings, edResult);
  AlarmTokenPrinter := GUIPrinters.CreateAlarmTokenPrinter(edAlarmToken, tabAlarmToken);
  AlarmStatementPrinter := GUIPrinters.CreateAlarmStatementPrinter(edAlarmStatement, tabAlarmStatement);
  Printer.SyncNotification := Self.SyncNotification;
  try
    IntoUpdateSettings := true;
    edDeclarationSingleLineParamLimit.Value := Settings.DeclarationSingleLineParamLimit;
    edNamedArgumentSingleLineParamLimit.Value := Settings.NamedArgumentSingleLineParamLimit;
    edPositionalArgumentSingleLineParamLimit.Value := Settings.PositionalArgumentSingleLineParamLimit;
    edMatchParamLimit.Value              := Settings.MatchParamLimit;
    edPreferredExpressionLength.Value    := Settings.PreferredExpressionLength;
    checkAlignFields.Checked             := Settings.AlignFields;
    checkAlignColumns.Checked            := Settings.AlignColumns;
    checkAlignVariables.Checked          := Settings.AlignVariables;
    checkAlignSpecialComments.Checked    := Settings.AlignSpecialComments;
    checkAlignCommands.Checked           := Settings.AlignCommands;
    checkAlignExpressions.Checked        := Settings.AlignExpressions;
    checkUseSpace.Checked                := Settings.AlignUseSpace;
    checkAlignRightComments.Checked      := Settings.AlignRightComments;
    checkAlignFrom.Checked               := Settings.AlignFrom;
    checkReplaceDefault.Checked          := Settings.ReplaceDefault;
    checkReplaceAsIs.Checked             := Settings.ReplaceAsIs;
    checkAddInAccessSpecificator.Checked := Settings.AddInAccessSpecificator;
    checkChangeCommentType.Checked       := Settings.ChangeCommentType;
    checkCommentCorrectSpaces.Checked    := Settings.CorrectCommentSpaces;
    checkRemovePasswords.Checked         := Settings.RemoveConnectPasswords;
    checkLongOperands.Checked            := Settings.BeautifyLongOperands;
    checkShiftPackageHeader.Checked      := Settings.ShiftPackageHeader;
    checkShiftPackageBody.Checked        := Settings.ShiftPackageBody;
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

procedure TFormMain.edCompareAutoTestResultChange(Sender: TObject);
begin
  CheckReformatAutoTestResult;
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

{ Обновление дерева как реакция на перещёлкивание чекбокса }
procedure TFormMain.checkShowTransparentClick(Sender: TObject);
begin
  UpdateData;
end;

{ Показ панели отладочной информации }
procedure TFormMain.checkShowDebugInfoClick(Sender: TObject);
begin
  edDebugInfo.Visible := checkShowDebugInfo.Checked;
  spDebugInfo.Visible := checkShowDebugInfo.Checked;
end;

{ Вывод отладочной информации по лексеме }
procedure TFormMain.UpdateDebugInfo(AToken: TToken);
begin
  edDebugInfo.Text := StringReplace(AToken.DebugInfo, #13, #13#10, [rfReplaceAll]);
end;

{ Вывод отладочной информации по синтаксической конструкции }
procedure TFormMain.UpdateDebugInfo(AStatement: TStatement);
begin
  edDebugInfo.Text := StringReplace(AStatement.DebugInfo, #13, #13#10, [rfReplaceAll]);
end;

{ Переформатирование результата автотеста, вставленного в поле редактирования }
procedure TFormMain.CheckReformatAutoTestResult;
var
  Expected, Actual, i: integer;
begin
  with TStringList.Create do
  try
    Text := edCompareAutoTestResult.Text;
    { Найдём опорные строки вывода (и среди прочего, спасёмся от зацикливания)}
    Expected := IndexOf('expected: <');
    Actual := IndexOf('> but was: <');
    if (Expected < 0) or (Actual < 0) then exit;
    { Грохнем ненужные строки }
    for i := Expected downto 0 do Delete(0);
    Dec(Actual, Expected + 1);
    repeat
      i := IndexOf('---------->>----------');
      if i >= 0 then
      begin
        Delete(i);
        if Actual > i then Dec(Actual);
      end;
    until i < 0;
    Delete(Count - 1);
    Delete(Actual);
    { И наконец, перемешаем их }
    for i := 0 to Actual - 1 do Move(Actual + i, 2 * i + 1);
    { Результат вернём обратно }
    edCompareAutoTestResult.Text := Text;
  finally
    Free;
  end;
end;

{ Сохранение настроек в файл }
procedure TFormMain.btnSaveToFileClick(Sender: TObject);
var S: TStringList;
begin
  UpdateSettings;
  if not OpenDialog.Execute then exit;
  S := TStringList.Create(dupIgnore, true, false);
  try
    Settings.Save(S);
    S.SaveToFile(OpenDialog.FileName);
  finally
    FreeAndNil(S);
  end;
end;

end.

