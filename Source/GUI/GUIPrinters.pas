////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                Вывод форматированного текста в GUI-интерфейс               //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit GUIPrinters;

interface

uses
  SysUtils, StdCtrls, ExtCtrls, ComCtrls, System.Generics.Collections,
  Printer, Tokens;

{ Функции создания принтеров различных типов }
function CreateTokenizerPrinter(AListBox: TListBox): TPrinter;
function CreateSyntaxTreePrinter(ATreeView: TTreeView; AShowTransparentCheckBox: TCheckBox): TPrinter;
function CreateFormatterPrinter(ASettings: TFormatSettings; AMemo: TMemo): TPrinter;
function CreateAlarmTokenPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
function CreateAlarmStatementPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;

implementation

uses BasePrinter, FormatterPrinter, Statements;

type

  { Принтер для вывода форматированного текста }
  TGUIFormatterPrinter = class(TFineCopyPrinter)
  strict private
    Memo: TMemo;
    IntoSync: boolean;
  public
    constructor Create(ASettings: TFormatSettings; AMemo: TMemo);
    procedure EndPrint; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AObject: TObject; ALine, ACol, ALen: integer); override;
  end;

  { Принтер для вывода последовательности лексем }
  TTokenizerPrinter = class(TBasePrinter)
  private
    ListBox: TListBox;
    Tokens: TDictionary<integer, TToken>;
    LineNumbers: TDictionary<TToken, integer>;
  public
    constructor Create(AListBox: TListBox);
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure Clear; override;
    procedure PrintToken(AToken: TToken); override;
    procedure EndPrint; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AObject: TObject; ALine, ACol, ALen: integer); override;
  end;

  { Принтер для построения синтаксического дерева }
  TSyntaxTreePrinter = class(TBasePrinter)
  private
    TreeView: TTreeView;
    ShowTransparentCheckBox: TCheckBox;
    Parents: TStack<TTreeNode>;
    IntoSync: boolean;
    Tokens: TDictionary<TToken, TTreeNode>;
  public
    constructor Create(ATreeView: TTreeView; AShowTransparentCheckBox: TCheckBox);
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure Clear; override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure EndPrint; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AObject: TObject; ALine, ACol, ALen: integer); override;
  end;

  { Отладочный принтер для вывода пропущенных лексем }
  TAlarmTokenPrinter = class(TTokenizerPrinter)
  private
    TabSheet: TTabSheet;
  protected
    procedure BeginPrint; override;
    procedure PrintToken(AToken: TToken); override;
    function CheckPrintToken(AToken: TToken): boolean; virtual;
  public
    constructor Create(AListBox: TListBox; ATabSheet: TTabSheet);
  end;

  { Отладочный принтер для вывода неожиданных конструкций }
  TAlarmStatementPrinter = class(TAlarmTokenPrinter)
  private
    Unexpected: boolean;
  protected
    procedure PrintStatement(AStatement: TStatement); override;
    function CheckPrintToken(AToken: TToken): boolean; override;
  end;

function CreateFormatterPrinter(ASettings: TFormatSettings; AMemo: TMemo): TPrinter;
begin
  Result := TGUIFormatterPrinter.Create(ASettings, AMemo);
end;

function CreateSyntaxTreePrinter(ATreeView: TTreeView; AShowTransparentCheckBox: TCheckBox): TPrinter;
begin
  Result := TSyntaxTreePrinter.Create(ATreeView, AShowTransparentCheckBox);
end;

function CreateTokenizerPrinter(AListBox: TListBox): TPrinter;
begin
  Result := TTokenizerPrinter.Create(AListBox);
end;

function CreateAlarmTokenPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
begin
  Result := TAlarmTokenPrinter.Create(AListBox, ATabSheet);
end;

function CreateAlarmStatementPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
begin
  Result := TAlarmStatementPrinter.Create(AListBox, ATabSheet);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//               Принтер для печати форматированного текста в GUI             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TGUIFormatterPrinter.Create(ASettings: TFormatSettings; AMemo: TMemo);
begin
  Assert(AMemo <> nil);
  inherited Create(ASettings, false, [], true);
  Memo := AMemo;
  Assert(Assigned(TokenPos) and Assigned(TokenLen));
end;

{ При завершении печати выведем сформированный текст в Memo }
procedure TGUIFormatterPrinter.EndPrint;
begin
  inherited;
  Memo.Text := GetText;
end;

{ При действиях пользователя в Memo найдём текущую лексему и оповестим о ней
  другие элементы интерфейса }
procedure TGUIFormatterPrinter.ControlChanged;
var
  P: integer;
  T: TToken;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    P := Memo.SelStart;
    for T in TokenPos.Keys do
      if (TokenPos[T] <= P) and (TokenPos[T] + TokenLen[T] >= P) then
        SendSyncNotification(T, T.Line, T.Col, Length(T.Value));
  finally
    IntoSync := false;
  end;
end;

{ При оповещении от других элементов интерфейса выделим указанную лексему }
procedure TGUIFormatterPrinter.SyncNotification(AObject: TObject; ALine, ACol, ALen: integer);
var T: TToken;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    { Если лексема не указана - найдём подходящую по позиции }
    if not Assigned(AObject) then
      for T in TokenPos.Keys do
        if (T.Line = ALine) and (T.Col <= ACol) and (T.Col + Length(T.Value) > ACol) then
          AObject := T;
    { Если лексема не передана и не найдена - ничего не делаем }
    if AObject is TToken
      then T := TToken(AObject)
      else exit;
    { И выделим её в тексте }
    if not TokenPos.ContainsKey(T) then exit;
    Memo.SelStart := TokenPos[T] + TokenLen[T];
    Memo.SelLength := -TokenLen[T]; { благодаря выделению в обратную сторону для длинных лексем скроллбар мотается к началу, а не к концу }
  finally
    IntoSync := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              Принтер для печати вывода лексического анализатора            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TTokenizerPrinter.Create(AListBox: TListBox);
begin
  Assert(AListBox <> nil);
  inherited Create;
  ListBox := AListBox;
  Tokens := TDictionary<integer, TToken>.Create;
  LineNumbers := TDictionary<TToken, integer>.Create;
end;

destructor TTokenizerPrinter.Destroy;
begin
  FreeAndNil(Tokens);
  FreeAndNil(LineNumbers);
  inherited;
end;

{ Подготовка к печати }
procedure TTokenizerPrinter.BeginPrint;
begin
  inherited;
  ListBox.Items.BeginUpdate;
end;

{ Инициализация принтера }
procedure TTokenizerPrinter.Clear;
begin
  ListBox.Items.Clear;
  Tokens.Clear;
  LineNumbers.Clear;
end;

{ Вывод очередной лексемы }
procedure TTokenizerPrinter.PrintToken(AToken: TToken);
var
  Str: string;
  i: integer;
begin
  { Выдадим лексему в листбокс }
  with AToken do
  begin
    Str := Format('%s "%s", строка %d, позиция %d', [TokenType, Value, Line, Col]);
    if not Printed then Str := '>>>>> NOT PRINTED >>>>> ' + Str + ' <<<<< NOT PRINTED <<<<<';
  end;
  i := ListBox.Items.Add(Str);
  { Запомним размещение лексемы }
  Tokens.Add(i, AToken);
  LineNumbers.Add(AToken, i);
end;

{ Завершение печати }
procedure TTokenizerPrinter.EndPrint;
begin
  ListBox.Items.EndUpdate;
  inherited;
end;

{ Реакция на изменения в привязанном листбоксе }
procedure TTokenizerPrinter.ControlChanged;
begin
  if ListBox.ItemIndex < 0 then exit;
  SendSyncNotification(Tokens[ListBox.ItemIndex], 0, 0, 0);
end;

{ Реакция на оповещения от других элементов интерфейса }
procedure TTokenizerPrinter.SyncNotification(AObject: TObject; ALine, ACol, ALen: integer);
var T: TToken;
begin
  { Если не указана лексема, найдём подходящую по позиции в тексте }
  if not Assigned(AObject) then
    for T in LineNumbers.Keys do
      if (T.Line = ALine) and (T.Col <= ACol) and (T.Col + T.Value.Length > ACol) then
        AObject := T;
  { Если лексема не передана и не найдена - делать нечего }
  if AObject is TToken
    then T := TToken(AObject)
    else exit;
  { Сфокусируемся на неё }
  if LineNumbers.ContainsKey(T) then ListBox.ItemIndex := LineNumbers[T];
  { Для того, чтобы отладочная панель корректно обновилась при движении по
    исходнику, пошлём отсюда сигнал }
  ControlChanged;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                   Принтер для выдачи синтаксического дерева                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TSyntaxTreePrinter.Create(ATreeView: TTreeView; AShowTransparentCheckBox: TCheckBox);
begin
  TreeView := ATreeView;
  ShowTransparentCheckBox := AShowTransparentCheckBox;
  inherited Create;
  Tokens := TDictionary<TToken, TTreeNode>.Create;
  Parents := TStack<TTreeNode>.Create;
end;

destructor TSyntaxTreePrinter.Destroy;
begin
  Clear;
  FreeAndNil(Tokens);
  FreeAndNil(Parents);
  inherited;
end;

{ Инициализация и подготовка к печати }
procedure TSyntaxTreePrinter.BeginPrint;
begin
  inherited;
  TreeView.Items.BeginUpdate;
end;

procedure TSyntaxTreePrinter.Clear;
begin
  TreeView.Items.Clear;
  Tokens.Clear;
  Parents.Clear;
  Parents.Push(nil);
end;

{ Завершение печати и вывод результата }
procedure TSyntaxTreePrinter.EndPrint;
begin
  TreeView.Items.EndUpdate;
  inherited;
end;

{ Печать выражения - делаем каждый объект TStatement узлом дерева и привязываем к нему дочерние }
procedure TSyntaxTreePrinter.PrintStatement(AStatement: TStatement);
var Transparent: boolean;
begin
  Transparent := AStatement.Transparent and (not Assigned(ShowTransparentCheckBox) or not ShowTransparentCheckBox.Checked);
  if not Transparent then Parents.Push(TreeView.Items.AddChildObject(Parents.Peek, '< ' + Trim(AStatement.Name) + ' >', AStatement));
  try
    inherited;
  finally
    if not Transparent then Parents.Pop;
  end;
end;

{ Печать лексемы - делаем каждый объект TToken листом дерева }
procedure TSyntaxTreePrinter.PrintToken(AToken: TToken);
begin
  Tokens.Add(AToken, TreeView.Items.AddChildObject(Parents.Peek, Format('%s [ %s ]', [AToken.TokenType, AToken.Value]), AToken));
end;

{ Реакция на действия пользователя в дереве }
procedure TSyntaxTreePrinter.ControlChanged;
var Ptr: pointer;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    Ptr := TreeView.Selected.Data;
    if Assigned(Ptr) then SendSyncNotification(TObject(Ptr), 0, 0, 0);
  finally
    IntoSync := false;
  end;
end;

{ Обработка оповещений от других элементов интерфейса }
procedure TSyntaxTreePrinter.SyncNotification(AObject: TObject; ALine, ACol, ALen: integer);
var T: TToken;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    { Найдём подходящую лексему и сфокусируемся на соответствующем ей узле дерева }
    for T in Tokens.Keys do
    begin
      if (T = AObject) or (T.Line = ALine) and (T.Col <= ACol) and (T.Col + T.Value.Length > ACol) then
      try
        TreeView.Items.BeginUpdate;
        TreeView.FullCollapse;
        TreeView.Selected := Tokens[T];
        exit;
      finally
        TreeView.Items.EndUpdate;
      end;
    end;
  finally
    IntoSync := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                Принтеры для вывода отладочных предупреждений               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TAlarmTokenPrinter.Create(AListBox: TListBox; ATabSheet: TTabSheet);
begin
  TabSheet := ATabSheet;
  inherited Create(AListBox);
  TabSheet.TabVisible := false;
end;

procedure TAlarmTokenPrinter.BeginPrint;
begin
  TabSheet.TabVisible := false;
  inherited;
end;

procedure TAlarmTokenPrinter.PrintToken(AToken: TToken);
begin
  if CheckPrintToken(AToken) then
  begin
    inherited;
    TabSheet.TabVisible := true;
  end;
end;

function TAlarmTokenPrinter.CheckPrintToken(AToken: TToken): boolean;
begin
  Result := not AToken.Printed;
end;

procedure TAlarmStatementPrinter.PrintStatement(AStatement: TStatement);
begin
  Unexpected := (AStatement is TUnexpectedToken);
  inherited;
  Unexpected := false;
end;

function TAlarmStatementPrinter.CheckPrintToken(AToken: TToken): boolean;
begin
  Result := Unexpected;
end;

end.

