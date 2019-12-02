////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Вывод информации в разных видах                      //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Printers_;

{ ----- Примечания -------------------------------------------------------------

  Для выдачи результатов разбора сформированные объекты печатают себя в
  предложенный им "принтер". Помимо основного принтера, выводящего
  отформатированный текст, приложение поддерживает дополнительные для
  выдачи отладочной информации - соль решения в том, что это оказывается
  доступным бесплатно, без нагрузки классов анализатора дополнительными
  методами. Получившееся решение настолько универсально, что принтер лексем,
  например, одинаково хорошо работает, имея на входе как поток лексического,
  так и поток синтаксического анализатора.

  Класс TPrinter - это, по сути, интерфейс вывода на принтер. Пустая реализация
  спрятана в классе TBasePrinter; их не следует объединять, поскольку тогда
  получится циклическая ссылка между модулями Printers, Tokens и Statements.

  В ходе работы оказалось, что после правильного синтаксического разбора
  конструкции я нередко забываю напечатать какую-нибудь мелкую лексему и она
  пропадает. Это очень неприятная тенденция, так как она не только выдаёт
  некомпилируемый код, но и может погубить работу пользователя, грохнув
  что-то существенное в том, что он форматировал. Поэтому среди инструментов
  предусмотрен специальный принтер, задача которого - отлавливать пропущенные
  лексемы и бить о них в барабан. Также для индикации проблем предназначен
  принтер, выдающий только места с нераспознанными синтаксическими
  конструкциями.

  В приложении широко используется возможность выравнивания соседних однотипных
  конструкций по ширине. Для этого в принтере используется специальный режим
  примерки, когда реальный вывод отключён, но счётчики текущего положения
  модифицируются, позволяя оценить, как будет напечатан последующий текст
  и рассчитать необходимое для выравнивания блока количество пробелов.

------------------------------------------------------------------------------ }

interface

uses
  Classes, SysUtils, Math, Vcl.StdCtrls, Vcl.ComCtrls, System.Generics.Collections,
  Tokens, Utils;

type

  { Настройки вывода }
  TFormatSettings = class
  public
    DeclarationSingleLineParamLimit: integer;
    ArgumentSingleLineParamLimit: integer;
    MatchParamLimit: integer;
    AlignVariables: boolean;
    AlignFields: boolean;
    AlignExpressions: boolean;
    AlignTableColumnComments: boolean;
    AlignSpecialComments: boolean;
    ReplaceDefault: boolean;
    ReplaceAsIs: boolean;
    PreferredExpressionLength: integer;
  public
    constructor Default;
  end;

  { Интерфейс вывода на принтер }
  TPrinter = class
  private
    FSettings: TFormatSettings;
    function HasItems(AItems: array of TObject): boolean;
  public
    procedure BeginPrint; virtual; abstract;
    procedure PrintItem(AItem: TObject); virtual; abstract;
    procedure EndPrint; virtual; abstract;
    procedure Indent; virtual; abstract;
    procedure Undent; virtual; abstract;
    procedure NextLine; virtual; abstract;
    procedure SupressNextLine(ASupress: boolean); virtual; abstract;
    procedure SupressSpaces(ASupress: boolean); virtual; abstract;
    procedure PrintSpecialComment(AValue: string); virtual; abstract;
    procedure StartRuler(Enabled: boolean); virtual; abstract;
    procedure Ruler(const ARuler: string); virtual; abstract;
    function  MakeDraftPrinter: TPrinter; virtual; abstract;
    function  CurrentCol: integer; virtual; abstract;
    procedure ControlChanged; virtual; abstract;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); virtual; abstract;
    function  GetText: string; virtual; abstract;
  public
    procedure PrintItems(AItems: array of TObject);
    procedure PrintRulerItem(const ARuler: string; AItem: TObject); virtual;
    procedure PrintRulerItems(const ARuler: string; AItems: array of TObject); virtual;
    procedure PrintIndented(AItem: TObject); overload;
    procedure PrintIndented(AItems: array of TObject); overload;
    function  NextLineIf(AItem: TObject): boolean; overload;
    function  NextLineIf(AItems: array of TObject): boolean; overload;
  public
    property Settings: TFormatSettings read FSettings write FSettings;
  public
    class function CreateTokenizerPrinter(AListBox: TListBox): TPrinter;
    class function CreateSyntaxTreePrinter(ATreeView: TTreeView): TPrinter;
    class function CreateFormatterPrinter(AMemo: TMemo): TPrinter;
    class function CreateAlarmTokenPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
    class function CreateAlarmStatementPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
  end;

  { Тип извещения о необходимости синхронизации интерфейса }
  TSyncNotification = procedure (AToken: TToken; ALine, ACol, ALen: integer) of object;

var
  { Извещение о необходимости синхронизации интерфейса }
  SyncNotification: TSyncNotification;

{ Отправка извещения о необходимости синхронизации интерфейса }
procedure SendSyncNotification(AToken: TToken; ALine, ACol, ALen: integer);

{ Конструкции, которые можно применять в TFormatterPrinter.PrintItems для форматирования }
function _NextLine: TObject;
function _Indent: TObject;
function _Undent: TObject;
function _IndentNextLine: TObject;
function _UndentNextLine: TObject;

implementation

uses Statements, Attributes, SQLPlus;

type
  { Тип лексемы для вывода специальных комментариев }
  TSpecialComment = class(TComment);

{ Отправка извещения о необходимости синхронизации интерфейса }
procedure SendSyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
begin
  if Assigned(SyncNotification) then
    SyncNotification(AToken, ALine, ACol, ALen);
end;

type

  { Базовая (пустая) реализация принтера }
  TBasePrinter = class(TPrinter)
  public
    procedure BeginPrint; override;
    procedure PrintItem(AItem: TObject); override;
    procedure PrintToken(AToken: TToken); virtual;
    procedure PrintStatement(AStatement: TStatement); virtual;
    procedure EndPrint; override;
    procedure Indent; override;
    procedure Undent; override;
    procedure NextLine; override;
    procedure SupressNextLine(ASupress: boolean); override;
    procedure SupressSpaces(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure StartRuler(Enabled: boolean); override;
    procedure Ruler(const ARuler: string); override;
    function  MakeDraftPrinter: TPrinter; override;
    function  CurrentCol: integer; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
    function  GetText: string; override;
  end;

  { Информация о выравниваниях }
  TRulers = class
  private
    Names: TStringList;
    MaxWidths: TDictionary<String, integer>;
    PrevLine, PrevCol, Shift: integer;
    DisablePadding: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure NewLine(ALine, ACol: integer);
    procedure Take(const ARuler: string; ALine, ACol: integer);
    function Fix(const ARuler: string; ACol: integer): integer;
  end;

  { Режим принтера }
  TFormatterPrinterMode = (fpmNormal, fpmGetRulers, fpmSetRulers);

  { Принтер для вывода форматированного текста }
  TFormatterPrinter = class(TBasePrinter)
  strict private
    Memo:    TMemo;
    Builder: TStringBuilder;
    Mode:    TFormatterPrinterMode;
    Shift:   integer;
    BOL:     boolean;
    EmptyLine: boolean;
    Line:    integer;
    Col:     integer;
    PrevCol: integer;
    Rulers:  TRulers;
    Padding: integer;
    PrevStatement: TStatement;
    PrevToken: TToken;
    IntoSync: boolean;
    TokenPos, TokenLen: TDictionary<TToken, integer>;
    SpecialComments: TObjectList<TToken>;
    SupSpace, SupNextLine: integer;
    IsDraft: boolean;
    Text:    string;
    RulerEnabled: boolean;
  strict protected
    function SpaceRequired(ALeft, ARight: TToken): boolean;
    function EmptyLineRequired(APrev, ANext: TStatement): boolean;
  public
    constructor Create(AMemo: TMemo);
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure EndPrint; override;
    procedure PrintItem(AItem: TObject); override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure PrintRulerItem(const ARuler: string; AItem: TObject); override;
    procedure PrintRulerItems(const ARuler: string; AItems: array of TObject); override;
    procedure Indent; override;
    procedure Undent; override;
    procedure NextLine; override;
    procedure SupressNextLine(ASupress: boolean); override;
    procedure SupressSpaces(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure StartRuler(Enabled: boolean); override;
    procedure Ruler(const ARuler: string); override;
    function MakeDraftPrinter: TPrinter; override;
    function CurrentCol: integer; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
    function  GetText: string; override;
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
    procedure PrintToken(AToken: TToken); override;
    procedure EndPrint; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
  end;

  { Принтер для построения синтаксического дерева }
  TSyntaxTreePrinter = class(TBasePrinter)
  private
    TreeView: TTreeView;
    Parents: TStack<TTreeNode>;
    IntoSync: boolean;
    Tokens: TDictionary<TToken, TTreeNode>;
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure EndPrint; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//          Спецконструкции для более удобного форматирования вывода          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  TFormatterCmd = class
  public
    procedure PrintSelf(APrinter: TPrinter); virtual; abstract;
  end;

  TNextLine = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TIndent = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TUndent = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TIndentNextLine = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TUndentNextLine = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

function _NextLine: TObject;
begin
  Result := TNextLine.Create;
end;

function _Indent: TObject;
begin
  Result := TIndent.Create;
end;

function _Undent: TObject;
begin
  Result := TUndent.Create;
end;

function _IndentNextLine: TObject;
begin
  Result := TIndentNextLine.Create;
end;

function _UndentNextLine: TObject;
begin
  Result := TUndentNextLine.Create;
end;

procedure TNextLine.PrintSelf(APrinter: TPrinter);
begin
  APrinter.NextLine;
end;

procedure TIndent.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Indent;
end;

procedure TUndent.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Undent;
end;

procedure TIndentNextLine.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Indent;
  APrinter.NextLine;
end;

procedure TUndentNextLine.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Undent;
  APrinter.NextLine;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//      Интерфейс вывода на принтер и фабрики создания реальных принтеров     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class function TPrinter.CreateFormatterPrinter(AMemo: TMemo): TPrinter;
begin
  Result := TFormatterPrinter.Create(AMemo);
end;

class function TPrinter.CreateSyntaxTreePrinter(ATreeView: TTreeView): TPrinter;
begin
  Result := TSyntaxTreePrinter.Create(ATreeView);
end;

class function TPrinter.CreateTokenizerPrinter(AListBox: TListBox): TPrinter;
begin
  Result := TTokenizerPrinter.Create(AListBox);
end;

class function TPrinter.CreateAlarmTokenPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
begin
  Result := TAlarmTokenPrinter.Create(AListBox, ATabSheet);
end;

class function TPrinter.CreateAlarmStatementPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
begin
  Result := TAlarmStatementPrinter.Create(AListBox, ATabSheet);
end;

procedure TPrinter.PrintItems(AItems: array of TObject);
var i: integer;
begin
  for i := Low(AItems) to High(AItems) do PrintItem(AItems[i]);
end;

procedure TPrinter.PrintRulerItem(const ARuler: string; AItem: TObject);
begin
  Ruler(ARuler);
  PrintItem(AItem);
end;

procedure TPrinter.PrintRulerItems(const ARuler: string; AItems: array of TObject);
begin
  Ruler(ARuler);
  PrintItems(AItems);
end;

procedure TPrinter.PrintIndented(AItem: TObject);
begin
  if not Assigned(AItem) then exit;
  NextLine;
  Indent;
  PrintItem(AItem);
  Undent;
end;

procedure TPrinter.PrintIndented(AItems: array of TObject);
begin
  if not HasItems(AItems) then exit;
  NextLine;
  Indent;
  PrintItems(AItems);
  Undent;
end;

function TPrinter.NextLineIf(AItem: TObject): boolean;
begin
  Result := NextLineIf([AItem]);
end;

function TPrinter.NextLineIf(AItems: array of TObject): boolean;
begin
  Result := HasItems(AItems);
  if not Result then exit;
  NextLine;
  PrintItems(AItems);
end;

function TPrinter.HasItems(AItems: array of TObject): boolean;
var i: integer;
begin
  for i := Low(AItems) to High(AItems) do
    if Assigned(AItems[i]) and not (AItems[i] is TFormatterCmd) then exit(true);
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                   Дефолтовая (пустая) реализация принтера                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TBasePrinter.BeginPrint;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.EndPrint;
begin
  { ничего не делаем }
end;

{ Разбиваем PrintItem на PrintToken и PrintStatement }
procedure TBasePrinter.PrintItem(AItem: TObject);
begin
  if AItem is TToken then
    PrintToken(AItem as TToken)
  else if AItem is TStatement then
    PrintStatement(AItem as TStatement);
end;

procedure TBasePrinter.PrintToken(AToken: TToken);
begin
  { ничего не делаем }
end;

procedure TBasePrinter.PrintSpecialComment(AValue: string);
begin
  { ничего не делаем }
end;

procedure TBasePrinter.StartRuler;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.Ruler(const ARuler: string);
begin
  { ничего не делаем }
end;

function TBasePrinter.MakeDraftPrinter: TPrinter;
begin
  Result := TBasePrinter.Create;
end;

function TBasePrinter.CurrentCol: integer;
begin
  Result := -1;
end;

procedure TBasePrinter.ControlChanged;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
begin
  { ничего не делаем }
end;

procedure TBasePrinter.PrintStatement(AStatement: TStatement);
begin
  AStatement.PrintSelf(Self);
end;

procedure TBasePrinter.Indent;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.Undent;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.NextLine;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.SupressNextLine;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.SupressSpaces(ASupress: boolean);
begin
  { ничего не делаем }
end;

function TBasePrinter.GetText: string;
begin
  Result := '';
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                  Принтер для печати форматированного текста                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TFormatterPrinter.Create(AMemo: TMemo);
begin
  Memo     := AMemo;
  inherited Create;
  TokenPos := TDictionary<TToken, integer>.Create;
  TokenLen := TDictionary<TToken, integer>.Create;
  SpecialComments := TObjectList<TToken>.Create(true);
end;

destructor TFormatterPrinter.Destroy;
begin
  FreeAndNil(TokenPos);
  FreeAndNil(TokenLen);
  FreeAndNil(SpecialComments);
  inherited;
end;

{ Инициализация перед началом печати }
procedure TFormatterPrinter.BeginPrint;
begin
  inherited;
  Builder := TStringBuilder.Create;
  Shift   := 0;
  Line    := 1;
  Col     := 1;
  BOL     := false;
  Padding := 0;
  TokenPos.Clear;
  TokenLen.Clear;
  SpecialComments.Clear;
end;

{ Вывод готового результата }
procedure TFormatterPrinter.EndPrint;
begin
  if Assigned(Builder) then Text := Builder.ToString else Text := '';
  FreeAndNil(Builder);
  if Assigned(Memo) then Memo.Text := Text;
  PrevToken := nil;
  PrevStatement := nil;
  inherited;
end;

{ Универсальный метод вывода на принтер поддерживаемых объектов }
procedure TFormatterPrinter.PrintItem(AItem: TObject);
begin
  if AItem is TFormatterCmd then
    begin
      TFormatterCmd(AItem).PrintSelf(Self);
      FreeAndNil(AItem);
    end
  else
    inherited;
end;

{ Проверка, нужен ли пробел между двумя лексемами }
function TFormatterPrinter.SpaceRequired(ALeft, ARight: TToken): boolean;
begin
  Result := false;
  { Если пробелы запрещены - не ставим их }
  if SupSpace > 0 then exit;
  { Точку с запятой прижимаем справа ко всему }
  if ARight.Value = ';' then exit;
  { Точку прижимаем с обеих сторон ко всему }
  if (ALeft.Value = '.') or (ARight.Value = '.') then exit;
  { Двоеточие/амперсанд прижимаем к следующему за ним идентификатору }
  if ((ALeft.Value = ':') or (ALeft.Value = '&')) and (ARight is TEpithet) then exit;
  { Запятую прижимаем справа ко всему }
  if ARight.Value = ',' then exit;
  { В конструкции number(5,2) запятую прижимаем и слева тоже }
  if (ALeft.Value = ',') and TTerminal(ALeft).IntoNumber then exit;
  { Открывающую скобку прижимаем справа к идентификаторам и ключевым словам char, table, row, lob, key, unique }
  if (ARight.Value = '(') and
     (ALeft is TEpithet) and
     (SameText(ALeft.Value, 'char') or
      SameText(ALeft.Value, 'table') or
      SameText(ALeft.Value, 'row') or
      SameText(ALeft.Value, 'lob') or
      SameText(ALeft.Value, 'key') or
      SameText(ALeft.Value, 'unique') or
      not TEpithet(ALeft).IsKeyword) then exit;
  { К открывающей скобке прижимаем справа всё }
  if ALeft.Value = '(' then exit;
  { Открывающие/закрывающие скобки всегда прижимаем друг к другу }
  if ((ALeft.Value = '(') or (ALeft.Value = ')')) and ((ARight.Value = '(') or (ARight.Value = ')')) then exit;
  { Закрывающую скобку прижимаем справа ко всему }
  if ARight.Value = ')' then exit;
  { Суффиксы %type и подобные прижимаем справа ко всему }
  if ARight.Value.StartsWith('%') then exit;
  { Унарные операции прижимаем слева к следующему за ними }
  if (ALeft is TTerminal) and (TTerminal(ALeft).OpType = otUnary) then exit;
  { Если правила не сработали, ставим  пробел }
  Result := true;
end;

{ Проверка, нужна ли пустая строка между двумя конструкциями }
function TFormatterPrinter.EmptyLineRequired(APrev, ANext: TStatement): boolean;
begin
  { Команды set, @ лепим вместе }
  if (APrev.ClassType = ANext.ClassType) and ((APrev.ClassType = TSet) or (APrev.ClassType = TAt)) then exit(false);
  { Вставим пустую строку перед новой конструкцией верхнего уровня }
  if not Assigned(ANext.Parent) then exit(true);
  { Условия не выполнены }
  Result := false;
end;

{ Вывод очередной лексемы с навешиванием всех наворотов по форматированию }
procedure TFormatterPrinter.PrintToken(AToken: TToken);
var
  Value: string;
  AllowLF, StartOfText: boolean;
begin
  AllowLF := (SupNextLine = 0);
  StartOfText := (Line = 1) and (Col = 1);
  { Обработаем вставку пустой строки }
  if EmptyLine and not StartOfText then
  begin
    if Assigned(Builder) and AllowLF then Builder.AppendLine;
    Inc(Line);
    BOL := true;
    EmptyLine := false;
  end;
  { Обработаем переход на новую строку }
  if BOL then
  begin
    if Assigned(Builder) and AllowLF and not StartOfText then Builder.AppendLine.Append(StringOfChar(' ', Shift));
    BOL := false;
    if AllowLF and not StartOfText then
    begin
      Inc(Line);
      Col := Shift + 1;
      PrevCol := 1;
      PrevToken := nil;
    end;
  end;
  { Если нужно, внедрим пробел между предыдущей и новой лексемами }
  if Assigned(PrevToken) and Assigned(AToken) and SpaceRequired(PrevToken, AToken) then
    begin
      if Assigned(Builder) then Builder.Append(' ');
      Inc(Col);
    end
  else if Padding > 0 then
    Inc(Padding);
  { Если задано выравнивание, вставим соответствующее количество пробелов }
  if (Padding > 0) and Assigned(AToken) then
  begin
    if Assigned(Builder) then Builder.Append(StringOfChar(' ', Padding));
    Inc(Col, Padding);
    Inc(PrevCol, Padding);
    Padding := 0;
  end;
  { И, наконец, если задана лексема - напечатаем её }
  if Assigned(AToken) then
  begin
    Value := AToken.Value;
    { Учтём настройки замены лексем на синонимы и вывод в нижнем регистре }
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and
       SameText(AToken.Value, 'default') and Settings.ReplaceDefault and AToken.CanReplace
      then Value := ':=';
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and
       SameText(AToken.Value, 'as') and Settings.ReplaceAsIs and AToken.CanReplace
      then Value := 'is';
    if Attributes.HasAttribute(AToken.ClassType, LowerCaseAttribute)
      then Value := Value.ToLower;
    { В режиме реальной печати - напечатаем лексему, иначе только учтём сдвиг позиции }
    if Assigned(Builder) then
    begin
      if not IsDraft and not (AToken is TSpecialComment) then
      begin
        TokenPos.Add(AToken, Builder.Length);
        TokenLen.Add(AToken, Value.Length);
      end;
      Builder.Append(Value);
      AToken.Printed := true;
    end;
    Inc(Col, Value.Length);
    PrevToken := AToken;
  end;
end;

{ Вывод синтаксической конструкции с расстановкой выравниваний }
procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);

  var
    _Mode: TFormatterPrinterMode;
    _Shift, _Col, _Line, _Padding: integer;
    _BOL, _EmptyLine: boolean;
    _Builder: TStringBuilder;
    _Rulers: TRulers;
    _PrevToken: TToken;
    _PrevStatement: TStatement;

  { Печать выражения без наворотов, связанных с выравниваниями }
  procedure SimplePrintStatement(AStatement: TStatement);
  var _Shift: integer;
  begin
    EmptyLine := EmptyLine or (Assigned(PrevStatement) and Assigned(AStatement) and EmptyLineRequired(PrevStatement, AStatement));
    PrevStatement := AStatement;
    _Shift := Shift;
    inherited PrintStatement(AStatement);
    if Shift <> _Shift then
      raise Exception.CreateFmt('Invalid indents into %s - was %d but %d now', [AStatement.ClassName, _Shift, Shift]);
  end;

  { Сохранение конфигурации для возврата к ней после примерки выравнивания }
  procedure SaveCfg;
  begin
    _Shift   := Shift;
    _Col     := Col;
    _Line    := Line;
    _Padding := Padding;
    _BOL     := BOL;
    _EmptyLine := EmptyLine;
    _Builder := Builder;
    _Rulers  := Rulers;
    _PrevToken := PrevToken;
    _PrevStatement := PrevStatement;
    _Mode    := Mode;
  end;

  { Восстановление той части конфигурации, которая нужна для печати с выравниваниями }
  procedure RestoreCfgBeforePrint;
  begin
    Shift := _Shift;
    Col   := _Col;
    Line  := _Line;
    Padding := _Padding;
    BOL   := _BOL;
    EmptyLine := _EmptyLine;
    Builder := _Builder;
    PrevToken := _PrevToken;
    PrevStatement := _PrevStatement;
  end;

  { Окончательное восстановление конфигурации после печати с выравниваниями }
  procedure RestoreCfgAfterPrint;
  begin
    FreeAndNil(Rulers);
    Rulers := _Rulers;
    Mode := _Mode;
  end;

  { Пробная печать - сбор информации о выравниваниях }
  procedure PrintGetRulersMode;
  begin
    Builder := nil;
    Rulers  := TRulers.Create;
    Mode    := fpmGetRulers;
    PrevCol := Col;
    try
      SimplePrintStatement(AStatement);
    except
      { проглотим ошибку и дадим аналогично упасть на SetRulers, чтобы выдать проблемный текст на принтер }
    end;
  end;

  { Печать с расстановкой выравниваний }
  procedure PrintSetRulersMode;
  begin
    Mode  := fpmSetRulers;
    PrevCol := Col;
    SimplePrintStatement(AStatement);
  end;

begin
  if AStatement.Aligned then
    begin
      SaveCfg;
      PrintGetRulersMode;
      RestoreCfgBeforePrint;
      PrintSetRulersMode;
      RestoreCfgAfterPrint;
    end
  else
    SimplePrintStatement(AStatement);
end;

procedure TFormatterPrinter.PrintRulerItem(const ARuler: string; AItem: TObject);
begin
  inherited;
  Padding := 0;
end;

procedure TFormatterPrinter.PrintRulerItems(const ARuler: string; AItems: array of TObject);
begin
  inherited;
  Padding := 0;
end;

{ Управление отступами }

procedure TFormatterPrinter.Indent;
begin
  Inc(Shift, 4);
end;

procedure TFormatterPrinter.Undent;
begin
  if Shift >= 4 then Dec(Shift, 4);
end;

{ Переход на следующую строку }
procedure TFormatterPrinter.NextLine;
begin
  BOL := true;
end;

{ Установка режима подавления переводов строки }
procedure TFormatterPrinter.SupressNextLine(ASupress: boolean);
begin
  PrintToken(nil);
  if ASupress
    then Inc(SupNextLine)
    else Dec(SupNextLine);
  PrintToken(nil);
end;

{ Установка режима подавления пробелов }
procedure TFormatterPrinter.SupressSpaces(ASupress: boolean);
begin
  if ASupress
    then Inc(SupSpace)
    else Dec(SupSpace);
end;

{ Вывод на принтер специального комментария (отсутствующего в исходном тексте)}
procedure TFormatterPrinter.PrintSpecialComment(AValue: string);

  procedure PrintSpecialToken(const Text: string);
  var T: TToken;
  begin
    T := TSpecialComment.Create(Text, -1, -1);
    SpecialComments.Add(T);
    PrintToken(T);
  end;

begin
  if not RulerEnabled then StartRuler(Settings.AlignSpecialComments);
  if Settings.AlignSpecialComments then Self.PrintRulerItem('special-comment-start', nil);
  PrintSpecialToken('/*/');
  PrintSpecialToken(AValue);
  if Settings.AlignSpecialComments then Self.PrintRulerItem('special-comment-finish', nil);
  PrintSpecialToken('/*/');
end;

{ Начало выравнивания в очередной строке }
procedure TFormatterPrinter.StartRuler(Enabled: boolean);
begin
  RulerEnabled := Enabled;
  if (Mode <> fpmGetRulers) or not Enabled then exit;
  PrintToken(nil);
  Rulers.NewLine(Line, Col);
end;

{ Установка "линейки" для выравнивания }
procedure TFormatterPrinter.Ruler(const ARuler: string);
begin
  if not RulerEnabled then exit;
  PrintToken(nil);
  case Mode of
    fpmGetRulers: Rulers.Take(ARuler, Line, Col); { В режиме примерки запомним ширину фрагмента }
    fpmSetRulers: Padding := Rulers.Fix(ARuler, Col); { В режиме разметки рассчитаем необходимое количество пробелов }
  end;
end;

{ Создание принтера для пробной печати }
function TFormatterPrinter.MakeDraftPrinter: TPrinter;
begin
  Result := TFormatterPrinter.Create(nil);
  Result.Settings := Self.Settings;
  TFormatterPrinter(Result).IsDraft := true;
end;

{ Возврат текущего положения каретки }
function TFormatterPrinter.CurrentCol: integer;
begin
  Result := Col;
end;

{ Реакция на действия пользователя в привязанном к принтере Memo }
procedure TFormatterPrinter.ControlChanged;
var
  P: integer;
  T: TToken;
begin
  if IntoSync or not Assigned(Memo) then exit;
  try
    IntoSync := true;
    { Найдём лексему под курсором и пошлём другим оповещение о синхронизации }
    P := Memo.SelStart;
    for T in TokenPos.Keys do
      if (TokenPos[T] <= P) and (TokenPos[T] + TokenLen[T] >= P) then
        SendSyncNotification(T, T.Line, T.Col, T.Value.Length);
  finally
    IntoSync := false;
  end;
end;

{ Обработка оповещения о синхронизации от других элементов интерфейса }
procedure TFormatterPrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
var T: TToken;
begin
  if IntoSync or not Assigned(Memo) then exit;
  try
    IntoSync := true;
    { Если лексема не указана - найдём подходящую по позиции }
    if not Assigned(AToken) then
      for T in TokenPos.Keys do
        if (T.Line = ALine) and (T.Col <= ACol) and (T.Col + T.Value.Length > ACol) then
          AToken := T;
    { И выделим её в тексте }
    if not TokenPos.ContainsKey(AToken) then exit;
    Memo.SelStart := TokenPos[AToken];
    Memo.SelLength := TokenLen[AToken];
  finally
    IntoSync := false;
  end;
end;

function TFormatterPrinter.GetText: string;
begin
  if Assigned(Builder)
    then Result := Builder.ToString
    else Result := Text;
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

{ Инициализация и подготовка к печати }
procedure TTokenizerPrinter.BeginPrint;
begin
  inherited;
  ListBox.Items.BeginUpdate;
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
  SendSyncNotification(Tokens[ListBox.ItemIndex], 0, 0, 0);
end;

{ Реакция на оповещения от других элементов интерфейса }
procedure TTokenizerPrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
var T: TToken;
begin
  { Если не указана лексема, найдём подходящую по позиции в тексте }
  if not Assigned(AToken) then
    for T in LineNumbers.Keys do
      if (T.Line = ALine) and (T.Col <= ACol) and (T.Col + T.Value.Length > ACol) then
        AToken := T;
  { И сфокусируемся на неё }
  if Assigned(AToken) and LineNumbers.ContainsKey(AToken) then
    ListBox.ItemIndex := LineNumbers[AToken];
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                   Принтер для выдачи синтаксического дерева                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TSyntaxTreePrinter.Create(ATreeView: TTreeView);
begin
  TreeView := ATreeView;
  inherited Create;
  Tokens := TDictionary<TToken, TTreeNode>.Create;
end;

destructor TSyntaxTreePrinter.Destroy;
begin
  FreeAndNil(Tokens);
  inherited;
end;

{ Инициализация и подготовка к печати }
procedure TSyntaxTreePrinter.BeginPrint;
begin
  inherited;
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  Tokens.Clear;
  Parents := TStack<TTreeNode>.Create;
  Parents.Push(nil);
end;

{ Завершение печати и вывод результата }
procedure TSyntaxTreePrinter.EndPrint;
begin
  FreeAndNil(Parents);
  TreeView.Items.EndUpdate;
  inherited;
end;

{ Печать выражения - делаем каждый объект TStatement узлом дерева и привязываем к нему дочерние }
procedure TSyntaxTreePrinter.PrintStatement(AStatement: TStatement);
begin
  Parents.Push(TreeView.Items.AddChild(Parents.Peek, '< ' + Trim(AStatement.Name) + ' >'));
  try
    inherited;
  finally
    Parents.Pop;
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
    if Assigned(Ptr) then SendSyncNotification(TToken(Ptr), 0, 0, 0);
  finally
    IntoSync := false;
  end;
end;

{ Обработка оповещений от других элементов интерфейса }
procedure TSyntaxTreePrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
var T: TToken;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    { Найдём подходящую лексему и сфокусируемся на соответствующем ей узле дерева }
    for T in Tokens.Keys do
    begin
      if (T = AToken) or (T.Line = ALine) and (T.Col <= ACol) and (T.Col + T.Value.Length > ACol) then
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
  Result := not AToken.Printed and not (AToken is TComment);
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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                Сбор и применение информации о выравниваниях                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TRulers.Create;
begin
  Names := TStringList.Create;
  MaxWidths := TDictionary<String, integer>.Create;
end;

destructor TRulers.Destroy;
begin
  FreeAndNil(Names);
  FreeAndNil(MaxWidths);
  inherited;
end;

procedure TRulers.NewLine(ALine, ACol: integer);
begin
  _Debug('Rulers::NewLine, line = %d, col = %d', [ALine, ACol]);
  if PrevLine >= ALine then
    DisablePadding := true;
  if DisablePadding then exit;
  PrevLine  := ALine;
  PrevCol   := ACol;
  Shift     := ACol;
end;

procedure TRulers.Take(const ARuler: string; ALine, ACol: integer);
var Width: integer;
begin
  _Debug('Rulers::Take, ruler = %s, col = %d', [ARuler, ACol]);
  if DisablePadding then exit;
  if ALine <> PrevLine then raise Exception.Create('Line changed inside the ruler');
  if Names.IndexOf(ARuler) < 0 then Names.Add(ARuler);
  if PrevLine <= 0 then raise Exception.Create('TRuler.NewLine required');
  Width := ACol - PrevCol;
  PrevCol := ACol;
  if MaxWidths.ContainsKey(ARuler)
    then MaxWidths[ARuler] := Math.Max(MaxWidths[ARuler], Width)
    else MaxWidths.Add(ARuler, Width);
  _Debug('  width = %d, max width = %d', [Width, MaxWidths[ARuler]]);
end;

function TRulers.Fix(const ARuler: string; ACol: integer): integer;
var
  Index, Width, i: integer;
begin
  _Debug('Rulers::Fix, ruler = %s, col = %d, disable = %s', [ARuler, ACol, BoolToStr(DisablePadding, true)]);
  if DisablePadding then exit(0);
  Index  := Names.IndexOf(ARuler);
  Width  := Shift;
  for i := 0 to Index do
    Inc(Width, MaxWidths[Names[i]]);
  Result := Math.Max(0, Width - ACol);
  _Debug('  result = %d', [Result]);
end;

{ TFormatSettings }

constructor TFormatSettings.Default;
begin
  DeclarationSingleLineParamLimit := 1;
  ArgumentSingleLineParamLimit    := 3;
  MatchParamLimit                 := 3;
  AlignVariables                  := true;
  AlignFields                     := true;
  AlignExpressions                := true;
  AlignTableColumnComments        := true;
  AlignSpecialComments            := true;
  ReplaceDefault                  := true;
  ReplaceAsIs                     := true;
  PreferredExpressionLength       := 60;
end;

end.

