unit FormatterPrinter;

interface

uses
  Classes, SysUtils, Math, System.Generics.Collections, Printers_, Tokens, Statements;

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
    MaxWidth: TDictionary<String, integer>;
    PrevLine, PrevCol: integer;
    DisablePadding: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure NewLine(ALine: integer);
    procedure Take(const ARuler: string; ALine, ACol: integer);
    function Fix(const ARuler: string): integer;
  end;

  { Режим принтера }
  TFormatterPrinterMode = (fpmNormal, fpmGetRulers, fpmSetRulers);

  { Принтер для вывода форматированного текста }
  TFormatterPrinter = class(TBasePrinter)
  strict private
    Builder: TStringBuilder;
    Mode:    TFormatterPrinterMode;
    Shift:   integer;
    BOL:     boolean;
    EmptyLine: boolean;
    WasComment: boolean;
    WasBOL: boolean;
    Line:    integer;
    Col:     integer;
    Rulers:  TRulers;
    PaddingCol: integer;
    PrevStatement: TStatement;
    PrevToken: TToken;
    SpecialComments: TObjectList<TToken>;
    SupSpace, SupNextLine: integer;
    IsDraft: boolean;
    Text:    string;
    RulerEnabled: boolean;
  strict protected
    TokenPos, TokenLen: TDictionary<TToken, integer>;
    function SpaceRequired(ALeft, ARight: TToken): boolean;
    function EmptyLineRequired(APrev, ANext: TStatement): boolean;
  public
    constructor Create;
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
    function  GetText: string; override;
  end;

implementation

uses Utils, Attributes, SQLPlus;

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

constructor TFormatterPrinter.Create;
begin
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
  PaddingCol := 0;
  TokenPos.Clear;
  TokenLen.Clear;
  SpecialComments.Clear;
end;

{ Вывод готового результата }
procedure TFormatterPrinter.EndPrint;
begin
  if Assigned(Builder) then Text := Builder.ToString else Text := '';
  FreeAndNil(Builder);
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
  { Команды set, @, exec лепим вместе }
  if (APrev.ClassType = ANext.ClassType) and
     ((APrev.ClassType = TSet) or
      (APrev.ClassType = TAt) or
      (APrev.ClassType = TCall)) then exit(false);
  { Вставим пустую строку перед новой конструкцией верхнего уровня }
  if not Assigned(ANext.Parent) then exit(true);
  { Условия не выполнены }
  Result := false;
end;

{ Вывод очередной лексемы с навешиванием всех наворотов по форматированию }
procedure TFormatterPrinter.PrintToken(AToken: TToken);
var
  Value: string;
  AllowLF, StartOfText, ForceNextLine: boolean;
  i: integer;
begin
  StartOfText := (Line = 1) and (Col = 1);
  AllowLF := (SupNextLine = 0) or WasComment;
  { Обработаем вставку пустой строки }
  if EmptyLine and not StartOfText then
  begin
    if Assigned(Builder) and AllowLF then
    begin
      Builder.AppendLine;
      Inc(Line);
    end;
    BOL := true;
    EmptyLine := false;
  end;
  { Обработаем переход на новую строку }
  if BOL and WasBOL then
    BOL := false
  else if BOL then
  begin
    if Assigned(Builder) and AllowLF and not StartOfText then
    begin
      Builder.AppendLine;
      WasBOL := true;
    end;
    BOL := false;
    if AllowLF and not StartOfText then
    begin
      Inc(Line);
      Col := 1;
      PrevToken := nil;
    end;
  end;
  { Если задано выравнивание, вставим соответствующее количество пробелов }
  if (PaddingCol > Col) and Assigned(AToken) then
  begin
    _Debug(' => col = %d, padding col = %d', [Col, PaddingCol]);
    if WasBOL then Dec(PaddingCol, Shift);
    if Assigned(Builder) then Builder.Append(StringOfChar(' ', PaddingCol - Col));
    Col := PaddingCol;
    PaddingCol := 0;
  end;
  { Если нужно, внедрим пробел между предыдущей и новой лексемами }
  if Assigned(PrevToken) and Assigned(AToken) and SpaceRequired(PrevToken, AToken) then
  begin
    if Assigned(Builder) then Builder.Append(' ');
    Inc(Col);
  end;
  { Если до лексемы есть комментарии, напечатаем их }
  if Assigned(AToken) and Assigned(AToken.CommentsAbove) and Assigned(Builder) and not IsDraft then
    for i := 0 to AToken.CommentsAbove.Count - 1 do
    begin
      if not WasBOL then NextLine;
      PrintToken(AToken.CommentsAbove[i]);
      NextLine;
      PrintToken(nil);
    end;
  { И, наконец, если задана лексема - напечатаем её }
  if Assigned(AToken) then
  begin
    Value := AToken.Value;
    Value := StringReplace(Value, #13, #13#10, [rfReplaceAll]);
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
      if WasBOL then Builder.Append(StringOfChar(' ', Shift));
      if not IsDraft and not (AToken is TSpecialComment) then
      begin
        TokenPos.Add(AToken, Builder.Length);
        TokenLen.Add(AToken, Value.Length);
      end;
      Builder.Append(Value);
      AToken.Printed := true;
    end;
    if WasBOL then Inc(Col, Shift);
    Inc(Col, Value.Length);
    PrevToken := AToken;
    WasBOL := false;
  end;
  { Если это был комментарий, взведём флажок }
  if Assigned(AToken) then WasComment := AToken is TComment;
  { Если после лексемы есть комментарии, напечатаем их }
  if Assigned(AToken) then
  begin
    ForceNextLine := false;
    if Assigned(AToken.CommentsAfter) then
      for i := 0 to AToken.CommentsAfter.Count - 1 do
      begin
        WasComment := true;
        Ruler('right-comment');
        PrintToken(AToken.CommentsAfter[i]);
        ForceNextLine := ForceNextLine or AToken.CommentsAfter[i].Value.StartsWith('--');
      end;
    if ForceNextLine then NextLine;
    if Assigned(AToken.CommentsBelow) and Assigned(Builder) then
      for i := 0 to AToken.CommentsBelow.Count - 1 do
      begin
        WasComment := true;
        NextLine;
        PrintToken(AToken.CommentsBelow[i]);
        NextLine;
      end;
  end;
end;

{ Вывод синтаксической конструкции с расстановкой выравниваний }
procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);

  var
    _Mode: TFormatterPrinterMode;
    _Shift, _Col, _Line, _PaddingCol: integer;
    _BOL, _EmptyLine, _WasComment, _WasBOL: boolean;
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
    _PaddingCol := PaddingCol;
    _BOL     := BOL;
    _EmptyLine := EmptyLine;
    _WasComment := WasComment;
    _WasBOL  := WasBOL;
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
    PaddingCol := _PaddingCol;
    BOL   := _BOL;
    EmptyLine := _EmptyLine;
    WasComment := _WasComment;
    WasBOL := _WasBOL;
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
  PaddingCol := 0;
end;

procedure TFormatterPrinter.PrintRulerItems(const ARuler: string; AItems: array of TObject);
begin
  inherited;
  PaddingCol := 0;
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

  function SpecialToken(const Text: string): TToken;
  begin
    Result := TSpecialComment.Create(Text, -1, -1);
    SpecialComments.Add(Result);
  end;

begin
  if not RulerEnabled then StartRuler(Settings.AlignSpecialComments);
  Self.PrintRulerItem('special-comment-start', SpecialToken('/*/'));
  PrintItem(SpecialToken(AValue));
  Self.PrintRulerItem('special-comment-finish', SpecialToken('/*/'));
end;

{ Начало выравнивания в очередной строке }
procedure TFormatterPrinter.StartRuler(Enabled: boolean);
begin
  RulerEnabled := Enabled;
  if (Mode <> fpmGetRulers) or not Enabled then exit;
  PrintToken(nil);
  Rulers.NewLine(Line);
end;

{ Установка "линейки" для выравнивания }
procedure TFormatterPrinter.Ruler(const ARuler: string);
begin
  if not RulerEnabled then exit;
  case Mode of
    fpmGetRulers: Rulers.Take(ARuler, Line, Col); { В режиме примерки запомним ширину фрагмента }
    fpmSetRulers: PaddingCol := Rulers.Fix(ARuler) + Shift; { В режиме разметки рассчитаем необходимое количество пробелов }
  end;
end;

{ Создание принтера для пробной печати }
function TFormatterPrinter.MakeDraftPrinter: TPrinter;
begin
  Result := TFormatterPrinter.Create;
  Result.Settings := Self.Settings;
  TFormatterPrinter(Result).IsDraft := true;
end;

{ Возврат текущего положения каретки }
function TFormatterPrinter.CurrentCol: integer;
begin
  Result := Col;
end;

function TFormatterPrinter.GetText: string;
begin
  if Assigned(Builder)
    then Result := Builder.ToString
    else Result := Text;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                Сбор и применение информации о выравниваниях                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TRulers.Create;
begin
  Names := TStringList.Create;
  MaxWidth := TDictionary<String, integer>.Create;
end;

destructor TRulers.Destroy;
begin
  FreeAndNil(MaxWidth);
  FreeAndNil(Names);
  inherited;
end;

procedure TRulers.NewLine(ALine: integer);
begin
  if PrevLine < ALine
    then PrevLine := ALine
    else DisablePadding := true;
  PrevCol := 1;
end;

procedure TRulers.Take(const ARuler: string; ALine, ACol: integer);
var Width: integer;
begin
  _Debug('Rulers.Take ruler = "%s", line = %d, col = %d, prev col = %d', [ARuler, ALine, ACol, PrevCol]);
  if ALine <> PrevLine then DisablePadding := true;
  if DisablePadding then exit;
  if PrevLine <= 0 then raise Exception.Create('TRuler.NewLine required');
  if Names.IndexOf(ARuler) < 0 then Names.Add(ARuler);
  Width := ACol - PrevCol;
  if MaxWidth.ContainsKey(ARuler)
    then MaxWidth[ARuler] := Math.Max(MaxWidth[ARuler], Width)
    else MaxWidth.Add(ARuler, Width);
  _Debug(' => width = %d, max width = %d', [Width, MaxWidth[ARuler]]);
  PrevCol := ACol;
end;

function TRulers.Fix(const ARuler: string): integer;
var i: integer;
begin
  Result := 0;
  if DisablePadding then exit;
  for i := 0 to Names.IndexOf(ARuler) do
    Inc(Result, MaxWidth[Names[i]]);
  Inc(Result);
  _Debug('Rulers.Fix, ruler = "%s", fix = %d', [ARuler, Result]);
end;

end.

