////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                        Печать форматированного текста                      //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit FormatterPrinter;

interface

uses
  Classes, SysUtils, Math, System.Generics.Collections, PrinterIntf, BasePrinter,
  Tokens, Statements;

type

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
    EOLCount: integer;
    ForceNextLine: boolean;
    EmptyLine: boolean;
    WasComment: boolean;
    Line:    integer;
    Col:     integer;
    Rulers:  TRulers;
    PaddingCol: integer;
    PrevToken, FarPrevToken: TToken;
    SpecialComments: TObjectList<TToken>;
    SupSpace, SupNextLine: integer;
    IsDraft: boolean;
    Text:    string;
    RulerEnabled: boolean;
    Indents: TStack<integer>;
    FixShift: boolean;
    OriginalFormatCount, OriginalFormatStartLine: integer;
    OriginalFormatToken: TToken;
    PrevRealLine: integer;
  strict protected
    TokenPos, TokenLen: TDictionary<TToken, integer>;
    function SpaceRequired(ALeft, ARight: TToken): boolean;
    procedure PrintEmptyToken;
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
    procedure PushIndent; override;
    procedure PopIndent; override;
    procedure NextLine; override;
    procedure CancelNextLine; override;
    procedure SupressNextLine(ASupress: boolean); override;
    procedure SupressSpaces(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure StartRuler(Enabled: boolean); override;
  protected
    procedure Ruler(const ARuler: string); override;
    procedure __Debug(const S: string; Args: array of const);
  public
    function MakeDraftPrinter: TPrinter; override;
    function CurrentCol: integer; override;
    function  GetText: string; override;
  end;

const
  { Обрамление фрагмента, в котором отключается форматирование }
  C_UNFORMAT_START = '/* -- vvv --';
  C_UNFORMAT_STOP  = '/* -- ^^^ --';

implementation

uses Utils, SQLPlus, PLSQL;

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
  Indents := TStack<integer>.Create;
end;

destructor TFormatterPrinter.Destroy;
begin
  FreeAndNil(TokenPos);
  FreeAndNil(TokenLen);
  FreeAndNil(SpecialComments);
  FreeAndNil(Indents);
  inherited;
end;

{ Инициализация перед началом печати }
procedure TFormatterPrinter.BeginPrint;
begin
  inherited;
  FreeAndNil(Builder);
  Builder := TStringBuilder.Create;
  Shift   := 0;
  Line    := 1;
  Col     := 1;
  ForceNextLine := false;
  EOLCount := 666; { начало текста съедает стартовые пустые строки }
  PaddingCol := 0;
  OriginalFormatCount := 0;
  OriginalFormatToken := nil;
  TokenPos.Clear;
  TokenLen.Clear;
  SpecialComments.Clear;
  Indents.Clear;
end;

{ Вывод готового результата }
procedure TFormatterPrinter.EndPrint;
begin
  { Предупредим о дисбалансе команд запрета/возобновления форматирования, если он есть }
  if OriginalFormatCount > 0 then
  begin
    OriginalFormatCount := 0;
    EmptyLine := true;
    PrintSpecialComment('!!! ВНИМАНИЕ !!! К концу печати осталась незакрытая команда отмены форматирования !!!');
  end;
  { Закроем текущую печать }
  if Assigned(Builder) then Text := Builder.ToString else Text := '';
  FreeAndNil(Builder);
  PrevToken := nil;
  FarPrevToken := nil;
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
  { Собаку прижимаем с обеих сторон ко всему }
  if (ALeft.Value = '@') or (ARight.Value = '@') then exit;
  { Двоеточие/амперсанд прижимаем к следующему за ним идентификатору }
  if ((ALeft.Value = ':') or (ALeft.Value = '&')) and ((ARight is TEpithet) or (ARight is TNumber)) then exit;
  { Запятую прижимаем справа ко всему }
  if ARight.Value = ',' then exit;
  { В конструкции number(5,2) запятую прижимаем и слева тоже }
  if (ALeft.Value = ',') and TTerminal(ALeft).IntoNumber then exit;
  { Открывающую скобку прижимаем справа к идентификаторам и ключевым словам char, table, row, lob, key, unique, listagg }
  if (ARight.Value = '(') and
     (ALeft is TEpithet) and
     (SameText(ALeft.Value, 'char') or
      SameText(ALeft.Value, 'table') or
      SameText(ALeft.Value, 'row') or
      SameText(ALeft.Value, 'lob') or
      SameText(ALeft.Value, 'key') or
      SameText(ALeft.Value, 'unique') or
      SameText(ALeft.Value, 'listagg') or
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

{ Печать пустой лексемы - применяется в тех случаях, где нужно вывести переводы
  строк и т. п. прежде чем менять режимы, которые изменят или запретят их вывод }
procedure TFormatterPrinter.PrintEmptyToken;
var T: TToken;
begin
  T := TToken.Create('', -1, -1);
  try
    PrintToken(T);
  finally
    FreeAndNil(T);
  end;
end;

{ Вывод очередной лексемы с навешиванием всех наворотов по форматированию }
procedure TFormatterPrinter.PrintToken(AToken: TToken);

  var
    Tokens: TQueue<TToken>;

  { Добавление лексемы в очередь с разворачиванием комментариев }
  procedure AddToken(AToken: TToken);
  begin
    if not Assigned(AToken) then exit;
    AddToken(AToken.CommentFarAbove);
    AddToken(AToken.CommentAbove);
    Tokens.Enqueue(AToken);
    AddToken(AToken.CommentAfter);
    AddToken(AToken.CommentBelow);
    AddToken(AToken.CommentFarBelow);
  end;

  { Вывод лексемы в выходной поток }
  procedure InternalPrint(AToken: TToken);
  var
    Value: string;
    IsComment, LineComment, SpecComment, FakeToken, EmptyToken, HasBuilder, OriginalFormat: boolean;
    EOLLimit: integer;
  begin
     IsComment := AToken is TComment;
     LineComment := IsComment and TComment(AToken).LineComment;
     SpecComment := AToken is TSpecialComment;
    { Если это команда начала исходного форматирования, скорректируем режим до печати лексемы }
    if IsComment and AToken.Value.StartsWith(C_UNFORMAT_START)
       and Assigned(Builder) and not IsDraft then
    begin
      Inc(OriginalFormatCount);
      if OriginalFormatCount = 1 then
      begin
        OriginalFormatToken := AToken;
        OriginalFormatStartLine := Line;
        if Assigned(FarPrevToken) then
          Inc(OriginalFormatStartLine, AToken.Line - FarPrevToken.Line);
      end;
    end;
    { Выставим флаги }
    FakeToken := (AToken.Col <= 0);
    EmptyToken := FakeToken and (AToken.Value = '');
    OriginalFormat := (OriginalFormatCount > 0);
    HasBuilder := Assigned(Builder);
    EmptyLine := EmptyLine and not OriginalFormat;
    ForceNextLine := ForceNextLine and not OriginalFormat;
    { В режиме исходного форматирования не печатаем "добавленных" лексем }
    if OriginalFormat and FakeToken then exit;
    { В режиме примерки нам не нужны комментарии, кроме расположенных справа }
    if IsComment and (Mode = fpmGetRulers) and not SpecComment and (TComment(AToken).Position <> POSITION_AFTER) then exit;
    { Если это комментарий под предыдущей лексемой, выставим переводы строк }
    if IsComment and not OriginalFormat then
      case TComment(AToken).Position of
        POSITION_BELOW,
        POSITION_ABOVE,
        POSITION_FAR_ABOVE: ForceNextLine := true;
        POSITION_FAR_BELOW: EmptyLine := true;
        POSITION_AFTER    : Ruler('right-comment');
      end;
    { Обработаем переход на новую строку и вставку пустой строки }
    if EmptyLine then
      begin
        EOLLimit := 2;
        EmptyLine := false;
        ForceNextLine := false;
      end
    else if ForceNextLine then
      begin
        EOLLimit := 1;
        ForceNextLine := false;
      end
    else
      EOLLimit := 0;
    while EOLCount < EOLLimit do
    begin
      if HasBuilder then Builder.AppendLine;
      Inc(EOLCount);
      Inc(Line);
      Col := 1;
      PrevToken := nil;
      EmptyLine := false;
      ForceNextLine := false;
    end;
    { Если задано выравнивание, вставим соответствующее количество пробелов }
    if (PaddingCol > Col) and not OriginalFormat then
    begin
      if HasBuilder then Builder.Append(StringOfChar(' ', PaddingCol - Col));
      Col := PaddingCol;
      EOLCount := 0;
      PaddingCol := 0;
    end;
    { Если нужно, внедрим пробел между предыдущей и новой лексемами }
    if Assigned(PrevToken) and not EmptyToken and SpaceRequired(PrevToken, AToken) and not OriginalFormat then
    begin
      if HasBuilder then Builder.Append(' ');
      Inc(Col);
      EOLCount := 0;
    end;
    { Определим окончательное значение печатаемого текста }
    if IsComment and not LineComment and (EOLCount > 0) then
      Value := TComment(AToken).ShiftedValue(Shift + 1)
    else if IsComment and not LineComment and (EOLCount = 0) then
      Value := TComment(AToken).ShiftedValue(Col)
    else if IsComment and LineComment and (SupNextLine > 0) and (TComment(AToken).Position = POSITION_AFTER) then
      begin
        Value := '/* ' + Trim(AToken.Value.Substring(2)) + ' */';
        LineComment := false;
      end
    else
      Value := AToken.Value;
    Value := StringReplace(Value, #13, #13#10, [rfReplaceAll]);
    Value := Trim(Value);
    { Учтём настройки замены лексем на синонимы и вывод в нижнем регистре }
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and not OriginalFormat and
       SameText(AToken.Value, 'default') and Settings.ReplaceDefault and AToken.CanReplace
      then Value := ':=';
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and not OriginalFormat and
       SameText(AToken.Value, 'as') and Settings.ReplaceAsIs and AToken.CanReplace
      then Value := 'is';
    { Выполним отступ }
    if (EOLCount > 0) and not OriginalFormat then
    begin
      if HasBuilder then Builder.Append(StringOfChar(' ', Shift));
      Inc(Col, Shift);
    end;
    { Если нужно сохранить текущий отступ - сделаем это }
    if FixShift and not IsComment then
    begin
      if not OriginalFormat then Shift := Col - 1;
      FixShift := false;
    end;
    { В режиме исходного форматирования подгоним позицию к нужной строке и колонке }
    if OriginalFormat then
    begin
      while Line < AToken.Line - OriginalFormatToken.Line + OriginalFormatStartLine do
      begin
        if HasBuilder then Builder.AppendLine;
        Inc(Line);
        Col := 1;
      end;
      if Col < AToken.Col then
      begin
        if HasBuilder then Builder.Append(StringOfChar(' ', AToken.Col - Col));
        Col := AToken.Col;
      end;
    end;
    { Запомним позицию лексемы }
    if HasBuilder and not IsDraft and not FakeToken then
    begin
      TokenPos.Add(AToken, Builder.Length);
      TokenLen.Add(AToken, Value.Length);
    end;
    { Напечатаем лексему }
    if HasBuilder and not IsDraft then
    begin
      Builder.Append(Value);
      AToken.Printed := true;
    end;
    Inc(Col, Value.Length);
    { Запомним её }
    if not EmptyToken then
    begin
      PrevToken := AToken;
      FarPrevToken := PrevToken;
    end;
    EOLCount := 0;
    { Если это был комментарий, взведём флажок и выставим переводы строк перед следующей лексемой }
    WasComment := IsComment;
    if WasComment then
      case TComment(AToken).Position of
        POSITION_FAR_ABOVE: EmptyLine := true;
        POSITION_ABOVE    : ForceNextLine := true;
        POSITION_AFTER    : ForceNextLine := LineComment;
      end;
    { Если это команда завершения исходного форматирования, сменим режим после печати }
    if HasBuilder and not IsDraft and IsComment and Value.StartsWith(C_UNFORMAT_STOP) then
      Dec(OriginalFormatCount);
  end;

begin
  { Создадим очередь печати лексем }
  Tokens := TQueue<TToken>.Create;
  try
    { Добавим туда лексему и рекурсивно развернём все связанные с ней комментарии }
    AddToken(AToken);
    { Ну а теперь напечатаем очередь }
    while Tokens.Count > 0 do InternalPrint(Tokens.Dequeue);
  finally
    FreeAndNil(Tokens);
  end;
end;

{ Вывод синтаксической конструкции с расстановкой выравниваний }
procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);

  var
    _Mode: TFormatterPrinterMode;
    _Shift, _Col, _Line, _PaddingCol, _EOLCount, _OriginalFormatCount, _OriginalFormatStartLine: integer;
    _ForceNextLine, _EmptyLine, _WasComment: boolean;
    _Builder: TStringBuilder;
    _Rulers: TRulers;
    _PrevToken, _FarPrevToken, _OriginalFormatToken: TToken;

  { Печать выражения без наворотов, связанных с выравниваниями }
  procedure SimplePrintStatement(AStatement: TStatement);
  var
    _Shift: integer;
    _EmptyBefore, _EmptyInside, _EmptyAfter: boolean;
  begin
    { Соберём указания по расстановке пустых строк }
    _EmptyBefore := AStatement.EmptyLineBefore;
    _EmptyInside := not Assigned(AStatement.Parent) or AStatement.Parent.EmptyLineInside;
    _EmptyAfter  := AStatement.EmptyLineAfter;
    { Пустая строка перед конструкцией }
    EmptyLine := EmptyLine or _EmptyBefore or _EmptyInside;
    { Печать и проверка отступа }
    _Shift := Shift;
    inherited PrintStatement(AStatement);
    if (Shift <> _Shift) and (Indents.Count = 0) then
      raise Exception.CreateFmt('Invalid indents into %s - was %d but %d now', [AStatement.ClassName, _Shift, Shift]);
    { Пустая строка после конструкции }
    EmptyLine := EmptyLine or _EmptyInside or _EmptyAfter;
  end;

  { Сохранение конфигурации для возврата к ней после примерки выравнивания }
  procedure SaveCfg;
  begin
    _Shift   := Shift;
    _Col     := Col;
    _Line    := Line;
    _PaddingCol := PaddingCol;
    _EOLCount   := EOLCount;
    _OriginalFormatCount := OriginalFormatCount;
    _OriginalFormatStartLine := OriginalFormatStartLine;
    _OriginalFormatToken := OriginalFormatToken;
    _ForceNextLine := ForceNextLine;
    _EmptyLine := EmptyLine;
    _WasComment := WasComment;
    _Builder := Builder;
    _Rulers  := Rulers;
    _PrevToken := PrevToken;
    _FarPrevToken := FarPrevToken;
    _Mode    := Mode;
  end;

  { Восстановление той части конфигурации, которая нужна для печати с выравниваниями }
  procedure RestoreCfgBeforePrint;
  begin
    Shift := _Shift;
    Col   := _Col;
    Line  := _Line;
    PaddingCol := _PaddingCol;
    EOLCount   := _EOLCount;
    OriginalFormatCount := _OriginalFormatCount;
    OriginalFormatStartLine := _OriginalFormatStartLine;
    OriginalFormatToken := _OriginalFormatToken;
    ForceNextLine := _ForceNextLine;
    EmptyLine := _EmptyLine;
    WasComment := _WasComment;
    Builder := _Builder;
    PrevToken := _PrevToken;
    FarPrevToken := _FarPrevToken;
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

procedure TFormatterPrinter.PushIndent;
begin
  Indents.Push(Shift);
  FixShift := true;
end;

procedure TFormatterPrinter.PopIndent;
begin
  Shift := Indents.Pop;
end;

{ Переход на следующую строку }
procedure TFormatterPrinter.NextLine;
begin
  if SupNextLine = 0 then ForceNextLine := true;
end;

{ Отмена перехода на следующую строку }
procedure TFormatterPrinter.CancelNextLine;
begin
  if (PrevToken is TComment) and TComment(PrevToken).LineComment
    then { отменять нельзя, поэтому оставим как есть }
    else ForceNextLine := false;
end;

{ Установка режима подавления переводов строки }
procedure TFormatterPrinter.SupressNextLine(ASupress: boolean);
begin
  if ASupress
    then Inc(SupNextLine)
    else Dec(SupNextLine);
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
  PrintEmptyToken;
  Rulers.NewLine(Line);
end;

{ Установка "линейки" для выравнивания }
procedure TFormatterPrinter.Ruler(const ARuler: string);
begin
  if not RulerEnabled then exit;
  PrintItem(nil);
  case Mode of
    fpmGetRulers: Rulers.Take(ARuler, Line, Col); { В режиме примерки запомним ширину фрагмента }
    fpmSetRulers: PaddingCol := Rulers.Fix(ARuler); { В режиме разметки рассчитаем необходимое количество пробелов }
  end;
end;

procedure TFormatterPrinter.__Debug(const S: string; Args: array of const);
begin
  if Assigned(Builder) and not IsDraft and (Mode <> fpmGetRulers) then
    _Debug(S, Args);
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
  { Если остались на той же строке, когда должны были перейти на новую -
    отключаем выравнивание, иначе наломаем дров }
  if PrevLine < ALine
    then PrevLine := ALine
    else DisablePadding := true;
  PrevCol := 1;
  PrevLine := -1;
end;

procedure TRulers.Take(const ARuler: string; ALine, ACol: integer);
var Width: integer;
begin
  { Если в первый раз на новой строке, фиксируем её номер }
  if PrevLine <= 0 then PrevLine := ALine;
  { А если не в первый, и строка неожиданно сменилась - отключаем выравнивание }
  if ALine <> PrevLine then DisablePadding := true;
  if DisablePadding then exit;
  { Высчитаем и сохраним максимальную ширину ячейки }
  if Names.IndexOf(ARuler) < 0 then Names.Add(ARuler);
  Width := ACol - PrevCol;
  if MaxWidth.ContainsKey(ARuler)
    then MaxWidth[ARuler] := Math.Max(MaxWidth[ARuler], Width)
    else MaxWidth.Add(ARuler, Width);
  { И сохраним позицию для следующего расчёта }
  PrevCol := ACol;
end;

function TRulers.Fix(const ARuler: string): integer;
var i: integer;
begin
  Result := 0;
  if DisablePadding then exit;
  for i := 0 to Names.IndexOf(ARuler) do Inc(Result, MaxWidth[Names[i]]);
  Inc(Result);
end;

end.

