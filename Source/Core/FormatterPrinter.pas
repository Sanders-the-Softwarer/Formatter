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
  Classes, SysUtils, Math, System.Generics.Collections, Printer, BasePrinter,
  Tokens, Statements, TextBuilder, Rulers;

type

  { Режим принтера }
  TFormatterPrinterMode = (fpmNormal, fpmGetRulers, fpmSetRulers, fpmCheckSpecialComments);

  { Принтер для вывода форматированного текста }
  TFormatterPrinter = class (TBasePrinter)
  strict private
    { Очередь печати }
    TokenQueue: TQueue<TToken>;
    { Компонент выходного текста и его метрик }
    TextBuilder: TTextBuilder;
    { Режим печати }
    Mode: TFormatterPrinterMode;
    { Текущий отступ }
    Shift: integer;
    { Флаг перехода на следующую строку }
    ForceNextLine: boolean;
    { Флаг вставки пустой строки }
    EmptyLine: boolean;
    { Блок информации о выравниваниях }
    FRulers: TRulers;
    { Предыдущие напечатанные лексемы - в текущей строке и вообще по тексту }
    PrevToken, FarPrevToken: TToken;
    { Блокировка вставки перехода на следующую строку }
    SupNextLine: integer;
    { Название линейки, которую нужно установить на следующей печатаемой лексеме }
    RulerName: string;
    { Флаг фиксации текущего отступа как базового }
    FixShift: boolean;
    { Стек зафиксированных отступов }
    Indents: TStack<integer>;
    { Игнорируемые виды комментариев }
    SkipComments: TCommentPositions;
    { Флаг сохранения информации о местоположении лексем }
    FSaveTokenPositions: boolean;
    { Информация режима сохранения исходного форматирования }
    OriginalFormatCount, OriginalFormatStartLine: integer;
    OriginalFormatToken: TToken;
    { Текущая печатаемая конструкция }
    CurrentStatement: TStatement;
    { Принтер для отлова спецкомментариев }
    SpecCommentDraftPrinter: TFormatterPrinter;
    { Последняя лексема, прошедшая через очередь печати }
    LastToken: TToken;
    { Разрешение принтеру на подмену типа комментариев }
    CanChangeLineCommentsToBrackets: boolean;
    { Служебная переменная для правильной обработки разделителей в выравниваемых списках }
    DelimiterCol: integer;
  strict protected
    TokenPos, TokenLen: TDictionary<TToken, integer>;
    function StartShift: integer; virtual;
    function SpaceRequired(ALeft, ARight: TToken): boolean;
    procedure PrintEmptyToken;
    procedure MeasureRuler;
    function HasActiveRulers: boolean;
    property Rulers: TRulers read FRulers write FRulers;
  public
    constructor Create(ASettings: TFormatSettings;
                       AWithoutText: boolean = false;
                       ASkipComments: TCommentPositions = [];
                       ACanChangeLineCommentsToBrackets: boolean = true);
    destructor Destroy; override;
    procedure Clear; override;
    procedure EndPrint; override;
    procedure PrintItem(AItem: TObject); override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure Indent; override;
    procedure Undent; override;
    procedure PushIndent; override;
    procedure PopIndent; override;
    procedure NextLine; override;
    procedure CancelNextLine; override;
    procedure SupressNextLine(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure BeforePrintDelimiter; override;
    procedure AfterPrintDelimiter; override;
  public
    procedure PrintRulerItems(const ARuler: string; AItems: array of TObject); override;
    function GetText: string; override;
    function CurrentLine: integer;
    function CurrentCol: integer;
    function CurrentMaxWidth: integer;
    procedure Ruler(const ARuler: string);
  public
    procedure AbstractForWarning; virtual; abstract;
  public
    property SaveTokenPositions: boolean read FSaveTokenPositions write FSaveTokenPositions;
  end;

  { Принтер для черновиков }
  TDraftPrinter = class(TFormatterPrinter)
  public
    procedure AbstractForWarning; override;
  end;

  { Принтер для итогового результата }
  TFineCopyPrinter = class(TFormatterPrinter)
  protected
    function StartShift: integer; override;
  public
    procedure AbstractForWarning; override;
  end;

const
  { Обрамление фрагмента, в котором отключается форматирование }
  C_UNFORMAT_START = '/* -- vvv --';
  C_UNFORMAT_STOP  = '/* -- ^^^ --';

implementation

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                  Принтер для печати форматированного текста                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TFormatterPrinter.Create(ASettings: TFormatSettings;
                                     AWithoutText: boolean = false;
                                     ASkipComments: TCommentPositions = [];
                                     ACanChangeLineCommentsToBrackets: boolean = true);
begin
  Assert(ASettings <> nil);
  inherited Create;
  Settings := ASettings;
  SkipComments := ASkipComments;
  CanChangeLineCommentsToBrackets := ACanChangeLineCommentsToBrackets and Settings.ChangeCommentType;
  TextBuilder := TTextBuilder.Create(AWithoutText);
  Indents := TStack<integer>.Create;
  TokenQueue := TQueue<TToken>.Create;
  TokenPos := TDictionary<TToken, integer>.Create;
  TokenLen := TDictionary<TToken, integer>.Create;
end;

destructor TFormatterPrinter.Destroy;
begin
  FreeAndNil(TokenPos);
  FreeAndNil(TokenLen);
  FreeAndNil(Indents);
  FreeAndNil(TextBuilder);
  FreeAndNil(TokenQueue);
  FreeAndNil(SpecCommentDraftPrinter);
  inherited;
end;

{ Инициализация перед началом печати }
procedure TFormatterPrinter.Clear;
begin
  inherited;
  TextBuilder.Clear;
  Shift := StartShift;
  ForceNextLine := false;
  OriginalFormatCount := 0;
  OriginalFormatToken := nil;
  if Assigned(TokenPos) then TokenPos.Clear;
  if Assigned(TokenLen) then TokenLen.Clear;
  Indents.Clear;
  TokenQueue.Clear;
  PrevToken := nil;
  FarPrevToken := nil;
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
  { Всё, можно закрывать }
  inherited;
end;

{ Универсальный метод вывода на принтер поддерживаемых объектов }
procedure TFormatterPrinter.PrintItem(AItem: TObject);
begin
  if AItem is TFormatterCmd
    then TFormatterCmd(AItem).PrintSelf(Self)
    else inherited;
end;

{ Функция установки стартового отступа }
function TFormatterPrinter.StartShift: integer;
begin
  Result := 0;
end;

{ Проверка, нужен ли пробел между двумя лексемами }
function TFormatterPrinter.SpaceRequired(ALeft, ARight: TToken): boolean;
begin
  Result := false;
  { Терминалы, которым явно указали не отделяться - не отделяем }
  if (ALeft is TTerminal) and TTerminal(ALeft).WithoutSpace then exit;
  if (ARight is TTerminal) and TTerminal(ARight).WithoutSpace then exit;
  { Точку с запятой прижимаем справа ко всему }
  if ARight.Value = ';' then exit;
  { Точку прижимаем с обеих сторон ко всему, кроме ключевых слов }
  if (ALeft.Value = '.') and not ((ARight is TEpithet) and TEpithet(ARight).IsKeyword) then exit;
  if (ARight.Value = '.') and not ((ALeft is TEpithet) and TEpithet(ALeft).IsKeyword) then exit;
  { Собаку прижимаем с обеих сторон ко всему }
  if (ALeft.Value = '@') or (ARight.Value = '@') then exit;
  { Двойную собаку прижимаем слева ко всему }
  if (ALeft.Value = '@@') then exit;
  { Двоеточие/амперсанд прижимаем к следующему за ним идентификатору }
  if ((ALeft.Value = ':') or (ALeft.Value = '&') or (ALeft.Value = '&&')) and ((ARight is TEpithet) or (ARight is TNumber)) then exit;
  { Запятую прижимаем справа ко всему }
  if ARight.Value = ',' then exit;
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
  { Если правила не сработали, ставим пробел }
  Result := true;
end;

{ Печать пустой лексемы - применяется в тех случаях, где нужно вывести переводы
  строк и т. п. прежде чем менять режимы, которые изменят или запретят их вывод }
procedure TFormatterPrinter.PrintEmptyToken;
var T: TToken;
begin
  T := TEmptyToken.Create;
  try
    PrintToken(T);
  finally
    FreeAndNil(T);
  end;
end;

{ Отметка текущего положения в расчёте выравниваний }
procedure TFormatterPrinter.MeasureRuler;
begin
  if HasActiveRulers and (Mode = fpmGetRulers) then Rulers.Measure(TextBuilder.Line, TextBuilder.Col);
end;

{ Проверка режима выравнивания }
function TFormatterPrinter.HasActiveRulers: boolean;
begin
  Result := Assigned(Rulers);
end;

{ Вывод очередной лексемы с навешиванием всех наворотов по форматированию }
procedure TFormatterPrinter.PrintToken(AToken: TToken);

  { Добавление лексемы в очередь с разворачиванием и фильтрацией комментариев }
  procedure AddToken(AToken: TToken);
  begin
    if not Assigned(AToken) then exit;
    AddToken(AToken.CommentFarAbove);
    AddToken(AToken.CommentAbove);
    LastToken := AToken;
    if (AToken is TComment) and (TComment(AToken).Position in SkipComments)
      then { пропускаем }
      else TokenQueue.Enqueue(AToken);
    AddToken(AToken.CommentAfter);
    AddToken(AToken.CommentBelowBOL);
    AddToken(AToken.CommentBelow);
    AddToken(AToken.CommentFarBelow);
  end;

  { Вывод лексемы в выходной поток }
  procedure InternalPrint(AToken: TToken);
  var
    AComment: TComment absolute AToken;
    LeadComment: TComment;
    Value: string;
    IsComment, LineComment, SpecComment, FakeToken, EmptyToken, OriginalFormat: boolean;
    EOLLimit, ActualShift, Fix: integer;
  begin
    if HasActiveRulers then Rulers.Token := AToken;
    { Определим статус комментариев }
    IsComment := AToken is TComment;
    LineComment := IsComment and AComment.LineComment;
    SpecComment := AToken is TSpecialComment;
    { Если это команда начала исходного форматирования, скорректируем режим до печати лексемы }
    if IsComment and AComment.Value.StartsWith(C_UNFORMAT_START) then
    begin
      Inc(OriginalFormatCount);
      if OriginalFormatCount = 1 then
      begin
        OriginalFormatToken := AToken;
        OriginalFormatStartLine := TextBuilder.Line;
        if Assigned(FarPrevToken) then Inc(OriginalFormatStartLine, AToken.Line - FarPrevToken.Line);
      end;
    end;
    { Если попали в ситуацию бага №71, скорректируем сдвиг }
    if LineComment and AComment.FixBug71 and (Indents.Count > 0) then
      Shift := Indents.Peek;
    { Выставим флаги }
    FakeToken := (AToken.Col <= 0);
    EmptyToken := FakeToken and (AToken.Value = '');
    OriginalFormat := (OriginalFormatCount > 0);
    EmptyLine := EmptyLine and not OriginalFormat;
    ForceNextLine := ForceNextLine and not OriginalFormat;
    ActualShift := Shift;
    { В режиме исходного форматирования не печатаем "добавленных" лексем }
    if OriginalFormat and FakeToken then exit;
    { Если печатаем в одну строку - отметим это в лексеме }
    if (SupNextLine > 0) and (Mode <> fpmCheckSpecialComments) then
      AToken.IntoSupNextLine := AToken.IntoSupNextLine + 1;
    { Для комментариев справа от текста приготовим выравнивающую линейку }
    if IsComment and not SpecComment and (AComment.Position = poAfter) and Settings.AlignRightComments then
      Ruler(RIGHT_COMMENT);
    { Если это комментарий под предыдущей лексемой, надо урегулировать переводы строк }
    if IsComment and not OriginalFormat and not (FarPrevToken is TComment) then
    begin
      { Если комментарий далеко внизу, точно нужна пустая строка }
      if AComment.Position = poFarBelow then
        EmptyLine := true
      { Если комментарий прижат снизу, достаточно перевода строки }
      else if AComment.Position = poBelow then
        ForceNextLine := true
      { Если комментарий сдвинут к началу строки, нужно поправить отступ }
      else if AComment.Position = poBelowBOL then
        begin
          ForceNextLine := true;
          if Indents.Count > 0 then ActualShift := Indents.Peek;
        end
      { Если комментарий далеко вверху от чего-то ниже себя, значит к текущей лексеме он прижат не был }
      else if AComment.Position = poFarAbove then
        EmptyLine := true
      { Если комментарий прижат сверху, нужно определить правильную дистанцию до ведущего комментария }
      else if AComment.Position = poAbove then
        begin
          LeadComment := AComment.LeadComment;
          if LeadComment.Position in [poFarAbove, poFarBelow]
            then EmptyLine := true
            else ForceNextLine := true;
        end;
    end;
    { Определим необходимое количество переводов строки }
    if EmptyLine then
      EOLLimit := 2
    else if ForceNextLine then
      EOLLimit := 1
    else
      EOLLimit := 0;
    { И выведем их, если мы не в начале текста }
    if EOLLimit > 0 then
    begin
      MeasureRuler;
      if (TextBuilder.Line > 1) or (TextBuilder.Col > 1) then TextBuilder.AppendLine(EOLLimit);
      PrevToken := nil;
      EmptyLine := false;
      ForceNextLine := false;
    end;
    { Если нужно, внедрим пробел между предыдущей и новой лексемами }
    if Assigned(PrevToken) and not EmptyToken and SpaceRequired(PrevToken, AToken) and not OriginalFormat then
      TextBuilder.AppendSpace(1);
    { Если установлена линейка, прореагируем на неё в соответствии с режимом принтера }
    if RulerName <> '' then
    begin
      if HasActiveRulers then
        case Mode of
          { В режиме примерки запомним ширину фрагмента }
          fpmGetRulers:
            begin
              Rulers.Start(RulerName, TextBuilder.Line, TextBuilder.Col);
              {$IFDEF DEBUG}
              AToken.AddDebugInfo('%s(%p) [rulers=%p] Start :: ruler = {%s}, current col = %d, indent = %d', [ClassName, pointer(Self), pointer(Rulers), RulerName, TextBuilder.Col, Rulers.Shift^]);
              {$ENDIF}
            end;
          { В режиме вставим необходимое количество пробелов }
          fpmSetRulers:
            if not OriginalFormat then
            begin
              Fix := Rulers.Fix(RulerName) + Shift;
              {$IFDEF DEBUG}
              AToken.AddDebugInfo('%s(%p) [rulers=%p] Fix :: ruler = {%s}, current col = %d, target col = %d, indent = %d', [ClassName, pointer(Self), pointer(Rulers), RulerName, TextBuilder.Col, Fix, Rulers.Shift^]);
              {$ENDIF}
              TextBuilder.AppendSpace(Fix - TextBuilder.Col);
            end;
        end;
      RulerName := '';
    end;
    { Внесём в многострочный комментарий поправку на сдвиг к колонке, в которой он должен быть выведен }
    if IsComment and not LineComment then
      if (EOLLimit > 0) and (TextBuilder.Col = 1)
        then AComment.ShiftTo(ActualShift + 1)
        else AComment.ShiftTo(TextBuilder.Col);
    { В режиме подавления переводов строки заменим строчный комментарий справа от лексемы скобочным }
    if IsComment and LineComment and (SupNextLine > 0) and (AComment.Position = poAfter) and
       CanChangeLineCommentsToBrackets and not OriginalFormat and not AComment.SkipChangeLineComment then
    begin
      AComment.ChangeTypeToBrackets := true;
      LineComment := false;
    end;
    { Получим значение выводимого текста }
    if OriginalFormat then
      Value := AToken.InitialValue
    else if IsComment and not SpecComment and Settings.CorrectCommentSpaces then
      Value := AComment.CorrectSpaces
    else
      Value := Trim(AToken.Value);
    { Учтём настройки замены лексем на синонимы и вывод в нижнем регистре }
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and not OriginalFormat and
       SameText(AToken.Value, 'default') and Settings.ReplaceDefault and AToken.CanReplace
      then Value := ':=';
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and not OriginalFormat and
       SameText(AToken.Value, 'as') and Settings.ReplaceAsIs and AToken.CanReplace
      then Value := 'is';
    { Учтём замену строчных комментариев на скобочные }
    if IsComment and AComment.ChangeTypeToBrackets and not AComment.SkipChangeLineComment then
      Value := '/* ' + Trim(Value.Substring(2)) + ' */';
    { Выполним отступ }
    if (EOLLimit > 0) and (TextBuilder.Col = 1) and not OriginalFormat then
      TextBuilder.AppendSpace(ActualShift);
    { Если нужно сохранить текущий отступ - сделаем это }
    if FixShift and not IsComment then
    begin
      if not OriginalFormat then Shift := TextBuilder.Col - 1;
      FixShift := false;
    end;
    { В режиме исходного форматирования подгоним позицию к нужной строке и колонке }
    if OriginalFormat then
    begin
      while TextBuilder.Line < AToken.Line - OriginalFormatToken.Line + OriginalFormatStartLine do
        TextBuilder.AppendLine(1);
      if TextBuilder.Col < AToken.Col then
        TextBuilder.AppendSpace(AToken.Col - TextBuilder.Col);
    end;
    { Напечатаем лексему и запомним её позицию }
    if SaveTokenPositions and not FakeToken then TokenPos.Add(AToken, TextBuilder.Length);
    {$IFDEF DEBUG}
    if Self is TFineCopyPrinter then
      AToken.AddDebugInfo('Позиция в тексте: [%d, %d]', [TextBuilder.Line, TextBuilder.Col]);
    {$ENDIF}
    TextBuilder.AppendText(Value);
    if SaveTokenPositions then AToken.Printed := true;
    if SaveTokenPositions and not FakeToken then TokenLen.Add(AToken, TextBuilder.Length - TokenPos[AToken]);
    { Запомним её }
    if not EmptyToken then
    begin
      PrevToken := AToken;
      FarPrevToken := PrevToken;
    end;
    { Если это был комментарий, выставим переводы строк перед следующей лексемой }
    if IsComment then
      case AComment.Position of
        poFarAbove: EmptyLine := true;
        poAbove   : ForceNextLine := true;
        else        ForceNextLine := LineComment;
      end;
    { Если это команда завершения исходного форматирования, сменим режим после печати }
    if IsComment and Value.StartsWith(C_UNFORMAT_STOP) then
      Dec(OriginalFormatCount);
    { Сбросим теряющий валидность указатель, без этого падает на строчке DraftPrinter.MeasureRuler }
    if HasActiveRulers then
      Rulers.Token := nil;
  end;

begin
  { Добавим лексему в очередь и рекурсивно развернём связанные с ней комментарии }
  AddToken(AToken);
  { Напечатаем очередь }
  while TokenQueue.Count > 0 do InternalPrint(TokenQueue.Dequeue);
end;

{ Вывод синтаксической конструкции с расстановкой выравниваний }
procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);

  { Печать конструкции и обработка пустых строк до и после конструкции }
  procedure SimplePrintStatement;
  var
    _Shift: integer;
    _EmptyBefore, _EmptyInside, _EmptyAfter: boolean;
    _PrevStatement: TStatement;
  begin
    { Соберём указания по расстановке пустых строк }
    _EmptyBefore := AStatement.EmptyLineBefore;
    _EmptyInside := not Assigned(AStatement.Parent) or AStatement.Parent.EmptyLineInside;
    _EmptyAfter  := AStatement.EmptyLineAfter;
    { Пустая строка перед конструкцией }
    EmptyLine := (EmptyLine or _EmptyBefore or _EmptyInside);
    { Печать и проверка отступа }
    _Shift := Shift;
    _PrevStatement := CurrentStatement;
    try
      CurrentStatement := AStatement;
      inherited PrintStatement(AStatement);
    finally
      CurrentStatement := _PrevStatement;
    end;
    if (Shift <> _Shift) and (Indents.Count = 0) then
      raise Exception.CreateFmt('Invalid indents into %s - was %d but %d now', [AStatement.ClassName, _Shift, Shift]);
    { Пустая строка после конструкции }
    EmptyLine := EmptyLine or _EmptyInside or _EmptyAfter;
  end;

  { Сбор информации о выравниваниях }
  procedure CollectRulers;
  var DraftPrinter: TFormatterPrinter;
  begin
    if AStatement.Rulers.Full then exit;
    DraftPrinter := TDraftPrinter.Create(Self.Settings, false, [poFarAbove..poFarBelow], false);
    try
      AStatement.Rulers.UseSpaces := Settings.AlignUseSpace;
      DraftPrinter.Rulers := AStatement.Rulers;
      DraftPrinter.Mode   := fpmGetRulers;
      DraftPrinter.BeginPrint;
      DraftPrinter.PrintItem(AStatement);
      DraftPrinter.MeasureRuler;
      DraftPrinter.EndPrint;
      AStatement.Rulers.Full := true;
    finally
      FreeAndNil(DraftPrinter);
    end;
  end;

  { Печать по выравниваниям }
  procedure PrintRuledStatement;
  var
    _Rulers: TRulers;
    _Mode: TFormatterPrinterMode;
  begin
    _Rulers := Self.Rulers;
    _Mode   := Self.Mode;
    try
      Self.Rulers := AStatement.Rulers;
      Self.Mode   := fpmSetRulers;
      AStatement.Rulers.Shift := @Self.Shift;
      SimplePrintStatement;
    finally
      Self.Rulers := _Rulers;
      Self.Mode   := _Mode;
    end;
  end;

  { Проверка наличия в конструкции спецкомментариев }
  procedure CheckSpecialComments;
  begin
    if not Assigned(SpecCommentDraftPrinter) then
    begin
      SpecCommentDraftPrinter := TDraftPrinter.Create(Self.Settings, true, [poFarAbove..poFarBelow], false);
      SpecCommentDraftPrinter.BeginPrint;
      SpecCommentDraftPrinter.Mode := fpmCheckSpecialComments;
    end;
    SpecCommentDraftPrinter.PrintItem(AStatement);
  end;

begin
  { Проверим наличие спецкомментариев - это может повлиять на выравнивание }
  if (Mode = fpmNormal) and (AStatement.HasSpecialComments = hscUnknown) then
    CheckSpecialComments;
  { Сначала предположим, что спецкомментариев нет }
  if Mode = fpmCheckSpecialComments then
  begin
    CurrentStatement := AStatement;
    CurrentStatement.HasSpecialComments := hscNo;
  end;
  { Если конструкцию нужно ровнять - соберём информацию и распечатаем в соответствии }
  if AStatement.IsAligned and (Mode = fpmNormal) then
    begin
      CollectRulers;
      PrintRuledStatement;
    end
  { Иначе просто выдадим всё как есть }
  else
    SimplePrintStatement;
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
  { Сбросим флажок, чтобы замыкающие запятые не отрывались от предыдущей лексемы }
  RulerName := '';
end;

{ Установка режима подавления переводов строки }
procedure TFormatterPrinter.SupressNextLine(ASupress: boolean);
begin
  if ASupress
    then Inc(SupNextLine)
    else Dec(SupNextLine);
  if (SupNextLine = 0) and Assigned(LastToken) then
    LastToken.LastIntoSupNextLine := LastToken.LastIntoSupNextLine + 1;
end;

{ Вывод на принтер специального комментария (отсутствующего в исходном тексте)}
procedure TFormatterPrinter.PrintSpecialComment(AValue: string);
var T1, T2: TSpecialComment;
begin
  if Assigned(CurrentStatement) then CurrentStatement.HasSpecialComments := hscYes;
  T1 := TSpecialComment.Create('/*/ ' + AValue);
  T2 := TSpecialComment.Create('/*/');
  try
    if Settings.AlignSpecialComments then
      begin
        PrintRulerItems(SPECIAL_COMMENT_START, [T1]);
        PrintRulerItems(SPECIAL_COMMENT_FINISH, [T2]);
      end
    else
      PrintItems([T1, T2]);
  finally
    FreeAndNil(T1);
    FreeAndNil(T2);
  end;
end;

{ Перед печатью разделителя запомним позицию }
procedure TFormatterPrinter.BeforePrintDelimiter;
begin
  DelimiterCol := TextBuilder.Col;
end;

{ После печати правильно учтём длину разделителя }
procedure TFormatterPrinter.AfterPrintDelimiter;
begin
  if HasActiveRulers then Rulers.ConsiderDelimiter(TextBuilder.Col - DelimiterCol);
end;

{ Установка "линейки" для выравнивания }
procedure TFormatterPrinter.Ruler(const ARuler: string);
begin
  if not HasActiveRulers then exit;
  { Сразу запомним линейку, чтобы всегда сохранить их правильный порядок }
  Rulers.AddRuler(ARuler);
  { А остальную работу отложим до PrintToken, иначе возникают проблемы с
    пробелами, вставляемыми по SpaceRequired }
  RulerName := ARuler;
end;

procedure TFormatterPrinter.PrintRulerItems(const ARuler: string; AItems: array of TObject);
begin
  Ruler(ARuler);
  PrintItems(AItems);
  if (RulerName <> '') and (Mode = fpmGetRulers) then PrintEmptyToken;
end;

function TFormatterPrinter.GetText: string;
begin
  Result := TextBuilder.Text;
end;

function TFormatterPrinter.CurrentLine: integer;
begin
  Result := TextBuilder.Line;
end;

function TFormatterPrinter.CurrentMaxWidth: integer;
begin
  Result := TextBuilder.MaxWidth;
end;

function TFormatterPrinter.CurrentCol: integer;
begin
  Result := TextBuilder.Col;
end;

{ TDraftPrinter }

procedure TDraftPrinter.AbstractForWarning;
begin
  { тело не нужно, метод используется только для warning-а при попытке
    создания TFormatterPrinter }
end;

{ TFineCopyPrinter }

function TFineCopyPrinter.StartShift: integer;
begin
  Result := Settings.StartIndent;
end;

procedure TFineCopyPrinter.AbstractForWarning;
begin
  { тело не нужно, метод используется только для warning-а при попытке
    создания TFormatterPrinter }
end;

end.

