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
  TFormatterPrinterMode = (fpmNormal, fpmGetRulers, fpmSetRulers);

  { Принтер для вывода форматированного текста }
  TFormatterPrinter = class(TBasePrinter)
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
    Rulers: TRulers;
    { Флаг активности режима выравнивания }
    RulerEnabled: boolean;
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
    SaveTokenPositions: boolean;
    { Информация режима сохранения исходного форматирования }
    OriginalFormatCount, OriginalFormatStartLine: integer;
    OriginalFormatToken: TToken;
    { Флаг перекрытия настройки из Settings }
    FForceChangeCommentType: boolean;
  strict protected
    TokenPos, TokenLen: TDictionary<TToken, integer>;
    function SpaceRequired(ALeft, ARight: TToken): boolean;
    procedure PrintEmptyToken;
  public
    constructor Create(ASettings: TFormatSettings;
                       AWithoutText: boolean = false;
                       ASkipComments: TCommentPositions = [];
                       ASaveTokenPositions: boolean = false);
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
    procedure StartRuler(Enabled: boolean; Continued: boolean = false); override;
  protected
    procedure Ruler(const ARuler: string); override;
  public
    procedure PrintRulerItems(const ARuler: string; AItems: array of TObject); override;
    function GetText: string; override;
    function CurrentLine: integer;
    function CurrentCol: integer;
    function CurrentMaxWidth: integer;
  public
    property ForceChangeCommentType: boolean read FForceChangeCommentType write FForceChangeCommentType;
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

constructor TFormatterPrinter.Create(ASettings: TFormatSettings;
                                     AWithoutText: boolean = false;
                                     ASkipComments: TCommentPositions = [];
                                     ASaveTokenPositions: boolean = false);
begin
  Assert(ASettings <> nil);
  inherited Create;
  Settings := ASettings;
  SkipComments := ASkipComments;
  SaveTokenPositions := ASaveTokenPositions;
  TextBuilder := TTextBuilder.Create(AWithoutText);
  if SaveTokenPositions then
  begin
    TokenPos := TDictionary<TToken, integer>.Create;
    TokenLen := TDictionary<TToken, integer>.Create;
  end;
  Indents := TStack<integer>.Create;
  TokenQueue := TQueue<TToken>.Create;
end;

destructor TFormatterPrinter.Destroy;
begin
  FreeAndNil(TokenPos);
  FreeAndNil(TokenLen);
  FreeAndNil(Indents);
  FreeAndNil(TextBuilder);
  FreeAndNil(TokenQueue);
  inherited;
end;

{ Инициализация перед началом печати }
procedure TFormatterPrinter.Clear;
begin
  inherited;
  TextBuilder.Clear;
  Shift := 0;
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
  { Если правила не сработали, ставим  пробел }
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

{ Вывод очередной лексемы с навешиванием всех наворотов по форматированию }
procedure TFormatterPrinter.PrintToken(AToken: TToken);

  { Добавление лексемы в очередь с разворачиванием и фильтрацией комментариев }
  procedure AddToken(AToken: TToken);
  begin
    if not Assigned(AToken) then exit;
    if (AToken is TComment) and (TComment(AToken).Position in SkipComments) then exit;
    AddToken(AToken.CommentFarAbove);
    AddToken(AToken.CommentAbove);
    TokenQueue.Enqueue(AToken);
    AddToken(AToken.CommentAfter);
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
    EOLLimit: integer;
  begin
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
    { Выставим флаги }
    FakeToken := (AToken.Col <= 0);
    EmptyToken := FakeToken and (AToken.Value = '');
    OriginalFormat := (OriginalFormatCount > 0);
    EmptyLine := EmptyLine and not OriginalFormat;
    ForceNextLine := ForceNextLine and not OriginalFormat;
    { В режиме исходного форматирования не печатаем "добавленных" лексем }
    if OriginalFormat and FakeToken then exit;
    { Для комментариев справа от текста приготовим выравнивающую линейку }
    if IsComment and not SpecComment and (AComment.Position = poAfter) then
      Ruler('right-comment');
    { Если это комментарий под предыдущей лексемой, надо урегулировать переводы строк }
    if IsComment and not OriginalFormat and not (FarPrevToken is TComment) then
    begin
      { Если комментарий далеко внизу, точно нужна пустая строка }
      if AComment.Position = poFarBelow then
        EmptyLine := true
      { Если комментарий прижат снизу, точно достаточно перевода строки }
      else if AComment.Position = poBelow then
        ForceNextLine := true
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
      if Assigned(Rulers) and RulerEnabled then
        case Mode of
          { В режиме примерки запомним ширину фрагмента }
          fpmGetRulers:
            Rulers.Take(RulerName, TextBuilder.Line, TextBuilder.Col);
          { В режиме вставим необходимое количество пробелов }
          fpmSetRulers:
            if not OriginalFormat then
              TextBuilder.AppendSpace(Rulers.Fix(RulerName) - TextBuilder.Col);
        end;
      RulerName := '';
    end;
    { Определим окончательное значение печатаемого текста }
    if IsComment and not LineComment then
      if (EOLLimit > 0) and (TextBuilder.Col = 1)
        then Value := AComment.ShiftedValue(Shift + 1)
        else Value := AComment.ShiftedValue(TextBuilder.Col)
    else if IsComment and LineComment and (SupNextLine > 0) and
            (AComment.Position = poAfter) and
            (ForceChangeCommentType or Settings.ChangeCommentType) then
      begin
        Value := '/* ' + Trim(AToken.Value.Substring(2)) + ' */';
        LineComment := false;
      end
    else
      Value := AToken.Value;
    Value := Trim(Value);
    { Учтём настройки замены лексем на синонимы и вывод в нижнем регистре }
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and not OriginalFormat and
       SameText(AToken.Value, 'default') and Settings.ReplaceDefault and AToken.CanReplace
      then Value := ':=';
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and not OriginalFormat and
       SameText(AToken.Value, 'as') and Settings.ReplaceAsIs and AToken.CanReplace
      then Value := 'is';
    { Выполним отступ }
    if (EOLLimit > 0) and (TextBuilder.Col = 1) and not OriginalFormat then TextBuilder.AppendSpace(Shift);
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
        poAfter   : ForceNextLine := LineComment;
      end;
    { Если это команда завершения исходного форматирования, сменим режим после печати }
    if IsComment and Value.StartsWith(C_UNFORMAT_STOP) then
      Dec(OriginalFormatCount);
  end;

begin
  { Добавим лексему в очередь и рекурсивно развернём все связанные с ней комментарии }
  AddToken(AToken);
  { Ну а теперь напечатаем очередь }
  while TokenQueue.Count > 0 do InternalPrint(TokenQueue.Dequeue);
end;

{ Вывод синтаксической конструкции с расстановкой выравниваний }
procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);

  { Печать конструкции и обработка пустых строк до и после конструкции }
  procedure SimplePrintStatement;
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

  { Сбор информации о выравниваниях }
  procedure CollectRulers;
  var DraftPrinter: TFormatterPrinter;
  begin
    if Assigned(AStatement.Rulers) then exit;
    DraftPrinter := TFormatterPrinter.Create(Self.Settings, false, [poFarAbove..poFarBelow], false);
    try
      DraftPrinter.Rulers := TRulers.Create;
      DraftPrinter.Rulers.UseSpaces := Settings.AlignUseSpace;
      DraftPrinter.Mode   := fpmGetRulers;
      DraftPrinter.BeginPrint;
      DraftPrinter.PrintItem(AStatement);
      DraftPrinter.EndPrint;
      AStatement.Rulers   := DraftPrinter.Rulers;
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
      Self.Rulers  := AStatement.Rulers;
      Self.Mode    := fpmSetRulers;
      Rulers.Shift := Self.Shift;
      SimplePrintStatement;
    finally
      Self.Rulers := _Rulers;
      Self.Mode   := _Mode;
    end;
  end;

begin
  if AStatement.Aligned and (Mode <> fpmGetRulers) then
    begin
      CollectRulers;
      PrintRuledStatement;
    end
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
end;

{ Вывод на принтер специального комментария (отсутствующего в исходном тексте)}
procedure TFormatterPrinter.PrintSpecialComment(AValue: string);
var T1, T2: TSpecialComment;
begin
  T1 := TSpecialComment.Create('/*/ ' + AValue);
  T2 := TSpecialComment.Create('/*/');
  try
    if not RulerEnabled then StartRuler(Settings.AlignSpecialComments, true);
    Self.PrintRulerItems('special-comment-start', [T1]);
    Self.PrintRulerItems('special-comment-finish', [T2]);
  finally
    FreeAndNil(T1);
    FreeAndNil(T2);
  end;
end;

{ Начало выравнивания в очередной строке }
procedure TFormatterPrinter.StartRuler(Enabled: boolean; Continued: boolean = false);
begin
  if not Assigned(Rulers) then exit;
  RulerEnabled := Enabled;
  if (Mode = fpmGetRulers) and Enabled then PrintEmptyToken;
end;

{ Установка "линейки" для выравнивания }
procedure TFormatterPrinter.Ruler(const ARuler: string);
begin
  if not Assigned(Rulers) or not RulerEnabled then exit;
  { Сразу запомним линейку, чтобы всегда сохранить их правильный порядок }
  Rulers.AddRuler(ARuler);
  { А остальную работу отложим до PrintToken, иначе возникают проблемы с
    пробелами, вставляемыми по SpaceRequired }
  RulerName := ARuler;
end;

procedure TFormatterPrinter.PrintRulerItems(const ARuler: string; AItems: array of TObject);
begin
  inherited;
  { В режиме примерки гарантируем вызов Rulers.Take даже при пустой ячейке,
    это позволит правильно собрать информацию и уберёт баги типа "неожиданно
    убегающих вправо" строчных комментариев. В режиме печати наоборот, не
    будем этого делать, тогда завершающие запятые в списках не будут отрываться
    от текста }
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

end.

