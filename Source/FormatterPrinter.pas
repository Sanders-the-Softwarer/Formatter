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
    NextLineComment: TToken;
    EmptyLine: boolean;
    WasComment: boolean;
    Line:    integer;
    Col:     integer;
    Rulers:  TRulers;
    PaddingCol: integer;
    PrevToken: TToken;
    SpecialComments: TObjectList<TToken>;
    SupSpace, SupNextLine: integer;
    IsDraft: boolean;
    Text:    string;
    RulerEnabled: boolean;
  strict protected
    TokenPos, TokenLen: TDictionary<TToken, integer>;
    function SpaceRequired(ALeft, ARight: TToken): boolean;
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
    procedure CancelNextLine; override;
    procedure SupressNextLine(ASupress: boolean); override;
    procedure SupressSpaces(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure StartRuler(Enabled: boolean); override;
  protected
    procedure Ruler(const ARuler: string); override;
  public
    function MakeDraftPrinter: TPrinter; override;
    function CurrentCol: integer; override;
    function  GetText: string; override;
  end;

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
  FreeAndNil(Builder);
  Builder := TStringBuilder.Create;
  Shift   := 0;
  Line    := 1;
  Col     := 1;
  ForceNextLine := false;
  EOLCount := 666; { начало текста съедает стартовые пустые строки }
  PaddingCol := 0;
  TokenPos.Clear;
  TokenLen.Clear;
  SpecialComments.Clear;
  NextLineComment := nil;
end;

{ Вывод готового результата }
procedure TFormatterPrinter.EndPrint;
var T: TToken;
begin
  if Assigned(NextLineComment) then
  begin
    EmptyLine := false;
    ForceNextLine := false;
    T := NextLineComment;
    NextLineComment := nil;
    PrintToken(T);
  end;
  if Assigned(Builder) then Text := Builder.ToString else Text := '';
  FreeAndNil(Builder);
  PrevToken := nil;
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

{ Вывод очередной лексемы с навешиванием всех наворотов по форматированию }
procedure TFormatterPrinter.PrintToken(AToken: TToken);
var
  Value: string;
  HasToken, HasBuilder, AllowLF: boolean;
  EOLLimit: integer;
begin
  HasToken := Assigned(AToken);
  HasBuilder := Assigned(Builder);
  AllowLF := (SupNextLine = 0) or WasComment;
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
  while (EOLCount < EOLLimit) and AllowLF do
  begin
    if Assigned(NextLineComment) then
    begin
      Ruler('right-comment');
      PrintToken(NextLineComment);
      NextLineComment := nil;
    end;
    if HasBuilder then Builder.AppendLine;
    Inc(EOLCount);
    Inc(Line);
    Col := 1;
    PrevToken := nil;
    EmptyLine := false;
    ForceNextLine := false;
  end;
  { Если до лексемы есть комментарии, напечатаем их }
  if HasToken and HasBuilder and not IsDraft then
  begin
    if Assigned(AToken.CommentFarAbove) then
    begin
      EmptyLine := true;
      PrintToken(AToken.CommentFarAbove);
      EmptyLine := true;
      PrintToken(nil);
    end;
    if Assigned(AToken.CommentAbove) then
    begin
      NextLine;
      PrintToken(AToken.CommentAbove);
      NextLine;
      PrintToken(nil);
    end;
  end;
  { Если задано выравнивание, вставим соответствующее количество пробелов }
  if (PaddingCol > Col) and HasToken then
  begin
    _Debug('Padding, col = %d, padding col = %d', [Col, PaddingCol]);
    if EOLCount = 0 then Dec(PaddingCol, Shift);
    if HasBuilder then Builder.Append(StringOfChar(' ', PaddingCol - Col));
    Col := PaddingCol;
    PaddingCol := 0;
    EOLCount := 0;
  end;
  { Если нужно, внедрим пробел между предыдущей и новой лексемами }
  if Assigned(PrevToken) and HasToken and SpaceRequired(PrevToken, AToken) then
  begin
    if HasBuilder then Builder.Append(' ');
    Inc(Col);
    EOLCount := 0;
  end;
  { И, наконец, если задана лексема - напечатаем её }
  if HasToken then
  begin
    if AToken is TComment then
      if EOLCount > 0 then
        Value := TComment(AToken).ShiftedValue(Shift + 1)
      else
        Value := TComment(AToken).ShiftedValue(Col)
    else
      Value := AToken.Value;
    Value := StringReplace(Value, #13, #13#10, [rfReplaceAll]);
    { Учтём настройки замены лексем на синонимы и вывод в нижнем регистре }
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and
       SameText(AToken.Value, 'default') and Settings.ReplaceDefault and AToken.CanReplace
      then Value := ':=';
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and
       SameText(AToken.Value, 'as') and Settings.ReplaceAsIs and AToken.CanReplace
      then Value := 'is';
    { В режиме реальной печати - напечатаем лексему, иначе только учтём сдвиг позиции }
    if HasBuilder then
    begin
      if EOLCount > 0 then Builder.Append(StringOfChar(' ', Shift));
      if not IsDraft and not (AToken is TSpecialComment) then
      begin
        TokenPos.Add(AToken, Builder.Length);
        TokenLen.Add(AToken, Value.Length);
      end;
      Builder.Append(Value);
      AToken.Printed := true;
    end;
    if EOLCount > 0 then Inc(Col, Shift);
    Inc(Col, Value.Length);
    PrevToken := AToken;
    EOLCount := 0;
  end;
  { Если это был комментарий, взведём флажок }
  if HasToken then WasComment := AToken is TComment;
  { Если после лексемы есть комментарии, напечатаем их }
  if HasToken then
  begin
    if Assigned(AToken.CommentAfter) then
      if AToken.CommentAfter.LineComment
        then NextLineComment := AToken.CommentAfter
        else PrintToken(AToken.CommentAfter);
    if Assigned(AToken.CommentBelow) and HasBuilder then
    begin
      WasComment := true;
      NextLine;
      PrintToken(AToken.CommentBelow);
      NextLine;
    end;
    if Assigned(AToken.CommentFarBelow) and HasBuilder then
    begin
      WasComment := true;
      EmptyLine := true;
      PrintToken(AToken.CommentFarBelow);
      EmptyLine := true;
    end;
  end;
end;

{ Вывод синтаксической конструкции с расстановкой выравниваний }
procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);

  var
    _Mode: TFormatterPrinterMode;
    _Shift, _Col, _Line, _PaddingCol, _EOLCount: integer;
    _ForceNextLine, _EmptyLine, _WasComment: boolean;
    _Builder: TStringBuilder;
    _Rulers: TRulers;
    _PrevToken, _NextLineComment: TToken;

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
    if Shift <> _Shift then
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
    _ForceNextLine := ForceNextLine;
    _EmptyLine := EmptyLine;
    _WasComment := WasComment;
    _Builder := Builder;
    _Rulers  := Rulers;
    _PrevToken := PrevToken;
    _Mode    := Mode;
    _NextLineComment := NextLineComment;
  end;

  { Восстановление той части конфигурации, которая нужна для печати с выравниваниями }
  procedure RestoreCfgBeforePrint;
  begin
    Shift := _Shift;
    Col   := _Col;
    Line  := _Line;
    PaddingCol := _PaddingCol;
    EOLCount   := _EOLCount;
    ForceNextLine := _ForceNextLine;
    EmptyLine := _EmptyLine;
    WasComment := _WasComment;
    Builder := _Builder;
    PrevToken := _PrevToken;
    NextLineComment := _NextLineComment;
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
  ForceNextLine := true;
end;

{ Отмена перехода на следующую строку }
procedure TFormatterPrinter.CancelNextLine;
begin
  ForceNextLine := false;
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
  PrintItem(nil);
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
  _Debug('Rulers.NewLine prev = %d, line = %d', [PrevLine, ALine]);
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
var i: integer; S: string;
begin
  Result := 0;
  if DisablePadding then exit;
  S := '1';
  for i := 0 to Names.IndexOf(ARuler) do
  begin
    Inc(Result, MaxWidth[Names[i]]);
    S := S + ' + ' + IntToStr(MaxWidth[Names[i]]) + ' {' + Names[i] + '}';
  end;
  Inc(Result);
  _Debug('Rulers.Fix, ruler = "%s", fix = %d', [ARuler, Result]);
  _Debug('Sum = %s', [S]);
end;

end.

