////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Блок информации о выравниваниях                      //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Rulers;

{ ----- Примечание -------------------------------------------------------------

  Одной из основных фич форматизатора является выравнивание - то есть вывод
  логически связанных лексем "по колонкам". Например, группа переменных
  выводится не так:

      declare
        a integer; -- переменная А
        bbb varchar2(30); -- переменная Б

  а так:

      declare
        a   integer;      -- переменная А
        bbb varchar2(30); -- переменная Б

  Для того, чтобы этого добиться, класс TRulers собирает информацию о размерах
  отдельных элементов синтаксических конструкций и выдаёт рекомендации в виде
  набора "линеек", под которые они должны подравниваться.

  Технически выравнивание делается следующим образом: принтер сначала выдаёт
  текст в режиме fmGetRulers, при этом с помощью метода Measure собирая
  информацию о выравниваниях, а затем выдаёт его в режиме fmSetRulers, при этом
  с помощью метода Fix определяет необходимое в каждый момент число
  выравнивающих пробелов.

  Сам по себе алгоритм выравнивания действует следующим образом. Выводимый
  текст представляется заполняющим прямоугольную таблицу. Каждая ячейка
  характеризуется шириной, которая получается из величины заполняющего её
  текста. В простом случае каждая колонка выстраивается по ширине максимальной
  ячейки. При действии настройки "заполнять пустоты" вступает в действие
  механизм переноса. Его суть в том, что текст, справа от которого пустота,
  может "залезть" в следующую ячейку, поэтому ширина этого текста не учитывается
  при определении ширины колонки, а оставшийся сверх ширины колонки "хвост"
  идёт в зачёт ширины следующей колонки (и следующей, если у той тоже пустота
  справа...)

  Особенным образом приходится обрабатывать разделители (запятые как разделители
  полей, точки с запятой как разделители деклараций и т. п.) Выравниваемая
  конструкция состоит из нескольких "колонок", после которых идёт разделитель.
  Для корректной работы функционала, в том числе выравнивания комментариев
  справа и заполнения пустот, размер разделителя необходимо учитывать в составе
  последней непустой колонки каждой строки.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections, Math, Printer, Tokens;

const
  { Названия стандартных линеек для выравнивания }
  LEFT_RULER             = '$left$';
  RIGHT_COMMENT          = '$right-comment$';
  SPECIAL_COMMENT_START  = '$spec-comment-start$';
  SPECIAL_COMMENT_FINISH = '$spec-comment-finish$';
  CONCAT_DELIM_RULER     = '$concat-delim$';
  WHERE_DELIM_RULER      = '$where-delim$';

type
  { Информация о выравниваниях }
  TRulers = class
  private
    FOwner: TObject;
    { Текущая линейка }
    CurrentRuler: string;
    { Первая пользовательская линейка в строке }
    FirstRuler: string;
    { Линейки в том порядке, в котором они идут в обрабатываемой конструкции }
    Names: TStringList;
    { Информация о ячейках }
    Cells: array of array of array of integer;
    { Позиция на момент вызова Start }
    StartLine, StartCol: integer;
    { Прочие параметры измерения ширин }
    MinLine, MaxLine, MaxColIndex: integer;
    { Отступы конструкции, относительно которых нормируем Take и Fix }
    FShift: PInteger;
    { Настройка заполнения пустот }
    FUseSpaces: boolean;
    { Флаг заполненности информации }
    FFull: boolean;
    { Итоговые позиции линеек }
    Rulers: TDictionary<string, integer>;
    { Индекс для добавления следующей линейки }
    NextRulerIndex: integer;
    { Текущая лексема - для отладочной печати }
    FToken: TToken;
  protected
    function GetCell(ALine, ACol, AIndex: integer): integer;
    procedure SetCell(ALine, ACol, AIndex, AValue: integer);
    procedure CalcRulers;
    property Width[ALine, ACol: integer]: integer index 0 read GetCell write SetCell;
    property Carry[ALine, ACol: integer]: integer index 1 read GetCell write SetCell;
    property Delim[ALine, ACol: integer]: integer index 2 read GetCell write SetCell;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure AddRuler(const ARuler: string);
    procedure Start(const ARuler: string; ALine, ACol: integer);
    procedure Measure(ALine, ACol: integer; AFromStart: boolean = false);
    { Учесть ширину разделителя в списках }
    procedure ConsiderDelimiter(AWidth: integer);
    function Fix(const ARuler: string): integer;
    function Empty: boolean;
    property Shift: PInteger read FShift write FShift;
    property UseSpaces: boolean read FUseSpaces write FUseSpaces;
    property Full: boolean read FFull write FFull;
    property Owner: TObject read FOwner;
    property Token: TToken read FToken write FToken;
  public
    function DebugInfo: string;
    procedure Print(APrinter: TPrinter);
  end;

implementation

const
  Zero: integer = 0;

constructor TRulers.Create;
begin
  Names := TStringList.Create;
  Names.Add(LEFT_RULER);
  NextRulerIndex := Names.Add(RIGHT_COMMENT);
  Names.Add(SPECIAL_COMMENT_START);
  Names.Add(SPECIAL_COMMENT_FINISH);
  FOwner := AOwner;
  FShift := @Zero;
end;

destructor TRulers.Destroy;
begin
  FreeAndNil(Rulers);
  FreeAndNil(Names);
  inherited;
end;

procedure TRulers.AddRuler(const ARuler: string);
begin
  if Names.IndexOf(ARuler) >= 0 then exit;
  if FirstRuler = '' then FirstRuler := ARuler;
  Names.Insert(NextRulerIndex, ARuler);
  Inc(NextRulerIndex);
end;

procedure TRulers.Start(const ARuler: string; ALine, ACol: integer);
begin
  if (ARuler = FirstRuler) or ((FirstRuler = '') and (ARuler = SPECIAL_COMMENT_START)) then CurrentRuler := '';
  Measure(ALine, ACol, true);
  CurrentRuler := ARuler;
  StartLine := ALine;
  StartCol := ACol;
end;

procedure TRulers.Measure(ALine, ACol: integer; AFromStart: boolean = false);
var ColIndex: integer;
begin
  { При смене строки запоминаем левый отступ }
  if AFromStart and (ALine <> StartLine) and (CurrentRuler = '') then
  begin
    CurrentRuler := LEFT_RULER;
    StartLine := ALine;
    StartCol := Shift^ + 1;
    {$IFDEF DEBUG}
    if Assigned(Token) then
      Token.AddDebugInfo('[rulers = %p] :: Measure, ruler = %s, line = %d, col = %d, start col = %d', [pointer(Self), CurrentRuler, ALine, ACol, StartCol]);
    {$ENDIF}
  end;
  { Если вызваны без CurrentRuler - это технический вызов после перевода строки }
  if CurrentRuler = '' then exit;
  { Скорректируем данные о диапазоне строк, в которых идёт выравнивание }
  if MinLine = 0 then MinLine := ALine;
  if MaxLine < ALine then MaxLine := ALine;
  { Добавим очередную линейку в список, если её там ещё нет }
  AddRuler(CurrentRuler);
  { При смене строки внутри текста выйдем, чтобы ничего не портить }
  if not AFromStart and (ALine <> StartLine) then exit;
  { Найдём её номер }
  ColIndex := Names.IndexOf(CurrentRuler);
  {$IFDEF DEBUG}
  if Assigned(Token) then
    Token.AddDebugInfo('[%p] :: Measure, ruler = %s, index = %d, line = %d, col = %d, old width = %d, new width = %d', [pointer(Self), CurrentRuler, ColIndex, ALine, ACol, Width[ALine, ColIndex], ACol - StartCol]);
  {$ENDIF}
  { Сохраним ширину текущей ячейки }
  Width[ALine, ColIndex] := ACol - StartCol;
  MaxColIndex := Math.Max(MaxColIndex, ColIndex);
end;

{ Учесть ширину разделителя в списках }
procedure TRulers.ConsiderDelimiter(AWidth: integer);
var i: integer;
begin
  for i := MaxColIndex downto 0 do
    if Width[StartLine, i] > 0 then
    begin
      Delim[StartLine, i] := AWidth;
      break;
    end;
end;

function TRulers.Fix(const ARuler: string): integer;
begin
  if not Assigned(Rulers) then CalcRulers;
  if not Rulers.TryGetValue(ARuler, Result) then Result := 0;
end;

function TRulers.Empty: boolean;
begin
  Result := (Names.Count = 0);
end;

function TRulers.DebugInfo: string;
var
  i, j: integer;
  N: string;
begin
  Result := '';
  if not Assigned(Rulers) then exit;
  for i := 0 to Names.Count - 1 do
  begin
    N := Names[i];
    Result := Result + N;
    if Rulers.ContainsKey(N) then Result := Result + Format('[ruler = %d]', [Rulers[N]]);
    Result := Result + #13;
  end;
  for i := 0 to Names.Count - 1 do
    for j := MinLine to MaxLine do
      Result := Result + Format('width[%d, %d] = %d, carry = %d, delim = %d (%s)', [j, i, Width[j, i], Carry[j, i], Delim[j, i], Names[i]]) + #13;
end;

procedure TRulers.Print(APrinter: TPrinter);
begin
  APrinter.NextLine;
  APrinter.PrintSpecialComment(#13 + DebugInfo);
  APrinter.NextLine;
end;

function TRulers.GetCell(ALine, ACol, AIndex: integer): integer;
begin
  Dec(ALine, MinLine);
  if (AIndex < Length(Cells)) and (ALine < Length(Cells[AIndex])) and (ACol < Length(Cells[AIndex][ALine]))
    then Result := Cells[AIndex][ALine][ACol]
    else Result := 0;
end;

procedure TRulers.SetCell(ALine, ACol, AIndex, AValue: integer);
begin
  Dec(ALine, MinLine);
  if Length(Cells) <= AIndex then SetLength(Cells, AIndex + 1);
  if Length(Cells[AIndex]) <= ALine then SetLength(Cells[AIndex], ALine + 1);
  if Length(Cells[AIndex][ALine]) <= ACol then SetLength(Cells[AIndex][ALine], ACol + 1);
  Cells[AIndex][ALine][ACol] := AValue;
end;

procedure TRulers.CalcRulers;
var Col, Line, Max, Ruler: integer;
begin
  Rulers := TDictionary<string, integer>.Create;
  Ruler := 1;
  { Пройдём по колонкам слева направо }
  for Col := 0 to Names.Count - 1 do
  begin
    { Если можем переносить и у колонки нет правого соседа, записываем её
      ширину в переносимые, иначе в непереносимые }
    for Line := MinLine to MaxLine do
      if UseSpaces and (Width[Line, Col] > 0) and (Col < MaxColIndex) and (Width[Line, Col + 1] = 0) then
        begin Carry[Line, Col] := Width[Line, Col]; Width[Line, Col] := 0; end
      else if (Carry[Line, Col] > 0) and (Width[Line, Col + 1] > 0) then
        begin Width[Line, Col] := Carry[Line, Col]; Carry[Line, Col] := 0; end;
    { Вычислим максимальную ширину непереносимых ячеек }
    Max := 0;
    for Line := MinLine to MaxLine do
      Max := Math.Max(Max, Width[Line, Col]);
    { Сохраним позицию очередной направляющей }
    Rulers.Add(Names[Col], Ruler);
    Inc(Ruler, Max);
    { И, наконец, учтём хвосты, перелезающие в следующие ячейки }
    for Line := MinLine to MaxLine do
      if Carry[Line, Col] > Max then
        Carry[Line, Col + 1] := Carry[Line, Col] - Max;
  end;
end;

end.

