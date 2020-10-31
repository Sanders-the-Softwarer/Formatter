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
  текст в режиме fmGetRulers, при этом с помощью метода Take собирая информацию
  о выравниваниях, а затем выдаёт его в режиме fmSetRulers, при этом с помощью
  метода Fix определяет необходимое в каждый момент число выравнивающих
  пробелов.

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

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections, Math, Utils, Printer;

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
    { Отступ конструкции, относительно которого нормируем Take и Fix }
    FShift: integer;
    { Настройка заполнения пустот }
    FUseSpaces: boolean;
    { Флаг заполненности информации }
    FFull: boolean;
    { Итоговые позиции линеек }
    Rulers: TDictionary<string, integer>;
  protected
    function GetCell(ALine, ACol, AIndex: integer): integer;
    procedure SetCell(ALine, ACol, AIndex, AValue: integer);
    procedure CalcRulers;
    property Width[ALine, ACol: integer]: integer index 0 read GetCell write SetCell;
    property Carry[ALine, ACol: integer]: integer index 1 read GetCell write SetCell;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure AddRuler(const ARuler: string);
    procedure Start(const ARuler: string; ALine, ACol: integer);
    procedure Measure(ALine, ACol: integer; AFromStart: boolean = false);
    function Fix(const ARuler: string): integer;
    function Empty: boolean;
    property Shift: integer read FShift write FShift;
    property UseSpaces: boolean read FUseSpaces write FUseSpaces;
    property Full: boolean read FFull write FFull;
  public
    function DebugInfo: string;
    procedure Print(APrinter: TPrinter);
  end;

implementation

const
  LEFT_RULER = '$left$';

constructor TRulers.Create;
begin
  Names := TStringList.Create;
  Names.Add(LEFT_RULER);
  FOwner := AOwner;
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
  Names.Add(ARuler);
  if FirstRuler = '' then FirstRuler := ARuler;
end;

procedure TRulers.Start(const ARuler: string; ALine, ACol: integer);
begin
  if ARuler = FirstRuler then CurrentRuler := '';
  Measure(ALine, ACol, true);
  CurrentRuler := ARuler;
  StartLine := ALine;
  StartCol := ACol;
  Utils._Debug('[%p] Start, ruler = %s, line = %d, col = %d', [pointer(Self), ARuler, ALine, ACol]);
end;

procedure TRulers.Measure(ALine, ACol: integer; AFromStart: boolean = false);
var ColIndex: integer;
begin
  { При смене строки запоминаем левый отступ }
  if AFromStart and (ALine <> StartLine) and (CurrentRuler = '') then
  begin
    CurrentRuler := LEFT_RULER;
    StartLine := ALine;
    StartCol := 1;
  end;
  { Если вызваны без CurrentRuler - это технический вызов после перевода строки }
  if CurrentRuler = '' then exit;
  Utils._Debug('[%p] Measure, ruler = %s, line = %d, col = %d', [pointer(Self), CurrentRuler, ALine, ACol]);
  { Скорректируем данные о диапазоне строк, в которых идёт выравнивание }
  if MinLine = 0 then MinLine := ALine;
  if MaxLine < ALine then MaxLine := ALine;
  { Добавим очередную линейку в список, если её там ещё нет }
  AddRuler(CurrentRuler);
  { При смене строки внутри текста выйдем, чтобы ничего не портить }
  if not AFromStart and (ALine <> StartLine) then exit;
  { Найдём её номер }
  ColIndex := Names.IndexOf(CurrentRuler);
  CurrentRuler := '';
  { Сохраним ширину текущей ячейки }
  Width[ALine, ColIndex] := ACol - StartCol - Shift;
  Utils._Debug('[%p] Measure %s (%d, %d) => index = %d, width = %d', [pointer(Self), CurrentRuler, ALine, ACol, ColIndex, Width[ALine, ColIndex]]);
  MaxColIndex := Math.Max(MaxColIndex, ColIndex);
end;

function TRulers.Fix(const ARuler: string): integer;
begin
  if not Assigned(Rulers) then CalcRulers;
  if Rulers.ContainsKey(ARuler)
    then Utils._Debug('[%p] Fix %s, shift = %d, offset = %d', [pointer(Self), ARuler, Shift, Rulers[ARuler]])
    else Utils._Debug('[%p] Fix %s, shift = %d, - no ruler -', [pointer(Self), ARuler, Shift]);
  if Rulers.ContainsKey(ARuler)
    then Result := Shift + Rulers[ARuler]
    else Result := 0;
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
      Result := Result + Format('width[%d, %d] = %d, carry = %d (%s)', [j, i, Width[j, i], Carry[j, i], Names[i]]) + #13;
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

