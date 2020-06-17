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

uses Classes, SysUtils, System.Generics.Collections, Math, Utils;

type

  { Информация о выравниваниях }
  TRulers = class
  private
    { Линейки в том порядке, в котором они идут в обрабатываемой конструкции }
    Names: TStringList;
    { Информация о ячейках }
    Cells: array of array of array of integer;
    { Позиция, на которой мы остановились }
    PrevLine, PrevCol, PrevColIndex, MinLine, MaxLine, MaxColIndex: integer;
    { Отступ конструкции, относительно которого нормируем Take и Fix }
    FShift: integer;
    { Настройка заполнения пустот }
    FUseSpaces: boolean;
    { Флаг пропуска после строчного комментария }
    SkipUntilNewLine: boolean;
    { Итоговые позиции линеек }
    Rulers: TDictionary<string, integer>;
  protected
    function GetCell(ALine, ACol, AIndex: integer): integer;
    procedure SetCell(ALine, ACol, AIndex, AValue: integer);
    procedure CalcRulers;
    property Width[ALine, ACol: integer]: integer index 0 read GetCell write SetCell;
    property Carry[ALine, ACol: integer]: integer index 1 read GetCell write SetCell;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRuler(const ARuler: string);
    procedure Take(const ARuler: string; ALine, ACol: integer);
    function Fix(const ARuler: string): integer;
    function Empty: boolean;
    property Shift: integer read FShift write FShift;
    property UseSpaces: boolean read FUseSpaces write FUseSpaces;
  end;

implementation

constructor TRulers.Create;
begin
  Names := TStringList.Create;
end;

destructor TRulers.Destroy;
begin
  FreeAndNil(Rulers);
  FreeAndNil(Names);
  inherited;
end;

procedure TRulers.AddRuler(const ARuler: string);
begin
  if Names.IndexOf(ARuler) < 0 then Names.Add(ARuler);
end;

procedure TRulers.Take(const ARuler: string; ALine, ACol: integer);
var ColIndex, FactIndex: integer;
begin
  { Скорректируем данные о диапазоне строк, в которых идёт выравнивание }
  if MinLine = 0 then MinLine := ALine;
  if MaxLine < ALine then MaxLine := ALine;
  { Добавим очередную линейку в список, если её там ещё нет }
  AddRuler(ARuler);
  { Найдём её номер }
  ColIndex := Names.IndexOf(ARuler);
  { После комментария пропустим всё до нулевой колонки, стартующей новую строку }
  if SkipUntilNewLine then
    if ColIndex > 0
      then exit
      else SkipUntilNewLine := false;
  { При смене строки сбросим информацию о достигнутой в строке позиции }
  if ALine <> PrevLine then
  begin
    PrevCol := 1;
    PrevColIndex := -1;
  end;
  { Строчные комментарии могут спутать порядок вызовов, что приводит к
    ложной информации о ширине последней колонки (баг №10). Чтобы этого
    избежать, отловим ситуацию такого прыжка и скорректируем номер
    колонки }
  if ColIndex = PrevColIndex + 1 then
    FactIndex := ColIndex
  else
    begin
      FactIndex := PrevColIndex + 1;
      SkipUntilNewLine := true;
    end;
  { Сохраним ширину текущей ячейки }
  Width[ALine, FactIndex] := ACol - PrevCol - Shift;
  { Сохраним текущее положение, от которого будем отсчитывать следующую ячейку }
  PrevLine := ALine;
  PrevCol := ACol;
  PrevColIndex := ColIndex;
  MaxColIndex := Math.Max(MaxColIndex, FactIndex);
end;

function TRulers.Fix(const ARuler: string): integer;
begin
  if not Assigned(Rulers) then CalcRulers;
  if Rulers.ContainsKey(ARuler)
    then Result := Shift + Rulers[ARuler]
    else Result := 0;
end;

function TRulers.Empty: boolean;
begin
  Result := (Names.Count = 0);
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
    Inc(Ruler, Max);
    Rulers.Add(Names[Col], Ruler);
    { И, наконец, учтём хвосты, перелезающие в следующие ячейки }
    for Line := MinLine to MaxLine do
      if Carry[Line, Col] > Max then
        Carry[Line, Col + 1] := Carry[Line, Col] - Max;
  end;
end;

end.

