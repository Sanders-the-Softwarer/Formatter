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

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections, Math, Utils;

type
  { Информация о выравниваниях }
  TRulers = class
  private
    { Линейки в том порядке, в котором они идут в обрабатываемой конструкции }
    Names: TStringList;
    { Максимальные ширины ячеек }
    MaxWidth: TDictionary<String, integer>;
    { Позиция, на которой мы остановились }
    PrevLine, PrevCol: integer;
    { Отступ конструкции, относительно которого нормируем Take и Fix }
    FShift: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRuler(const ARuler: string);
    procedure Take(const ARuler: string; ALine, ACol: integer);
    function Fix(const ARuler: string): integer;
    function Empty: boolean;
    property Shift: integer read FShift write FShift;
  end;

implementation

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

procedure TRulers.AddRuler(const ARuler: string);
begin
  if Names.IndexOf(ARuler) >= 0 then exit;
  Names.Add(ARuler);
  MaxWidth.Add(ARuler, 0);
end;

procedure TRulers.Take(const ARuler: string; ALine, ACol: integer);
begin
  AddRuler(ARuler);
  if ALine <> PrevLine then PrevCol := 1;
  PrevLine := ALine;
  MaxWidth[ARuler] := Math.Max(MaxWidth[ARuler], ACol - PrevCol - Shift);
  PrevCol := ACol;
end;

function TRulers.Fix(const ARuler: string): integer;
var i: integer;
begin
  Result := Shift;
  for i := 0 to Names.IndexOf(ARuler) do Inc(Result, MaxWidth[Names[i]]);
  Inc(Result);
end;

function TRulers.Empty: boolean;
begin
  Result := (Names.Count = 0);
end;

end.
