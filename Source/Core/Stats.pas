////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ‘орматизатор исходников                          //
//                                                                            //
//                           ћодуль сбора статистики                          //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Stats;

{ ----- ѕримечани€ -------------------------------------------------------------

  »значально дл€ выполнени€ в проекте синтаксического анализа был выбран подход,
  позвол€вший писать короткий, простой и хорошо сопровождаемый код в ущерб
  эффективности, котора€, скорее всего, на пор€дки хуже теоретически возможной.
  ѕри этом проект всЄ равно работает вполне достаточно быстро дл€ практических
  задач. “ем не менее, в ходе отладки не раз по€вл€лись подозрени€ в том, что
  из-за погрешностей реализации делаетс€ множество совершенно ненужных
  повторений одних и тех же действий, что приводит лишь к дополнительным
  проблемам.

  ћодуль сбора статистики предназначен дл€ получени€ количественной информации
  о работе модулей и классов форматизатора. ќн позволит выделить откровенно
  неадекватные решени€ и не оставл€ть незамеченными доработки, приведшие к
  новым провалам в эффективности.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections;

type
  Statistics = class
  strict private
    class var Measures: TDictionary<string, int64>;
  public
    class constructor Create;
    class destructor Destroy;
    { —брос статистики }
    class procedure Clear;
    { ”величение показател€ на единицу }
    class procedure Increase(const AMeasure: string);
    { ¬ывод накопленной статистики }
    class function Output: string;
  end;

implementation

uses Utils;

{ Statistics }

class constructor Statistics.Create;
begin
  Measures := TDictionary<string, int64>.Create;
end;

class destructor Statistics.Destroy;
begin
  FreeAndNil(Measures);
end;

{ —брос статистики }
class procedure Statistics.Clear;
begin
  Measures.Clear;
end;

{ ”величение показател€ на единицу }
class procedure Statistics.Increase(const AMeasure: string);
begin
  if not GetIsDebug then exit;
  if Measures.ContainsKey(AMeasure)
    then Measures[AMeasure] := Measures[AMeasure] + 1
    else Measures.Add(AMeasure, 1);
end;

{ ¬ывод накопленной статистики }
class function Statistics.Output: string;
var Key: string;
begin
  if not GetIsDebug then exit;
  with TStringList.Create do
  try
    for Key in Measures.Keys do
      Add(Key + '=' + IntToStr(Measures[Key]));
    Sort;
    Result := Text;
  finally
    Free;
  end;
end;

end.
