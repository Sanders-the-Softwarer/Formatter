////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                            Списки  ключевых слов                           //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Keywords;

{ ----- Примечания -------------------------------------------------------------

  Специфика оракловой грамматики в том, что не существует какого-либо вменяемого
  списка ключевых слов языка. В разных ситуациях одно и то же слово может как
  быть ключевым, так и нет. Например, в PL/SQL можно употреблять оператор
  delete, и это совершенно не мешает описать процедуру delete.

  По этой причине классы TKeyword и TIdentifier оказалось невозможным отделить
  друг от друга, пришлось сделать общий класс TEpithet и на ходу решать, что
  и где является ключевым словом, а что -  нет. Это, в свою очередь, порождает
  разнообразные проблемы синтаксического анализа, так как одна и та же
  последовательность <эпитет> <эпитет> может соответствовать вариантам,
  например:

    i integer;
    e exception;
    pragma serially_reusable;
    procedure delete;

  В итоге используется следующая модель: в каждом контексте (задаваемом
  разбираемой синтаксической конструкцией) используется свой список ключевых
  слов. Конструкции, не имеющие списка ключевых слов, наследуют его от
  родительской и могут его дополнять.

  Вести списки ключевых слов в классах синтаксических конструкций оказалось
  достаточно неудобным. Поэтому и был сделан отдельный синглтон, берущий на
  себя ведение подобных списков.

  P.S. "Родительство" в контексте данного модуля понимается двояко: в смысле
  иерархии классов и в смысле положения в синтаксическом дереве.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections, Tokens, Statements;

{ Проверка, является ли слово ключевым в контексте указанной конструкции }
function IsKeyword(const AValue: string; AStatement: TStatement): boolean; overload;

{ Регистрация списка ключевых слов, относящихся к заданной конструкции }
procedure RegisterKeywords(AStatementClass: TStatementClass; AKeywords: array of string);

{ Регистрация "меняющих контекст" синтаксических конструкций, не берущих
  родительских ключевых слов }
procedure RegisterOrphan(AStatementClass: TStatementClass);

implementation

var
  { Список классов-сирот }
  Orphans: TList<TClass>;
  { Списки ключевых слов, зарегистрированных по классам }
  Reserved: TDictionary<TClass, TStringList>;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//           Регистрация ключевых слов и синтаксических конструкций           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure RegisterOrphan(AStatementClass: TStatementClass);
begin
  Assert(AStatementClass <> nil);
  if Orphans.IndexOf(AStatementClass) < 0 then Orphans.Add(AStatementClass);
end;

function KeywordList: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := true;
  Result.CaseSensitive := false;
  Result.Duplicates := dupIgnore;
end;

procedure RegisterKeywords(AStatementClass: TStatementClass; AKeywords: array of string);
var Keyword: string;
begin
  if not Reserved.ContainsKey(AStatementClass) then Reserved.Add(AStatementClass, KeywordList);
  with Reserved[AStatementClass] do for Keyword in AKeywords do Add(Keyword);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   Проверка, является ли слово ключевым в контексте указанной конструкции   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function IsOrphan(AClass: TClass): boolean;
begin
  Result := (Orphans.IndexOf(AClass) >= 0);
end;

function IsKeyword(const AValue: string; AClass: TClass): boolean; overload;
begin
  Assert(AClass <> nil);
  Result := Reserved.ContainsKey(AClass) and (Reserved[AClass].IndexOf(AValue) >= 0)
            or
            (AClass <> TStatement) and IsKeyword(AValue, AClass.ClassParent);
end;

function IsKeyword(const AValue: string; AStatement: TStatement): boolean; overload;
var AClass: TClass;
begin
  if not Assigned(AStatement) then exit(false);
  AClass := AStatement.ClassType;
  Result := IsKeyword(AValue, AStatement.ClassType) or
            not IsOrphan(AClass) and IsKeyword(AValue, AStatement.Parent);
end;

initialization
  Orphans := TList<TClass>.Create;
  Reserved := TObjectDictionary<TClass, TStringList>.Create([doOwnsValues]);

finalization
  FreeAndNil(Orphans);
  FreeAndNil(Reserved);

end.
