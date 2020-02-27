////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Автотесты  на выражения                          //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestExpressions;

interface

uses
  TestFramework, FileBasedTest;

type

  { Тесты на выражения }
  _Expressions = class(TFileBasedTest)
  published
    procedure Операнд_Может_Быть_Числом;
    procedure Операнд_Может_Иметь_Знак;
    procedure Операнд_Может_Быть_True_False_Или_Null;
    procedure Операнд_Может_Иметь_Префикс_Not;
    procedure Операнд_Может_Быть_Конструкцией_Case;
    procedure Операнд_Может_Быть_Выражением_Cast;
    procedure Операнд_Может_Быть_Выражением_Exists;
    procedure Операнд_Может_Быть_Идентификатором_И_Прочим_LValue;
    procedure В_Квалифицированных_Идентификаторах_Некстати_Попадаются_Пробелы;
    procedure В_Квалифицированных_Идентификаторах_Бывают_Скобки_Подряд;
    procedure Операнд_Может_Иметь_Суффикс;
    procedure Операнд_Может_Быть_Выражением_В_Скобках;
    procedure Операнд_Может_Быть_Вложенным_Запросом;
    procedure Операнд_Может_Сопровождаться_Постфиксными_Операциями;
    procedure Операнд_Может_Быть_Вызовом_Функции;
    procedure Выражения_Состоят_Из_Операндов_Связанных_Бинарными_Операциями;
    procedure Выражение_Может_Содержать_Multiset_Операции;
    procedure Несколько_Апострофов_Подряд_В_Литерале;
  public
    { отложим }
    procedure Форматирование_Case;
  published
    procedure And_И_Or_Большими_Буквами;
  end;

implementation

{ _Expressions }

procedure _Expressions.And_И_Or_Большими_Буквами;
begin
end;

procedure _Expressions.В_Квалифицированных_Идентификаторах_Бывают_Скобки_Подряд;
begin
end;

procedure _Expressions.В_Квалифицированных_Идентификаторах_Некстати_Попадаются_Пробелы;
begin
end;

procedure _Expressions.Выражение_Может_Содержать_Multiset_Операции;
begin
end;

procedure _Expressions.Выражения_Состоят_Из_Операндов_Связанных_Бинарными_Операциями;
begin
end;

procedure _Expressions.Несколько_Апострофов_Подряд_В_Литерале;
begin
end;

procedure _Expressions.Операнд_Может_Быть_True_False_Или_Null;
begin
end;

procedure _Expressions.Операнд_Может_Быть_Вложенным_Запросом;
begin
end;

procedure _Expressions.Операнд_Может_Быть_Вызовом_Функции;
begin
end;

procedure _Expressions.Операнд_Может_Быть_Выражением_Cast;
begin
end;

procedure _Expressions.Операнд_Может_Быть_Выражением_Exists;
begin
end;

procedure _Expressions.Операнд_Может_Быть_Выражением_В_Скобках;
begin
end;

procedure _Expressions.Операнд_Может_Быть_Идентификатором_И_Прочим_LValue;
begin
end;

procedure _Expressions.Операнд_Может_Быть_Конструкцией_Case;
begin
end;

procedure _Expressions.Операнд_Может_Быть_Числом;
begin
end;

procedure _Expressions.Операнд_Может_Иметь_Знак;
begin
end;

procedure _Expressions.Операнд_Может_Иметь_Префикс_Not;
begin
end;

procedure _Expressions.Операнд_Может_Иметь_Суффикс;
begin
end;

procedure _Expressions.Операнд_Может_Сопровождаться_Постфиксными_Операциями;
begin
end;

procedure _Expressions.Форматирование_Case;
begin
  Settings.PreferredExpressionLength := 60;
end;

initialization
  RegisterTest(_Expressions.Suite);

end.
