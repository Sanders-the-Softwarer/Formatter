////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Автотесты на модуль PLSQL                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestPLSQL;

interface

uses
  TestFramework, FileBasedTest;

type

  { Тесты на PL/SQL }
  _PLSQL = class(TFileBasedTest)
  published
    procedure Анонимный_Блок;
    procedure Блок_Declare_End;
    procedure Серии_Переменных_И_Операторов;
    procedure Обработчики_Исключений;
    procedure Заголовок_Пакета;
    procedure Тело_Пакета;
    procedure Процедура;
    procedure Функция;
    procedure Процедура_С_Параметрами;
    procedure Pipelined_Функция;
    procedure Deterministic_Функция;
    procedure Объявление_Переменных_В_Процедуре;
    procedure Типы_Переменных;
    procedure Параметры_In_Out_Nocopy;
    procedure Параметры_Значения_По_Умолчанию;
    procedure Выравнивание_В_Параметрах_Подпрограмм;
    procedure Константы_И_Значения_По_Умолчанию;
    procedure Выравнивание_В_Переменных;
    procedure Объявление_Курсора;
    procedure Параметризированный_Курсор;
    procedure Курсор_С_Типом_Результата;
    procedure Объявление_Исключений;
    procedure Прагмы;
    procedure Записи;
    procedure Табличные_Типы;
    procedure Оператор_Присваивания;
    procedure Вызовы_Процедур;
    procedure Выравнивание_В_Вызовах_Процедур;
    procedure Возврат_Из_Подпрограммы;
    procedure Оператор_Null;
    procedure Оператор_Raise;
    procedure Оператор_If;
    procedure Оператор_Case;
    procedure Оператор_Loop;
    procedure Оператор_For;
    procedure Оператор_While;
    procedure Оператор_ForAll;
    procedure Оператор_Pipe_Row;
    procedure Оператор_Open_For;
    procedure Оператор_Fetch;
    procedure Оператор_Close;
    procedure Оператор_Exit;
    procedure Оператор_Execute_Immediate;
    procedure Анонимный_Блок_С_Bind_Переменными;
    procedure Анонимный_Блок_С_Подстановками;
  public
    { отложим }
    procedure Комментарии_В_Пакете;
  public
    { отложим }
    procedure Пустота_Не_Должна_Сдвигать_Выравнивание;
  end;

implementation

{ _PLSQL }

procedure _PLSQL.Deterministic_Функция;
begin
end;

procedure _PLSQL.Pipelined_Функция;
begin
end;

procedure _PLSQL.Анонимный_Блок;
begin
end;

procedure _PLSQL.Анонимный_Блок_С_Bind_Переменными;
begin
end;

procedure _PLSQL.Анонимный_Блок_С_Подстановками;
begin
end;

procedure _PLSQL.Блок_Declare_End;
begin
end;

procedure _PLSQL.Возврат_Из_Подпрограммы;
begin
end;

procedure _PLSQL.Вызовы_Процедур;
begin
end;

procedure _PLSQL.Выравнивание_В_Вызовах_Процедур;
begin
  Settings.AlignVariables := true;
  Settings.ArgumentSingleLineParamLimit := 3;
end;

procedure _PLSQL.Выравнивание_В_Параметрах_Подпрограмм;
begin
  Settings.AlignVariables := true;
end;

procedure _PLSQL.Выравнивание_В_Переменных;
begin
  Settings.AlignVariables := true;
end;

procedure _PLSQL.Заголовок_Пакета;
begin
end;

procedure _PLSQL.Записи;
begin
end;

procedure _PLSQL.Комментарии_В_Пакете;
begin
end;

procedure _PLSQL.Пустота_Не_Должна_Сдвигать_Выравнивание;
begin
  Settings.AlignFields := true;
  Settings.AlignVariables := true;
end;

procedure _PLSQL.Константы_И_Значения_По_Умолчанию;
begin
end;

procedure _PLSQL.Курсор_С_Типом_Результата;
begin
end;

procedure _PLSQL.Обработчики_Исключений;
begin
end;

procedure _PLSQL.Объявление_Исключений;
begin
end;

procedure _PLSQL.Объявление_Курсора;
begin
end;

procedure _PLSQL.Объявление_Переменных_В_Процедуре;
begin
end;

procedure _PLSQL.Оператор_Case;
begin
end;

procedure _PLSQL.Оператор_Close;
begin
end;

procedure _PLSQL.Оператор_Execute_Immediate;
begin
end;

procedure _PLSQL.Оператор_Exit;
begin
end;

procedure _PLSQL.Оператор_Fetch;
begin
end;

procedure _PLSQL.Оператор_For;
begin
end;

procedure _PLSQL.Оператор_ForAll;
begin
end;

procedure _PLSQL.Оператор_If;
begin
end;

procedure _PLSQL.Оператор_Loop;
begin
end;

procedure _PLSQL.Оператор_Null;
begin
end;

procedure _PLSQL.Оператор_Open_For;
begin
end;

procedure _PLSQL.Оператор_Pipe_Row;
begin
end;

procedure _PLSQL.Оператор_Raise;
begin
end;

procedure _PLSQL.Оператор_While;
begin
end;

procedure _PLSQL.Оператор_Присваивания;
begin
end;

procedure _PLSQL.Параметризированный_Курсор;
begin
end;

procedure _PLSQL.Параметры_In_Out_Nocopy;
begin
end;

procedure _PLSQL.Параметры_Значения_По_Умолчанию;
begin
end;

procedure _PLSQL.Прагмы;
begin
end;

procedure _PLSQL.Процедура;
begin
end;

procedure _PLSQL.Процедура_С_Параметрами;
begin
end;

procedure _PLSQL.Серии_Переменных_И_Операторов;
begin
end;

procedure _PLSQL.Табличные_Типы;
begin
end;

procedure _PLSQL.Тело_Пакета;
begin
end;

procedure _PLSQL.Типы_Переменных;
begin
end;

procedure _PLSQL.Функция;
begin
end;

initialization
  RegisterTest(_PLSQL.Suite);

end.


