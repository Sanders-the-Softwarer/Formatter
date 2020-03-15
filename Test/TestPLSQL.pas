﻿////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Автотесты на модуль PLSQL                         //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestPLSQL;

interface

uses
  SysUtils, TestFramework, FileBasedTest;

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
    procedure Константы_И_Значения_По_Умолчанию;
    procedure Объявление_Курсора;
    procedure Параметризированный_Курсор;
    procedure Курсор_С_Типом_Результата;
    procedure Объявление_Исключений;
    procedure Прагмы;
    procedure Записи;
    procedure Оператор_Присваивания;
    procedure Вызовы_Процедур;
    procedure Возврат_Из_Подпрограммы;
    procedure Оператор_If;
    procedure Оператор_Case;
    procedure Оператор_For;
    procedure Оператор_While;
    procedure Оператор_ForAll;
    procedure Анонимный_Блок_С_Bind_Переменными;
    procedure Анонимный_Блок_С_Подстановками;
    procedure Комментарии_В_Пакете;
    procedure Пустота_Не_Должна_Сдвигать_Выравнивание;
    procedure Добавление_In;
  end;

  { Тесты на выравнивание конструкций PL/SQL }
  _Выравнивание = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Комментарий_Разрывает_Выравнивание_В_Блоке_Переменных;
    procedure Комментариев_Справа_От_Деклараций;
    procedure Колонок_В_Таблице;
    procedure В_Comment;
    procedure Полей_В_Update;
    procedure Полей_В_Запросе;
    procedure Переменных;
    procedure Параметров_Подпрограмм;
    procedure Аргументов_В_Вызовах_Подпрограмм;
    procedure Условий_В_Where;
    procedure Параметров_В_Open_For;
    procedure Параметров_В_Execute_Immediate;
  end;

  { Тесты на расстановку комментариев }
  _Комментарии = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Отдельно_Стоящие_Комментарии;
    procedure Комментарий_Справа_От_Переменной;
    procedure Комментарий_Не_Должен_Убивать_Закрывающую_Скобку;
    procedure Заголовочный_Комментарий;
    procedure Многострочный_Комментарий_Сдвигается_Целиком;
    procedure Комментарий_Не_Должен_Приводить_К_Переносу_Значения_На_Следующую_Строку;
  end;

  { Тесты на операторы PL/SQL }
  _Операторы = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Close;
    procedure Execute_Immediate;
    procedure Exit;
    procedure Fetch_Into;
    procedure Fetch_Bulk_Collect_Limit;
    procedure Goto_;
    procedure Loop;
    procedure Null;
    procedure Open_For;
    procedure Open_For_Using;
    procedure Pipe_Row;
    procedure Raise_;
    procedure Прагма_Inline_В_Коде;
  end;

  { Тесты на пакеты }
  _Пакеты = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure С_Указанием_AuthId;
  end;

  { Тесты на расстановку пустых строк }
  _Пустые_Строки = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure В_Пакете;
    procedure В_Пакете_С_Блоком_Инициализации;
    procedure at_Собираются_Вместе;
    procedure Exec_Собираются_Вместе;
    procedure Set_Собираются_Вместе;
  end;

  { Тесты на SQL типы }
  _SQL_Типы = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Object_;
    procedure Timestamp_With_Time_Zone;
  end;

  { Тесты на PL/SQL типы }
  _PLSQL_Типы = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Ref_Cursor;
    procedure Ref_Cursor_Returning;
    procedure Табличные_Типы;
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

procedure _PLSQL.Добавление_In;
begin
  Settings.AddInAccessSpecificator := true;
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

procedure _PLSQL.Оператор_For;
begin
end;

procedure _PLSQL.Оператор_ForAll;
begin
end;

procedure _PLSQL.Оператор_If;
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

procedure _PLSQL.Тело_Пакета;
begin
end;

procedure _PLSQL.Типы_Переменных;
begin
end;

procedure _PLSQL.Функция;
begin
end;

{ _Выравнивание }

function _Выравнивание.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Выравнивание';
end;

procedure _Выравнивание.Комментарий_Разрывает_Выравнивание_В_Блоке_Переменных;
begin
  Settings.AlignVariables := true;
end;

procedure _Выравнивание.Параметров_В_Execute_Immediate;
begin
  Settings.AlignVariables := true;
end;

procedure _Выравнивание.Параметров_В_Open_For;
begin
  Settings.AlignVariables := true;
end;

procedure _Выравнивание.Параметров_Подпрограмм;
begin
  Settings.AlignVariables := true;
end;

procedure _Выравнивание.Переменных;
begin
  Settings.AlignVariables := true;
end;

procedure _Выравнивание.Полей_В_Update;
begin
  Settings.AlignFields := true;
end;

procedure _Выравнивание.Полей_В_Запросе;
begin
  Settings.AlignFields := true;
end;

procedure _Выравнивание.Условий_В_Where;
begin
  Settings.AlignExpressions := true;
end;

procedure _Выравнивание.Аргументов_В_Вызовах_Подпрограмм;
begin
  Settings.AlignVariables := true;
  Settings.NamedArgumentSingleLineParamLimit := 3;
end;

procedure _Выравнивание.В_Comment;
begin
  Settings.AlignTableColumnComments := true;
end;

procedure _Выравнивание.Колонок_В_Таблице;
begin
  Settings.AlignColumns := true;
end;

procedure _Выравнивание.Комментариев_Справа_От_Деклараций;
begin
  Settings.AlignVariables := true;
end;

{ _Комментарии }

function _Комментарии.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Комментарии';
end;

procedure _Комментарии.Комментарий_Не_Должен_Приводить_К_Переносу_Значения_На_Следующую_Строку;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Комментарии.Комментарий_Не_Должен_Убивать_Закрывающую_Скобку;
begin
end;

procedure _Комментарии.Заголовочный_Комментарий;
begin
end;

procedure _Комментарии.Многострочный_Комментарий_Сдвигается_Целиком;
begin
end;

procedure _Комментарии.Комментарий_Справа_От_Переменной;
begin
end;

procedure _Комментарии.Отдельно_Стоящие_Комментарии;
begin
end;

{ _Пустые_Строки }

function _Пустые_Строки.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Пустые строки';
end;

procedure _Пустые_Строки.at_Собираются_Вместе;
begin
end;

procedure _Пустые_Строки.Exec_Собираются_Вместе;
begin
end;

procedure _Пустые_Строки.Set_Собираются_Вместе;
begin
end;

procedure _Пустые_Строки.В_Пакете;
begin
end;

procedure _Пустые_Строки.В_Пакете_С_Блоком_Инициализации;
begin
end;

{ _PLSQL_Типы }

function _PLSQL_Типы.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\PLSQL Типы';
end;

procedure _PLSQL_Типы.Ref_Cursor;
begin
end;

procedure _PLSQL_Типы.Ref_Cursor_Returning;
begin
end;

procedure _PLSQL_Типы.Табличные_Типы;
begin
end;

{ _Операторы }

function _Операторы.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Операторы';
end;

procedure _Операторы.Goto_;
begin
end;

procedure _Операторы.Loop;
begin
end;

procedure _Операторы.Null;
begin
end;

procedure _Операторы.Open_For;
begin
end;

procedure _Операторы.Open_For_Using;
begin
end;

procedure _Операторы.Pipe_Row;
begin
end;

procedure _Операторы.Raise_;
begin
end;

procedure _Операторы.Прагма_Inline_В_Коде;
begin
end;

procedure _Операторы.Fetch_Into;
begin
end;

procedure _Операторы.Close;
begin
end;

procedure _Операторы.Execute_Immediate;
begin
end;

procedure _Операторы.Exit;
begin
end;

procedure _Операторы.Fetch_Bulk_Collect_Limit;
begin
end;

{ _Пакеты }

function _Пакеты.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Пакеты';
end;

procedure _Пакеты.С_Указанием_AuthId;
begin
end;

{ _SQL_Типы }

function _SQL_Типы.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\SQL Типы';
end;

procedure _SQL_Типы.Object_;
begin
end;

procedure _SQL_Типы.Timestamp_With_Time_Zone;
begin
end;

initialization
  RegisterTest(_PLSQL.Suite);
  RegisterTest(_Выравнивание.Suite);
  RegisterTest(_Комментарии.Suite);
  RegisterTest(_Операторы.Suite);
  RegisterTest(_Пакеты.Suite);
  RegisterTest(_Пустые_Строки.Suite);
  RegisterTest(_SQL_Типы.Suite);
  RegisterTest(_PLSQL_Типы.Suite);

end.


