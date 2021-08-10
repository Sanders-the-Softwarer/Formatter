////////////////////////////////////////////////////////////////////////////////
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
  SysUtils, TestFramework, FileBasedTest, Printer;

type

  { Тесты на PL/SQL }
  _PLSQL = class(TFileBasedTest)
  published
    procedure Анонимный_Блок;
    procedure Блок_Declare_End;
    procedure Серии_Переменных_И_Операторов;
    procedure Обработчики_Исключений;
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
    procedure Оператор_Присваивания;
    procedure Вызовы_Процедур;
    procedure Анонимный_Блок_С_Bind_Переменными;
    procedure Анонимный_Блок_С_Подстановками;
    procedure Добавление_In;
  end;

  { Тесты на выравнивание конструкций PL/SQL }
  _Выравнивание = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Комментарий_Разрывает_Выравнивание_В_Блоке_Переменных;
    procedure Комментариев_Справа_От_Деклараций;
    procedure В_Comment;
    procedure Полей_В_Update;
    procedure Полей_В_Запросе;
    procedure Параметров_Подпрограмм;
    procedure Аргументов_В_Вызовах_Подпрограмм;
    procedure Условий_В_Where;
    procedure Таблиц_Во_From;
    procedure Параметров_В_Open_For;
    procedure Параметров_В_Execute_Immediate;
    procedure Команд_Set;
    procedure Команд_Whenever;
    procedure Команд_Define;
    procedure Грантов;
    procedure Синонимов;
    procedure Спецкомментариев_В_Select_Into;
    procedure Спецкомментариев_В_Insert_Values;
    procedure Спецкомментариев_В_Insert_Select;
    procedure Конкатенаций_По_Одинаковым_Символам;
    procedure Конкатенаций_С_Одинаковыми_Комбинациями_Символов;
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
    procedure Комментарии_В_Пакете;
    procedure Многострочный_Комментарий_Сдвигается_Целиком;
    procedure Комментарий_Не_Должен_Приводить_К_Переносу_Значения_На_Следующую_Строку;
  end;

  { Тесты на локальную отмену форматирования }
  _Отмена_Форматирования = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Отмена_Форматирования;
    procedure Вложенная_Отмена_Форматирования;
    procedure Предупреждение_О_Дисбалансе_Отмены_Форматирования;
    procedure Вставки_Лексем_И_Замены_Текста_При_Отмене_Форматирования;
  end;

  { Тесты на операторы PL/SQL }
  _Операторы = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Case_;
    procedure Close;
    procedure Execute_Immediate;
    procedure Exit;
    procedure Fetch_Into;
    procedure Fetch_Bulk_Collect_Limit;
    procedure For_;
    procedure ForAll;
    procedure Goto_;
    procedure If_;
    procedure Loop;
    procedure Null;
    procedure Open_For;
    procedure Open_For_Using;
    procedure Pipe_Row;
    procedure Raise_;
    procedure Return;
    procedure While_;
    procedure Прагма_Inline_В_Коде;
    procedure Прагмы;
  end;

  { Тесты на пакеты }
  _Пакеты = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure С_Указанием_AuthId;
    procedure Заголовок_Пакета;
    procedure Тело_Пакета;
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
    procedure Define_Собираются_Вместе;
    procedure Grant_Собираются_Вместе;
    procedure Set_Собираются_Вместе;
    procedure Whenever_Собираются_Вместе;
    procedure Синонимы_Собираются_Вместе;
    procedure Alter_Package_Собираются_Вместе;
  end;

  { Тесты на SQL типы }
  _SQL_Типы = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Object_;
    procedure Timestamp_With_Time_Zone;
    procedure Interval;
  end;

  { Тесты на PL/SQL типы }
  _PLSQL_Типы = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Ref_Cursor;
    procedure Ref_Cursor_Return;
    procedure Табличные_Типы;
    procedure Записи;
  end;

  { Тесты на форматирование выражений }
  _Форматирование_Выражений = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Перенос_При_Присвоении_Многострочного_Выражения;
    procedure Конкатенация_Должна_Делать_Перенос_По_Одинаковым_Символам;
  end;

  { Тесты на сопоставление полей }
  _Сопоставление_Полей = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Insert;
    procedure Insert_Select;
    procedure Select_Into;
  end;

  { Прочие фичи }
  _Прочее = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Переносы_По_And_Or;
  end;

  { Контрольные примеры - большие файлы, на которых проверяется глобально
    удовлетворительный результат }
  _Контрольные_Примеры = class(TFileBasedTest)
  protected
    function GetDir: string; override;
    function GetExtIn: string; override;
    function GetExtOut: string; override;
    function GetFileNameToSave(const AFileName: string): string; override;
    procedure SetUp; override;
  published
    procedure fm_cc_user;
    procedure fm_pc_sequences;
    procedure fm_pc_triggers;
    procedure fm_pc_user;
    procedure fm_pc_v_pcgi_link;
    procedure top_lc_calc_utils;
    procedure root_container_init;
    procedure top_report_api;
    procedure top_ref_api;
    procedure top_dev_style_sop_link_api;
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

procedure _PLSQL.Вызовы_Процедур;
begin
end;

procedure _PLSQL.Добавление_In;
begin
  Settings.AddInAccessSpecificator := true;
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

procedure _PLSQL.Процедура;
begin
end;

procedure _PLSQL.Процедура_С_Параметрами;
begin
end;

procedure _PLSQL.Серии_Переменных_И_Операторов;
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

procedure _Выравнивание.Конкатенаций_С_Одинаковыми_Комбинациями_Символов;
begin
  Settings.AlignExpressions := true;
  Settings.PreferredExpressionLength := 100;
end;

procedure _Выравнивание.Конкатенаций_По_Одинаковым_Символам;
begin
  PostponeTill(2021, 8, 1);
  Settings.AlignExpressions := true;
  Settings.PreferredExpressionLength := 80;
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
  PostponeTill(2021, 8, 1);
  Settings.AlignExpressions := true;
end;

procedure _Выравнивание.Таблиц_Во_From;
begin
  Settings.AlignFrom := true;
end;

procedure _Выравнивание.Аргументов_В_Вызовах_Подпрограмм;
begin
  Settings.AlignVariables := true;
  Settings.NamedArgumentSingleLineParamLimit := 3;
end;

procedure _Выравнивание.В_Comment;
begin
  Settings.AlignCommands := true;
end;

procedure _Выравнивание.Грантов;
begin
  Settings.AlignCommands := true;
end;

procedure _Выравнивание.Синонимов;
begin
  Settings.AlignCommands := true;
end;

procedure _Выравнивание.Спецкомментариев_В_Insert_Select;
begin
  Settings.AlignSpecialComments := true;
  Settings.MatchParamLimit := 1;
end;

procedure _Выравнивание.Спецкомментариев_В_Insert_Values;
begin
  Settings.AlignSpecialComments := true;
  Settings.MatchParamLimit := 1;
end;

procedure _Выравнивание.Спецкомментариев_В_Select_Into;
begin
  Settings.MatchParamLimit := 1;
  Settings.AlignSpecialComments := true;
end;

procedure _Выравнивание.Команд_Define;
begin
  Settings.AlignCommands := true;
end;

procedure _Выравнивание.Команд_Set;
begin
  Settings.AlignCommands := true;
end;

procedure _Выравнивание.Команд_Whenever;
begin
  Settings.AlignCommands := true;
end;

procedure _Выравнивание.Комментариев_Справа_От_Деклараций;
begin
  Settings.AlignVariables := true;
  Settings.AlignRightComments := true;
end;

{ _Комментарии }

function _Комментарии.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Комментарии';
end;

procedure _Комментарии.Комментарии_В_Пакете;
begin
end;

procedure _Комментарии.Комментарий_Не_Должен_Приводить_К_Переносу_Значения_На_Следующую_Строку;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Комментарии.Комментарий_Не_Должен_Убивать_Закрывающую_Скобку;
begin
  Settings.ChangeCommentType := true;
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

procedure _Пустые_Строки.Grant_Собираются_Вместе;
begin
  Settings.AlignCommands := true;
end;

procedure _Пустые_Строки.Alter_Package_Собираются_Вместе;
begin
end;

procedure _Пустые_Строки.at_Собираются_Вместе;
begin
end;

procedure _Пустые_Строки.Define_Собираются_Вместе;
begin
end;

procedure _Пустые_Строки.Exec_Собираются_Вместе;
begin
end;

procedure _Пустые_Строки.Set_Собираются_Вместе;
begin
end;

procedure _Пустые_Строки.Whenever_Собираются_Вместе;
begin
end;

procedure _Пустые_Строки.В_Пакете;
begin
end;

procedure _Пустые_Строки.В_Пакете_С_Блоком_Инициализации;
begin
end;

procedure _Пустые_Строки.Синонимы_Собираются_Вместе;
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

procedure _PLSQL_Типы.Ref_Cursor_Return;
begin
end;

procedure _PLSQL_Типы.Записи;
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

procedure _Операторы.If_;
begin
  Settings.PreferredExpressionLength := 100;
  Settings.BeautifyLongOperands := true;
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

procedure _Операторы.Return;
begin
end;

procedure _Операторы.While_;
begin
end;

procedure _Операторы.Прагма_Inline_В_Коде;
begin
end;

procedure _Операторы.Прагмы;
begin
end;

procedure _Операторы.Fetch_Into;
begin
end;

procedure _Операторы.Case_;
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

procedure _Операторы.For_;
begin
end;

procedure _Операторы.ForAll;
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

procedure _Пакеты.Заголовок_Пакета;
begin
end;

procedure _Пакеты.Тело_Пакета;
begin
end;

{ _SQL_Типы }

function _SQL_Типы.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\SQL Типы';
end;

procedure _SQL_Типы.Interval;
begin
end;

procedure _SQL_Типы.Object_;
begin
end;

procedure _SQL_Типы.Timestamp_With_Time_Zone;
begin
end;

{ _Отмена_Форматирования }

function _Отмена_Форматирования.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Отмена форматирования';
end;

procedure _Отмена_Форматирования.Вложенная_Отмена_Форматирования;
begin
end;

procedure _Отмена_Форматирования.Отмена_Форматирования;
begin
end;

procedure _Отмена_Форматирования.Предупреждение_О_Дисбалансе_Отмены_Форматирования;
begin
end;

procedure _Отмена_Форматирования.Вставки_Лексем_И_Замены_Текста_При_Отмене_Форматирования;
begin
  Settings.ReplaceDefault := true;
  Settings.ReplaceAsIs := true;
  Settings.AddInAccessSpecificator := true;
end;

{ _Разное }

function _Форматирование_Выражений.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Форматирование выражений';
end;

procedure _Форматирование_Выражений.Конкатенация_Должна_Делать_Перенос_По_Одинаковым_Символам;
begin
  Settings.PreferredExpressionLength := 100;
end;

procedure _Форматирование_Выражений.Перенос_При_Присвоении_Многострочного_Выражения;
begin
  Settings.PreferredExpressionLength := 100;
end;

{ _Контрольные_Примеры }

function _Контрольные_Примеры.GetDir: string;
begin
  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(inherited GetDir)) + '\Контрольные примеры';
end;

function _Контрольные_Примеры.GetExtIn: string;
begin
  { НЕ НАДО !! менять это расширение. Его суть в том, чтобы файлы не попали
    под маску команды git add и не уехали в репозиторий. Поскольку это файлы
    исходников Спортмастера, будет очень плохо опубликовать их таким образом }
  Result := '.original';
end;

function _Контрольные_Примеры.GetExtOut: string;
begin
  { НЕ НАДО !! менять это расширение. Его суть в том, чтобы файлы не попали
    под маску команды git add и не уехали в репозиторий. Поскольку это файлы
    исходников Спортмастера, будет очень плохо опубликовать их таким образом }
  Result := '.formatted';
end;

function _Контрольные_Примеры.GetFileNameToSave(const AFileName: string): string;
begin
  Result := AFileName + '.actual';
end;

procedure _Контрольные_Примеры.root_container_init;
begin
end;

procedure _Контрольные_Примеры.SetUp;
begin
  Settings := TFormatSettings.Default;
end;

procedure _Контрольные_Примеры.fm_pc_sequences;
begin
end;

procedure _Контрольные_Примеры.fm_pc_triggers;
begin
  PostponeTill(2021, 8, 1);
end;

procedure _Контрольные_Примеры.fm_pc_user;
begin
end;

procedure _Контрольные_Примеры.fm_pc_v_pcgi_link;
begin
  PostponeTill(2021, 8, 1);
end;

procedure _Контрольные_Примеры.fm_cc_user;
begin
end;

procedure _Контрольные_Примеры.top_dev_style_sop_link_api;
begin
  PostponeTill(2021, 8, 1);
end;

procedure _Контрольные_Примеры.top_lc_calc_utils;
begin
  PostponeTill(2021, 8, 1);
end;

procedure _Контрольные_Примеры.top_ref_api;
begin
  PostponeTill(2021, 8, 1);
end;

procedure _Контрольные_Примеры.top_report_api;
begin
  PostponeTill(2021, 8, 1);
end;

{ _Сопоставление_Полей }

function _Сопоставление_Полей.GetDir: string;
begin
  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(inherited GetDir)) + '\Фичи\Сопоставление полей';
end;

procedure _Сопоставление_Полей.Insert;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Сопоставление_Полей.Insert_Select;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Сопоставление_Полей.Select_Into;
begin
  Settings.MatchParamLimit := 5;
end;

{ _Прочее }

function _Прочее.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Прочее';
end;

procedure _Прочее.Переносы_По_And_Or;
begin
end;

initialization
  RegisterTest(_PLSQL.Suite);
  RegisterTest(_Выравнивание.Suite);
  RegisterTest(_Комментарии.Suite);
  RegisterTest(_Отмена_Форматирования.Suite);
  RegisterTest(_Сопоставление_Полей.Suite);
  RegisterTest(_Операторы.Suite);
  RegisterTest(_Пакеты.Suite);
  RegisterTest(_Пустые_Строки.Suite);
  RegisterTest(_SQL_Типы.Suite);
  RegisterTest(_PLSQL_Типы.Suite);
  RegisterTest(_Форматирование_Выражений.Suite);
  RegisterTest(_Контрольные_Примеры.Suite);
  RegisterTest(_Прочее.Suite);

end.


