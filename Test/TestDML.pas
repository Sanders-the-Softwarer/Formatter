////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Автотесты на модуль DML                          //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestDML;

interface

uses
  SysUtils, TestFramework, FileBasedTest;

type

  { Тесты команд DML }
  _Команды_DML = class(TFileBasedTest)
  end;

  { Тесты на select }
  _Select = class(TFileBasedTest)
  published
    procedure Запрос_С_Алиасом;
    procedure Запрос_С_Алиасами_В_Полях;
    procedure Запрос_С_Выражениями_В_Полях;
    procedure Запрос_С_Выражениями_И_Алиасами;
    procedure Связывание_Запросов_Через_Операции_Над_Множествами;
    procedure Форматирование_Подзапросов_В_Select;
    procedure Переносы_По_And_В_Where;
    procedure Commit_Rollback_Savepoint;
  end;

  { Тесты на SELECT }
  _Запросы = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Ansi_Синтаксис;
    procedure Bind_Переменные;
    procedure Bulk_Collect_Into;
    procedure Database_Links;
    procedure Distinct_Unique_All;
    procedure Distinct_Unique_All_В_Count;
    procedure For_Update;
    procedure Group_By;
    procedure Having;
    procedure Into;
    procedure Keep;
    procedure Lateral;
    procedure Left_Right_Full_Join;
    procedure Order_By;
    procedure Outer_Cross_Apply;
    procedure Start_With_И_Connect_By;
    procedure Using;
    procedure Where;
    procedure With_;
    procedure Из_Подзапроса;
    procedure Лишние_Слова_В_Joinах;
    procedure Аналитические_Функции;
    procedure Подзапрос_В_Select;
    procedure Подзапрос_В_Where;
    procedure Простейший;
    procedure С_Несколькими_Полями;
    procedure Направление_Сортировки;
    procedure Оракловый_Синтаксис_Внешних_Соединений;
    procedure Из_Вложенных_Структур_И_Табличных_Типов;
    procedure С_Подстановками;
    procedure Из_Нескольких_Таблиц;
    procedure Из_Табличной_Функции;
  end;

  { Тесты на insert }
  _Insert = class(TFileBasedTest)
  published
    procedure Простой_Insert;
    procedure Insert_С_Алиасом_Таблицы;
    procedure Insert_В_Подзапрос;
    procedure Insert_В_Подзапрос_С_Алиасом;
    procedure Insert_С_Указанием_Полей;
    procedure Insert_С_Returning;
    procedure Insert_С_Returning_С_Сопоставлением;
  end;

  { Тесты на update }
  _Update = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Простой_Update;
    procedure Update_С_Алиасом;
    procedure Update_С_Подзапросом;
    procedure Update_С_Множественными_Присваиваниями;
    procedure Update_С_Where;
    procedure Update_С_Returning;
    procedure Update_С_Returning_Bulk_Collect_Into;
    procedure Update_С_Сопоставлением_В_Returning;
  end;

  { Тесты на delete }
  _Delete = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Простой_Delete;
    procedure Delete_С_Алиасом;
    procedure Delete_С_Where;
    procedure Delete_С_Returning;
    procedure Delete_С_Returning_Bulk_Collect;
  end;

  { Тесты на merge }
  _Merge = class(TFileBasedTest)
  published
    procedure Простой_Merge;
    procedure Merge_С_Другим_Порядком_Выражений;
    procedure Merge_С_Подзапросом;
    procedure Merge_С_Insert_Where_Update_Delete_Where;
    procedure Merge_С_Последующим_Delete;
  end;

  { Функции с особым синтаксисом }
  _Функции_С_Особым_Синтаксисом = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure ListAgg;
    procedure Trim;
    procedure Extract;
  end;

implementation

{ _Select }

procedure _Select.Связывание_Запросов_Через_Операции_Над_Множествами;
begin
end;

procedure _Select.Запрос_С_Алиасом;
begin
end;

procedure _Select.Запрос_С_Алиасами_В_Полях;
begin
end;

procedure _Select.Запрос_С_Выражениями_В_Полях;
begin
end;

procedure _Select.Запрос_С_Выражениями_И_Алиасами;
begin
end;

procedure _Select.Форматирование_Подзапросов_В_Select;
begin
  Settings.PreferredExpressionLength := 80;
end;

procedure _Select.Переносы_По_And_В_Where;
begin
end;

procedure _Select.Commit_Rollback_Savepoint;
begin
end;

{ _Insert }

procedure _Insert.Insert_С_Returning;
begin
end;

procedure _Insert.Insert_В_Подзапрос;
begin
end;

procedure _Insert.Insert_В_Подзапрос_С_Алиасом;
begin
end;

procedure _Insert.Insert_С_Returning_С_Сопоставлением;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Insert.Insert_С_Алиасом_Таблицы;
begin
end;

procedure _Insert.Insert_С_Указанием_Полей;
begin
end;

procedure _Insert.Простой_Insert;
begin
end;

{ _Update }

function _Update.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Команды DML\Update';
end;

procedure _Update.Update_С_Returning;
begin
end;

procedure _Update.Update_С_Returning_Bulk_Collect_Into;
begin
end;

procedure _Update.Update_С_Where;
begin
end;

procedure _Update.Update_С_Алиасом;
begin
end;

procedure _Update.Update_С_Множественными_Присваиваниями;
begin
end;

procedure _Update.Update_С_Подзапросом;
begin
end;

procedure _Update.Update_С_Сопоставлением_В_Returning;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Update.Простой_Update;
begin
end;

{ _Delete }

procedure _Delete.Delete_С_Returning;
begin
end;

procedure _Delete.Delete_С_Returning_Bulk_Collect;
begin
end;

procedure _Delete.Delete_С_Where;
begin
end;

procedure _Delete.Delete_С_Алиасом;
begin
end;

procedure _Delete.Простой_Delete;
begin
end;

function _Delete.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Команды DML\Delete';
end;

{ _Merge }

procedure _Merge.Merge_С_Insert_Where_Update_Delete_Where;
begin
end;

procedure _Merge.Merge_С_Другим_Порядком_Выражений;
begin
end;

procedure _Merge.Merge_С_Подзапросом;
begin
end;

procedure _Merge.Merge_С_Последующим_Delete;
begin
end;

procedure _Merge.Простой_Merge;
begin
end;

{ _Запросы }

function _Запросы.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Запросы';
end;

procedure _Запросы.Ansi_Синтаксис;
begin
end;

procedure _Запросы.Lateral;
begin
end;

procedure _Запросы.Left_Right_Full_Join;
begin
end;

procedure _Запросы.Order_By;
begin
end;

procedure _Запросы.Outer_Cross_Apply;
begin
end;

procedure _Запросы.Start_With_И_Connect_By;
begin
end;

procedure _Запросы.Using;
begin
end;

procedure _Запросы.Where;
begin
end;

procedure _Запросы.With_;
begin
end;

procedure _Запросы.Аналитические_Функции;
begin
end;

procedure _Запросы.Из_Вложенных_Структур_И_Табличных_Типов;
begin
end;

procedure _Запросы.Из_Нескольких_Таблиц;
begin
end;

procedure _Запросы.Из_Подзапроса;
begin
end;

procedure _Запросы.Из_Табличной_Функции;
begin

end;

procedure _Запросы.Лишние_Слова_В_Joinах;
begin
end;

procedure _Запросы.Направление_Сортировки;
begin
end;

procedure _Запросы.Оракловый_Синтаксис_Внешних_Соединений;
begin
end;

procedure _Запросы.Подзапрос_В_Select;
begin
end;

procedure _Запросы.Подзапрос_В_Where;
begin
end;

procedure _Запросы.Простейший;
begin
end;

procedure _Запросы.С_Несколькими_Полями;
begin
end;

procedure _Запросы.С_Подстановками;
begin
end;

procedure _Запросы.Bind_Переменные;
begin
end;

procedure _Запросы.Bulk_Collect_Into;
begin
end;

procedure _Запросы.Database_Links;
begin
end;

procedure _Запросы.Distinct_Unique_All;
begin
end;

procedure _Запросы.Distinct_Unique_All_В_Count;
begin
end;

procedure _Запросы.For_Update;
begin
end;

procedure _Запросы.Group_By;
begin
end;

procedure _Запросы.Having;
begin
end;

procedure _Запросы.Into;
begin
end;

procedure _Запросы.Keep;
begin
  Settings.PreferredExpressionLength := 60;
end;

{ _Функции_С_Особым_Синтаксисом }

procedure _Функции_С_Особым_Синтаксисом.Extract;
begin
end;

function _Функции_С_Особым_Синтаксисом.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Функции с особым синтаксисом';
end;

procedure _Функции_С_Особым_Синтаксисом.ListAgg;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Функции_С_Особым_Синтаксисом.Trim;
begin
end;

initialization
  RegisterTest(_Команды_DML.Suite);
  RegisterTest('_Команды_DML', _Запросы.Suite);
  RegisterTest('_Команды_DML', _Select.Suite);
  RegisterTest('_Команды_DML', _Insert.Suite);
  RegisterTest('_Команды_DML', _Update.Suite);
  RegisterTest('_Команды_DML', _Delete.Suite);
  RegisterTest('_Команды_DML', _Merge.Suite);
  RegisterTest('_Команды_DML\_Запросы', _Функции_С_Особым_Синтаксисом.Suite);
end.

