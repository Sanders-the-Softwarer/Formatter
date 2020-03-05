////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Автотесты на модуль DML                          //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestDML;

interface

uses
  TestFramework, FileBasedTest;

type

  { Тесты на select }
  _Select = class(TFileBasedTest)
  published
    procedure Простейший_Запрос;
    procedure Запрос_С_With;
    procedure Запрос_С_Алиасом;
    procedure Запрос_С_Несколькими_Полями;
    procedure Запрос_С_Алиасами_В_Полях;
    procedure Запрос_С_Выражениями_В_Полях;
    procedure Запрос_С_Выражениями_И_Алиасами;
    procedure Запрос_С_Into;
    procedure Запрос_С_Bulk_Collect_Into;
    procedure Запрос_Из_Нескольких_Таблиц;
    procedure Запрос_Из_Табличной_Функции;
    procedure Запрос_Из_Подзапроса;
    procedure Запрос_Из_Вложенных_Структур_И_Табличных_Типов;
    procedure Запрос_С_Lateral;
    procedure Запрос_С_Where;
    procedure Запрос_С_Group_By;
    procedure Запрос_С_Having;
    procedure Запрос_Со_Start_With_И_Connect_By;
    procedure Запрос_С_Order_By;
    procedure Запрос_С_Bind_Переменными;
    procedure Запрос_С_Подстановками;
    procedure Указание_Направления_Сортировки_И_Расположения_Null_В_Order_By;
    procedure Связывание_Запросов_Через_Операции_Над_Множествами;
    procedure Выравнивание_Полей_В_Запросе;
    procedure Сопоставление_Полей_В_Into;
    procedure Сопоставление_Полей_В_Into_С_Выравниванием;
    procedure Запрос_С_Ansi_Синтаксисом;
    procedure Запрос_С_Using;
    procedure Запрос_С_Left_Right_Full_Join;
    procedure Запрос_С_Лишними_Словами_В_Joinах;
    procedure Запрос_С_Оракловым_Синтаксисом_Внешних_Соединений;
    procedure Запрос_С_Outer_Cross_Apply;
    procedure Запрос_С_Distinct_Unique_All;
    procedure Distinct_Unique_All_В_Count;
    procedure Подзапрос_В_Select;
    procedure Подзапрос_В_Where;
    procedure В_Запросе_Можно_Употреблять_Аналитические_Функции;
    procedure В_Запросе_Можно_Употреблять_Функцию_Listagg;
    procedure В_Запросе_Можно_Употреблять_Функцию_Listagg_С_Переносом;
    procedure В_Запросе_Можно_Употреблять_Конструкцию_Keep;
    procedure В_Запросе_Можно_Употреблять_Конструкцию_Keep_С_Переносом;
    procedure Форматирование_Подзапросов_В_Select;
    procedure Переносы_По_And_В_Where;
  public
    { отложим }
    procedure Выравнивание_Условий_В_Where;
  published
    procedure Commit_Rollback_Savepoint;
  public
    { отложим }
    procedure Длинный_И_Сложный_Запрос_В_Форматировании_Которого_Куча_Мелких_Недостатков;
  public
    { отложим }
    procedure Комментарий_Не_Должен_Приводить_К_Переносу_Значения_На_Следующую_Строку;
  end;

  { Тесты на insert }
  _Insert = class(TFileBasedTest)
  published
    procedure Простой_Insert;
    procedure Insert_С_Алиасом_Таблицы;
    procedure Insert_В_Подзапрос;
    procedure Insert_В_Подзапрос_С_Алиасом;
    procedure Insert_С_Указанием_Полей;
    procedure Insert_С_Сопоставлением_Полей;
    procedure Insert_Select_С_Сопоставлением_Полей;
    procedure Insert_С_Returning;
    procedure Insert_С_Returning_С_Сопоставлением;
  end;

  { Тесты на update }
  _Update = class(TFileBasedTest)
  published
    procedure Простой_Update;
    procedure Update_С_Алиасом;
    procedure Update_С_Подзапросом;
    procedure Update_С_Множественными_Присваиваниями;
    procedure Выравнивание_В_Update;
    procedure Update_С_Where;
    procedure Update_С_Returning;
    procedure Update_С_Сопоставлением_В_Returning;
  end;

  { Тесты на delete }
  _Delete = class(TFileBasedTest)
  published
    procedure Простой_Delete;
    procedure Delete_С_Алиасом;
    procedure Delete_С_Where;
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

implementation

{ _Select }

procedure _Select.Подзапрос_В_Select;
begin
end;

procedure _Select.Подзапрос_В_Where;
begin
end;

procedure _Select.Простейший_Запрос;
begin
end;

procedure _Select.Связывание_Запросов_Через_Операции_Над_Множествами;
begin
end;

procedure _Select.Сопоставление_Полей_В_Into;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Select.Сопоставление_Полей_В_Into_С_Выравниванием;
begin
  Settings.MatchParamLimit := 3;
  Settings.AlignSpecialComments := true;
end;

procedure _Select.Указание_Направления_Сортировки_И_Расположения_Null_В_Order_By;
begin
end;

procedure _Select.Запрос_С_Where;
begin
end;

procedure _Select.Запрос_С_With;
begin
end;

procedure _Select.Запрос_С_Алиасом;
begin
end;

procedure _Select.Запрос_С_Несколькими_Полями;
begin
end;

procedure _Select.Запрос_С_Оракловым_Синтаксисом_Внешних_Соединений;
begin
end;

procedure _Select.Запрос_С_Подстановками;
begin
end;

procedure _Select.Запрос_Со_Start_With_И_Connect_By;
begin
end;

procedure _Select.Комментарий_Не_Должен_Приводить_К_Переносу_Значения_На_Следующую_Строку;
begin
  Settings.PreferredExpressionLength := 60;
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

procedure _Select.Запрос_С_Лишними_Словами_В_Joinах;
begin
end;

procedure _Select.Запрос_С_Into;
begin
end;

procedure _Select.Distinct_Unique_All_В_Count;
begin
end;

procedure _Select.В_Запросе_Можно_Употреблять_Аналитические_Функции;
begin
end;

procedure _Select.В_Запросе_Можно_Употреблять_Конструкцию_Keep;
begin
end;

procedure _Select.В_Запросе_Можно_Употреблять_Конструкцию_Keep_С_Переносом;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Select.Форматирование_Подзапросов_В_Select;
begin
  Settings.PreferredExpressionLength := 80;
  Settings.AlignExpressions := true;
end;

procedure _Select.Переносы_По_And_В_Where;
begin
end;

procedure _Select.Выравнивание_Условий_В_Where;
begin
  Settings.AlignExpressions := true;
end;

procedure _Select.Длинный_И_Сложный_Запрос_В_Форматировании_Которого_Куча_Мелких_Недостатков;
begin
end;

procedure _Select.Commit_Rollback_Savepoint;
begin
end;

procedure _Select.В_Запросе_Можно_Употреблять_Функцию_Listagg;
begin
end;

procedure _Select.В_Запросе_Можно_Употреблять_Функцию_Listagg_С_Переносом;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Select.Выравнивание_Полей_В_Запросе;
begin
  Settings.AlignFields := true;
end;

procedure _Select.Запрос_Из_Вложенных_Структур_И_Табличных_Типов;
begin
end;

procedure _Select.Запрос_С_Lateral;
begin
end;

procedure _Select.Запрос_С_Left_Right_Full_Join;
begin
end;

procedure _Select.Запрос_С_Order_By;
begin
end;

procedure _Select.Запрос_С_Outer_Cross_Apply;
begin
end;

procedure _Select.Запрос_С_Using;
begin
end;

procedure _Select.Запрос_Из_Нескольких_Таблиц;
begin
end;

procedure _Select.Запрос_Из_Подзапроса;
begin
end;

procedure _Select.Запрос_Из_Табличной_Функции;
begin
end;

procedure _Select.Запрос_С_Ansi_Синтаксисом;
begin
end;

procedure _Select.Запрос_С_Bind_Переменными;
begin
end;

procedure _Select.Запрос_С_Bulk_Collect_Into;
begin
end;

procedure _Select.Запрос_С_Distinct_Unique_All;
begin
end;

procedure _Select.Запрос_С_Group_By;
begin
end;

procedure _Select.Запрос_С_Having;
begin
end;

{ _Insert }

procedure _Insert.Insert_С_Returning;
begin
end;

procedure _Insert.Insert_Select_С_Сопоставлением_Полей;
begin
  Settings.MatchParamLimit := 5;
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

procedure _Insert.Insert_С_Сопоставлением_Полей;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Insert.Insert_С_Указанием_Полей;
begin
end;

procedure _Insert.Простой_Insert;
begin
end;

{ _Update }

procedure _Update.Update_С_Returning;
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

procedure _Update.Выравнивание_В_Update;
begin
  Settings.AlignFields := true;
end;

procedure _Update.Простой_Update;
begin
end;

{ _Delete }

procedure _Delete.Delete_С_Where;
begin
end;

procedure _Delete.Delete_С_Алиасом;
begin
end;

procedure _Delete.Простой_Delete;
begin
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

initialization
  RegisterTest(_Select.Suite);
  RegisterTest(_Insert.Suite);
  RegisterTest(_Update.Suite);
  RegisterTest(_Delete.Suite);
  RegisterTest(_Merge.Suite);
end.

