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
    procedure Указание_Направления_Сортировки_И_Расположения_Null_В_Order_By;
    procedure Связывание_Запросов_Через_Операции_Над_Множествами;
    procedure Выравнивание_Полей_В_Запросе;
    procedure Невыравнивание_Полей_В_Запросе;
    procedure Сопоставление_Полей_В_Into;
    procedure Несопоставление_Полей_В_Into;
  end;

  { Тесты на insert }
  _Insert = class(TTestCase)
  end;

  { Тесты на update }
  _Update = class(TTestCase)
  end;

  { Тесты на delete }
  _Delete = class(TTestCase)
  end;

  { Тесты на merge }
  _Merge = class(TTestCase)
  end;

implementation

{ _Select }

procedure _Select.Простейший_Запрос;
begin
end;

procedure _Select.Связывание_Запросов_Через_Операции_Над_Множествами;
begin
end;

procedure _Select.Сопоставление_Полей_В_Into;
begin
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

procedure _Select.Запрос_Со_Start_With_И_Connect_By;
begin
end;

procedure _Select.Невыравнивание_Полей_В_Запросе;
begin
  Settings.AlignFields := false;
end;

procedure _Select.Несопоставление_Полей_В_Into;
begin
  Settings.MatchParamLimit := 6;
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

procedure _Select.Запрос_С_Into;
begin
end;

procedure _Select.Выравнивание_Полей_В_Запросе;
begin
end;

procedure _Select.Запрос_Из_Вложенных_Структур_И_Табличных_Типов;
begin
end;

procedure _Select.Запрос_С_Lateral;
begin
end;

procedure _Select.Запрос_С_Order_By;
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

procedure _Select.Запрос_С_Bulk_Collect_Into;
begin
end;

procedure _Select.Запрос_С_Group_By;
begin
end;

procedure _Select.Запрос_С_Having;
begin
end;

initialization
  RegisterTest(_Select.Suite);
  RegisterTest(_Insert.Suite);
  RegisterTest(_Update.Suite);
  RegisterTest(_Delete.Suite);
  RegisterTest(_Merge.Suite);
end.


