////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Автотесты на модуль DDL                          //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestDDL;

interface

uses
  TestFramework, FileBasedTest;

type

  { Тесты на create }
  _Create = class(TFileBasedTest)
  published
    procedure Create;
    procedure Create_Or_Replace;
    procedure Create_View;
    procedure Create_View_With;
    procedure Create_Index;
    procedure Create_Unique_Index;
    procedure Create_Functional_Index;
    procedure Create_Table;
    procedure Create_Indexed_Table;
    procedure Create_Temporary_Table;
    procedure Поля_Таблиц_Разных_Типов;
    procedure Поля_И_Ограничения_Вперемешку;
    procedure Указание_Места_Хранения_Lobов;
    procedure Create_Sequence;
    procedure Комментарии_К_Таблицам;
  end;

  { Тесты на drop }
  _Drop = class(TFileBasedTest)
  published
    procedure Drop_Table;
    procedure Drop_Procedure_Function_Package;
    procedure Drop_View;
    procedure Drop_Index;
    procedure Drop_Type;
    procedure Drop_Sequence;
    procedure Drop_Trigger;
  end;


implementation

{ _Create }

procedure _Create.Create;
begin
end;

procedure _Create.Create_Functional_Index;
begin
end;

procedure _Create.Create_Index;
begin
end;

procedure _Create.Create_Indexed_Table;
begin
end;

procedure _Create.Create_Or_Replace;
begin
end;

procedure _Create.Create_Sequence;
begin
end;

procedure _Create.Create_Table;
begin
end;

procedure _Create.Create_Temporary_Table;
begin
end;

procedure _Create.Create_Unique_Index;
begin
end;

procedure _Create.Create_View;
begin
end;

procedure _Create.Create_View_With;
begin
end;

procedure _Create.Комментарии_К_Таблицам;
begin
  Settings.AlignTableColumnComments := true;
end;

procedure _Create.Поля_И_Ограничения_Вперемешку;
begin
end;

procedure _Create.Поля_Таблиц_Разных_Типов;
begin
end;

procedure _Create.Указание_Места_Хранения_Lobов;
begin
end;

{ _Drop }

procedure _Drop.Drop_Index;
begin
end;

procedure _Drop.Drop_Procedure_Function_Package;
begin
end;

procedure _Drop.Drop_Sequence;
begin
end;

procedure _Drop.Drop_Table;
begin
end;

procedure _Drop.Drop_Trigger;
begin
end;

procedure _Drop.Drop_Type;
begin
end;

procedure _Drop.Drop_View;
begin
end;

initialization
  RegisterTest(_Create.Suite);
  RegisterTest(_Drop.Suite);

end.


