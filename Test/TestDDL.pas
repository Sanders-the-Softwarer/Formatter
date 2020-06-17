////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Автотесты на модуль DDL                          //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestDDL;

interface

uses
  SysUtils, TestFramework, FileBasedTest;

type

  { Тесты на create }
  _Create = class(TFileBasedTest)
  published
    procedure Create;
    procedure Create_Or_Replace;
    procedure Create_View;
    procedure Create_View_With;
    procedure Create_View_С_Колонками;
    procedure Create_Index;
    procedure Create_Unique_Index;
    procedure Create_Functional_Index;
    procedure Create_Table;
    procedure Create_Table_Partitioned;
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
  protected
    function GetDir: string; override;
  published
    procedure Drop_Function;
    procedure Drop_Package;
    procedure Drop_Package_Body;
    procedure Drop_Procedure;
    procedure Drop_Synonym;
    procedure Drop_Table;
    procedure Drop_View;
    procedure Drop_Index;
    procedure Drop_Role;
    procedure Drop_Sequence;
    procedure Drop_Type;
    procedure Drop_Type_Body;
    procedure Drop_Trigger;
    procedure Drop_Database_Link;
  end;

  { Тесты на comment }
  _Comment = class(TFileBasedTest)
  published
    procedure Comment_On_Table;
  end;

  { Команды DDL }
  _Команды_DDL = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Create_Synonym;
    procedure Create_Role;
    procedure Create_Database_Link;
    procedure Grant;
    procedure Alter_Role;
    procedure Alter_Session;
    procedure Alter_Sequence;
    procedure Alter_Database_Link;
    procedure Set_Role;
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

procedure _Create.Create_Table_Partitioned;
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

procedure _Create.Create_View_С_Колонками;
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

function _Drop.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Команды DDL\Drop';
end;

procedure _Drop.Drop_Database_Link;
begin
end;

procedure _Drop.Drop_Function;
begin
end;

procedure _Drop.Drop_Index;
begin
end;

procedure _Drop.Drop_Package;
begin
end;

procedure _Drop.Drop_Package_Body;
begin
end;

procedure _Drop.Drop_Procedure;
begin
end;

procedure _Drop.Drop_Role;
begin
end;

procedure _Drop.Drop_Sequence;
begin
end;

procedure _Drop.Drop_Synonym;
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

procedure _Drop.Drop_Type_Body;
begin
end;

procedure _Drop.Drop_View;
begin
end;

{ _Comment }

procedure _Comment.Comment_On_Table;
begin
end;

{ _Команды_DDL }

function _Команды_DDL.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Команды DDL';
end;

procedure _Команды_DDL.Alter_Role;
begin
end;

procedure _Команды_DDL.Alter_Database_Link;
begin
end;

procedure _Команды_DDL.Alter_Sequence;
begin
end;

procedure _Команды_DDL.Alter_Session;
begin
end;

procedure _Команды_DDL.Create_Database_Link;
begin
end;

procedure _Команды_DDL.Create_Role;
begin
end;

procedure _Команды_DDL.Create_Synonym;
begin
end;

procedure _Команды_DDL.Grant;
begin
end;

procedure _Команды_DDL.Set_Role;
begin
  PostponeTill(2020, 4, 30);
end;

initialization
  RegisterTest(_Create.Suite);
  RegisterTest(_Drop.Suite);
  RegisterTest(_Comment.Suite);
  RegisterTest(_Команды_DDL.Suite);

end.


