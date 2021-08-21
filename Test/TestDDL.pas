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
  protected
    function GetDir: string; override;
  published
    procedure Create;
    procedure Create_Or_Replace;
    procedure View;
    procedure View_With;
    procedure View_С_Колонками;
    procedure Index;
    procedure Unique_Index;
    procedure Functional_Index;
    procedure Table;
    procedure Partitioned_Table;
    procedure Indexed_Table;
    procedure Temporary_Table;
    procedure Поля_Таблиц_Разных_Типов;
    procedure Поля_И_Ограничения_Вперемешку;
    procedure Указание_Места_Хранения_Lobов;
    procedure Sequence;
    procedure Комментарии_К_Таблицам;
    procedure Database_Link;
    procedure Role;
    procedure Synonym;
    procedure Context;
  end;

  { Тесты на alter }
  _Alter = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure View;
  end;

  { Тесты на drop }
  _Drop = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Function_;
    procedure Package;
    procedure Package_Body;
    procedure Procedure_;
    procedure Synonym;
    procedure Table;
    procedure View;
    procedure Index;
    procedure Role;
    procedure Sequence;
    procedure Type_;
    procedure Type_Body;
    procedure Trigger;
    procedure Database_Link;
    procedure Context;
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
    procedure Grant;
    procedure Alter_Role;
    procedure Alter_Session;
    procedure Alter_Sequence;
    procedure Alter_Database_Link;
    procedure Set_Role;
  end;

implementation

{ _Create }

function _Create.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Команды DDL\Create';
end;

procedure _Create.Context;
begin
end;

procedure _Create.Create;
begin
end;

procedure _Create.Functional_Index;
begin
end;

procedure _Create.Index;
begin
end;

procedure _Create.Indexed_Table;
begin
end;

procedure _Create.Create_Or_Replace;
begin
end;

procedure _Create.Database_Link;
begin
end;

procedure _Create.Sequence;
begin
end;

procedure _Create.Synonym;
begin
end;

procedure _Create.Table;
begin
end;

procedure _Create.Partitioned_Table;
begin
end;

procedure _Create.Role;
begin
end;

procedure _Create.Temporary_Table;
begin
end;

procedure _Create.Unique_Index;
begin
end;

procedure _Create.View;
begin
end;

procedure _Create.View_With;
begin
end;

procedure _Create.View_С_Колонками;
begin
end;

procedure _Create.Комментарии_К_Таблицам;
begin
  Settings.AlignCommands := true;
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

procedure _Drop.Context;
begin
end;

procedure _Drop.Database_Link;
begin
end;

procedure _Drop.Function_;
begin
end;

procedure _Drop.Index;
begin
end;

procedure _Drop.Package;
begin
end;

procedure _Drop.Package_Body;
begin
end;

procedure _Drop.Procedure_;
begin
end;

procedure _Drop.Role;
begin
end;

procedure _Drop.Sequence;
begin
end;

procedure _Drop.Synonym;
begin
end;

procedure _Drop.Table;
begin
end;

procedure _Drop.Trigger;
begin
end;

procedure _Drop.Type_;
begin
end;

procedure _Drop.Type_Body;
begin
end;

procedure _Drop.View;
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

procedure _Команды_DDL.Grant;
begin
end;

procedure _Команды_DDL.Set_Role;
begin
end;

{ _Alter }

function _Alter.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Команды DDL\Alter';
end;

procedure _Alter.View;
begin
end;

initialization
  RegisterTest(_Create.Suite);
  RegisterTest(_Alter.Suite);
  RegisterTest(_Drop.Suite);
  RegisterTest(_Comment.Suite);
  RegisterTest(_Команды_DDL.Suite);

end.


