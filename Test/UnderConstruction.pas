unit UnderConstruction;

interface

uses SysUtils, TestFramework, FileBasedTest;

type
  _UnderConstruction = class(TFileBasedTest)
  published
    procedure Тесты_Команд_SQLPLUS;
    procedure Тесты_Объектные_Типы;
    procedure Тесты_Create_Trigger;
    procedure Тесты_Group_By_Rollup_Cube_Grouping_Sets;
    procedure Тесты_Alter_Session;
    procedure Тесты_Alter_Package;
  end;

implementation

{ _UnderConstruction }

procedure _UnderConstruction.Тесты_Alter_Package;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.Тесты_Alter_Session;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.Тесты_Create_Trigger;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.Тесты_Group_By_Rollup_Cube_Grouping_Sets;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.Тесты_Команд_SQLPLUS;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.Тесты_Объектные_Типы;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

initialization
  RegisterTest(_UnderConstruction.Suite);

end.
