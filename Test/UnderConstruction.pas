unit UnderConstruction;

interface

uses SysUtils, TestFramework, FileBasedTest;

type
  _UnderConstruction = class(TFileBasedTest)
  published
    procedure �����_������_SQLPLUS;
    procedure �����_���������_����;
    procedure �����_Create_Trigger;
    procedure �����_Group_By_Rollup_Cube_Grouping_Sets;
    procedure �����_Alter_Session;
    procedure �����_Alter_Package;
  end;

implementation

{ _UnderConstruction }

procedure _UnderConstruction.�����_Alter_Package;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.�����_Alter_Session;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.�����_Create_Trigger;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.�����_Group_By_Rollup_Cube_Grouping_Sets;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.�����_������_SQLPLUS;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

procedure _UnderConstruction.�����_���������_����;
begin
  PostponeTill(2020, 12, 6);
//  UnderConstruction;
end;

initialization
  RegisterTest(_UnderConstruction.Suite);

end.
