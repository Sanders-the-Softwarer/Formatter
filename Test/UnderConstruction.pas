unit UnderConstruction;

interface

uses SysUtils, TestFramework;

type
  _UnderConstruction = class(TTestCase)
  protected
    procedure PostponeTill(AYear, AMonth, ADay: integer);
  published
    procedure �����_������_SQLPLUS;
    procedure �����_���������_����;
    procedure �����_Create_Trigger;
    procedure �����_��������������_�����_���_����_��_������������;
    procedure �����_DeclarationSingleLineParamLimit;
    procedure �����_ArgumentSingleLineParamLimit;
    procedure �����_PreferredExpressionLength;
    procedure �����_MatchParamLimit;
    procedure �����_ReplaceDefault;
    procedure �����_ReplaceAsIs;
    procedure �����_Group_By_Rollup_Cube_Grouping_Sets;
    procedure �����_Alter_Session;
    procedure �����_Alter_Package;
  end;

implementation

{ _UnderConstruction }

procedure _UnderConstruction.PostponeTill(AYear, AMonth, ADay: integer);
begin
  Check(Now < EncodeDate(AYear, AMonth, ADay), '���� �� �������!');
end;

procedure _UnderConstruction.�����_Alter_Package;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_Alter_Session;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_ArgumentSingleLineParamLimit;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_Create_Trigger;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_DeclarationSingleLineParamLimit;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_Group_By_Rollup_Cube_Grouping_Sets;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_MatchParamLimit;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_PreferredExpressionLength;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_ReplaceAsIs;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_ReplaceDefault;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_������_SQLPLUS;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_��������������_�����_���_����_��_������������;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.�����_���������_����;
begin
  PostponeTill(2020, 6, 30);
end;

initialization
  RegisterTest(_UnderConstruction.Suite);

end.
