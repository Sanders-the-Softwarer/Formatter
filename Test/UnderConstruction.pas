unit UnderConstruction;

interface

uses SysUtils, TestFramework;

type
  _UnderConstruction = class(TTestCase)
  protected
    procedure PostponeTill(AYear, AMonth, ADay: integer);
  published
    procedure Тесты_Команд_SQLPLUS;
    procedure Тесты_Объектные_Типы;
    procedure Тесты_Create_Trigger;
    procedure Тесты_Невыравнивание_Везде_Где_Есть_На_Выравнивание;
    procedure Тесты_DeclarationSingleLineParamLimit;
    procedure Тесты_ArgumentSingleLineParamLimit;
    procedure Тесты_PreferredExpressionLength;
    procedure Тесты_MatchParamLimit;
    procedure Тесты_ReplaceDefault;
    procedure Тесты_ReplaceAsIs;
    procedure Тесты_Group_By_Rollup_Cube_Grouping_Sets;
    procedure Тесты_Alter_Session;
    procedure Тесты_Alter_Package;
  end;

implementation

{ _UnderConstruction }

procedure _UnderConstruction.PostponeTill(AYear, AMonth, ADay: integer);
begin
  Check(Now < EncodeDate(AYear, AMonth, ADay), 'Пока не сделано!');
end;

procedure _UnderConstruction.Тесты_Alter_Package;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_Alter_Session;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_ArgumentSingleLineParamLimit;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_Create_Trigger;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_DeclarationSingleLineParamLimit;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_Group_By_Rollup_Cube_Grouping_Sets;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_MatchParamLimit;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_PreferredExpressionLength;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_ReplaceAsIs;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_ReplaceDefault;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_Команд_SQLPLUS;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_Невыравнивание_Везде_Где_Есть_На_Выравнивание;
begin
  PostponeTill(2020, 6, 30);
end;

procedure _UnderConstruction.Тесты_Объектные_Типы;
begin
  PostponeTill(2020, 6, 30);
end;

initialization
  RegisterTest(_UnderConstruction.Suite);

end.
