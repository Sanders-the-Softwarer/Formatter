unit TestSettings;

interface

uses
  SysUtils, TestFramework, FileBasedTest, Printer;

type

  { ����� �� ��������� ������������� }
  _Settings = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure �����������_����������;
    procedure �����������_����;
    procedure �����������_�������;
    procedure �����������_���������;
    procedure �����������_�������;
    procedure �����������_���������������;
    procedure ���������_�������;
    procedure ��������_Default;
    procedure ��������_As_��_Is;
    procedure ������_����_������������;
    procedure ����������_�������_�_������������;
    procedure ���������_In;
    procedure ���������_From_�_Delete;
    procedure �������_������_��_Connect;
    procedure ���������_�������_��������;
    procedure ��_�����������_����������;
    procedure ��_�����������_����;
    procedure ��_�����������_�������;
    procedure ��_�����������_���������;
    procedure ��_�����������_�������;
    procedure ��_�����������_���������������;
    procedure ��_���������_�������;
    procedure ��_��������_Default;
    procedure ��_��������_As_��_Is;
    procedure ��_������_����_������������;
    procedure ��_����������_�������_�_������������;
    procedure ��_���������_In;
    procedure ��_���������_From_�_Delete;
    procedure ��_�������_������_��_Connect;
    procedure ��_���������_�������_��������;
    procedure ������_����������_�_������;
    procedure ������_�����������_����������_�_������;
    procedure ������_�����������_����������_�_������;
    procedure ������_��������������_����������;
    procedure ��������������_�����_���������;
    procedure ��������_����������_�_������;
    procedure ��������_����������_�_����_������;
    procedure ��_��������_����������_�_������;
    procedure ��_��������_����������_�_����_������;
    procedure ���������_�����;
  end;

implementation

{ _Settings }

function _Settings.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\���������';
end;

procedure _Settings.�����������_���������;
begin
  Settings.AlignExpressions := true;
end;

procedure _Settings.�����������_�������;
begin
  Settings.AlignColumns := true;
  Settings.AlignRightComments := true;
end;

procedure _Settings.�����������_�������;
begin
  Settings.AlignCommands := true;
end;

procedure _Settings.�����������_����������;
begin
  Settings.AlignVariables := true;
end;

procedure _Settings.�����������_����;
begin
  Settings.AlignFields := true;
end;

procedure _Settings.�����������_���������������;
begin
  Settings.AlignSpecialComments := true;
  Settings.MatchParamLimit := 1;
end;

procedure _Settings.���������_From_�_Delete;
begin
  Settings.AddFromToDelete := true;
end;

procedure _Settings.���������_In;
begin
  Settings.AddInAccessSpecificator := true;
end;

procedure _Settings.��������_As_��_Is;
begin
  Settings.ReplaceAsIs := true;
end;

procedure _Settings.��������_Default;
begin
  Settings.ReplaceDefault := true;
end;

procedure _Settings.���������_�������;
begin
  PostponeTill(2021, 8, 1);
  Settings.AlignUseSpace := true;
  Settings.AlignVariables := true;
  Settings.AlignFields := true;
end;

procedure _Settings.����������_�������_�_������������;
begin
  Settings.CorrectCommentSpaces := true;
end;

procedure _Settings.������_����_������������;
begin
  Settings.ChangeCommentType := true;
end;

procedure _Settings.��_�����������_���������;
begin
  Settings.AlignExpressions := false;
end;

procedure _Settings.��_�����������_�������;
begin
  Settings.AlignColumns := false;
end;

procedure _Settings.��_�����������_�������;
begin
  Settings.AlignCommands := false;
end;

procedure _Settings.��_�����������_����������;
begin
  Settings.AlignVariables := false;
end;

procedure _Settings.��_�����������_����;
begin
  Settings.AlignFields := false;
end;

procedure _Settings.��_�����������_���������������;
begin
  Settings.AlignSpecialComments := false;
  Settings.MatchParamLimit := 1;
end;

procedure _Settings.��_���������_From_�_Delete;
begin
  Settings.AddFromToDelete := false;
end;

procedure _Settings.��_���������_In;
begin
  Settings.AddInAccessSpecificator := false;
end;

procedure _Settings.��_��������_As_��_Is;
begin
  Settings.ReplaceAsIs := false;
end;

procedure _Settings.��_��������_Default;
begin
  Settings.ReplaceDefault := false;
end;

procedure _Settings.��_���������_�������;
begin
  Settings.AlignUseSpace := false;
  Settings.AlignVariables := true;
  Settings.AlignFields := true;
end;

procedure _Settings.��_����������_�������_�_������������;
begin
  Settings.CorrectCommentSpaces := false;
end;

procedure _Settings.��_������_����_������������;
begin
  Settings.ChangeCommentType := false;
end;

procedure _Settings.��_��������_����������_�_������;
begin
  Settings.ShiftPackageHeader := false;
  Settings.ShiftPackageBody   := false;
end;

procedure _Settings.��_��������_����������_�_����_������;
begin
  Settings.ShiftPackageHeader := false;
  Settings.ShiftPackageBody   := false;
end;

procedure _Settings.��_�������_������_��_Connect;
begin
  Settings.RemoveConnectPasswords := false;
end;

procedure _Settings.��_���������_�������_��������;
begin
  Settings.BeautifyLongOperands := false;
  Settings.PreferredExpressionLength := 40;
end;

procedure _Settings.������_�����������_����������_�_������;
begin
  Settings.NamedArgumentSingleLineParamLimit := 3;
end;

procedure _Settings.������_����������_�_������;
begin
  Settings.DeclarationSingleLineParamLimit := 3;
end;

procedure _Settings.������_�����������_����������_�_������;
begin
  Settings.PositionalArgumentSingleLineParamLimit := 3;
end;

procedure _Settings.������_��������������_����������;
begin
  Settings.MatchParamLimit := 3;
end;

procedure _Settings.��������������_�����_���������;
begin
  Settings.PreferredExpressionLength := 40;
end;

procedure _Settings.��������_����������_�_������;
begin
  Settings.ShiftPackageHeader := true;
  Settings.ShiftPackageBody := false;
end;

procedure _Settings.��������_����������_�_����_������;
begin
  Settings.ShiftPackageHeader := false;
  Settings.ShiftPackageBody := true;
end;

procedure _Settings.���������_�����;
begin
  Settings.StartIndent := 7;
end;

procedure _Settings.�������_������_��_Connect;
begin
  Settings.RemoveConnectPasswords := true;
end;

procedure _Settings.���������_�������_��������;
begin
  Settings.BeautifyLongOperands := true;
  Settings.PreferredExpressionLength := 40;
end;

initialization
  RegisterTest(_Settings.Suite);

end.
