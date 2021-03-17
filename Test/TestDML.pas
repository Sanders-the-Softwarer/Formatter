////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                           ��������� �� ������ DML                          //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestDML;

interface

uses
  SysUtils, TestFramework, FileBasedTest;

type

  { ����� ������ DML }
  _�������_DML = class(TFileBasedTest)
  end;

  { ����� �� select }
  _Select = class(TFileBasedTest)
  published
    procedure ������_�_�������;
    procedure ������_�_��������_�_�����;
    procedure ������_�_�����������_�_�����;
    procedure ������_�_�����������_�_��������;
    procedure ����������_��������_�����_��������_���_�����������;
    procedure ��������������_�����������_�_Select;
    procedure ��������_��_And_�_Where;
    procedure Commit_Rollback_Savepoint;
  end;

  { ����� �� SELECT }
  _������� = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Ansi_���������;
    procedure Bind_����������;
    procedure Bulk_Collect_Into;
    procedure Database_Links;
    procedure Distinct_Unique_All;
    procedure Distinct_Unique_All_�_Count;
    procedure For_Update;
    procedure Group_By;
    procedure Having;
    procedure Into;
    procedure Keep;
    procedure Lateral;
    procedure Left_Right_Full_Join;
    procedure Order_By;
    procedure Outer_Cross_Apply;
    procedure Start_With_�_Connect_By;
    procedure Using;
    procedure Where;
    procedure With_;
    procedure ��_����������;
    procedure ������_�����_�_Join��;
    procedure �������������_�������;
    procedure ���������_�_Select;
    procedure ���������_�_Where;
    procedure ����������;
    procedure �_�����������_������;
    procedure �����������_����������;
    procedure ���������_���������_�������_����������;
    procedure ��_���������_��������_�_���������_�����;
    procedure �_�������������;
    procedure ��_����������_������;
    procedure ��_���������_�������;
  end;

  { ����� �� insert }
  _Insert = class(TFileBasedTest)
  published
    procedure �������_Insert;
    procedure Insert_�_�������_�������;
    procedure Insert_�_���������;
    procedure Insert_�_���������_�_�������;
    procedure Insert_�_���������_�����;
    procedure Insert_�_Returning;
    procedure Insert_�_Returning_�_��������������;
  end;

  { ����� �� update }
  _Update = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure �������_Update;
    procedure Update_�_�������;
    procedure Update_�_�����������;
    procedure Update_�_��������������_��������������;
    procedure Update_�_Where;
    procedure Update_�_Returning;
    procedure Update_�_Returning_Bulk_Collect_Into;
    procedure Update_�_��������������_�_Returning;
  end;

  { ����� �� delete }
  _Delete = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure �������_Delete;
    procedure Delete_�_�������;
    procedure Delete_�_Where;
    procedure Delete_�_Returning;
    procedure Delete_�_Returning_Bulk_Collect;
  end;

  { ����� �� merge }
  _Merge = class(TFileBasedTest)
  published
    procedure �������_Merge;
    procedure Merge_�_������_��������_���������;
    procedure Merge_�_�����������;
    procedure Merge_�_Insert_Where_Update_Delete_Where;
    procedure Merge_�_�����������_Delete;
  end;

  { ������� � ������ ����������� }
  _�������_�_������_����������� = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure ListAgg;
    procedure Trim;
    procedure Extract;
  end;

implementation

{ _Select }

procedure _Select.����������_��������_�����_��������_���_�����������;
begin
end;

procedure _Select.������_�_�������;
begin
end;

procedure _Select.������_�_��������_�_�����;
begin
end;

procedure _Select.������_�_�����������_�_�����;
begin
end;

procedure _Select.������_�_�����������_�_��������;
begin
end;

procedure _Select.��������������_�����������_�_Select;
begin
  Settings.PreferredExpressionLength := 80;
end;

procedure _Select.��������_��_And_�_Where;
begin
end;

procedure _Select.Commit_Rollback_Savepoint;
begin
end;

{ _Insert }

procedure _Insert.Insert_�_Returning;
begin
end;

procedure _Insert.Insert_�_���������;
begin
end;

procedure _Insert.Insert_�_���������_�_�������;
begin
end;

procedure _Insert.Insert_�_Returning_�_��������������;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Insert.Insert_�_�������_�������;
begin
end;

procedure _Insert.Insert_�_���������_�����;
begin
end;

procedure _Insert.�������_Insert;
begin
end;

{ _Update }

function _Update.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\������� DML\Update';
end;

procedure _Update.Update_�_Returning;
begin
end;

procedure _Update.Update_�_Returning_Bulk_Collect_Into;
begin
end;

procedure _Update.Update_�_Where;
begin
end;

procedure _Update.Update_�_�������;
begin
end;

procedure _Update.Update_�_��������������_��������������;
begin
end;

procedure _Update.Update_�_�����������;
begin
end;

procedure _Update.Update_�_��������������_�_Returning;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Update.�������_Update;
begin
end;

{ _Delete }

procedure _Delete.Delete_�_Returning;
begin
end;

procedure _Delete.Delete_�_Returning_Bulk_Collect;
begin
end;

procedure _Delete.Delete_�_Where;
begin
end;

procedure _Delete.Delete_�_�������;
begin
end;

procedure _Delete.�������_Delete;
begin
end;

function _Delete.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\������� DML\Delete';
end;

{ _Merge }

procedure _Merge.Merge_�_Insert_Where_Update_Delete_Where;
begin
end;

procedure _Merge.Merge_�_������_��������_���������;
begin
end;

procedure _Merge.Merge_�_�����������;
begin
end;

procedure _Merge.Merge_�_�����������_Delete;
begin
end;

procedure _Merge.�������_Merge;
begin
end;

{ _������� }

function _�������.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\�������';
end;

procedure _�������.Ansi_���������;
begin
end;

procedure _�������.Lateral;
begin
end;

procedure _�������.Left_Right_Full_Join;
begin
end;

procedure _�������.Order_By;
begin
end;

procedure _�������.Outer_Cross_Apply;
begin
end;

procedure _�������.Start_With_�_Connect_By;
begin
end;

procedure _�������.Using;
begin
end;

procedure _�������.Where;
begin
end;

procedure _�������.With_;
begin
end;

procedure _�������.�������������_�������;
begin
end;

procedure _�������.��_���������_��������_�_���������_�����;
begin
end;

procedure _�������.��_����������_������;
begin
end;

procedure _�������.��_����������;
begin
end;

procedure _�������.��_���������_�������;
begin

end;

procedure _�������.������_�����_�_Join��;
begin
end;

procedure _�������.�����������_����������;
begin
end;

procedure _�������.���������_���������_�������_����������;
begin
end;

procedure _�������.���������_�_Select;
begin
end;

procedure _�������.���������_�_Where;
begin
end;

procedure _�������.����������;
begin
end;

procedure _�������.�_�����������_������;
begin
end;

procedure _�������.�_�������������;
begin
end;

procedure _�������.Bind_����������;
begin
end;

procedure _�������.Bulk_Collect_Into;
begin
end;

procedure _�������.Database_Links;
begin
end;

procedure _�������.Distinct_Unique_All;
begin
end;

procedure _�������.Distinct_Unique_All_�_Count;
begin
end;

procedure _�������.For_Update;
begin
end;

procedure _�������.Group_By;
begin
end;

procedure _�������.Having;
begin
end;

procedure _�������.Into;
begin
end;

procedure _�������.Keep;
begin
  Settings.PreferredExpressionLength := 60;
end;

{ _�������_�_������_����������� }

procedure _�������_�_������_�����������.Extract;
begin
end;

function _�������_�_������_�����������.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\������� � ������ �����������';
end;

procedure _�������_�_������_�����������.ListAgg;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _�������_�_������_�����������.Trim;
begin
end;

initialization
  RegisterTest(_�������_DML.Suite);
  RegisterTest('_�������_DML', _�������.Suite);
  RegisterTest('_�������_DML', _Select.Suite);
  RegisterTest('_�������_DML', _Insert.Suite);
  RegisterTest('_�������_DML', _Update.Suite);
  RegisterTest('_�������_DML', _Delete.Suite);
  RegisterTest('_�������_DML', _Merge.Suite);
  RegisterTest('_�������_DML\_�������', _�������_�_������_�����������.Suite);
end.

