////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                           ��������� �� ������ DML                          //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestDML;

interface

uses
  TestFramework, FileBasedTest;

type

  { ����� �� select }
  _Select = class(TFileBasedTest)
  published
    procedure ����������_������;
    procedure ������_�_With;
    procedure ������_�_�������;
    procedure ������_�_�����������_������;
    procedure ������_�_��������_�_�����;
    procedure ������_�_�����������_�_�����;
    procedure ������_�_�����������_�_��������;
    procedure ������_�_Into;
    procedure ������_�_Bulk_Collect_Into;
    procedure ������_��_����������_������;
    procedure ������_��_���������_�������;
    procedure ������_��_����������;
    procedure ������_��_���������_��������_�_���������_�����;
    procedure ������_�_Lateral;
    procedure ������_�_Where;
    procedure ������_�_Group_By;
    procedure ������_�_Having;
    procedure ������_��_Start_With_�_Connect_By;
    procedure ������_�_Order_By;
    procedure ������_�_Bind_�����������;
    procedure ������_�_�������������;
    procedure ��������_�����������_����������_�_������������_Null_�_Order_By;
    procedure ����������_��������_�����_��������_���_�����������;
    procedure ������������_�����_�_�������;
    procedure �������������_�����_�_Into;
    procedure �������������_�����_�_Into_�_�������������;
    procedure ������_�_Ansi_�����������;
    procedure ������_�_Using;
    procedure ������_�_Left_Right_Full_Join;
    procedure ������_�_�������_�������_�_Join��;
    procedure ������_�_���������_�����������_�������_����������;
    procedure ������_�_Outer_Cross_Apply;
    procedure ������_�_Distinct_Unique_All;
    procedure Distinct_Unique_All_�_Count;
    procedure ���������_�_Select;
    procedure ���������_�_Where;
    procedure �_�������_�����_�����������_�������������_�������;
    procedure �_�������_�����_�����������_�������_Listagg;
    procedure �_�������_�����_�����������_�������_Listagg_�_���������;
    procedure �_�������_�����_�����������_�����������_Keep;
    procedure �_�������_�����_�����������_�����������_Keep_�_���������;
    procedure ��������������_�����������_�_Select;
    procedure ��������_��_And_�_Where;
  public
    { ������� }
    procedure ������������_�������_�_Where;
  published
    procedure Commit_Rollback_Savepoint;
  public
    { ������� }
    procedure �������_�_�������_������_�_��������������_��������_����_������_�����������;
  public
    { ������� }
    procedure �����������_��_������_���������_�_��������_��������_��_���������_������;
  end;

  { ����� �� insert }
  _Insert = class(TFileBasedTest)
  published
    procedure �������_Insert;
    procedure Insert_�_�������_�������;
    procedure Insert_�_���������;
    procedure Insert_�_���������_�_�������;
    procedure Insert_�_���������_�����;
    procedure Insert_�_��������������_�����;
    procedure Insert_Select_�_��������������_�����;
    procedure Insert_�_Returning;
    procedure Insert_�_Returning_�_��������������;
  end;

  { ����� �� update }
  _Update = class(TFileBasedTest)
  published
    procedure �������_Update;
    procedure Update_�_�������;
    procedure Update_�_�����������;
    procedure Update_�_��������������_��������������;
    procedure ������������_�_Update;
    procedure Update_�_Where;
    procedure Update_�_Returning;
    procedure Update_�_��������������_�_Returning;
  end;

  { ����� �� delete }
  _Delete = class(TFileBasedTest)
  published
    procedure �������_Delete;
    procedure Delete_�_�������;
    procedure Delete_�_Where;
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

implementation

{ _Select }

procedure _Select.���������_�_Select;
begin
end;

procedure _Select.���������_�_Where;
begin
end;

procedure _Select.����������_������;
begin
end;

procedure _Select.����������_��������_�����_��������_���_�����������;
begin
end;

procedure _Select.�������������_�����_�_Into;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Select.�������������_�����_�_Into_�_�������������;
begin
  Settings.MatchParamLimit := 3;
  Settings.AlignSpecialComments := true;
end;

procedure _Select.��������_�����������_����������_�_������������_Null_�_Order_By;
begin
end;

procedure _Select.������_�_Where;
begin
end;

procedure _Select.������_�_With;
begin
end;

procedure _Select.������_�_�������;
begin
end;

procedure _Select.������_�_�����������_������;
begin
end;

procedure _Select.������_�_���������_�����������_�������_����������;
begin
end;

procedure _Select.������_�_�������������;
begin
end;

procedure _Select.������_��_Start_With_�_Connect_By;
begin
end;

procedure _Select.�����������_��_������_���������_�_��������_��������_��_���������_������;
begin
  Settings.PreferredExpressionLength := 60;
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

procedure _Select.������_�_�������_�������_�_Join��;
begin
end;

procedure _Select.������_�_Into;
begin
end;

procedure _Select.Distinct_Unique_All_�_Count;
begin
end;

procedure _Select.�_�������_�����_�����������_�������������_�������;
begin
end;

procedure _Select.�_�������_�����_�����������_�����������_Keep;
begin
end;

procedure _Select.�_�������_�����_�����������_�����������_Keep_�_���������;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Select.��������������_�����������_�_Select;
begin
  Settings.PreferredExpressionLength := 80;
  Settings.AlignExpressions := true;
end;

procedure _Select.��������_��_And_�_Where;
begin
end;

procedure _Select.������������_�������_�_Where;
begin
  Settings.AlignExpressions := true;
end;

procedure _Select.�������_�_�������_������_�_��������������_��������_����_������_�����������;
begin
end;

procedure _Select.Commit_Rollback_Savepoint;
begin
end;

procedure _Select.�_�������_�����_�����������_�������_Listagg;
begin
end;

procedure _Select.�_�������_�����_�����������_�������_Listagg_�_���������;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Select.������������_�����_�_�������;
begin
  Settings.AlignFields := true;
end;

procedure _Select.������_��_���������_��������_�_���������_�����;
begin
end;

procedure _Select.������_�_Lateral;
begin
end;

procedure _Select.������_�_Left_Right_Full_Join;
begin
end;

procedure _Select.������_�_Order_By;
begin
end;

procedure _Select.������_�_Outer_Cross_Apply;
begin
end;

procedure _Select.������_�_Using;
begin
end;

procedure _Select.������_��_����������_������;
begin
end;

procedure _Select.������_��_����������;
begin
end;

procedure _Select.������_��_���������_�������;
begin
end;

procedure _Select.������_�_Ansi_�����������;
begin
end;

procedure _Select.������_�_Bind_�����������;
begin
end;

procedure _Select.������_�_Bulk_Collect_Into;
begin
end;

procedure _Select.������_�_Distinct_Unique_All;
begin
end;

procedure _Select.������_�_Group_By;
begin
end;

procedure _Select.������_�_Having;
begin
end;

{ _Insert }

procedure _Insert.Insert_�_Returning;
begin
end;

procedure _Insert.Insert_Select_�_��������������_�����;
begin
  Settings.MatchParamLimit := 5;
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

procedure _Insert.Insert_�_��������������_�����;
begin
  Settings.MatchParamLimit := 5;
end;

procedure _Insert.Insert_�_���������_�����;
begin
end;

procedure _Insert.�������_Insert;
begin
end;

{ _Update }

procedure _Update.Update_�_Returning;
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

procedure _Update.������������_�_Update;
begin
  Settings.AlignFields := true;
end;

procedure _Update.�������_Update;
begin
end;

{ _Delete }

procedure _Delete.Delete_�_Where;
begin
end;

procedure _Delete.Delete_�_�������;
begin
end;

procedure _Delete.�������_Delete;
begin
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

initialization
  RegisterTest(_Select.Suite);
  RegisterTest(_Insert.Suite);
  RegisterTest(_Update.Suite);
  RegisterTest(_Delete.Suite);
  RegisterTest(_Merge.Suite);
end.

