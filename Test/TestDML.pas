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
    procedure ��������_�����������_����������_�_������������_Null_�_Order_By;
    procedure ����������_��������_�����_��������_���_�����������;
    procedure ������������_�����_�_�������;
    procedure ��������������_�����_�_�������;
    procedure �������������_�����_�_Into;
    procedure ���������������_�����_�_Into;
  end;

  { ����� �� insert }
  _Insert = class(TTestCase)
  end;

  { ����� �� update }
  _Update = class(TTestCase)
  end;

  { ����� �� delete }
  _Delete = class(TTestCase)
  end;

  { ����� �� merge }
  _Merge = class(TTestCase)
  end;

implementation

{ _Select }

procedure _Select.����������_������;
begin
end;

procedure _Select.����������_��������_�����_��������_���_�����������;
begin
end;

procedure _Select.�������������_�����_�_Into;
begin
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

procedure _Select.������_��_Start_With_�_Connect_By;
begin
end;

procedure _Select.��������������_�����_�_�������;
begin
  Settings.AlignFields := false;
end;

procedure _Select.���������������_�����_�_Into;
begin
  Settings.MatchParamLimit := 6;
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

procedure _Select.������_�_Into;
begin
end;

procedure _Select.������������_�����_�_�������;
begin
end;

procedure _Select.������_��_���������_��������_�_���������_�����;
begin
end;

procedure _Select.������_�_Lateral;
begin
end;

procedure _Select.������_�_Order_By;
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

procedure _Select.������_�_Bulk_Collect_Into;
begin
end;

procedure _Select.������_�_Group_By;
begin
end;

procedure _Select.������_�_Having;
begin
end;

initialization
  RegisterTest(_Select.Suite);
  RegisterTest(_Insert.Suite);
  RegisterTest(_Update.Suite);
  RegisterTest(_Delete.Suite);
  RegisterTest(_Merge.Suite);
end.


