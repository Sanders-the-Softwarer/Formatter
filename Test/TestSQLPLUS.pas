////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                         ��������� �� ������ SQL*Plus                       //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestSQLPLUS;

interface

uses
  TestFramework, FileBasedTest;

type

  { ����� �� SQL*Plus }
  _SQLPlus = class(TFileBasedTest)
  published
    procedure Exec;
  public
    { ������� }
    procedure ������������������_Exec;
  end;

implementation

{ _SQLPlus }

procedure _SQLPlus.Exec;
begin
end;

procedure _SQLPlus.������������������_Exec;
begin
end;

initialization
  RegisterTest(_SQLPlus.Suite);

end.

