////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                         ��������� �� ������ SQL*Plus                       //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestSQLPLUS;

interface

uses
  SysUtils, TestFramework, FileBasedTest;

type

  { ����� �� ������� SQL*Plus }
  _�������_SQLPLUS = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Chcp;
    procedure Define;
    procedure Exec;
    procedure Set_;
    procedure Whenever;
  end;

implementation

{ _SQLPlus }

function _�������_SQLPLUS.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\������� SQLPLUS';
end;

procedure _�������_SQLPLUS.Define;
begin
end;

procedure _�������_SQLPLUS.Set_;
begin
end;

procedure _�������_SQLPLUS.Whenever;
begin
end;

procedure _�������_SQLPLUS.Chcp;
begin
end;

procedure _�������_SQLPLUS.Exec;
begin
end;

initialization
  RegisterTest(_�������_SQLPLUS.Suite);

end.


