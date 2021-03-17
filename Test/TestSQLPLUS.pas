////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                         Автотесты на модуль SQL*Plus                       //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestSQLPLUS;

interface

uses
  SysUtils, TestFramework, FileBasedTest;

type

  { Тесты на команды SQL*Plus }
  _Команды_SQLPLUS = class(TFileBasedTest)
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

function _Команды_SQLPLUS.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Команды SQLPLUS';
end;

procedure _Команды_SQLPLUS.Define;
begin
end;

procedure _Команды_SQLPLUS.Set_;
begin
end;

procedure _Команды_SQLPLUS.Whenever;
begin
end;

procedure _Команды_SQLPLUS.Chcp;
begin
end;

procedure _Команды_SQLPLUS.Exec;
begin
end;

initialization
  RegisterTest(_Команды_SQLPLUS.Suite);

end.


