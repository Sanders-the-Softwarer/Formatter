////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                         Автотесты на модуль SQL*Plus                       //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestSQLPLUS;

interface

uses
  TestFramework, FileBasedTest;

type

  { Тесты на SQL*Plus }
  _SQLPlus = class(TFileBasedTest)
  published
    procedure Exec;
  end;

implementation

{ _SQLPlus }

procedure _SQLPlus.Exec;
begin
end;

initialization
  RegisterTest(_SQLPlus.Suite);

end.


