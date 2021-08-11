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
    procedure Clear;
    procedure Define;
    procedure Execute;
    procedure Accept;

    procedure Set_;
    procedure Whenever;
    procedure Exit;

    procedure At;
    procedure Slash;
    procedure Append;
    procedure ArchiveLog;
    procedure Attribute;
    procedure Break;
    procedure BTitle;
    procedure Change;
    procedure Column;
    procedure Compute;
    procedure Connect;
    procedure Copy;
    procedure Del;
    procedure Describe;
    procedure Disconnect;
    procedure Edit;
    procedure Get;
    procedure Help;
    procedure History;
    procedure Host;
    procedure Input;
    procedure List;
    procedure Password;
    procedure Pause;
    procedure Print;
    procedure Prompt;
    procedure Recover;
    procedure Remark;
    procedure RepHeader;
    procedure RepFooter;
    procedure Run;
    procedure Save;
    procedure Show;
    procedure Shutdown;
    procedure Spool;
    procedure Start;
    procedure Startup;
    procedure Store;
    procedure Timing;
    procedure Ttitle;
    procedure Undefine;
    procedure Variable;
    procedure XQuery;
  end;

implementation

{ _SQLPlus }

function _Команды_SQLPLUS.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Команды SQLPLUS';
end;

procedure _Команды_SQLPLUS.Get;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Accept;
begin
  Settings.AlignCommands := true;
end;

procedure _Команды_SQLPLUS.Append;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.ArchiveLog;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.At;
begin
end;

procedure _Команды_SQLPLUS.Attribute;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Break;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.BTitle;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Change;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Help;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.History;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Host;
begin
  Settings.AlignCommands := true;
end;

procedure _Команды_SQLPLUS.Input;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.List;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Password;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Pause;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Print;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Prompt;
begin
end;

procedure _Команды_SQLPLUS.Recover;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Remark;
begin
end;

procedure _Команды_SQLPLUS.RepFooter;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.RepHeader;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Run;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Clear;
begin
end;

procedure _Команды_SQLPLUS.Column;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Compute;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Connect;
begin
  PostponeTill(2021, 9, 1);
end;

procedure _Команды_SQLPLUS.Copy;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Define;
begin
  Settings.AlignCommands := true;
end;

procedure _Команды_SQLPLUS.Del;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Describe;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Disconnect;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Save;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Set_;
begin
end;

procedure _Команды_SQLPLUS.Show;
begin
end;

procedure _Команды_SQLPLUS.Shutdown;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Slash;
begin
end;

procedure _Команды_SQLPLUS.Spool;
begin
end;

procedure _Команды_SQLPLUS.Start;
begin
end;

procedure _Команды_SQLPLUS.Startup;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Store;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Timing;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Ttitle;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Undefine;
begin
  Settings.AlignCommands := true;
end;

procedure _Команды_SQLPLUS.Variable;
begin
  Settings.AlignCommands := true;
end;

procedure _Команды_SQLPLUS.Whenever;
begin
end;

procedure _Команды_SQLPLUS.Edit;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.XQuery;
begin
  PostponeTill(2021, 9, 30);
end;

procedure _Команды_SQLPLUS.Execute;
begin
end;

procedure _Команды_SQLPLUS.Exit;
begin
end;

initialization
  RegisterTest(_Команды_SQLPLUS.Suite);

end.
