unit TestBugs;

interface

uses TestFramework, FileBasedTest;

type
  _Bugs = class(TFileBasedTest)
  protected
    function GetDir: string; override;
    function GetExtIn: string; override;
  published
    procedure _1;
    procedure _2;
    procedure _3;
    procedure _4;
    procedure _5;
    procedure _6;
    procedure _7;
    procedure _8;
    procedure _9;
    procedure _10;
    procedure _11;
    procedure _12;
    procedure _13;
    procedure _14;
    procedure _15;
    procedure _16;
    procedure _17;
  end;

implementation

{ TBugs }

function _Bugs.GetDir: string;
begin
  Result := '..\����\';
end;

function _Bugs.GetExtIn: string;
begin
  Result := '.bug';
end;

procedure _Bugs._1;
begin
  Settings.AlignTableColumnComments := true;
end;

procedure _Bugs._2;
begin
end;

procedure _Bugs._3;
begin
end;

procedure _Bugs._4;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Bugs._5;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._6;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._7;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._8;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._9;
begin
end;

procedure _Bugs._10;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._11;
begin
end;

procedure _Bugs._12;
begin
end;

procedure _Bugs._13;
begin
end;

procedure _Bugs._14;
begin
end;

procedure _Bugs._15;
begin
end;

procedure _Bugs._16;
begin
  Settings.ArgumentSingleLineParamLimit := 1;
end;

procedure _Bugs._17;
begin
  Settings.PreferredExpressionLength := 120;
end;

initialization
  RegisterTest(_Bugs.Suite);

end.

