unit TestBugs;

interface

uses TestFramework, FileBasedTest;

type
  _Bugs = class(TFileBasedTest)
  protected
    procedure CheckFile(const AFileName: string); override;
  published
    procedure Bug1;
    procedure Bug2;
    procedure Bug3;
    procedure Bug4;
    procedure Bug5;
    procedure Bug6;
    procedure Bug7;
    procedure Bug8;
    procedure Bug9;
  end;

implementation

{ TBugs }

procedure _Bugs.CheckFile(const AFileName: string);
var S: string;
begin
  S := LoadFile('..\Баги\' + Copy(AFileName, 4, 255) + '.bug');
  Check(S, S);
end;

procedure _Bugs.Bug1;
begin
  Settings.AlignTableColumnComments := true;
end;

procedure _Bugs.Bug2;
begin
end;

procedure _Bugs.Bug3;
begin
end;

procedure _Bugs.Bug4;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Bugs.Bug5;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs.Bug6;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs.Bug7;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs.Bug8;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs.Bug9;
begin
end;

initialization
  RegisterTest(_Bugs.Suite);

end.

