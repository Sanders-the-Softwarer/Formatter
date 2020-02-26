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
  end;

implementation

{ TBugs }

procedure _Bugs.CheckFile(const AFileName: string);
var S: string;
begin
  S := LoadFile('.\Ошибки\' + Copy(AFileName, 4, 255) + '.bug');
  Check(S, S);
end;

procedure _Bugs.Bug1;
begin
  Settings.AlignTableColumnComments := true;
end;

procedure _Bugs.Bug2;
begin
end;

initialization
  RegisterTest(_Bugs.Suite);

end.

