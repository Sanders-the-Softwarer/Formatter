////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Тесты на найденные ошибки                         //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TestBugs;

interface

uses TestFramework, FileBasedTest;

type
  { Автотесты на найденные ошибки, просто по порядковым номерам }
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
    procedure _18;
    procedure _19;
    procedure _20;
    procedure _21;
    procedure _22;
    procedure _23;
    procedure _24;
    procedure _25;
    procedure _26;
    procedure _27;
    procedure _28;
    procedure _29;
    procedure _30;
    procedure _31;
    procedure _32;
    procedure _33;
    procedure _34;
    procedure _35;
    procedure _36;
    procedure _37;
    procedure _38;
    procedure _39;
    procedure _40;
    procedure _41;
    procedure _42;
    procedure _43;
    procedure _44;
    procedure _45;
    procedure _46;
    procedure _47;
    procedure _48;
    procedure _49;
  end;

implementation

{ TBugs }

function _Bugs.GetDir: string;
begin
  Result := '..\Баги\';
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

procedure _Bugs._20;
begin
  Settings.PreferredExpressionLength := 82;
end;

procedure _Bugs._21;
begin
  Settings.AlignVariables := true;
  Settings.ChangeCommentType := true;
  PostponeTill(2020, 4, 30);
end;

procedure _Bugs._22;
begin
end;

procedure _Bugs._23;
begin
end;

procedure _Bugs._24;
begin
end;

procedure _Bugs._25;
begin
  Settings.AlignSQLPLUS := true;
  PostponeTill(2020, 4, 30);
end;

procedure _Bugs._26;
begin
end;

procedure _Bugs._27;
begin
end;

procedure _Bugs._28;
begin
  Settings.ChangeCommentType := true;
end;

procedure _Bugs._29;
begin
end;

procedure _Bugs._3;
begin
end;

procedure _Bugs._30;
begin
end;

procedure _Bugs._31;
begin
  Settings.AlignSQLPLUS := true;
end;

procedure _Bugs._32;
begin
  Settings.PreferredExpressionLength := 100;
  Settings.NamedArgumentSingleLineParamLimit := 1;
end;

procedure _Bugs._33;
begin
  Settings.PreferredExpressionLength := 120;
end;

procedure _Bugs._34;
begin
end;

procedure _Bugs._35;
begin
  Settings.PreferredExpressionLength := 120;
end;

procedure _Bugs._36;
begin
  Settings.PreferredExpressionLength := 120;
end;

procedure _Bugs._37;
begin
  PostponeTill(2020, 4, 30);
  Settings.PreferredExpressionLength := 120;
end;

procedure _Bugs._38;
begin
  PostponeTill(2020, 4, 30);
  Settings.PreferredExpressionLength := 120;
end;

procedure _Bugs._39;
begin
  Settings.PreferredExpressionLength := 120;
end;

procedure _Bugs._4;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Bugs._40;
begin
end;

procedure _Bugs._41;
begin
end;

procedure _Bugs._42;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Bugs._43;
begin
end;

procedure _Bugs._44;
begin
end;

procedure _Bugs._45;
begin
end;

procedure _Bugs._46;
begin
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._47;
begin
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._48;
begin
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._49;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._5;
begin
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
  Settings.NamedArgumentSingleLineParamLimit := 1;
  Settings.PositionalArgumentSingleLineParamLimit := 4;
  Settings.PreferredExpressionLength := 120;
end;

procedure _Bugs._16;
begin
  Settings.NamedArgumentSingleLineParamLimit := 1;
end;

procedure _Bugs._17;
begin
  Settings.PreferredExpressionLength := 100;
  Settings.NamedArgumentSingleLineParamLimit := 1;
end;

procedure _Bugs._18;
begin
end;

procedure _Bugs._19;
begin
  Settings.PreferredExpressionLength := 120;
  PostponeTill(2020, 4, 30);
end;

initialization
  RegisterTest(_Bugs.Suite);

end.

