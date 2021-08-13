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
    procedure _50;
    procedure _51;
    procedure _52;
    procedure _53;
    procedure _54;
    procedure _55;
    procedure _56;
    procedure _57;
    procedure _58;
    procedure _59;
    procedure _60;
    procedure _61;
    procedure _62;
    procedure _63;
    procedure _64;
    procedure _65;
    procedure _66;
    procedure _67;
    procedure _68;
    procedure _69;
    procedure _70;
    procedure _71;
    procedure _72;
    procedure _73;
    procedure _74;
    procedure _75;
    procedure _76;
    procedure _77;
    procedure _78;
    procedure _79;
    procedure _80;
    procedure _81;
    procedure _82;
    procedure _83;
    procedure _84;
    procedure _85;
    procedure _86;
    procedure _87;
    procedure _88;
    procedure _89;
    procedure _90;
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
  Settings.AlignCommands := true;
end;

procedure _Bugs._2;
begin
end;

procedure _Bugs._20;
begin
  Settings.PreferredExpressionLength := 82;
  Settings.BeautifyLongOperands := true;
end;

procedure _Bugs._21;
begin
  Settings.AlignVariables := true;
  Settings.AlignRightComments := true;
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
  Settings.AlignCommands := true;
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
  Settings.AlignCommands := true;
  Settings.AlignUseSpace := true;
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
  PostponeTill(2021, 8, 1);
  Settings.AlignFields := true;
  Settings.AlignUseSpace := true;
  Settings.AlignExpressions := true;
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._38;
begin
  Settings.PreferredExpressionLength := 120;
  Settings.BeautifyLongOperands := true;
end;

procedure _Bugs._39;
begin
  Settings.PreferredExpressionLength := 120;
  Settings.BeautifyLongOperands := true;
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
  Settings.BeautifyLongOperands := true;
end;

procedure _Bugs._49;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._5;
begin
end;

procedure _Bugs._50;
begin
  Settings.AlignVariables := true;
  Settings.AlignSpecialComments := true;
  Settings.MatchParamLimit := 5;
  Settings.PreferredExpressionLength := 80;
  Settings.AlignUseSpace := true;
end;

procedure _Bugs._51;
begin
  Settings.AlignVariables := true;
  Settings.ChangeCommentType := true;
  Settings.AlignRightComments := true;
end;

procedure _Bugs._52;
begin
end;

procedure _Bugs._53;
begin
  Settings.AlignCommands := true;
  Settings.AlignUseSpace := true;
end;

procedure _Bugs._54;
begin
end;

procedure _Bugs._55;
begin
end;

procedure _Bugs._56;
begin
  Settings.AlignFields := true;
end;

procedure _Bugs._57;
begin
end;

procedure _Bugs._58;
begin
  Settings.PreferredExpressionLength := 80;
  Settings.BeautifyLongOperands := true;
end;

procedure _Bugs._59;
begin
  Settings.AlignCommands := true;
end;

procedure _Bugs._6;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._60;
begin
  Settings.AlignCommands := true;
end;

procedure _Bugs._61;
begin
  Settings.AlignFields := true;
  Settings.AlignUseSpace := false;
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._62;
begin
  Settings.AlignFields := true;
  Settings.AlignUseSpace := true;
  Settings.PreferredExpressionLength := 70;
end;

procedure _Bugs._63;
begin
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._64;
begin
  Settings.MatchParamLimit := 3;
  Settings.AlignSpecialComments := true;
  Settings.AlignUseSpace := true;
  Settings.AlignFields := true;
end;

procedure _Bugs._65;
begin
  Settings.MatchParamLimit := 3;
  Settings.AlignSpecialComments := true;
  Settings.AlignUseSpace := true;
end;

procedure _Bugs._66;
begin
  Settings.MatchParamLimit := 3;
  Settings.AlignSpecialComments := true;
end;

procedure _Bugs._67;
begin
  Settings.MatchParamLimit := 3;
end;

procedure _Bugs._68;
begin
  Settings.MatchParamLimit := 1;
  Settings.AlignSpecialComments := true;
  Settings.AlignUseSpace := true;
end;

procedure _Bugs._69;
begin
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._7;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._70;
begin
  Settings.PreferredExpressionLength := 100;
  Settings.MatchParamLimit := 3;
  Settings.AlignSpecialComments := true;
  Settings.AlignUseSpace := true;
end;

procedure _Bugs._71;
begin
  Settings.AlignVariables := true;
  Settings.AlignRightComments := true;
end;

procedure _Bugs._72;
begin
  PostponeTill(2021, 8, 1);
  Settings.PreferredExpressionLength := 100;
  Settings.AlignExpressions := true;
  Settings.AlignRightComments := true;
end;

procedure _Bugs._73;
begin
end;

procedure _Bugs._74;
begin
end;

procedure _Bugs._75;
begin
end;

procedure _Bugs._76;
begin
  Settings.AlignFields := true;
  Settings.AlignExpressions := true;
end;

procedure _Bugs._77;
begin
  PostponeTill(2021, 8, 1);
  Settings.AlignExpressions := true;
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._78;
begin
  Settings.AlignExpressions := true;
end;

procedure _Bugs._79;
begin
  Settings.AlignFields := true;
  Settings.AlignUseSpace := true;
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._8;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._80;
begin
  Settings.PreferredExpressionLength := 60;
end;

procedure _Bugs._81;
begin
  Settings.AlignVariables := true;
end;

procedure _Bugs._82;
begin
  Settings.AlignFields := true;
  Settings.AlignRightComments := true;
  Settings.AlignUseSpace := true;
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._83;
begin
end;

procedure _Bugs._84;
begin
  Settings.AlignFields := true;
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._85;
begin
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._86;
begin
end;

procedure _Bugs._87;
begin
end;

procedure _Bugs._88;
begin
  Settings.PreferredExpressionLength := 100;
end;

procedure _Bugs._89;
begin
  Settings.CorrectCommentSpaces := true;
end;

procedure _Bugs._9;
begin
end;

procedure _Bugs._90;
begin
end;

procedure _Bugs._10;
begin
  Settings.AlignVariables := true;
  Settings.AlignRightComments := true;
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
  Settings.PreferredExpressionLength := 100;
end;

initialization
  RegisterTest(_Bugs.Suite);

end.

