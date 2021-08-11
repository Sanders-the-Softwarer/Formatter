unit TestSettings;

interface

uses
  SysUtils, TestFramework, FileBasedTest, Printer;

type

  { Тесты на настройки форматизатора }
  _Settings = class(TFileBasedTest)
  protected
    function GetDir: string; override;
  published
    procedure Выравнивать_Переменные;
    procedure Выравнивать_Поля;
    procedure Выравнивать_Колонки;
    procedure Выравнивать_Выражения;
    procedure Выравнивать_Команды;
    procedure Выравнивать_Спецкомментарии;
    procedure Заполнять_Пустоты;
    procedure Заменять_Default;
    procedure Заменять_As_На_Is;
    procedure Менять_Типы_Комментариев;
    procedure Исправлять_Пробелы_В_Комментариях;
    procedure Добавлять_In;
    procedure Добавлять_From_В_Delete;
    procedure Удалять_Пароли_Из_Connect;
    procedure Учитывать_Длинные_Операнды;
    procedure Не_Выравнивать_Переменные;
    procedure Не_Выравнивать_Поля;
    procedure Не_Выравнивать_Колонки;
    procedure Не_Выравнивать_Выражения;
    procedure Не_Выравнивать_Команды;
    procedure Не_Выравнивать_Спецкомментарии;
    procedure Не_Заполнять_Пустоты;
    procedure Не_Заменять_Default;
    procedure Не_Заменять_As_На_Is;
    procedure Не_Менять_Типы_Комментариев;
    procedure Не_Исправлять_Пробелы_В_Комментариях;
    procedure Не_Добавлять_In;
    procedure Не_Добавлять_From_В_Delete;
    procedure Не_Удалять_Пароли_Из_Connect;
    procedure Не_Учитывать_Длинные_Операнды;
    procedure Предел_Параметров_В_Строке;
    procedure Предел_Именованных_Аргументов_В_Строке;
    procedure Предел_Позиционных_Аргументов_В_Строке;
    procedure Предел_Сопоставляемых_Параметров;
    procedure Предпочитаемая_Длина_Выражений;
    procedure Сдвигать_Декларации_В_Пакете;
    procedure Сдвигать_Декларации_В_Теле_Пакета;
    procedure Не_Сдвигать_Декларации_В_Пакете;
    procedure Не_Сдвигать_Декларации_В_Теле_Пакета;
    procedure Стартовый_Сдвиг;
  end;

implementation

{ _Settings }

function _Settings.GetDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(inherited GetDir) + '\Настройки';
end;

procedure _Settings.Выравнивать_Выражения;
begin
  Settings.AlignExpressions := true;
end;

procedure _Settings.Выравнивать_Колонки;
begin
  Settings.AlignColumns := true;
  Settings.AlignRightComments := true;
end;

procedure _Settings.Выравнивать_Команды;
begin
  Settings.AlignCommands := true;
end;

procedure _Settings.Выравнивать_Переменные;
begin
  Settings.AlignVariables := true;
end;

procedure _Settings.Выравнивать_Поля;
begin
  Settings.AlignFields := true;
end;

procedure _Settings.Выравнивать_Спецкомментарии;
begin
  Settings.AlignSpecialComments := true;
  Settings.MatchParamLimit := 1;
end;

procedure _Settings.Добавлять_From_В_Delete;
begin
  Settings.AddFromToDelete := true;
end;

procedure _Settings.Добавлять_In;
begin
  Settings.AddInAccessSpecificator := true;
end;

procedure _Settings.Заменять_As_На_Is;
begin
  Settings.ReplaceAsIs := true;
end;

procedure _Settings.Заменять_Default;
begin
  Settings.ReplaceDefault := true;
end;

procedure _Settings.Заполнять_Пустоты;
begin
  PostponeTill(2021, 8, 1);
  Settings.AlignUseSpace := true;
  Settings.AlignVariables := true;
  Settings.AlignFields := true;
end;

procedure _Settings.Исправлять_Пробелы_В_Комментариях;
begin
  Settings.CorrectCommentSpaces := true;
end;

procedure _Settings.Менять_Типы_Комментариев;
begin
  Settings.ChangeCommentType := true;
end;

procedure _Settings.Не_Выравнивать_Выражения;
begin
  Settings.AlignExpressions := false;
end;

procedure _Settings.Не_Выравнивать_Колонки;
begin
  Settings.AlignColumns := false;
end;

procedure _Settings.Не_Выравнивать_Команды;
begin
  Settings.AlignCommands := false;
end;

procedure _Settings.Не_Выравнивать_Переменные;
begin
  Settings.AlignVariables := false;
end;

procedure _Settings.Не_Выравнивать_Поля;
begin
  Settings.AlignFields := false;
end;

procedure _Settings.Не_Выравнивать_Спецкомментарии;
begin
  Settings.AlignSpecialComments := false;
  Settings.MatchParamLimit := 1;
end;

procedure _Settings.Не_Добавлять_From_В_Delete;
begin
  Settings.AddFromToDelete := false;
end;

procedure _Settings.Не_Добавлять_In;
begin
  Settings.AddInAccessSpecificator := false;
end;

procedure _Settings.Не_Заменять_As_На_Is;
begin
  Settings.ReplaceAsIs := false;
end;

procedure _Settings.Не_Заменять_Default;
begin
  Settings.ReplaceDefault := false;
end;

procedure _Settings.Не_Заполнять_Пустоты;
begin
  Settings.AlignUseSpace := false;
  Settings.AlignVariables := true;
  Settings.AlignFields := true;
end;

procedure _Settings.Не_Исправлять_Пробелы_В_Комментариях;
begin
  Settings.CorrectCommentSpaces := false;
end;

procedure _Settings.Не_Менять_Типы_Комментариев;
begin
  Settings.ChangeCommentType := false;
end;

procedure _Settings.Не_Сдвигать_Декларации_В_Пакете;
begin
  Settings.ShiftPackageHeader := false;
  Settings.ShiftPackageBody   := false;
end;

procedure _Settings.Не_Сдвигать_Декларации_В_Теле_Пакета;
begin
  Settings.ShiftPackageHeader := false;
  Settings.ShiftPackageBody   := false;
end;

procedure _Settings.Не_Удалять_Пароли_Из_Connect;
begin
  Settings.RemoveConnectPasswords := false;
end;

procedure _Settings.Не_Учитывать_Длинные_Операнды;
begin
  Settings.BeautifyLongOperands := false;
  Settings.PreferredExpressionLength := 40;
end;

procedure _Settings.Предел_Именованных_Аргументов_В_Строке;
begin
  Settings.NamedArgumentSingleLineParamLimit := 3;
end;

procedure _Settings.Предел_Параметров_В_Строке;
begin
  Settings.DeclarationSingleLineParamLimit := 3;
end;

procedure _Settings.Предел_Позиционных_Аргументов_В_Строке;
begin
  Settings.PositionalArgumentSingleLineParamLimit := 3;
end;

procedure _Settings.Предел_Сопоставляемых_Параметров;
begin
  Settings.MatchParamLimit := 3;
end;

procedure _Settings.Предпочитаемая_Длина_Выражений;
begin
  Settings.PreferredExpressionLength := 40;
end;

procedure _Settings.Сдвигать_Декларации_В_Пакете;
begin
  Settings.ShiftPackageHeader := true;
  Settings.ShiftPackageBody := false;
end;

procedure _Settings.Сдвигать_Декларации_В_Теле_Пакета;
begin
  Settings.ShiftPackageHeader := false;
  Settings.ShiftPackageBody := true;
end;

procedure _Settings.Стартовый_Сдвиг;
begin
  Settings.StartIndent := 7;
end;

procedure _Settings.Удалять_Пароли_Из_Connect;
begin
  Settings.RemoveConnectPasswords := true;
end;

procedure _Settings.Учитывать_Длинные_Операнды;
begin
  Settings.BeautifyLongOperands := true;
  Settings.PreferredExpressionLength := 40;
end;

initialization
  RegisterTest(_Settings.Suite);

end.
