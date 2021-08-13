library PLSQLDev_Plugin;

{$R *.res}

uses
  Windows,
  SysUtils,
  Classes,
  OracleCore,
  PlugInIntf in 'Source\PLSQL_Developer\PlugInIntf.pas',
  PLSQLDev_Formatter in 'Source\PLSQL_Developer\PLSQLDev_Formatter.pas',
  fSettings in 'Source\GUI\fSettings.pas' {FormSettings},
  frSettings in 'Source\GUI\frSettings.pas' {FrameSettings: TFrame};

var
  KeepDescription, KeepItem: AnsiString;
  MenuItems: TStrings;

{ Сохранение идентификатора плагина для последующих вызовов ядра }
function IdentifyPlugIn(ID: integer): PAnsiChar; cdecl;
begin
  with TPLSQLDevPlugIn.GetInstance do
  begin
    Identify(ID);
    KeepDescription := AnsiString(Description);
    Result := @KeepDescription[1];
  end;
end;

{ Указание создаваемых пунктов меню }
function CreateMenuItem(Index: integer): PAnsiChar; cdecl;
begin
  { Получим список пунктов меню }
  if not Assigned(MenuItems) then
  begin
    MenuItems := TStringList.Create;
    TPLSQLDevPlugIn.GetInstance.GetMenuItems(MenuItems);
  end;
  { И вернём соответствующий }
  if Index > MenuItems.Count
    then KeepItem := ''
    else KeepItem := AnsiString(MenuItems[Index - 1]);
  Result := @KeepItem[1];
end;

{ Выполнение операции, соответствующей пункту меню }
procedure OnMenuClick(Index: integer); cdecl;
begin
  TPLSQLDevPlugIn.GetInstance.MenuItemClick(Index);
end;

{ Действия при активации плагина }
procedure OnActivate; cdecl;
begin
  TPLSQLDevPlugIn.GetInstance.Activate;
end;

{ Действия при деактивации плагина }
procedure OnDeactivate; cdecl;
begin
  TPLSQLDevPlugIn.GetInstance.Deactivate;
end;

{ Строка описания плагина }
function About: PAnsiChar;
begin
  Result := @KeepDescription[1];
end;

{ Действия при вызове конфигурации }
procedure Configure; cdecl;
begin
  TFormSettings.ShowSettings;
end;

exports
  IdentifyPlugIn,
  CreateMenuItem,
  OnMenuClick,
  RegisterCallback,
  OnActivate,
  OnDeactivate,
  Configure,
  About;

end.

