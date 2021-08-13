library PLSQLDev_Plugin;

{$R *.res}

uses
  Windows,
  SysUtils,
  Classes,
  PlugInIntf in 'Source\PLSQL_Developer\PlugInIntf.pas',
  PLSQLDev_Formatter in 'Source\PLSQL_Developer\PLSQLDev_Formatter.pas';

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

(*
var SelectedText, FullText, Text, Formatted, Result: string;
begin
  { Найдём обрабатываемый текст }
  SelectedText := String(IDE_GetSelectedText);
  FullText := String(IDE_GetText);
  if SelectedText <> ''
    then Text := SelectedText
    else Text := FullText;
  { Отформатируем его }
  Controller.MakeFormatted(Text, nil, Formatted);
  if Formatted = '' then exit;
  { И выдадим обратно в PL/SQL Developer }
  case Index of
    1: begin
         Result := StringReplace(FullText, Text, Formatted, [rfReplaceAll]);
         TextForOutput := AnsiString(Result);
         IDE_SetText(PAnsiChar(TextForOutput));
       end;
    2: begin
         TextForOutput := AnsiString(Formatted);
         IDE_CreateWindow(wtSQL, PAnsiChar(TextForOutput), false);
       end;
  end;
end; *)

exports
  IdentifyPlugIn,
  CreateMenuItem,
  OnMenuClick,
  RegisterCallback,
  OnActivate,
  OnDeactivate;

end.

