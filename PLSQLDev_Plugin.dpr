library PLSQLDev_Plugin;

{$R *.res}

uses
  Windows,
  SysUtils,
  Classes,
  Alter in 'Source\DDL\Alter.pas',
  Create in 'Source\DDL\Create.pas',
  DatabaseLink in 'Source\DDL\DatabaseLink.pas',
  Role in 'Source\DDL\Role.pas',
  Sequence in 'Source\DDL\Sequence.pas',
  Synonym in 'Source\DDL\Synonym.pas',
  Trigger in 'Source\PLSQL\Trigger.pas',
  BasePrinter in 'Source\Core\BasePrinter.pas',
  Commons in 'Source\Core\Commons.pas',
  Controller in 'Source\Core\Controller.pas',
  DDL in 'Source\DDL.pas',
  DML in 'Source\DML.pas',
  Expressions in 'Source\Core\Expressions.pas',
  FormatterPrinter in 'Source\Core\FormatterPrinter.pas',
  Keywords in 'Source\Core\Keywords.pas',
  Parser in 'Source\Core\Parser.pas',
  PLSQL in 'Source\PLSQL.pas',
  Printer in 'Source\Core\Printer.pas',
  Rulers in 'Source\Core\Rulers.pas',
  SQLPlus in 'Source\SQLPlus.pas',
  Statements in 'Source\Core\Statements.pas',
  Streams in 'Source\Core\Streams.pas',
  TextBuilder in 'Source\Core\TextBuilder.pas',
  Tokenizer in 'Source\Core\Tokenizer.pas',
  Tokens in 'Source\Core\Tokens.pas',
  Utils in 'Source\Core\Utils.pas',
  Select in 'Source\DML\Select.pas',
  Insert in 'Source\DML\Insert.pas',
  Session in 'Source\DDL\Session.pas',
  Set_ in 'Source\DDL\Set_.pas',
  Set_SQLPlus in 'Source\SQL_Plus\Set_SQLPlus.pas',
  Exit_SQLPlus in 'Source\SQL_Plus\Exit_SQLPlus.pas',
  AlterPackageProcedureFunction in 'Source\DDL\AlterPackageProcedureFunction.pas',
  Grant in 'Source\DDL\Grant.pas',
  Drop in 'Source\DDL\Drop.pas',
  Goto_ in 'Source\PLSQL\Goto_.pas',
  Exit_ in 'Source\PLSQL\Exit_.pas',
  OpenFor in 'Source\PLSQL\OpenFor.pas',
  ForAll in 'Source\PLSQL\ForAll.pas',
  Assignment in 'Source\PLSQL\Assignment.pas',
  Label_ in 'Source\PLSQL\Label_.pas',
  Intervals in 'Source\Core\Intervals.pas',
  DML_Commons in 'Source\DML\DML_Commons.pas',
  Update in 'Source\DML\Update.pas',
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

