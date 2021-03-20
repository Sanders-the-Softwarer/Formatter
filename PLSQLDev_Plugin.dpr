library PLSQLDev_Plugin;

{$R *.res}

uses
  Windows,
  SysUtils,
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
  Intervals in 'Source\Core\Intervals.pas';

const
  Desc  = 'Formatter';
  wtSQL = 1;

var
  IDE_GetSelectedText: function: PAnsiChar; cdecl;
  IDE_GetText        : function: PAnsiChar;
  IDE_SetText        : function(Text: PAnsiChar): bool; cdecl;
  IDE_CreateWindow   : procedure(WindowType: Integer; Text: PAnsiChar; Execute: bool); cdecl;

var
  PluginID: Integer;
  TextForOutput: AnsiString;

function IdentifyPlugIn(ID: integer): PAnsiChar; cdecl;
begin
  PluginID := ID;
  Result := Desc;
end;

function CreateMenuItem(Index: integer): PAnsiChar; cdecl;
begin
  case Index of
    1: Result := 'SM Lab / Formatter';
    2: Result := 'SM Lab / Formatter (new window)';
    else Result := '';
  end;
end;

procedure OnMenuClick(Index: integer); cdecl;
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


end;

procedure RegisterCallback(Index: Integer; Addr: Pointer); cdecl;
begin
  case Index of
    20: @IDE_CreateWindow    := Addr;
    30: @IDE_GetText         := Addr;
    31: @IDE_GetSelectedText := Addr;
    34: @IDE_SetText         := Addr;
  end;
end;

exports
  IdentifyPlugIn,
  CreateMenuItem,
  OnMenuClick,
  RegisterCallback;

end.

