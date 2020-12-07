library PLSQLDev_Plugin;

{$R *.res}

uses
  Windows,
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
  Stats in 'Source\Core\Stats.pas',
  Drop in 'Source\DDL\Drop.pas',
  Label_ in 'Source\PLSQL\Label_.pas';

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
var Text, Result: string;
begin
  Text := String(IDE_GetSelectedText);
  if Text = '' then Text := String(IDE_GetText);
  if Text = '' then exit;
  Controller.MakeFormatted(Text, nil, Result);
  if Result = '' then exit;
  TextForOutput := AnsiString(Result);
  case Index of
    1: IDE_SetText(PAnsiChar(TextForOutput));
    2: IDE_CreateWindow(wtSQL, PAnsiChar(TextForOutput), false);
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

