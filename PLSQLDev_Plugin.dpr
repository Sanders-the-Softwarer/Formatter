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
  BasePrinter in 'Source\BasePrinter.pas',
  Commons in 'Source\Commons.pas',
  Controller in 'Source\Controller.pas',
  DDL in 'Source\DDL.pas',
  DML in 'Source\DML.pas',
  Expressions in 'Source\Expressions.pas',
  FormatterPrinter in 'Source\FormatterPrinter.pas',
  Keywords in 'Source\Keywords.pas',
  Parser in 'Source\Parser.pas',
  PLSQL in 'Source\PLSQL.pas',
  Printer in 'Source\Printer.pas',
  Rulers in 'Source\Rulers.pas',
  SQLPlus in 'Source\SQLPlus.pas',
  Statements in 'Source\Statements.pas',
  Streams in 'Source\Streams.pas',
  TextBuilder in 'Source\TextBuilder.pas',
  Tokenizer in 'Source\Tokenizer.pas',
  Tokens in 'Source\Tokens.pas',
  Utils in 'Source\Utils.pas',
  Select in 'Source\DML\Select.pas',
  Set_ in 'Source\DDL\Set_.pas';

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

