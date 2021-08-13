////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                    Синтаксические конструкции SQL*Plus                     //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit SQLPlus;

interface

uses SysUtils, Statements, Streams, Tokens, Parser, Printer, Contnrs,
  System.Generics.Collections;

type

  { Базовый класс команд SQL*Plus }
  TSQLPlusStatement = class(TSemicolonStatement)
  strict protected
    { Считывание лексемы "символы до конца строки" }
    function SqlPlusString: TTerminal;
    { Учёт сокращённой формы ключевых слов SQL*Plus }
    function Keyword(const AKeyword: string): TEpithet; overload;
    function Keyword(const AKeywords: array of string): TEpithet; overload;
  public
    function Grouping: TStatementClass; override;
    function SameTypeAligned: TAlignMode; override;
  end;

  { Команда whenever }
  TWhenever = class(TSQLPlusStatement)
  strict private
    _Whenever, _Condition, _Action, _Param1, _Param2: TEpithet;
    _Expr: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Команда spool }
  TSpool = class(TSQLPlusStatement)
  strict private
    _Spool: TEpithet;
    _FileName: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Анонимный блок в SQL*Plus }
  TStandaloneAnonymousBlock = class(TStatement)
  strict private
    _Block: TStatement;
    _Slash: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Команда connect }
  TConnect = class(TSQLPlusStatement)
  strict private
    _Connect: TEpithet;
    _ConnectString: TTerminal;
    procedure RemovePassword;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ Парсер для SQLPlus }
function SQLPlusParser: TParserInfo;

implementation

uses Expressions, Commons, PLSQL, Keywords, Set_SQLPlus, Exit_SQLPlus, Clear,
  Define, Execute, At, Slash, Accept, Host, Variable, Undefine, Controller;

var
  SQLPlusParserInfo: TParserInfo;

{ Парсер для DML }
function SQLPlusParser: TParserInfo;
begin
  if not Assigned(SQLPlusParserInfo) then SQLPlusParserInfo := TParserInfo.Create;
  Result := SQLPlusParserInfo;
end;

{ Роспись вариантов ключевых слов, заданных как 'undef[ine]' }
function ExpandKeyword(const AKeyword: string): TArray<string>;
var
  p1, p2, i: integer;
  S: string;
begin
  Result := [AKeyword];
  p1 := Pos('[', AKeyword);
  p2 := Pos(']', AKeyword);
  if (p1 <= 0) or (p2 <= p1) or (p2 < Length(AKeyword)) then exit;
  S := Copy(AKeyword, 1, p1 - 1);
  Result := [S];
  for i := p1 + 1 to p2 - 1 do
  begin
    S := S + AKeyword[i];
    Result := Result + [S];
  end;
end;

{ Регистрация ключевых слов, заданных как 'undef[ine]' }
procedure RegisterKeywords(AStatementClass: TStatementClass; AKeywords: array of string);
var Keyword: string;
begin
  for Keyword in AKeywords do
    Keywords.RegisterKeywords(AStatementClass, ExpandKeyword(Keyword));
end;

{ TWhenever }

function TWhenever.InternalParse: boolean;
begin
  Result := true;
  _Whenever := Keyword('whenever');
  if not Assigned(_Whenever) then exit(false);
  _Condition := Keyword(['sqlerror', 'oserror']);
  { В случае continue }
  _Action := Keyword('continue');
  if Assigned(_Action) then _Param1 := Keyword(['commit', 'rollback', 'none']);
  { В случае exit }
  if not Assigned(_Action) then
  begin
    _Action := Keyword('exit');
    _Param1 := Keyword(['success', 'failure', 'warning']);
    if not Assigned(_Param1) then TExpression.Parse(Self, Source, _Expr);
    _Param2 := Keyword(['commit', 'rollback']);
  end;
  inherited;
end;

procedure TWhenever.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('Cmd', [_Whenever, _Condition]);
  APrinter.PrintRulerItems('Action', [_Action, _Param1, _Expr, _Param2]);
  inherited;
end;

{ TSpool }

function TSpool.InternalParse: boolean;
begin
  _Spool := Keyword('spool');
  Result := Assigned(_Spool);
  if Result then _FileName := SqlPlusString;
  inherited;
end;

procedure TSpool.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Spool, _FileName]);
  inherited;
end;

{ TStandaloneAnonymousBlock }

function TStandaloneAnonymousBlock.InternalParse: boolean;
begin
  Result := TAnonymousBlock.Parse(Self, Source, _Block);
  if Result then _Slash := Terminal('/');
end;

procedure TStandaloneAnonymousBlock.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Block);
  APrinter.NextLineIf(_Slash);
end;

{ TConnect }

function TConnect.InternalParse: boolean;
begin
  Result := true;
  _Connect := Keyword('connect');
  if not Assigned(_Connect) then exit(false);
  _ConnectString := SqlPlusString;
  inherited;
end;

procedure TConnect.InternalPrintSelf(APrinter: TPrinter);
begin
  RemovePassword;
  APrinter.PrintItems([_Connect, _ConnectString]);
  inherited;
end;

procedure TConnect.RemovePassword;
var
  Str: string;
  p1, p2: integer;
begin
  if not Settings.RemoveConnectPasswords or not Assigned(_ConnectString) then exit;
  Str := _ConnectString.Value;
  p1 := Pos('/', Str);
  p2 := Pos('@', Str + '@');
  if p1 <= 0 then exit;
  _ConnectString.Value := Copy(Str, 1, p1 - 1) + Copy(Str, p2, length(Str));
end;

{ TSQLPlusStatement }

function TSQLPlusStatement.Grouping: TStatementClass;
begin
  Result := TStatementClass(Self.ClassType);
end;

function TSQLPlusStatement.SameTypeAligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignCommands);
end;

function TSQLPlusStatement.SqlPlusString: TTerminal;
var
  FirstToken, CurToken: TToken;
  Comment: TComment;
  Text: string;
  P: TMark;
begin
  Comment := nil;
  { Выделим лексемы до конца строки и сложим их в текстовую строку }
  P := Source.Mark;
  FirstToken := NextToken;
  CurToken := FirstToken;
  while (CurToken.Line = FirstToken.Line) and not (CurToken is TUnexpectedEOF)
        and not ((CurToken is TTerminal) and (CurToken.Value = ';')) do
  begin
    Text := Text + StringOfChar(' ', CurToken.Col - FirstToken.Col - Length(Text)) + CurToken.InitialValue;
    P := Source.Mark;
    CurToken.Printed := true; { лексема печатается в составе другой, поэтому гасим предупреждение }
    { Лексема не печатается, поэтому перепривязываем комментарии }
    if Assigned(CurToken.CommentAfter) then
    begin
      if Assigned(Comment)
        then Comment.CommentAfter := CurToken.CommentAfter
        else Comment := CurToken.CommentAfter;
      CurToken.CommentAfter := nil;
    end;
    { Идём дальше }
    CurToken := NextToken;
  end;
  Source.Restore(P);
  { Теперь сформируем лексему из этой строки, она и будет результатом }
  Result := TTerminal.Create(Text, FirstToken.Line, FirstToken.Col);
  ReplaceToken(FirstToken, Result);
  Result.CommentAfter := Comment;
end;

{ Учёт сокращённой формы ключевых слов SQL*Plus }

function TSQLPlusStatement.Keyword(const AKeyword: string): TEpithet;
begin
  Result := inherited Keyword(ExpandKeyword(AKeyword));
end;

function TSQLPlusStatement.Keyword(const AKeywords: array of string): TEpithet;
var
  Keywords: TArray<string>;
  S: string;
begin
  for S in AKeywords do Keywords := Keywords + ExpandKeyword(S);
  Result := inherited Keyword(Keywords);
end;

initialization
  { Команды SQL*Plus не разделяются точками с запятой, поэтому нужно явно
    назвать их ключевыми словами, в противном случае легко случится так,
    что начало следующей команды будет распознано как аргумент предыдущей
    whenever или подобной. По этой же причине здесь нужны стартовые
    ключевые слова основных SQL-комад }
  RegisterOrphan(TSQLPlusStatement);
  RegisterKeywords(TSQLPlusStatement, ['acc[ept]', 'appe[nd]', 'archive',
    'attr[ibute', 'bre[ak]', 'bti[tle]', 'cha[nge]', 'cl[ear]', 'col[umn]',
    'comp[ute]', 'conn[ect]', 'copy', 'def[ine]', 'del', 'desc[ribe]',
    'disc[onnect]', 'ed[it]', 'exec[ute]', 'exit', 'quit', 'get', 'help',
    'hist[ory]', 'ho[st]', 'in[put]', 'li[st]', 'passw[ord]', 'pau[se]',
    'print', 'pro[mpt]', 'recover', 'rem[ark]', 'repf[ooter]', 'reph[eader]',
    'ru[n]', 'sav[e]', 'set', 'sho[w]', 'shutdown', 'spo[ol]', 'sta[rt]',
    'startup', 'store', 'timi[ng]', 'tti[tle]', 'undef[ine]', 'var[iable]',
    'whenever', 'xquery', 'create', 'alter', 'begin', 'call', 'select',
    'insert', 'update', 'delete', 'merge', 'declare']);
  { Зарегистрируем конструкции SQL*Plus }
  with SQLPlusParser do
  begin
    Add(TClear);
    Add(TWhenever);
    Add(TSet);
    Add(TExit);
    Add(TAt);
    Add(TSpool);
    Add(TExecute);
    Add(TDefine);
    Add(TConnect);
    Add(TAccept);
    Add(TSlash);
    Add(THost);
    Add(TVariable);
    Add(TUndefine);
    Add(TStandaloneAnonymousBlock);
  end;
  { Добавим их в общеоракловый синтаксис }
  OracleParser.Add(SQLPlusParser);

finalization
  FreeAndNil(SQLPlusParserInfo);

end.

