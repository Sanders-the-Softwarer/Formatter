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

uses SysUtils, Statements, Streams, Tokens, Printer, Utils, Contnrs,
  System.Generics.Collections;

type

  { Парсер команд SQL*Plus }
  SQLPlusParser = class
  public
    class function Parse(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  end;

  { Базовый класс команд SQL*Plus }
  TSQLPlusStatement = class(TSemicolonStatement)
  strict private
    FreeList: TObjectList;
  strict protected
    { Считывание лексемы "символы до конца строки" }
    function SqlPlusString: TTerminal;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { Команда clear }
  TClear = class(TSQLPlusStatement)
  strict private
    _Clear: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Команда whenever }
  TWhenever = class(TSQLPlusStatement)
  strict private
    _Whenever, _Condition, _Action, _Param1, _Param2: TEpithet;
    _Expr: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

  { Команда define }
  TDefine = class(TSQLPlusStatement)
  strict private
    _Define, _Target: TEpithet;
    _Eq: TTerminal;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

  { Команда @ }
  TAt = class(TSQLPlusStatement)
  strict private
    _At, _FileName: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
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

  { Команда call }
  TCall = class(TSQLPlusStatement)
  strict private
    _Call: TEpithet;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
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

  { Команда CHCP }
  TChcp = class(TSQLPlusStatement)
  strict private
    _Chcp: TEpithet;
    _CodePage: TNumber;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Команда connect }
  TConnect = class(TSQLPlusStatement)
  strict private
    _Connect: TEpithet;
    _ConnectString: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Parser, Commons, PLSQL, Keywords, Set_SQLPlus, Exit_SQLPlus;

{ TClear }

function TClear.InternalParse: boolean;
begin
  _Clear := Keyword('clear');
  Result := Assigned(_Clear);
  inherited;
end;

procedure TClear.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Clear]);
  inherited;
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
    if not Assigned(_Param1) then TParser.ParseExpression(Self, Source, _Expr);
    _Param2 := Keyword(['commit', 'rollback']);
  end;
  inherited;
end;

procedure TWhenever.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignSQLPLUS);
  APrinter.PrintRulerItems('Cmd', [_Whenever, _Condition]);
  APrinter.PrintRulerItems('Action', [_Action, _Param1, _Expr, _Param2]);
  inherited;
end;

function TWhenever.Grouping: TStatementClass;
begin
  Result := TWhenever;
end;

{ TAt }

function TAt.InternalParse: boolean;
begin
  _At := Terminal(['@', '@@']);
  Result := Assigned(_At);
  if Result then _FileName := SqlPlusString;
  inherited;
end;

procedure TAt.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_At, _FileName]);
  inherited;
end;

function TAt.Grouping: TStatementClass;
begin
  Result := TAt;
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

{ TCall }

function TCall.InternalParse: boolean;
begin
  _Call := Keyword(['call', 'exec']);
  Result := Assigned(_Call);
  if Result then TQualifiedIndexedIdent.Parse(Self, Source, _What);
  Result := Result and inherited;
end;

procedure TCall.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Call, _What]);
  inherited;
end;

function TCall.Grouping: TStatementClass;
begin
  Result := TCall;
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

{ TChcp }

function TChcp.InternalParse: boolean;
begin
  Result := true;
  _Chcp := Keyword('chcp');
  if not Assigned(_Chcp) then exit(false);
  _CodePage := Number;
  inherited;
end;

procedure TChcp.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Chcp, _CodePage]);
  inherited;
end;

{ TDefine }

function TDefine.InternalParse: boolean;
begin
  Result := true;
  _Define := Keyword('define');
  if not Assigned(_Define) then exit(false);
  _Target := Identifier;
  _Eq := Terminal('=');
  TParser.ParseExpression(Self, Source, _Value);
  inherited;
end;

procedure TDefine.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignSQLPLUS);
  APrinter.PrintRulerItems('target', [_Define, _Target]);
  APrinter.PrintRulerItems('value', [_Eq, _Value]);
  inherited;
end;

function TDefine.Grouping: TStatementClass;
begin
  Result := TDefine;
end;

{ SQLPlusParser }

class function SQLPlusParser.Parse(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TClear.Parse(AParent, ASource, AResult) or
            TWhenever.Parse(AParent, ASource, AResult) or
            TSet.Parse(AParent, ASource, AResult) or
            TExit.Parse(AParent, ASource, AResult) or
            TAt.Parse(AParent, ASource, AResult) or
            TSpool.Parse(AParent, ASource, AResult) or
            TCall.Parse(AParent, ASource, AResult) or
            TChcp.Parse(AParent, ASource, AResult) or
            TDefine.Parse(AParent, ASource, AResult) or
            TConnect.Parse(AParent, ASource, AResult);
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
  APrinter.PrintItems([_Connect, _ConnectString]);
  inherited;
end;

{ TSQLPlusStatement }

procedure TSQLPlusStatement.AfterConstruction;
begin
  inherited;
  FreeList := TObjectList.Create(true);
end;

procedure TSQLPlusStatement.BeforeDestruction;
begin
  FreeAndNil(FreeList);
  inherited;
end;

function TSQLPlusStatement.SqlPlusString: TTerminal;
var
  Token: TToken;
  Comment: TComment;
  LastComment: TComment;
  Line, Col: integer;
  Text: string;
  P: TMark;
begin
  Comment := nil;
  { Выделим лексемы до конца строки и сложим их в текстовую строку }
  P := Source.Mark;
  Token := NextToken;
  Line := Token.Line;
  Col  := Token.Col;
  while (Line = Token.Line) and not (Token is TUnexpectedEOF)
        and not ((Token is TTerminal) and (Token.Value = ';')) do
  begin
    Text := Text + StringOfChar(' ', Token.Col - Col - Length(Text)) + Token.InitialValue;
    P := Source.Mark;
    Token.Printed := true; { лексема печатается в составе другой, поэтому гасим предупреждение }
    { Лексема не печатается, поэтому перепривязываем комментарии }
    if Assigned(Token.CommentAfter) then
    begin
      if Assigned(Comment)
        then LastComment.CommentAfter := Token.CommentAfter
        else Comment := Token.CommentAfter;
      LastComment := Token.CommentAfter;
      Token.CommentAfter := nil;
    end;
    { Идём дальше }
    Token := NextToken;
  end;
  Source.Restore(P);
  { Теперь сформируем лексему из этой строки, она и будет результатом }
  Result := TTerminal.Create(Text, Line, Col);
  Result.CommentAfter := Comment;
  FreeList.Add(Result);
end;

initialization
  { Команды SQL*Plus не разделяются точками с запятой, поэтому нужно явно
    назвать их ключевыми словами, в противном случае легко случится так,
    что начало следующей команды будет распознано как аргумент предыдущей
    whenever или подобной. По этой же причине здесь нужны стартовые
    ключевые слова основных SQL-комад }
  RegisterOrphan(TSQLPlusStatement);
  RegisterKeywords(TSQLPlusStatement, ['alter', 'begin', 'call', 'chcp',
    'connect', 'clear', 'create', 'declare', 'exec', 'set', 'define', 'spool',
    'whenever']);

end.

