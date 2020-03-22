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

uses SysUtils, Statements, Tokens, PrinterIntf, Utils, System.Generics.Collections;

type

  { Базовый класс команд SQL*Plus }
  TSQLPlusStatement = class(TStatement)
  strict protected
    function GetKeywords: TKeywords; override;
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

  { Команда set }
  TSet = class(TSQLPlusStatement)
  strict private
    _Set: TEpithet;
    _Target, _Value: TStatement;
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
    _At: TTerminal;
    _FileName: TStatement;
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
    _FileName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Команда call }
  TCall = class(TSemicolonStatement)
  strict private
    _Call: TEpithet;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

  { Имя файла }
  TFileName = class(TStatement)
  strict private
    _Tokens: TArray<TToken>;
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

  { Команда CHCP }
  TChcp = class(TSQLPlusStatement)
  strict private
    _Chcp: TEpithet;
    _CodePage: TNumber;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Parser, Streams, Commons, PLSQL;

{ TSQLPlusStatement }

var
  SQLPlusKeywords: TKeywords;

function TSQLPlusStatement.GetKeywords: TKeywords;
begin
  Result := SQLPlusKeywords;
end;

{ TClear }

function TClear.InternalParse: boolean;
begin
  _Clear := Keyword('clear');
  Result := Assigned(_Clear);
end;

procedure TClear.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Clear]);
end;

{ TWhenever }

function TWhenever.InternalParse: boolean;
begin
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
  Result := true;
end;

procedure TWhenever.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignSQLPLUS);
  APrinter.PrintRulerItems('Cmd', [_Whenever, _Condition]);
  APrinter.PrintRulerItems('Action', [_Action, _Param1, _Expr, _Param2]);
end;

function TWhenever.Grouping: TStatementClass;
begin
  Result := TWhenever;
end;

{ TSet }

function TSet.InternalParse: boolean;
begin
  _Set := Keyword('set');
  if not Assigned(_Set) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Target);
  TParser.ParseExpression(Self, Source, _Value);
  Result := true;
end;

procedure TSet.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignSQLPLUS);
  APrinter.PrintRulerItems('Target', [_Set, _Target]);
  APrinter.PrintRulerItem('Value', _Value);
end;

function TSet.Grouping: TStatementClass;
begin
  Result := TSet;
end;

{ TAt }

function TAt.InternalParse: boolean;
begin
  _At := Terminal(['@', '@@']);
  Result := Assigned(_At);
  if Result then TFileName.Parse(Self, Source, _FileName);
end;

procedure TAt.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.SupressSpaces(true);
  APrinter.PrintItems([_At, _FileName]);
  APrinter.SupressSpaces(false);
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
  if Result then TFileName.Parse(Self, Source, _FileName);
end;

procedure TSpool.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Spool, _FileName]);
end;

{ TCall }

function TCall.Grouping: TStatementClass;
begin
  Result := TCall;
end;

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

{ TFileName }

function TFileName.InternalParse: boolean;
var
  Tokens: TList<TToken>;
  P: TMark;
  T: TToken;
  Line: integer;
begin
  Tokens := TList<TToken>.Create;
  T := NextToken;
  Line := T.Line;
  repeat
    Tokens.Add(T);
    P := Source.Mark;
    if Source.Eof
      then break
      else T := NextToken;
  until T.Line <> Line;
  Source.Restore(P);
  Result := true;
  _Tokens := Tokens.ToArray;
  FreeAndNil(Tokens);
end;

procedure TFileName.InternalPrintSelf(APrinter: TPrinter);
var
  T: TToken;
  S: boolean;
begin
  S := false;
  for T in _Tokens do
  begin
    APrinter.PrintItem(T);
    if not S then
    begin
      APrinter.SupressSpaces(true); { первую лексему печатаем с пробелом, чтобы отделить от spool }
      S := true;
    end;
  end;
  if S then APrinter.SupressSpaces(false);
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
  _Chcp := Keyword('chcp');
  if not Assigned(_Chcp) then exit(false);
  _CodePage := Number;
  Result := true;
end;

procedure TChcp.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Chcp, _CodePage]);
end;

{ TDefine }

function TDefine.InternalParse: boolean;
begin
  _Define := Keyword('define');
  if not Assigned(_Define) then exit(false);
  _Target := Identifier;
  _Eq := Terminal('=');
  TParser.ParseExpression(Self, Source, _Value);
  Result := true;
end;

procedure TDefine.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignSQLPLUS);
  APrinter.PrintRulerItems('target', [_Define, _Target]);
  APrinter.PrintRulerItems('value', [_Eq, _Value]);
end;

function TDefine.Grouping: TStatementClass;
begin
  Result := TDefine;
end;

initialization
  { Команды SQL*Plus не разделяются точками с запятой, поэтому нужно явно
    назвать их ключевыми словами, в противном случае легко случится так,
    что начало следующей команды будет распознано как аргумент предыдущей
    whenever или подобной}
  SQLPlusKeywords := TKeywords.Create(['chcp', 'clear', 'set', 'define', 'whenever']);

finalization
  FreeAndNil(SQLPlusKeywords);

end.

