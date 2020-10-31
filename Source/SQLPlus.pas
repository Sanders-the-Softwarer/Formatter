////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                    �������������� ����������� SQL*Plus                     //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit SQLPlus;

interface

uses SysUtils, Statements, Streams, Tokens, Printer, Utils, Contnrs,
  System.Generics.Collections;

type

  { ������ ������ SQL*Plus }
  SQLPlusParser = class
  public
    class function Parse(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  end;

  { ������� ����� ������ SQL*Plus }
  TSQLPlusStatement = class(TSemicolonStatement)
  strict protected
    { ���������� ������� "������� �� ����� ������" }
    function SqlPlusString: TTerminal;
  public
    function SameTypeAligned: TAlignMode; override;
  end;

  { ������� clear }
  TClear = class(TSQLPlusStatement)
  strict private
    _Clear: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������� whenever }
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

  { ������� define }
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

  { ������� @ }
  TAt = class(TSQLPlusStatement)
  strict private
    _At, _FileName: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

  { ������� spool }
  TSpool = class(TSQLPlusStatement)
  strict private
    _Spool: TEpithet;
    _FileName: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������� call }
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

  { ��������� ���� � SQL*Plus }
  TStandaloneAnonymousBlock = class(TStatement)
  strict private
    _Block: TStatement;
    _Slash: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������� CHCP }
  TChcp = class(TSQLPlusStatement)
  strict private
    _Chcp: TEpithet;
    _CodePage: TNumber;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������� connect }
  TConnect = class(TSQLPlusStatement)
  strict private
    _Connect: TEpithet;
    _ConnectString: TTerminal;
    procedure RemovePassword;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Expressions, Commons, PLSQL, Keywords, Set_SQLPlus, Exit_SQLPlus;

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
  { � ������ continue }
  _Action := Keyword('continue');
  if Assigned(_Action) then _Param1 := Keyword(['commit', 'rollback', 'none']);
  { � ������ exit }
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
  TExpression.Parse(Self, Source, _Value);
  inherited;
end;

procedure TDefine.InternalPrintSelf(APrinter: TPrinter);
begin
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
  { ������� ������� �� ����� ������ � ������ �� � ��������� ������ }
  P := Source.Mark;
  FirstToken := NextToken;
  CurToken := FirstToken;
  while (CurToken.Line = FirstToken.Line) and not (CurToken is TUnexpectedEOF)
        and not ((CurToken is TTerminal) and (CurToken.Value = ';')) do
  begin
    Text := Text + StringOfChar(' ', CurToken.Col - FirstToken.Col - Length(Text)) + CurToken.InitialValue;
    P := Source.Mark;
    CurToken.Printed := true; { ������� ���������� � ������� ������, ������� ����� �������������� }
    { ������� �� ����������, ������� ��������������� ����������� }
    if Assigned(CurToken.CommentAfter) then
    begin
      if Assigned(Comment)
        then Comment.CommentAfter := CurToken.CommentAfter
        else Comment := CurToken.CommentAfter;
      CurToken.CommentAfter := nil;
    end;
    { ��� ������ }
    CurToken := NextToken;
  end;
  Source.Restore(P);
  { ������ ���������� ������� �� ���� ������, ��� � ����� ����������� }
  Result := TTerminal.Create(Text, FirstToken.Line, FirstToken.Col);
  ReplaceToken(FirstToken, Result);
  Result.CommentAfter := Comment;
end;

initialization
  { ������� SQL*Plus �� ����������� ������� � �������, ������� ����� ����
    ������� �� ��������� �������, � ��������� ������ ����� �������� ���,
    ��� ������ ��������� ������� ����� ���������� ��� �������� ����������
    whenever ��� ��������. �� ���� �� ������� ����� ����� ���������
    �������� ����� �������� SQL-����� }
  RegisterOrphan(TSQLPlusStatement);
  RegisterKeywords(TSQLPlusStatement, ['alter', 'begin', 'call', 'chcp',
    'connect', 'clear', 'create', 'declare', 'exec', 'set', 'define', 'spool',
    'whenever']);

end.
