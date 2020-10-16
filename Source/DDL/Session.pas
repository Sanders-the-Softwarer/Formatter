////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                         Синтаксис параметров сессии                        //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Session;

interface

uses Statements, Tokens, Printer;

type
  TSession = class(TStatement)
  strict private
    _Session, _Advise, _Commit, _Commit2, _Close, _DatabaseLink, _Sync, _With,
      _Primary, _Enable, _In, _Procedure, _Guard, _Parallel1, _Ddl, _Ddl2,
      _Parallel2, _Resumable, _Timeout, _Name, _Shard, _Set: TEpithet;
    _ParallelCnt, _TimeoutCnt: TNumber;
    _NameStr: TLiteral;
    _LinkName, _Params: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Streams, Commons, Expressions;

type

  { Параметр сессии }
  TSessionParam = class(TStatement)
  strict private
    _Name, _Add1, _Add2: TEpithet;
    _Eq: TTerminal;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Параметры сессии }
  TSessionParams = class(TStatementList<TSessionParam>)
  protected
    function AllowUnexpected: boolean; override;
  end;

{ TSession }

function TSession.InternalParse: boolean;
begin
  Result := true;
  _Session := Keyword('session');
  if not Assigned(_Session) then exit(false);
  { alter session advice }
  _Advise := Keyword('advise');
  if Assigned(_Advise) then
  begin
    _Commit := Keyword(['commit', 'rollback', 'nothing']);
    exit;
  end;
  { alter session close database link }
  _Close := Keyword('close');
  if Assigned(_Close) then
  begin
    _DatabaseLink := Keyword('database link');
    TQualifiedIdent.Parse(Self, Source, _LinkName);
    exit;
  end;
  { alter session sync with primary }
  _Sync := Keyword('sync');
  if Assigned(_Sync) then
  begin
    _With := Keyword('with');
    _Primary := Keyword('primary');
    exit;
  end;
  { alter session set }
  _Set := Keyword('set');
  if Assigned(_Set) then
  begin
    TSessionParams.Parse(Self, Source, _Params);
    exit;
  end;
  { alter session enable ... }
  _Enable := Keyword(['enable', 'disable', 'force']);
  if Assigned(_Enable) then
  begin
    _Commit2 := Keyword('commit');
    { commit in procedure }
    if Assigned(_Commit2) then
    begin
      _In := Keyword('in');
      _Procedure := Keyword('procedure');
      exit;
    end;
    { guard }
    _Guard := Keyword('guard');
    if Assigned(_Guard) then exit;
    { parallel }
    _Parallel1 := Keyword('parallel');
    if Assigned(_Parallel1) then
    begin
      _Ddl := Keyword(['ddl', 'dml', 'query']);
      _Parallel2 := Keyword('parallel');
      _ParallelCnt := Number;
      exit;
    end;
    { resumable }
    _Resumable := Keyword('resumable');
    if Assigned(_Resumable) then
    begin
      _Timeout := Keyword('timeout');
      _TimeoutCnt := Number;
      _Name := Keyword('name');
      _NameStr := Literal;
      exit;
    end;
    { shard ddl }
    _Shard := Keyword('shard');
    if Assigned(_Shard) then
    begin
      _Ddl2 := Keyword('ddl');
      exit;
    end;
  end;
end;

procedure TSession.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Session,
    _Advise, _Commit,
    _Close,  _DatabaseLink, _LinkName,
    _Sync,   _With,         _Primary,
    _Set,    _Params,
    _Enable,
             _Commit2,      _In,       _Procedure,
             _Guard,
             _Parallel1,    _Ddl,      _Parallel2,  _ParallelCnt,
             _Resumable,    _Timeout,  _TimeoutCnt, _Name,        _NameStr,
             _Shard,        _Ddl2
  ]);
end;

{ TSessionParam }

function TSessionParam.InternalParse: boolean;
var P: TMark;
begin
  Result := true;
  _Name := Identifier;
  if not Assigned(_Name) then exit(false);
  if _Name.Value = 'row' then
  begin
    P := Source.Mark;
    _Add1 := Identifier;
    _Add2 := Identifier;
    if not Assigned(_Add1) or not Assigned(_Add2) or (_Add1.Value <> 'archival')
       or (_Add2.Value <> 'visibility') then Source.Restore(P);
  end;
  _Eq := Terminal('=');
  if not Assigned(_Eq) then exit(false);
  TExpression.Parse(Self, Source, _Value);
end;

procedure TSessionParam.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _Add1, _Add2, _Eq, _Value]);
end;

{ TSessionParams }

function TSessionParams.AllowUnexpected: boolean;
begin
  Result := false;
end;

end.
