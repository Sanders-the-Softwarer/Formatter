////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                    Синтаксические конструкции SQL*Plus                     //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit SQLPlus;

interface

uses SysUtils, Statements, Tokens, Printers_, System.Generics.Collections;

type
  { Команда clear }
  TClear = class(TStatement)
  strict private
    _Clear: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Команда whenever }
  TWhenever = class(TStatement)
  strict private
    _Whenever, _SQLError, _Action: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Команда set }
  TSet = class(TStatement)
  strict private
    _Set, _Target: TEpithet;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Команда @ }
  TAt = class(TStatement)
  strict private
    _At: TTerminal;
    _FileName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Список команд @ }
  TAtList = class(TStatementList<TAt>)
  strict protected
    function AllowUnexpected: boolean; override;
  end;

  { Команда spool }
  TSpool = class(TStatement)
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

implementation

uses Parser, Streams, PLSQL, Expressions;

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
  _SQLError := Keyword('sqlerror');
  _Action   := Keyword(['exit', 'continue']);
  Result := true;
end;

procedure TWhenever.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Whenever, _SQLError, _Action]);
end;

{ TSet }

function TSet.InternalParse: boolean;
begin
  _Set := Keyword('set');
  if not Assigned(_Set) then exit(false);
  _Target := Identifier;
  TParser.ParseExpression(Self, Source, _Value);
  Result := true;
end;

procedure TSet.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Set, _Target, _Value]);
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
  while T.Line = Line do
  begin
    Tokens.Add(T);
    P := Source.Mark;
    if Source.Eof
      then break
      else T := NextToken;
  end;
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

{ TAtList }

function TAtList.AllowUnexpected: boolean;
begin
  Result := false;
end;

end.

