////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                    �������������� ����������� SQL*Plus                     //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit SQLPlus;

interface

uses SysUtils, Statements, Tokens, Printers_, System.Generics.Collections;

type
  { ������� clear }
  TClear = class(TStatement)
  strict private
    _Clear: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { ������� whenever }
  TWhenever = class(TStatement)
  strict private
    _Whenever, _SQLError, _Action: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { ������� set }
  TSet = class(TStatement)
  strict private
    _Set, _Target: TEpithet;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { ������� @ }
  TAt = class(TStatement)
  strict private
    _At: TTerminal;
    _FileName: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { ������� spool }
  TSpool = class(TStatement)
  strict private
    _Spool: TEpithet;
    _FileName: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { ��� ����� }
  TFileName = class(TStatement)
  strict private
    _Tokens: TArray<TToken>;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Parser, Streams;

{ TClear }

function TClear.InternalParse: boolean;
begin
  _Clear := Keyword('clear');
  Result := Assigned(_Clear);
end;

procedure TClear.PrintSelf(APrinter: TPrinter);
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

procedure TWhenever.PrintSelf(APrinter: TPrinter);
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

procedure TSet.PrintSelf(APrinter: TPrinter);
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

procedure TAt.PrintSelf(APrinter: TPrinter);
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

procedure TSpool.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Spool, _FileName]);
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

procedure TFileName.PrintSelf(APrinter: TPrinter);
var T: TToken;
begin
  for T in _Tokens do
  begin
    APrinter.PrintItem(T);
    APrinter.SupressSpaces(true); { ������ ������� �������� � ��������, ����� �������� �� spool }
  end;
  APrinter.SupressSpaces(false);
end;

end.

