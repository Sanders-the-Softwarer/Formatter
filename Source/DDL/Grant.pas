////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                Команда GRANT                               //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Grant;

interface

uses SysUtils, Statements, Tokens, Commons, Printer;

type
  { Команда grant }
  TGrant = class(TTopStatement)
  strict private
    _Grant, _On, _To, _IdentifiedBy, _WithAdminOption, _WithGrantOption, _WithHierarchyOption: TEpithet;
    _Privileges, _Object, _Grantee, _Password: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
    function SameTypeAligned: TAlignMode; override;
  end;

implementation

uses Streams;

type

  { Грантуемая привилегия }
  TPrivilege = class(TStatement)
  strict private
    Tokens: array of TToken;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Список грантуемых привилегий }
  TPrivileges = class(TCommaList<TPrivilege>)
  strict protected
    function ParseBreak: boolean; override;
    function OnePerLine: boolean; override;
  end;

  { Технические классы для внесения различия между grant и grant on }
  TGrantOn = class(TGrant);
  TGrantOff = class(TGrant);

{ TGrant }

function TGrant.InternalParse: boolean;
begin
  _Grant := Keyword('grant');
  if not Assigned(_Grant) then exit(false);
  TPrivileges.Parse(Self, Source, _Privileges);
  _On := Keyword('on');
  if Assigned(_On) then TQualifiedIdent.Parse(Self, Source, _Object);
  _To := Keyword('to');
  TQualifiedIdent.Parse(Self, Source, _Grantee);
  _IdentifiedBy := Keyword('identified by');
  if Assigned(_IdentifiedBy) then TQualifiedIdent.Parse(Self, Source, _Password);
  _WithAdminOption := Keyword('with admin option');
  _WithHierarchyOption := Keyword('with hierarchy option');
  _WithGrantOption := Keyword('with grant option');
  Result := true;
  inherited;
end;

procedure TGrant.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('Grant', [_Grant, _Privileges]);
  APrinter.PrintRulerItems('On', [_On, _Object]);
  APrinter.PrintRulerItems('To', [_To, _Grantee]);
  APrinter.PrintRulerItems('Identified', [_IdentifiedBy, _Password]);
  APrinter.PrintRulerItems('With', [_WithAdminOption, _WithHierarchyOption, _WithGrantOption]);
  inherited;
end;

function TGrant.Grouping: TStatementClass;
begin
  if Assigned(_On)
    then Result := TGrantOn
    else Result := TGrantOff;
end;

function TGrant.SameTypeAligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignCommands);
end;

{ TPrivileges }

function TPrivileges.ParseBreak: boolean;
begin
  Result := Any([Terminal(';'), Keyword(['on', 'to', 'with', 'with admin option', 'with grant option', 'with hierarchy option'])]);
end;

function TPrivileges.OnePerLine: boolean;
begin
  Result := false;
end;

{ TPrivilege }

function TPrivilege.InternalParse: boolean;
var
  Savepoint: TMark;
  T: TToken;
  L: integer;
begin
  Savepoint := Source.Mark;
  { Сначала идут один или несколько идентификаторов }
  T := NextToken;
  while (T is TEpithet) and not SameStr(T.Value, 'on') and not SameStr(T.Value, 'to') do
  begin
    TEpithet(T).IsIdent := true;
    Savepoint := Source.Mark;
    L := Length(Tokens);
    SetLength(Tokens, L + 1);
    Tokens[L] := T;
    T := NextToken;
  end;
  { Если теперь не открывающая скобка, разбор окончен }
  if not SameStr(T.Value, '(') then
  begin
    Source.Restore(Savepoint);
    exit(Length(Tokens) > 0);
  end;
  { А если открывающая - читаем названия колонок вплоть до закрывающей }
  repeat
    if T is TEpithet then TEpithet(T).IsIdent := true;
    L := Length(Tokens);
    SetLength(Tokens, L + 1);
    Tokens[L] := T;
    if SameStr(T.Value, ')')
      then exit(true)
      else T := NextToken;
  until false;
end;

procedure TPrivilege.InternalPrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := Low(Tokens) to High(Tokens) do APrinter.PrintItem(Tokens[i]);
end;

end.
