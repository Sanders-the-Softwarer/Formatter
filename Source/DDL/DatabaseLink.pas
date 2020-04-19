////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                             Синтаксис dblink-ов                            //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DatabaseLink;

interface

uses Statements, Tokens, Printer;

type
  TDatabaseLink = class(TStatement)
  strict private
    _Shared, _DatabaseLink, _Connect, _To, _IdentifiedBy, _AuthenticatedBy,
      _AuthIdentifiedBy, _Using: TEpithet;
    _Name, _User, _Password, _AuthUser, _AuthPassword: TStatement;
    _ConnectionString: TLiteral;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Commons;

{ TDatabaseLink }

function TDatabaseLink.InternalParse: boolean;
begin
  Result := true;
  _Shared := Keyword('shared');
  _DatabaseLink := Keyword(['database link', 'public database link']);
  if not Assigned(_DatabaseLink) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Name);
  _Connect := Keyword('connect');
  if Assigned(_Connect) then
  begin
    _To := Keyword('to');
    TQualifiedIdent.Parse(Self, Source, _User);
    _IdentifiedBy := Keyword('identified by');
    if Assigned(_IdentifiedBy) then TQualifiedIdent.Parse(Self, Source, _Password);
  end;
  _AuthenticatedBy := Keyword('authenticated by');
  if Assigned(_AuthenticatedBy) then
  begin
    TQualifiedIdent.Parse(Self, Source, _AuthUser);
    _AuthIdentifiedBy := Keyword('identified by');
    TQualifiedIdent.Parse(Self, Source, _AuthPassword);
  end;
  _Using := Keyword('using');
  if Assigned(_Using) then _ConnectionString := Literal;
end;

procedure TDatabaseLink.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([
    _Shared, _DatabaseLink, _Name, _IndentNextLine,
             _Connect, _To, _User, _IdentifiedBy, _Password, _NextLine,
             _AuthenticatedBy, _AuthUser, _AuthIdentifiedBy, _AuthPassword, _NextLine,
             _Using, _ConnectionString, _Undent]);
end;

end.
