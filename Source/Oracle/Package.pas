////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                         Пакет (декларация или тело)                        //
//                                                                            //
//               Copyright(c) 2019-2024 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Package;

interface

uses SysUtils, Statements, Tokens, Printer, ProgramBlock;

type

  { Заголовок пакета }
  TPackageHeader = class(TStatement)
  private
    _Package: TEpithet;
  strict private
    _AuthId, _AsIs: TEpithet;
    _Name: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Пакет }
  TPackage = class(THeadedProgramBlock<TPackageHeader, TSingleEndProgramBlock>)
  strict protected
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

implementation

uses Commons;

{ TPackageHeader }

function TPackageHeader.InternalParse: boolean;
begin
  { Если распознали слово package, то распознали конструкцию }
  _Package := Keyword(['package', 'package body']);
  if not Assigned(_Package) then exit(false);
  Result := true;
  { Проверим название пакета }
  TQualifiedIdent.Parse(Self, Source, _Name);
  { Проверим наличие authid }
  _AuthId := Keyword(['authid definer', 'authid current_user']);
  { Проверим наличие is }
  _AsIs := Keyword(['is', 'as']);
  if Assigned(_AsIs) then _AsIs.CanReplace := true;
end;

procedure TPackageHeader.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Package, _Name, _AuthId, _AsIs]);
  APrinter.NextLine;
end;

function TPackageHeader.StatementName: string;
begin
  Result := Concat([_Package, _Name]);
end;

{ TPackage }

function TPackage.StatementName: string;
begin
  Result := _Header.StatementName;
end;

procedure TPackage.InternalPrintSelf(APrinter: TPrinter);
begin
  Assert(Assigned(_Header));
  Assert(Assigned(_Body));
  { Определимся со сдвигом деклараций }
  if Assigned(APrinter) and Assigned(APrinter.Settings) then
    with _Header as TPackageHeader do
      if SameText(_Package.Value, 'package')
        then TProgramBlock(_Body).UnshiftDeclarations := not APrinter.Settings.ShiftPackageHeader
        else TProgramBlock(_Body).UnshiftDeclarations := not APrinter.Settings.ShiftPackageBody;
  { И, собственно, напечатаем }
  inherited;
end;

end.
