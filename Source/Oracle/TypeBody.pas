////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                         Тело объектного типа данных                        //
//                                                                            //
//               Copyright(c) 2019-2024 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TypeBody;

interface

uses Tokens, Statements, Printer, ProgramBlock;

type

  { Заголовок type body }
  TTypeBodyHeader = class(TStatement)
  strict private
    _TypeBody, _AsIs: TEpithet;
    _TypeName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Объект type body }
  TTypeBody = class(THeadedProgramBlock<TTypeBodyHeader, TUndeclaredProgramBlock>);

implementation

uses Commons, OracleCore;

{ TTypeBodyHeader }

function TTypeBodyHeader.InternalParse: boolean;
begin
  Result := true;
  _TypeBody := Keyword('type body');
  if not Assigned(_TypeBody) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _TypeName);
  _AsIs := Keyword(['as', 'is']);
  if Assigned(_AsIs) then _AsIs.CanReplace := true;
end;

procedure TTypeBodyHeader.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_TypeBody, _TypeName, _AsIs]);
end;

initialization
  OracleCreateParser.Add(TTypeBody);

end.
