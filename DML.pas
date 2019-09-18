////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Синтаксические конструкции DML                       //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DML;

{ ----- Примечание -------------------------------------------------------------

  В настоящей версии dml-операторы реализованы как список произвольных лексем,
  начинающихся с select/insert/update/delete/merge и заканчивающихся точкой с
  запятой. Это позволяет обработать их, встречая в PL/SQL коде и отложить их
  детальную спецификацию на будущее

------------------------------------------------------------------------------ }

interface

uses Parser, Tokens, Printers_;

type
  TDML = class(TStatementList)
  strict private
    _Keyword: TKeyword;
  strict protected
    function InternalParse: boolean; override;
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

implementation

{ TDML }

function TDML.InternalParse: boolean;
begin
  _Keyword := Keyword(['select', 'insert', 'update', 'delete', 'merge']);
  Result := Assigned(_Keyword);
  inherited;
end;

function TDML.Name: string;
begin
  Result := '< ' + _Keyword.Value + ' >';
end;

function TDML.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := false;
end;

function TDML.ParseBreak: boolean;
begin
  Result := Any([Terminal(';')]);
end;

procedure TDML.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Keyword);
  APrinter.NextLine;
  APrinter.Indent;
  inherited;
  APrinter.Undent;
  APrinter.NextLine;
end;

end.
