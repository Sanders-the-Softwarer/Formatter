////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                    Подпрограмма (процедура или функция)                    //
//                                                                            //
//               Copyright(c) 2019-2024 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Subroutine;

interface

uses SysUtils, Statements, Tokens, Printer, ProgramBlock, Commons;

type

  { Объявление подпрограммы }
  TSubroutineHeaderBase = class(TStatement)
  strict private
    _Map: TEpithet;
    _FunctionType: TEpithet;
    _ProcedureOrFunction: TEpithet;
    _Name: TStatement;
    _Params: TStatement;
    _Return: TEpithet;
    _SelfAsResult: TEpithet;
    _ReturnType: TStatement;
    _Attributes: TStatement;
    FIndentedBeforeIs, FIndentedAfterIs: boolean;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    property IndentedBeforeIs: boolean read FIndentedBeforeIs;
    property IndentedAfterIs: boolean read FIndentedAfterIs write FIndentedAfterIs;
  end;

  { Заголовок подпрограммы }
  TSubroutineHeader = class(TSubroutineHeaderBase)
  strict private
    _Is: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Name: string; override;
  end;

  { Предварительное объявление подпрограммы }
  TSubroutineForwardDeclaration = class(TSubroutineHeaderBase)
  strict private
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    class function Priority: integer; override;
  end;

  { Подпрограмма }
  TSubroutine = class(THeadedProgramBlock<TSubroutineHeader, TUndeclaredProgramBlock>)
  public
    function StatementName: string; override;
  end;

  { Атрибуты функции }
  TFunctionAttributes = class(TStatement)
  strict private
    _Keyword, _ReliesOn: TEpithet;
    _Datasources: TStatement;
    _Next: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Параметр подпрограммы }
  TParamDeclaration = class(TStatement)
  strict private
    _ParamName: TEpithet;
    _InOut: TEpithet;
    _Nocopy: TEpithet;
    _ParamType: TStatement;
    _Assignment: TToken;
    _DefaultValue: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Выровненный блок параметров подпрограммы }
  TAlignedParamsDeclaration = class(TCommaList<TParamDeclaration>)
  strict protected
    function Aligned: TAlignMode; override;
  public
    function Transparent: boolean; override;
    function OnePerLine: boolean; override;
  end;

  { Блок параметров подпрограммы в скобках }
  TParamsDeclaration = class(TBracketedStatement<TAlignedParamsDeclaration>)
  protected
    function MultiLine: boolean; override;
  end;

implementation

uses Parser, Expressions;

{ TSubroutineHeaderBase }

function TSubroutineHeaderBase.InternalParse: boolean;
begin
  _Map := Keyword('map');
  _FunctionType := Keyword(['member', 'static', 'constructor']);
  { Проверим procedure/function }
  _ProcedureOrFunction := Keyword(['procedure', 'function']);
  if not Assigned(_ProcedureOrFunction) then exit(false);
  Result := true;
  { Название процедуры и параметры }
  TQualifiedIdent.Parse(Self, Source, _Name);
  if not Assigned(_Name) then exit;
  TParamsDeclaration.Parse(Self, Source, _Params);
  { Возвращаемое значение }
  _Return := Keyword('return');
  _SelfAsResult := Keyword('self as result');
  if not Assigned(_SelfAsResult) then TTypeRef.Parse(Self, Source, _ReturnType);
  { Признаки }
  TFunctionAttributes.Parse(Self, Source, _Attributes);
end;

procedure TSubroutineHeaderBase.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Map, _FunctionType, _ProcedureOrFunction, _Name]);
  APrinter.Indent;
  if not Assigned(_Params) or (_Params is TParamsDeclaration) and TParamsDeclaration(_Params).MultiLine then
  {$B+}
    FIndentedBeforeIs := APrinter.NextLineIf([_Params]) or
                         APrinter.NextLineIf([_Return, _SelfAsResult, _ReturnType])
  else
    FIndentedBeforeIs := APrinter.PrintItems([_Params, _Return, _SelfAsResult, _ReturnType]);
  FIndentedBeforeIs := FIndentedBeforeIs or
                       APrinter.NextLineIf([_Attributes]);
  {$B-}
  APrinter.Undent;
end;

function TSubroutineHeaderBase.StatementName: string;
begin
  Result := Concat([_FunctionType, _ProcedureOrFunction, _Name]);
end;

{ TSubroutine }

function TSubroutine.StatementName: string;
begin
  Result := _Header.StatementName;
end;

{ TSubroutineHeader }

function TSubroutineHeader.InternalParse: boolean;
begin
  Result := inherited InternalParse;
  if Result then _Is := Keyword(['is', 'as']);
  if Assigned(_Is) then _Is.CanReplace := true;
end;

procedure TSubroutineHeader.InternalPrintSelf(APrinter: TPrinter);
var Indented: boolean;
begin
  inherited;
  Indented := IndentedBeforeIs and not IndentedAfterIs;
  if Indented then APrinter.Indent;
  APrinter.PrintItems([_NextLine, _Is]);
  if Indented then APrinter.Undent;
end;

function TSubroutineHeader.Name: string;
begin
  Result := StatementType;
end;

{ TSubroutineForwardDeclaration }

class function TSubroutineForwardDeclaration.Priority: integer;
begin
  Result := HIGHER_PRIORITY;
end;

function TSubroutineForwardDeclaration.InternalParse: boolean;
begin
  Result := inherited InternalParse;
  if Result then _Semicolon := Terminal(';');
  Result := Result and Assigned(_Semicolon);
end;

procedure TSubroutineForwardDeclaration.InternalPrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.CancelNextLine;
  APrinter.PrintItem(_Semicolon);
end;

{ TParamsDeclaration }

function TParamsDeclaration.MultiLine: boolean;
begin
  Result := (InnerStatement is TAlignedParamsDeclaration) and TAlignedParamsDeclaration(InnerStatement).OnePerLine;
end;

{ TFunctionAttributes }

function TFunctionAttributes.InternalParse: boolean;
begin
  Result := true;
  _Keyword := Keyword(['deterministic', 'pipelined', 'parallel_enable', 'result_cache']);
  if not Assigned(_Keyword) then exit(false);
  if SameText(_Keyword.Value, 'result_cache') then _ReliesOn := Keyword('relies_on');
  if Assigned(_ReliesOn) then TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _Datasources);
  TFunctionAttributes.Parse(Self, Source, _Next);
end;

procedure TFunctionAttributes.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Keyword, _ReliesOn, _Datasources, _NextLine, _Next]);
end;

{ TAlignedParamsDeclaration }

function TAlignedParamsDeclaration.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignVariables);
end;

function TAlignedParamsDeclaration.Transparent: boolean;
begin
  Result := true;
end;

function TAlignedParamsDeclaration.OnePerLine: boolean;
begin
  Result := (Self.Count > Settings.DeclarationSingleLineParamLimit);
end;

{ TParamDeclaration }

function TParamDeclaration.InternalParse: boolean;
begin
  _ParamName := Identifier;
  _InOut  := Keyword(['in', 'out', 'in out']);
  _Nocopy := Keyword('nocopy');
  TTypeRef.Parse(Self, Source, _ParamType);
  if not Assigned(_ParamName) and not Assigned(_InOut) and not Assigned(_Nocopy) and not Assigned(_ParamType) then exit(false);
  _Assignment := Terminal(':=');
  if not Assigned(_Assignment) then _Assignment := Keyword('default');
  if Assigned(_Assignment) then
  begin
    _Assignment.CanReplace := true;
    TExpression.Parse(Self, Source, _DefaultValue);
  end;
  Result := true;
end;

function TParamDeclaration.StatementName: string;
begin
  Result := Concat([_ParamName]);
end;

procedure TParamDeclaration.InternalPrintSelf(APrinter: TPrinter);
begin
  if Settings.AddInAccessSpecificator and not Assigned(_InOut) then
  begin
    _InOut := TEpithet.Create('in', -1, -1);
    AddToFreeList(_InOut);
  end;
  APrinter.PrintRulerItems('name', [_ParamName]);
  APrinter.PrintRulerItems('modifiers', [_InOut, _Nocopy]);
  APrinter.PrintRulerItems('type', [_ParamType]);
  APrinter.PrintRulerItems('assignment', [_Assignment]);
  APrinter.PrintRulerItems('value', [_DefaultValue]);
end;

end.
