////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Блок программного кода                           //
//                                                                            //
//               Copyright(c) 2019-2024 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ProgramBlock;

{ ----- Примечания -------------------------------------------------------------

  Класс TProgramBlock содержит основную функциональность - собственно разбор
  блока declare .. begin .. exception .. end. Класс TUndeclaredProgramBlock
  отличается от него только отсутствием declare и стартом сразу со списка
  деклараций - это нужно для Oracle. Класс THeadedProgramBlock реализует
  функциональность программного блока с заголовком и является базовым для
  подпрограмм, пакетов, триггеров и т. п.

  ---------------------------------------------------------------------------- }

interface

uses Tokens, Statements, Printer;

type

  { Программный блок - та или иная конструкция на основе begin .. end }
  TProgramBlock = class(TTopStatement)
  strict private
    _Declare, _Begin, _Exception, _End: TEpithet;
    _Declarations, _Statements, _Handlers, _AfterEnd: TStatement;
  private
    _Header: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  strict protected
    { Признак подразумеваемого, но не записываемого declare }
    function DeclareAssumed: boolean; virtual;
    { Признак допустимости единственного end как программного блока }
    function AllowSingleEnd: boolean; virtual;
  public
    { Флаг отсутствия сдвига в блоке деклараций }
    UnshiftDeclarations: boolean;
  end;

  { Программный блок без declare в начале }
  TUndeclaredProgramBlock = class(TProgramBlock)
  strict protected
    function DeclareAssumed: boolean; override;
  end;

  { Программный блок, допускающий единственный end }
  TSingleEndProgramBlock = class(TUndeclaredProgramBlock)
  strict protected
    function AllowSingleEnd: boolean; override;
  end;

  { Программный блок с заголовком }
  THeadedProgramBlock<H: TStatement; B: TProgramBlock> = class(TTopStatement)
  strict protected
    _Header, _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Блок деклараций }
  TDeclarations = class(TStatementList<TStatement>)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  end;

  { Блок операторов }
  TStatements = class(TStatementList<TStatement>)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
    function AllowWhen: boolean; virtual;
  end;

  { Блок обработки исключений }
  TExceptionStatements = class(TStatements)
  strict protected
    function ParseStatement(out AStatement: TStatement): boolean; override;
    function AllowWhen: boolean; override;
  end;

  { Секция when }
  TWhenStatements = class(TStatement)
  strict private
    _When: TEpithet;
    _Condition: TStatement;
    _Then: TEpithet;
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Customization, Parser, Expressions, Subroutine;

{ TProgramBlock }

function TProgramBlock.InternalParse: boolean;
begin
  { Если допустим declare, попробуем его распознать }
  if not DeclareAssumed then _Declare := Keyword('declare');
  { Если нашли declare или он не нужен, распознаём декларации }
  if Assigned(_Declare) or DeclareAssumed then
    TParser.Parse(Source, Settings, Customization.GetDeclarationsParser, Self, _Declarations);
  { После деклараций ожидаем begin }
  _Begin := Keyword('begin');
  { Если нашли begin, распознаем операторы }
  if Assigned(_Begin) then
  begin
    TStatements.Parse(Self, Source, _Statements);
    _Exception := Keyword('exception');
  end;
  { Если нашли exception, распознаём обработчики исключений }
  if Assigned(_Exception) then TExceptionStatements.Parse(Self, Source, _Handlers);
  { end }
  _End := Keyword('end');
  if Customization.HasAfterEndParser then
    TParser.Parse(Source, Settings, Customization.GetAfterEndParser, Self, _AfterEnd);
  { Всё }
  inherited;
  { Блок распознан, если распознали декларации либо операторы либо одинокий end }
  Result := Assigned(_Declare) or Assigned(_Declarations) or
            Assigned(_Begin) or Assigned(_Statements) or
            (AllowSingleEnd and Assigned(_End));
end;

procedure TProgramBlock.InternalPrintSelf(APrinter: TPrinter);
var _N1, _N2: TObject;
begin
  { Отработаем сдвиг деклараций }
  if UnshiftDeclarations
    then begin _N1 := _NextLine; _N2 := _NextLine; end
    else begin _N1 := _IndentNextLine; _N2 := _UndentNextLine; end;
  { Проставим флажок, красиво размещающий is }
  if _Header is TSubroutineHeaderBase then
    TSubroutineHeaderBase(_Header).IndentedAfterIs := Assigned(_Declarations);
  APrinter.PrintItems([_Declare,   _N1,
                                   _Declarations,   _N2,
                       _Begin,     _IndentNextLine,
                                   _Statements,     _UndentNextLine,
                       _Exception, _IndentNextLine,
                                   _Handlers,       _UndentNextLine,
                       _End,       _AfterEnd]);
  inherited;
end;

function TProgramBlock.DeclareAssumed: boolean;
begin
  Result := false;
end;

function TProgramBlock.AllowSingleEnd: boolean;
begin
  Result := false;
end;

{ TUndeclaredProgramBlock }

function TUndeclaredProgramBlock.DeclareAssumed: boolean;
begin
  Result := true;
end;

{ TSingleEndProgramBlock }

function TSingleEndProgramBlock.AllowSingleEnd: boolean;
begin
  Result := true;
end;

{ THeadedProgramBlock }

function THeadedProgramBlock<H, B>.InternalParse: boolean;
begin
  Result := H.Parse(Self, Source, _Header);
  if Result then B.Parse(Self, Source, _Body);
  inherited;
  { Проставим в ProgramBlock ссылку, которая потребуется при печати }
  if _Body is TProgramBlock then TProgramBlock(_Body)._Header := _Header;
end;

procedure THeadedProgramBlock<H, B>.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Header, _NextLine, _Body]);
  inherited;
end;

{ TDeclarations }

function TDeclarations.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TParser.Parse(Source, Settings, Customization.GetDeclarationParser, Self, AResult);
end;

function TDeclarations.ParseBreak: boolean;
begin
  Result := Any([Keyword(['begin', 'end', 'create', 'after each row',
                          'after statement', 'before each row',
                          'before statement', 'instead of each row'])]);
end;

{ TStatements }

function TStatements.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TParser.Parse(Source, Settings, Customization.GetStatementParser, Self, AResult);
end;

function TStatements.ParseBreak: boolean;
begin
  Result := Any([Keyword(['end', 'end if', 'end loop', 'end case', 'exception', 'else', 'elsif'])]);
  if not Result and not AllowWhen then Result := Any([Keyword('when')]);
end;

function TStatements.AllowWhen: boolean;
begin
  Result := false;
end;

{ TExceptionStatements }

function TExceptionStatements.ParseStatement(out AStatement: TStatement): boolean;
begin
  Result := TWhenStatements.Parse(Self, Source, AStatement) or inherited ParseStatement(AStatement);
end;

function TExceptionStatements.AllowWhen: boolean;
begin
  Result := true;
end;

{ TWhenStatements }

function TWhenStatements.InternalParse: boolean;
begin
  _When := Keyword('when');
  Result := Assigned(_When) and TSingleLine<TExpression>.Parse(Self, Source, _Condition);
  if not Result then exit;
  _Then := Keyword('then');
  TStatements.Parse(Self, Source, _Body);
end;

procedure TWhenStatements.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_When, _Condition, _Then]);
  APrinter.PrintIndented(_Body);
end;

end.
