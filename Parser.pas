////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Синтаксический анализатор                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Parser;

{ ----- Примечания -------------------------------------------------------------

  Синтаксический анализатор является потоком достаточно формально. Он
  возвращает конструкции верхнего уровня, каждая из которых содержит под
  собой целое дерево из вложенных элементов. Также он достаточно формально
  является анализатором - поскольку его роль сводится к тому, чтобы
  перебрать возможные конструкции верхнего уровня, спросив каждую, не
  согласится ли она разобрать входной поток с текущего места. Весь анализ
  проходит в файлах DDL, DML, PLSQL, Expressions, а модуль Parser и класс
  TParser являются по сути только точкой входа.

------------------------------------------------------------------------------ }

interface

uses Windows, System.SysUtils, Streams, Tokens, Statements, Printers_;

type
  { Синтаксический анализатор }
  TParser = class(TNextStream<TToken, TStatement>)
  strict private
    Settings: TFormatSettings;
  strict protected
    function InternalNext: TStatement; override;
  public
    constructor Create(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings);
    class function ParseDML(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseCreation(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseOperator(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseDeclaration(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseType(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseAny(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  end;

implementation

uses DDL, DML, PLSQL, Expressions;

type
  { "Пустое" выражение }
  TEOFStatement = class(TStatement)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

{ TParser }

constructor TParser.Create(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings);
begin
  inherited Create(AStream);
  Settings := ASettings;
end;

{ Разбор поддерживаемых конструкций DML }
class function TParser.ParseDML(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TSelect.Parse(AParent, ASource, AResult) or
            TInsert.Parse(AParent, ASource, AResult) or
            TUpdate.Parse(AParent, ASource, AResult) or
            TDelete.Parse(AParent, ASource, AResult) or
             TMerge.Parse(AParent, ASource, AResult);
end;

{ Разбор объектов, создаваемых командой create }
class function TParser.ParseCreation(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TPackage.Parse(AParent, ASource, AResult) or
            TSubroutine.Parse(AParent, ASource, AResult);
end;

{ Разбор операторов PL/SQL }
class function TParser.ParseOperator(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := ParseDML(AParent, ASource, AResult) or
            TAssignment.Parse(AParent, ASource, AResult) or
            TReturn.Parse(AParent, ASource, AResult) or
            TNull.Parse(AParent, ASource, AResult) or
            TRaise.Parse(AParent, ASource, AResult) or
            TIf.Parse(AParent, ASource, AResult) or
            TCase.Parse(AParent, ASource, AResult) or
            TLoop.Parse(AParent, ASource, AResult) or
            TFor.Parse(AParent, ASource, AResult) or
            TWhile.Parse(AParent, ASource, AResult) or
            TForAll.Parse(AParent,ASource, AResult) or
            TOpenFor.Parse(AParent, ASource, AResult) or
            TFetch.Parse(AParent, ASource, AResult) or
            TExit.Parse(AParent, ASource, AResult) or
            TPipeRow.Parse(AParent, ASource, AResult) or
            TClose.Parse(AParent, ASource, AResult) or
            TExecuteImmediate.Parse(AParent, ASource, AResult) or
            TProcedureCall.Parse(AParent, ASource, AResult) or
            TAnonymousBlock.Parse(AParent, ASource, AResult);
end;

{ Разбор деклараций (переменных, процедур, типов, курсоров, прагм и т. п. }
class function TParser.ParseDeclaration(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TSubroutineForwardDeclaration.Parse(AParent, ASource, AResult) or
            TSubroutine.Parse(AParent, ASource, AResult) or
            TPragma.Parse(AParent, ASource, AResult) or
            TType.Parse(AParent, ASource, AResult) or
            TCursor.Parse(AParent, ASource, AResult) or
            TExceptionDeclaration.Parse(AParent, ASource, AResult) or
            TVariableDeclarations.Parse(AParent, ASource, AResult);
end;

{ Разбор типов, описываемых предложением type }
class function TParser.ParseType(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TRecord.Parse(AParent, ASource, AResult) or
            TTable.Parse(AParent, ASource, AResult);
end;

{ Разбор произвольной заранее неизвестной конструкции }
class function TParser.ParseAny(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := ParseOperator(AParent, ASource, AResult) or
            ParseCreation(AParent, ASource, AResult) or
            ParseType(AParent, ASource, AResult) or
            TCreate.Parse(AParent, ASource, AResult) or
            TAnonymousBlock.Parse(AParent, ASource, AResult) or
            TExpression.Parse(AParent, ASource, AResult);
end;

{ Вычисление очередного выходного символа сводится к вызову ParseAny }
function TParser.InternalNext: TStatement;
begin
  if ParseAny(nil, Source, Result) or
     TUnexpectedToken.Parse(nil, Source, Result)
    then Result.Settings := Self.Settings
    else Result := TEOFStatement.Create(nil, Source);
end;

{ TEOFStatement }

procedure TEOFStatement.PrintSelf(APrinter: TPrinter);
begin
  { ничего не делаем }
end;

end.
