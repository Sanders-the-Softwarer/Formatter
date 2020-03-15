////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Синтаксический анализатор                         //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
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

  Ввиду того, что идентификаторы и ключевые слова слиты в единую конструкцию
  TEpithet, существует вероятность ошибочного распознавания конструкций,
  начинающихся с идентификаторов - описаний переменных, операторов присваивания
  и т. п. Для того, чтобы этого не происходило, в методах TParse.ParseXXXXX
  следует чётко следить затем, чтобы в перечислении пробовались конструкции в
  порядке от менее свободных к более свободным, например, сначала определение
  типа (чтобы ключевое слово type не было ошибочно распознано как имя
  переменной), при неудаче - определение исключения (чтобы ключевое слово
  exception не было ошибочно распознано как тип переменной), и только потом,
  когда не останется других вариантов - собственно декларация переменной.

------------------------------------------------------------------------------ }

interface

uses Windows, System.SysUtils, Streams, Tokens, Statements, PrinterIntf,
  System.Generics.Collections;

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
    class function ParseDDL(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParsePLSQL(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseSQLPlus(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseDeclaration(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseType(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseAny(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseExpression(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  end;

  { Конструкция для группировки элементов одного типа }
  TSameTypeList = class(TStatement)
  strict private
    FStatements: array of TStatement;
  strict protected
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    constructor Create(S: TStatement); reintroduce;
    procedure Add(S: TStatement);
    function ItemType: TStatementClass;
    function Transparent: boolean; override;
  end;

  { Поток, объединяющий однотипные выражения в блоки }
  TSameTypeLinker = class(TNextStream<TStatement, TStatement>)
  strict protected
    function InternalNext: TStatement; override;
  end;

implementation

uses DDL, DML, PLSQL, SQLPlus, Expressions;

type
  { "Пустое" выражение }
  TEOFStatement = class(TStatement)
  strict protected
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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
             TMerge.Parse(AParent, ASource, AResult) or
            TCommit.Parse(AParent, ASource, AResult) or
            TRollback.Parse(AParent, ASource, AResult) or
            TSavepoint.Parse(AParent, ASource, AResult);
end;

{ Разбор поддерживаемых конструкций DDL }
class function TParser.ParseDDL(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TCreate.Parse(AParent, ASource, AResult) or
            TDrop.Parse(AParent, ASource, AResult) or
            TComments.Parse(AParent, ASource, AResult) or
            TGrants.Parse(AParent, ASource, AResult);
end;

{ Разбор операторов PL/SQL }
class function TParser.ParsePLSQL(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TPragma.Parse(AParent, ASource, AResult) or
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
            TAnonymousBlock.Parse(AParent, ASource, AResult) or
            TAssignment.Parse(AParent, ASource, AResult) or
            TProcedureCall.Parse(AParent, ASource, AResult) or
            TStandaloneComment.Parse(AParent, ASource, AResult);
end;

{ Разбор операторов SQL*Plus }
class function TParser.ParseSQLPlus(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TClear.Parse(AParent, ASource, AResult) or
            TWhenever.Parse(AParent, ASource, AResult) or
            TSet.Parse(AParent, ASource, AResult) or
            TAt.Parse(AParent, ASource, AResult) or
            TSpool.Parse(AParent, ASource, AResult) or
            TCall.Parse(AParent, ASource, AResult);
end;

{ Разбор деклараций (переменных, процедур, типов, курсоров, прагм и т. п. }
class function TParser.ParseDeclaration(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TType.Parse(AParent, ASource, AResult) or
            TCursor.Parse(AParent, ASource, AResult) or
            TPragma.Parse(AParent, ASource, AResult) or
            TSubroutineForwardDeclaration.Parse(AParent, ASource, AResult) or
            TSubroutine.Parse(AParent, ASource, AResult) or
            TExceptionDeclaration.Parse(AParent, ASource, AResult) or
            TVariableDeclarations.Parse(AParent, ASource, AResult);
end;

{ Разбор типов, описываемых предложением type }
class function TParser.ParseType(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TRecord.Parse(AParent, ASource, AResult) or
            TObject_.Parse(AParent, ASource, AResult) or
            TPLSQLTable.Parse(AParent, ASource, AResult) or
            TRefCursor.Parse(AParent, ASource, AResult);
end;

{ Разбор произвольной заранее неизвестной конструкции }
class function TParser.ParseAny(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := ParseDDL(AParent, ASource, AResult) or
            ParseDML(AParent, ASource, AResult) or
            TStandaloneAnonymousBlock.Parse(AParent, ASource, AResult) { нужно до PLSQL } or
            ParsePLSQL(AParent, ASource, AResult) or
            ParseSQLPlus(AParent, ASource, AResult) or
            ParseDeclaration(AParent, ASource, AResult) { должно быть в конце из-за variable declaration } or
            ParseType(AParent, ASource, AResult) or
            ParseExpression(AParent, ASource, AResult);
end;

class function TParser.ParseExpression(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
var
  S: TStatement;
  SQL: boolean;
begin
  { Найдём в дереве конструкций SQL-оператор }
  SQL := false;
  S := AParent;
  while not SQL and Assigned(S) do
    if S is TDML
      then SQL := true
      else S := S.Parent;
  { И в зависимости от результата используем нужный класс выражения }
  TExpression.CreatedRight;
  if SQL
    then Result := TSQLExpression.Parse(AParent, ASource, AResult)
    else Result := TExpression.Parse(AParent, ASource, AResult);
end;

{ Вычисление очередного выходного символа сводится к вызову ParseAny }
function TParser.InternalNext: TStatement;
var
  MarkBefore, MarkAfter: TMark;
begin
  MarkBefore := Source.Mark;
  if ParseAny(nil, Source, Result) or
     TUnexpectedToken.Parse(nil, Source, Result) then
    begin
      MarkAfter := Source.Mark;
      if MarkBefore = MarkAfter then raise Exception.Create('There is no progress in parsing input stream, parser aborted');
      Result.Settings := Self.Settings;
    end
  else
    Result := TEOFStatement.Create(nil, Source);
end;

{ TEOFStatement }

procedure TEOFStatement.InternalPrintSelf(APrinter: TPrinter);
begin
  { ничего не делаем }
end;

{ TSameTypeLinker }

function TSameTypeLinker.InternalNext: TStatement;
var
  S: TStatement;
  List: TSameTypeList;
begin
  { Прочитаем очередной элемент }
  S := Source.Next;
  { Если элемент не группируемый, просто его вернём }
  if not S.Grouping then exit(Transit(S));
  { В противном случае начнём новую группу }
  List := TSameTypeList.Create(S);
  { Прочитаем следующие элементы того же класса и добавим их в группу }
  while not Source.Eof do
  begin
    Source.SaveMark;
    S := Source.Next;
    if List.ItemType = S.ClassType then
      List.Add(S)
    else
      begin
        Source.Restore;
        break;
      end;
  end;
  { Всё, группа идёт на выход }
  Result := List;
end;

{ TSameTypeList }

constructor TSameTypeList.Create(S: TStatement);
begin
  inherited Create(nil, nil);
  Add(S);
end;

procedure TSameTypeList.Add(S: TStatement);
var L: integer;
begin
  L := Length(FStatements);
  SetLength(FStatements, L + 1);
  FStatements[L] := S;
  S.Parent := Self;
end;

procedure TSameTypeList.InternalPrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := Low(FStatements) to High(FStatements) do
    APrinter.PrintItems([FStatements[i], _NextLine]);
end;

function TSameTypeList.ItemType: TStatementClass;
begin
  Result := TStatementClass(FStatements[0].ClassType);
end;

function TSameTypeList.Transparent: boolean;
begin
  Result := false;
end;

end.
