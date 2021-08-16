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

uses Windows, System.SysUtils, Streams, Tokens, Statements, Printer,
  System.Generics.Collections, System.Generics.Defaults;

type

  { Информация для синтаксического анализатора }
  TParserInfo = class
  strict protected
    Statements: TList<TStatementClass>;
    Prepared: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    { Добавление информации о разбираемой конструкции }
    procedure Add(AStatement: TStatementClass); overload;
    { Добавление информации из подчинённого списка }
    procedure Add(AInfo: TParserInfo); overload;
    { Возврат списка "кандидатов" для разбора очередной синтаксической конструкции }
    function Candidates: TList<TStatementClass>;
  public
    { Генератор синглтонов }
    class function InstanceFor(const AName: string): TParserInfo;
  end;

  { Синтаксический анализатор }
  TParser = class(TNextStream<TToken, TStatement>)
  strict private
    Settings: TFormatSettings;
  strict protected
    ParserInfo: TParserInfo;
    { Разбор входного потока имеющимися синтаксическими конструкциями }
    function Parse(AParent: TStatement; out AResult: TStatement): boolean; overload;
    { Вычисление очередного выходного символа }
    function InternalNext: TStatement; override;
  public
    constructor Create(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings; AParserInfo: TParserInfo);
    { Разбор одной конструкции из указанного потока }
    class function Parse(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings; AParserInfo: TParserInfo; AParent: TStatement; out AResult: TStatement): boolean; overload;
  end;

  { Конструкция для группировки элементов одного типа }
  TSameTypeList = class(TStatement)
  strict private
    FStatements: array of TStatement;
  strict protected
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function Aligned: TAlignMode; override;
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

const
  { Приоритеты синтаксических конструкций при разборе }
  MAX_PRIORITY    =  1000;
  HIGHER_PRIORITY =   100;
  STD_PRIORITY    =     0;
  LOWER_PRIORITY  =  -100;
  LOWEST_PRIORITY =  -900;
  MIN_PRIORITY    = -1000;

implementation

type
  { "Пустое" выражение }
  TEOFStatement = class(TStatement)
  strict protected
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ TParser }

constructor TParser.Create(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings; AParserInfo: TParserInfo);
begin
  Assert(AParserInfo <> nil);
  inherited Create(AStream);
  Settings := ASettings;
  ParserInfo := AParserInfo;
end;

{ Разбор одной конструкции из указанного потока }
class function TParser.Parse(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings; AParserInfo: TParserInfo; AParent: TStatement; out AResult: TStatement): boolean;
begin
  with TParser.Create(AStream, ASettings, AParserInfo) do
  try
    Result := Parse(AParent, AResult);
  finally
    Free;
  end;
end;

{ Разбор входного потока имеющимися синтаксическими конструкциями }
function TParser.Parse(AParent: TStatement; out AResult: TStatement): boolean;
var i: integer;
begin
  Assert(ParserInfo <> nil);
  Result := true;
  with ParserInfo.Candidates do
    for i := 0 to Count - 1 do
      if Items[i].Parse(AParent, Self.Source, AResult) then exit;
  Result := false;
end;

{ Вычисление очередного выходного символа }
function TParser.InternalNext: TStatement;
var MarkBefore: TMark;
begin
  MarkBefore := Source.Mark;
  if not Parse(nil, Result) then exit(TEOFStatement.Create(nil, Source));
  Result.Settings := Self.Settings;
  if MarkBefore = Source.Mark then raise Exception.Create('There is no progress in parsing input stream, parser aborted');
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
  if S.Grouping = nil then exit(Transit(S));
  { В противном случае начнём новую группу }
  List := TSameTypeList.Create(S);
  { Добавим следующие элементы того же класса, не разбитые комментариями }
  while not Source.Eof do
  begin
    Source.SaveMark;
    S := Source.Next;
    if (List.ItemType <> S.Grouping) or S.HasCommentsAbove then
    begin
      Source.Restore;
      break;
    end;
    List.Add(S);
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
  Assert((L = 0) or (S.Grouping = ItemType));
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

function TSameTypeList.Aligned: TAlignMode;
begin
  Result := FStatements[0].SameTypeAligned;
end;

function TSameTypeList.ItemType: TStatementClass;
begin
  Result := FStatements[0].Grouping;
end;

function TSameTypeList.Transparent: boolean;
begin
  Result := true;
end;

{ TParserInfo }

var
  ParserInstances: TDictionary<string, TParserInfo>;

{ Генератор синглтонов }
class function TParserInfo.InstanceFor(const AName: string): TParserInfo;
begin
  if ParserInstances.TryGetValue(AName, Result) then exit;
  Result := TParserInfo.Create;
  ParserInstances.Add(AName, Result);
end;

constructor TParserInfo.Create;
begin
  inherited;
  Statements := TList<TStatementClass>.Create;
end;

destructor TParserInfo.Destroy;
begin
  FreeAndNil(Statements);
  inherited;
end;

{ Добавление информации о разбираемой конструкции }
procedure TParserInfo.Add(AStatement: TStatementClass);
begin
  Prepared := false;
  Assert(AStatement <> nil);
  with Statements do
    if not Contains(AStatement) then Add(AStatement);
end;

{ Добавление информации из подчинённого списка }
procedure TParserInfo.Add(AInfo: TParserInfo);
begin
  Prepared := false;
  Assert(AInfo <> nil);
  Statements.AddRange(AInfo.Statements);
end;

{ Возврат списка "кандидатов" для разбора очередной синтаксической конструкции }

type
  TStatementClassComparer = class(TComparer<TStatementClass>)
  public
    function Compare(const Left, Right: TStatementClass): integer; override;
  end;

var
  StatementClassComparer: TStatementClassComparer;

function TParserInfo.Candidates: TList<TStatementClass>;
begin
  if not Prepared then
  begin
    Statements.Sort(StatementClassComparer);
    Prepared := true;
  end;
  Result := Statements;
end;

function TStatementClassComparer.Compare(const Left, Right: TStatementClass): integer;
begin
  Result := Right.Priority - Left.Priority;
  if Result = 0 then Result := CompareStr(Left.ClassName, Right.ClassName); { сделаем порядок однозначным, чтобы не возникало плавающих ошибок }
end;

initialization
  ParserInstances := TObjectDictionary<string, TParserInfo>.Create([doOwnsValues]);
  StatementClassComparer := TStatementClassComparer.Create;

finalization
  FreeAndNil(ParserInstances);
  FreeAndNil(StatementClassComparer);

end.
