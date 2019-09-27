////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//               Базовые классы потока синтаксических конструкций             //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Streams;

{ ----- Примечания -------------------------------------------------------------

  Архитектура решения выстроена в виде цепочки взаимодействующих потоков. Первый
  поток занимается тем, что читает источник данных и выдаёт его в виде
  последовательности символов. Каждый следующий поток анализирует выход
  предыдущего и выдаёт последовательность распознанных конструкций следующего
  уровня - так символы превращаются в лексемы, а лексемы в синтаксические
  конструкции. Принятый подход далёк от идеала с точки зрения производительности
  и накладных расходов, но обеспечивает максимальную гибкость, позволяя разбить
  всю последовательность обработки на маленькие простые этапы.

  На каждом этапе анализа создаются многочисленные объекты, представляющие
  символы, лексемы и синтаксические конструкции. Для их корректного освобождения
  принят следующий подход:

      а) В цепочке потоков каждый уничтожает предыдущий
      б) Благодаря классу ObjectStream каждый поток запоминает созданные объекты
         и уничтожает их вместе с собой
      в) В тех случаях, когда поток должен передать дальше объект, созданный
         предыдущим потоком, он должен уберечь его от попытки двойного
         уничтожения с помощью функции Transit

------------------------------------------------------------------------------ }

interface

uses SysUtils, System.Generics.Collections, Tokens, Windows, Printers_;

type

  { Базовый класс потока предоставляет интерфейс из двух функций, Eof и Next }
  TBaseStream<T: class> = class
  public
    function Eof: boolean; virtual; abstract;
    function Next: T; virtual; abstract;
  end;

  { Класс владельца запоминает созданные объекты и уничтожает их вместе с собой }
  TObjectStream<T: class> = class(TBaseStream<T>)
  strict private
    Owned: TObjectList<T>;
    InTransit: T;
  strict protected
    function InternalNext: T; virtual; abstract;
    function Transit(AValue: T): T;
  public
    destructor Destroy; override;
    function Next: T; override;
  end;

  { Отметка позиции в буфере }
  TMark = integer;

  { Класс буферизации позволяет откат и повторный возврат прочитанного }
  TBufferedStream<T: class> = class(TObjectStream<T>)
  strict private
    Output: TList<T>;
    RepeatMark, SavedMark: TMark;
    function PutIntoOutput(AValue: T): T;
  strict protected
    function InternalEof: boolean; virtual; abstract;
  public
    function Mark: TMark;
    procedure SaveMark;
    procedure Restore(AMark: TMark); overload;
    procedure Restore; overload;
  public
    destructor Destroy; override;
    function Eof: boolean; override; final;
    function Next: T; override; final;
    procedure First;
    procedure PrintAll(APrinter: TPrinter);
  end;

  { Класс чтения строки }
  TStringStream = class(TBufferedStream<TChar>)
  strict private
    FValue: string;
    FIndex: integer;
  strict protected
    function InternalNext: TChar; override;
    function InternalEof: boolean; override;
  public
    constructor Create(const AValue: string);
  end;

  { Базовый класс потока, обрабатывающего выход другого потока }
  TNextStream<I: class; O: class> = class(TBufferedStream<O>)
  strict private
    FSource: TBufferedStream<I>;
  strict protected
    property Source: TBufferedStream<I> read FSource;
    function InternalEof: boolean; override;
  public
    constructor Create(ASource: TBufferedStream<I>);
    destructor Destroy; override;
  end;

  { Класс, подсчитывающий положение символа во входном тексте }
  TPositionStream = class(TNextStream<TChar, TPositionedChar>)
  strict private
    Line, Col: integer;
    Initialized: boolean;
  strict protected
    function InternalNext: TPositionedChar; override;
  end;

implementation

/////////////////////////////////////////////////////////////////////////////////
//                                                                             //
// Класс владельца запоминает созданные объекты и уничтожает их вместе с собой //
//                                                                             //
/////////////////////////////////////////////////////////////////////////////////

destructor TObjectStream<T>.Destroy;
begin
  FreeAndNil(Output);
  inherited;
end;

function TObjectStream<T>.Next: T;
begin
  if Eof then raise Exception.Create('End of the text had been reached');
  Result := InternalNext;
  if Result = InTransit then exit;
  if not Assigned(Owned) then Owned := TObjectList<T>.Create(true);
  Owned.Add(Result);
end;

function TObjectStream<T>.Transit(AValue: T): T;
begin
  InTransit := AValue;
  Result := AValue;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//     Класс буферизации позволяет откат и повторный возврат прочитанного     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

destructor TBufferedStream<T>.Destroy;
begin
  FreeAndNil(Output);
  inherited;
end;

function TBufferedStream<T>.Eof: boolean;
begin
  Result := InternalEof and (not Assigned(Output) or (RepeatMark >= Output.Count));
end;

function TBufferedStream<T>.Next: T;
begin
  if Assigned(Output) and (RepeatMark < Output.Count)
    then Result := Output[RepeatMark]
    else Result := PutIntoOutput(inherited Next);
  Inc(RepeatMark);
end;

procedure TBufferedStream<T>.First;
begin
  Restore(0);
end;

procedure TBufferedStream<T>.PrintAll(APrinter: TPrinter);
begin
  First;
  try
    APrinter.BeginPrint;
    while not Eof do APrinter.PrintItem(Next);
  finally
    APrinter.EndPrint;
  end;
end;

function TBufferedStream<T>.Mark: TMark;
begin
  if Assigned(Output)
    then Result := RepeatMark
    else Result := 0;
end;

procedure TBufferedStream<T>.SaveMark;
begin
  SavedMark := Mark;
end;

procedure TBufferedStream<T>.Restore(AMark: TMark);
begin
  if Assigned(Output) and (AMark > Output.Count) or not Assigned(Output) and (AMark > 0)
    then raise Exception.Create('Invalid stream mark')
    else RepeatMark := AMark;
end;

procedure TBufferedStream<T>.Restore;
begin
  Restore(SavedMark);
end;

function TBufferedStream<T>.PutIntoOutput(AValue: T): T;
begin
  if not Assigned(Output) then Output := TList<T>.Create;
  Output.Add(AValue);
  Result := AValue;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                            Класс чтения строки                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TStringStream.Create(const AValue: string);
begin
  FValue := AValue + ' ';
end;

function TStringStream.InternalEof: boolean;
begin
  Result := FIndex >= Length(FValue);
end;

function TStringStream.InternalNext: TChar;
begin
  Inc(FIndex);
  Result := TChar.Create(FValue[FIndex]);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//         Базовый класс потока, обрабатывающего выход другого потока         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TNextStream<I, O>.Create(ASource: TBufferedStream<I>);
begin
  FSource := ASource;
end;

destructor TNextStream<I, O>.Destroy;
begin
  FreeAndNil(FSource);
  inherited;
end;

function TNextStream<I, O>.InternalEof: boolean;
begin
  Result := FSource.Eof;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//         Класс, подсчитывающий положение символа во входном тексте          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function LFtoCR(C: char): char;
begin
  if C = #10 then Result := #13 else Result := C;
end;

function TPositionStream.InternalNext: TPositionedChar;
var
  C: TChar;
begin
  { При первом вызове считаем позиции с первых строки и колонки }
  if not Initialized then
  begin
    Line := 1;
    Col  := 1;
    Initialized := true;
  end;
  { Читаем символ и формируем результат }
  C := Source.Next;
  Result := TPositionedChar.Create(LFtoCR(C.Value), Line, Col);
  { Если это не перевод строки - сдвигаем позицию вправо }
  if (C.Value <> #13) and (C.Value <> #10) then
    Inc(Col)
  { Если перевод, то перейдём на новую строку }
  else
    begin
      Inc(Line);
      Col := 1;
      { И если это CR, пропустим LF после него }
      if (C.Value = #13) and not Source.Eof then
      begin
        Source.SaveMark;
        if Source.Next.Value <> #10 then Source.Restore;
      end;
    end;
end;

end.
