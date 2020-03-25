////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                   Интерфейс вывода форматированного текста                 //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TextBuilder;

{ ----- Примечания -------------------------------------------------------------

  TextBuilder - абстракция, пришедшая на замену стандартному StringBuilder.
  Она поддерживает те операции вывода текста, которые нужны FormatterPrinter,
  подсчитывает текущие строку и позицию, максимальную ширину текста, освобождая
  от этих функций FormatterPrinter и упрощая его код. Кроме того, TextBuilder
  умеет работать не формируя собственно текста - только считая позиции. Это
  свойство используется в DraftPrinter для оптимизации.

------------------------------------------------------------------------------ }

interface

uses SysUtils;

type

  { Интерфейс вывода форматированного текста }
  TTextBuilder = class
  strict private
    { StringBuilder, в котором накапливается текст }
    Builder: TStringBuilder;
    { Счётчики текущей позиции }
    FLine, FCol, FMaxWidth, FLength: integer;
  public
    constructor Create(AMeasureOnly: boolean);
    destructor Destroy; override;
    procedure Clear;
  public
    { Добавление перевода строки (Count штук)}
    procedure AppendLine(Count: integer);
    { Добавление пробела (Count штук)}
    procedure AppendSpace(Count: integer);
    { Добавление текста }
    procedure AppendText(Text: string);
    { Возврат сформированного текста }
    function Text: string;
    { Характеристики текущего состояния текста }
    property Line: integer read FLine;
    property Col: integer read FCol;
    property Length: integer read FLength;
    function MaxWidth: integer;
  end;

implementation

{ TTextBuilder }

constructor TTextBuilder.Create(AMeasureOnly: boolean);
begin
  if not AMeasureOnly then Builder := TStringBuilder.Create;
  Clear;
end;

destructor TTextBuilder.Destroy;
begin
  FreeAndNil(Builder);
  inherited;
end;

procedure TTextBuilder.Clear;
begin
  if Assigned(Builder) then Builder.Clear;
  FLine := 1;
  FCol  := 1;
  FMaxWidth := 0;
  FLength := 0;
end;

function TTextBuilder.Text: string;
begin
  if Assigned(Builder)
    then Result := Builder.ToString
    else Result := '';
end;

{ Добавление перевода строки (Count штук)}
procedure TTextBuilder.AppendLine(Count: integer);
var i: integer;
begin
  Assert(Count >= 0);
  if Count = 0 then exit;
  if Col > FMaxWidth then FMaxWidth := Col - 1;
  Inc(FLine, Count);
  FCol := 1;
  Inc(FLength, 2 * Count);
  if Assigned(Builder) then
    for i := 1 to Count do Builder.AppendLine;
end;

{ Добавление пробела (Count штук)}
procedure TTextBuilder.AppendSpace(Count: integer);
begin
  Assert(Count >= 0);
  if Count = 0 then exit;
  Inc(FCol, Count);
  Inc(FLength, Count);
  if Assigned(Builder) then Builder.Append(' ', Count);
end;

{ Добавление текста }
procedure TTextBuilder.AppendText(Text: string);
var
  Lines: TArray<string>;
  i, Len: integer;
begin
  if Text = '' then exit;
  { Разобьём текст на строки }
  Text := StringReplace(Text, #13#10, #13, [rfReplaceAll]);
  Lines := Text.Split([#13, #10]);
  { И выведем каждую по очереди }
  for i := Low(Lines) to High(Lines) do
  begin
    Len := Lines[i].Length;
    Inc(FCol, Len);
    Inc(FLength, Len);
    if Assigned(Builder) then Builder.Append(Lines[i]);
    if i < High(Lines) then AppendLine(1);
  end;
end;

{ Возврат максимальной ширины текста }
function TTextBuilder.MaxWidth: integer;
begin
  { Пока нет перевода строки, длина последней строки могла и не попасть в FMaxWidth }
  if FCol > FMaxWidth
    then Result := FCol - 1
    else Result := FMaxWidth;
end;

end.
