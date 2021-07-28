////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Вывод информации в разных видах                      //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Printer;

{ ----- Примечания -------------------------------------------------------------

  Для выдачи результатов разбора сформированные объекты печатают себя в
  предложенный им "принтер". Помимо основного принтера, выводящего
  отформатированный текст, приложение поддерживает дополнительные для
  выдачи отладочной информации - соль решения в том, что это оказывается
  доступным бесплатно, без нагрузки классов анализатора дополнительными
  методами. Получившееся решение настолько универсально, что принтер лексем,
  например, одинаково хорошо работает, имея на входе как поток лексического,
  так и поток синтаксического анализатора.

  Класс TPrinter - это, по сути, интерфейс вывода на принтер. Пустая реализация
  спрятана в классе TBasePrinter; их не следует объединять, поскольку тогда
  получится циклическая ссылка между модулями Printers, Tokens и Statements.

  В ходе работы оказалось, что после правильного синтаксического разбора
  конструкции я нередко забываю напечатать какую-нибудь мелкую лексему и она
  пропадает. Это очень неприятная тенденция, так как она не только выдаёт
  некомпилируемый код, но и может погубить работу пользователя, грохнув
  что-то существенное в том, что он форматировал. Поэтому среди инструментов
  предусмотрен специальный принтер, задача которого - отлавливать пропущенные
  лексемы и бить о них в барабан. Также для индикации проблем предназначен
  принтер, выдающий только места с нераспознанными синтаксическими
  конструкциями.

  В приложении широко используется возможность выравнивания соседних однотипных
  конструкций по ширине. Для этого в принтере используется специальный режим
  примерки, когда реальный вывод отключён, но счётчики текущего положения
  модифицируются, позволяя оценить, как будет напечатан последующий текст
  и рассчитать необходимое для выравнивания блока количество пробелов.

------------------------------------------------------------------------------ }

interface

uses
  Classes, SysUtils, Math, System.Generics.Collections, Tokens, Utils;

type

  { Настройки вывода }
  TFormatSettings = class
  public
    DeclarationSingleLineParamLimit: integer;
    NamedArgumentSingleLineParamLimit: integer;
    PositionalArgumentSingleLineParamLimit: integer;
    MatchParamLimit: integer;
    AlignVariables: boolean;
    AlignFields: boolean;
    AlignColumns: boolean;
    AlignExpressions: boolean;
    AlignCommands: boolean;
    AlignSpecialComments: boolean;
    AlignUseSpace: boolean;
    AlignRightComments: boolean;
    AlignFrom: boolean;
    ShiftPackageHeader: boolean;
    ShiftPackageBody: boolean;
    ReplaceDefault: boolean;
    ReplaceAsIs: boolean;
    ChangeCommentType: boolean;
    CorrectCommentSpaces: boolean;
    AddInAccessSpecificator: boolean;
    AddFromToDelete: boolean;
    PreferredExpressionLength: integer;
    RemoveConnectPasswords: boolean;
    BeautifyLongOperands: boolean;
    StartIndent: integer;
  public
    constructor Default;
    constructor ForTest;
    procedure Load(Source: TStrings);
    procedure Save(Target: TStrings);
  end;

  { Интерфейс вывода на принтер }
  TPrinter = class
  private
    FSettings: TFormatSettings;
    function HasItems(AItems: array of TObject): boolean;
  public
    procedure BeginPrint; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure PrintItem(AItem: TObject); virtual; abstract;
    procedure EndPrint; virtual; abstract;
    procedure Indent; virtual; abstract;
    procedure Undent; virtual; abstract;
    procedure PushIndent; virtual; abstract;
    procedure PopIndent; virtual; abstract;
    procedure NextLine; virtual; abstract;
    procedure CancelNextLine; virtual; abstract;
    procedure SupressNextLine(ASupress: boolean); virtual; abstract;
    procedure PrintSpecialComment(AValue: string); virtual; abstract;
    procedure BeforePrintDelimiter; virtual; abstract;
    procedure AfterPrintDelimiter; virtual; abstract;
  public
    procedure ControlChanged; virtual; abstract;
    procedure SyncNotification(AObject: TObject; ALine, ACol, ALen: integer); virtual; abstract;
    function  GetText: string; virtual; abstract;
  public
    function PrintItems(AItems: array of TObject): boolean;
    procedure PrintRulerItems(const ARuler: string; AItems: array of TObject); virtual;
    procedure PrintIndented(AItem: TObject); overload;
    function PrintIndented(AItems: array of TObject): boolean; overload;
    function NextLineIf(AItem: TObject): boolean; overload;
    function NextLineIf(AItems: array of TObject): boolean; overload;
  public
    property Settings: TFormatSettings read FSettings write FSettings;
  end;

  { Спецконструкции для более удобного форматирования вывода }
  TFormatterCmd = class
  public
    procedure PrintSelf(APrinter: TPrinter); virtual; abstract;
  end;

  { Тип извещения о необходимости синхронизации интерфейса }
  TSyncNotification = procedure (AObject: TObject; ALine, ACol, ALen: integer) of object;

var
  { Извещение о необходимости синхронизации интерфейса }
  SyncNotification: TSyncNotification;

{ Отправка извещения о необходимости синхронизации интерфейса }
procedure SendSyncNotification(AObject: TObject; ALine, ACol, ALen: integer);

{ Конструкции, которые можно применять в TFormatterPrinter.PrintItems для форматирования }
function _NextLine: TObject;
function _NextLineIf(AItems: array of TObject): TObject;
function _Indent: TObject;
function _Undent: TObject;
function _IndentNextLine: TObject;
function _UndentNextLine: TObject;

implementation

uses SQLPlus, FormatterPrinter;

{ Отправка извещения о необходимости синхронизации интерфейса }
procedure SendSyncNotification(AObject: TObject; ALine, ACol, ALen: integer);
begin
  if Assigned(SyncNotification) then
    SyncNotification(AObject, ALine, ACol, ALen);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//          Спецконструкции для более удобного форматирования вывода          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  TNextLine = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TNextLineIf = class(TFormatterCmd)
  private
    Items: array of TObject;
  public
    constructor Create(AItems: array of TObject);
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TIndent = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TUndent = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TIndentNextLine = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TUndentNextLine = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

function _NextLine: TObject;
begin
  Result := TNextLine.Create;
end;

function _NextLineIf(AItems: array of TObject): TObject;
begin
  Result := TNextLineIf.Create(AItems);
end;

function _Indent: TObject;
begin
  Result := TIndent.Create;
end;

function _Undent: TObject;
begin
  Result := TUndent.Create;
end;

function _IndentNextLine: TObject;
begin
  Result := TIndentNextLine.Create;
end;

function _UndentNextLine: TObject;
begin
  Result := TUndentNextLine.Create;
end;

procedure TNextLine.PrintSelf(APrinter: TPrinter);
begin
  APrinter.NextLine;
end;

constructor TNextLineIf.Create(AItems: array of TObject);
var i: integer;
begin
  SetLength(Items, Length(AItems));
  for i := Low(AItems) to High(AItems) do Items[i] := AItems[i];
end;

procedure TNextLineIf.PrintSelf(APrinter: TPrinter);
begin
  APrinter.NextLineIf(Items);
end;

procedure TIndent.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Indent;
end;

procedure TUndent.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Undent;
end;

procedure TIndentNextLine.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Indent;
  APrinter.NextLine;
end;

procedure TUndentNextLine.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Undent;
  APrinter.NextLine;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//      Интерфейс вывода на принтер и фабрики создания реальных принтеров     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPrinter.PrintItems(AItems: array of TObject): boolean;
var i: integer;
begin
  Result := HasItems(AItems);
  for i := Low(AItems) to High(AItems) do PrintItem(AItems[i]);
end;

procedure TPrinter.PrintRulerItems(const ARuler: string; AItems: array of TObject);
begin
  PrintItems(AItems);
end;

procedure TPrinter.PrintIndented(AItem: TObject);
begin
  if not Assigned(AItem) then exit;
  NextLine;
  Indent;
  PrintItem(AItem);
  Undent;
end;

function TPrinter.PrintIndented(AItems: array of TObject): boolean;
begin
  Result := HasItems(AItems);
  if not Result then exit;
  NextLine;
  Indent;
  PrintItems(AItems);
  Undent;
end;

function TPrinter.NextLineIf(AItem: TObject): boolean;
begin
  Result := NextLineIf([AItem]);
end;

function TPrinter.NextLineIf(AItems: array of TObject): boolean;
begin
  Result := HasItems(AItems);
  if not Result then exit;
  NextLine;
  PrintItems(AItems);
end;

function TPrinter.HasItems(AItems: array of TObject): boolean;
var i: integer;
begin
  for i := Low(AItems) to High(AItems) do
    if Assigned(AItems[i]) and not (AItems[i] is TFormatterCmd) then exit(true);
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Настройки форматирования                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TFormatSettings.Default;
begin
  DeclarationSingleLineParamLimit := 0;
  NamedArgumentSingleLineParamLimit := 1;
  PositionalArgumentSingleLineParamLimit := 4;
  MatchParamLimit           := 3;
  AlignVariables            := true;
  AlignFields               := true;
  AlignColumns              := true;
  AlignExpressions          := true;
  AlignCommands             := true;
  AlignSpecialComments      := true;
  AlignUseSpace             := true;
  AlignRightComments        := true;
  AlignFrom                 := true;
  ReplaceDefault            := true;
  ReplaceAsIs               := true;
  AddInAccessSpecificator   := true;
  AddFromToDelete           := true;
  ChangeCommentType         := false;
  CorrectCommentSpaces      := true;
  RemoveConnectPasswords    := false;
  BeautifyLongOperands      := true;
  ShiftPackageHeader        := true;
  ShiftPackageBody          := true;
  PreferredExpressionLength := 100;
end;

constructor TFormatSettings.ForTest;
begin
  DeclarationSingleLineParamLimit        := 0;
  NamedArgumentSingleLineParamLimit      := 99;
  PositionalArgumentSingleLineParamLimit := 999;
  PreferredExpressionLength              := 9999;
  MatchParamLimit                        := 99;
  ShiftPackageHeader                     := true;
  ShiftPackageBody                       := true;
end;


procedure TFormatSettings.Save(Target: TStrings);
begin
  Assert(Target <> nil);
  Target.Clear;
  Target.AddPair('DeclarationSingleLineParamLimit', IntToStr(DeclarationSingleLineParamLimit));
  Target.AddPair('NamedArgumentSingleLineParamLimit', IntToStr(NamedArgumentSingleLineParamLimit));
  Target.AddPair('PositionalArgumentSingleLineParamLimit', IntToStr(PositionalArgumentSingleLineParamLimit));
  Target.AddPair('MatchParamLimit', IntToStr(MatchParamLimit));
  Target.AddPair('AlignVariables', BoolToStr(AlignVariables, true));
  Target.AddPair('AlignFields', BoolToStr(AlignFields, true));
  Target.AddPair('AlignColumns', BoolToStr(AlignColumns, true));
  Target.AddPair('AlignExpressions', BoolToStr(AlignExpressions, true));
  Target.AddPair('AlignCommands', BoolToStr(AlignCommands, true));
  Target.AddPair('AlignSpecialComments', BoolToStr(AlignSpecialComments, true));
  Target.AddPair('AlignUseSpace', BoolToStr(AlignUseSpace, true));
  Target.AddPair('AlignRightComments', BoolToStr(AlignRightComments, true));
  Target.AddPair('AlignFrom', BoolToStr(AlignFrom, true));
  Target.AddPair('ShiftPackageHeader', BoolToStr(ShiftPackageHeader, true));
  Target.AddPair('ShiftPackageBody', BoolToStr(ShiftPackageBody, true));
  Target.AddPair('ReplaceDefault', BoolToStr(ReplaceDefault, true));
  Target.AddPair('ReplaceAsIs', BoolToStr(ReplaceAsIs, true));
  Target.AddPair('ChangeCommentType', BoolToStr(ChangeCommentType, true));
  Target.AddPair('CorrectCommentSpaces', BoolToStr(CorrectCommentSpaces, true));
  Target.AddPair('AddInAccessSpecificator', BoolToStr(AddInAccessSpecificator, true));
  Target.AddPair('AddFromToDelete', BoolToStr(AddFromToDelete, true));
  Target.AddPair('PreferredExpressionLength', IntToStr(PreferredExpressionLength));
  Target.AddPair('RemoveConnectPasswords', BoolToStr(RemoveConnectPasswords, true));
  Target.AddPair('BeautifyLongOperands', BoolToStr(BeautifyLongOperands, true));
end;

procedure TFormatSettings.Load(Source: TStrings);

  procedure Apply(const Name, Value: string);
  begin
    if SameText(Name, 'DeclarationSingleLineParamLimit') then
      TryStrToInt(Value, DeclarationSingleLineParamLimit)
    else if SameText(Name, 'NamedArgumentSingleLineParamLimit') then
      TryStrToInt(Value, NamedArgumentSingleLineParamLimit)
    else if SameText(Name, 'PositionalArgumentSingleLineParamLimit') then
      TryStrToInt(Value, PositionalArgumentSingleLineParamLimit)
    else if SameText(Name, 'PreferredExpressionLength') then
      TryStrToInt(Value, PreferredExpressionLength)
    else if SameText(Name, 'MatchParamLimit') then
      TryStrToInt(Value, MatchParamLimit)
    else if SameText(Name, 'AlignVariables') then
      TryStrToBool(Value, AlignVariables)
    else if SameText(Name, 'AlignFields') then
      TryStrToBool(Value, AlignFields)
    else if SameText(Name, 'AlignColumns') then
      TryStrToBool(Value, AlignColumns)
    else if SameText(Name, 'AlignExpressions') then
      TryStrToBool(Value, AlignExpressions)
    else if SameText(Name, 'AlignCommands') then
      TryStrToBool(Value, AlignCommands)
    else if SameText(Name, 'AlignSpecialComments') then
      TryStrToBool(Value, AlignSpecialComments)
    else if SameText(Name, 'AlignUseSpace') then
      TryStrToBool(Value, AlignUseSpace)
    else if SameText(Name, 'AlignRightComments') then
      TryStrToBool(Value, AlignRightComments)
    else if SameText(Name, 'AlignFrom') then
      TryStrToBool(Value, AlignFrom)
    else if SameText(Name, 'ShiftPackageHeader') then
      TryStrToBool(Value, ShiftPackageHeader)
    else if SameText(Name, 'ShiftPackageBody') then
      TryStrToBool(Value, ShiftPackageBody)
    else if SameText(Name, 'ReplaceDefault') then
      TryStrToBool(Value, ReplaceDefault)
    else if SameText(Name, 'ReplaceAsIs') then
      TryStrToBool(Value, ReplaceAsIs)
    else if SameText(Name, 'ChangeCommentType') then
      TryStrToBool(Value, ChangeCommentType)
    else if SameText(Name, 'CorrectCommentSpaces') then
      TryStrToBool(Value, CorrectCommentSpaces)
    else if SameText(Name, 'AddInAccessSpecificator') then
      TryStrToBool(Value, AddInAccessSpecificator)
    else if SameText(Name, 'AddFromToDelete') then
      TryStrToBool(Value, AddFromToDelete)
    else if SameText(Name, 'RemoveConnectPasswords') then
      TryStrToBool(Value, RemoveConnectPasswords)
    else if SameText(Name, 'BeautifyLongOperands') then
      TryStrToBool(Value, BeautifyLongOperands);
  end;

  var
    i: integer;
    Name, Value: string;

begin
  if not Assigned(Source) then exit;
  for i := 0 to Source.Count - 1 do
  begin
    Name := Source.Names[i];
    Value := Source.ValueFromIndex[i];
    Apply(Name, Value);
  end;
end;

end.
