////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                 Основной объект плагина для PL/SQL Developer               //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit PLSQLDev_Formatter;

interface

uses SysUtils, Classes, Windows, Messages, Printer;

type
  TPLSQLDevPlugIn = class
  private
    FPluginID: integer;
    FSettings: TFormatSettings;
    HasIDE_GetEditorHandle, HasIDE_SetMenuVisible, HasIDE_GetPrefAsString,
      HasIDE_SetPrefAsString: boolean;
  protected
    { Управление доступностью пункта меню }
    procedure ShowMenuItem(AIndex: integer; AState: boolean);
    { Форматирование текста }
    procedure FormatText(ANewWindow: boolean);
  public
    { Возврат синглтона }
    class function GetInstance: TPLSQLDevPlugIn;
    { Создание-уничтожение объекта }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    { Сохранение идентификатора плагина }
    procedure Identify(APluginID: integer);
    { Возврат описания плагина }
    function Description: string;
    { Возврат списка пунктов меню }
    procedure GetMenuItems(const AItems: TStrings);
    { Активация плагина }
    procedure Activate;
    { Деактивация плагина }
    procedure Deactivate;
    { Выполнение пункта меню }
    procedure MenuItemClick(AIndex: integer);
    { Загрузка настроек форматирования }
    procedure LoadSettings;
    { Сохранение настроек форматирования }
    procedure SaveSettings;
  public
    property PluginID: integer read FPluginID;
    property Settings: TFormatSettings read FSettings;
  end;

implementation

uses PlugInIntf, RichEdit, Controller, OracleCore, fSettings;

resourcestring
  SDescription      = 'PL/SQL Formatter by Sanders the Softwarer';
  SFormatSameWindow = 'SM Lab / Formatter';
  SFormatNewWindow  = 'SM Lab / Formatter (new window)';
  SDivider          = 'SM Lab / -';
  SSettings         = 'SM Lab / Settings';
  SCannotLoad       = 'Плагин не полностью совместим с этой версией PL/SQL Developer. Считывание настроек невозможно';
  SCannotSave       = 'Плагин не полностью совместим с этой версией PL/SQL Developer. Сохранение настроек невозможно';

{ TPLSQLDevPlugIn }

var
  Instance: TPLSQLDevPlugIn;

{ Возврат синглтона }
class function TPLSQLDevPlugIn.GetInstance: TPLSQLDevPlugIn;
begin
  Result := Instance;
end;

{ Создание-уничтожение объекта }

procedure TPLSQLDevPlugIn.AfterConstruction;
begin
  inherited;
  FSettings := TFormatSettings.Default;
end;

procedure TPLSQLDevPlugIn.BeforeDestruction;
begin
  FreeAndNil(FSettings);
  inherited;
end;

{ Сохранение идентификатора плагина }
procedure TPLSQLDevPlugIn.Identify(APluginID: integer);
begin
  FPluginID := APluginID;
end;

{ Возврат описания плагина }
function TPLSQLDevPlugIn.Description: string;
begin
  Result := SDescription;
end;

{ Возврат списка пунктов меню }
procedure TPLSQLDevPlugIn.GetMenuItems(const AItems: TStrings);
begin
  Assert(AItems <> nil);
  AItems.Clear;
  AItems.Add(SFormatSameWindow);
  AItems.Add(SFormatNewWindow);
  AItems.Add(SDivider);
  AItems.Add(SSettings);
end;

{ Активация плагина }
procedure TPLSQLDevPlugIn.Activate;
begin
  { Определим имеющиеся функции }
  HasIDE_GetEditorHandle := Assigned(IDE_GetEditorHandle);
  HasIDE_SetMenuVisible  := Assigned(IDE_SetMenuVisible);
  HasIDE_GetPrefAsString := Assigned(IDE_GetPrefAsString);
  HasIDE_SetPrefAsString := Assigned(IDE_SetPrefAsString);
  { Если можем работать с редактором - можем и форматировать }
  ShowMenuItem(1, HasIDE_GetEditorHandle);
  ShowMenuItem(2, HasIDE_GetEditorHandle);
  { Загрузим настройки форматирования }
  LoadSettings;
end;

{ Деактивация плагина }
procedure TPLSQLDevPlugIn.Deactivate;
begin
  ShowMenuItem(1, false);
  ShowMenuItem(2, false);
end;

{ Выполнение пункта меню }
procedure TPLSQLDevPlugIn.MenuItemClick(AIndex: integer);
begin
  case AIndex of
    1, 2: FormatText(AIndex = 2);
    4: TFormSettings.ShowSettings;
  end;
end;

{ Загрузка настроек форматирования }
procedure TPLSQLDevPlugIn.LoadSettings;
var
  S: TStringList;
  i: integer;
  Name, Value, NewValue: AnsiString;
begin
  if not HasIDE_GetPrefAsString then raise Exception.Create(SCannotLoad);
  S := TStringList.Create;
  try
    Settings.Save(S);
    for i := 0 to S.Count - 1 do
    begin
      Name := AnsiString(S.Names[i]);
      Value := AnsiString(S.ValueFromIndex[i]);
      NewValue := IDE_GetPrefAsString(PluginID, nil, @Name[1], @Value[1]);
      S.ValueFromIndex[i] := string(NewValue);
    end;
    Settings.Load(S);
  finally
    FreeAndNil(S);
  end;
end;

{ Сохранение настроек форматирования }
procedure TPLSQLDevPlugIn.SaveSettings;
var
  S: TStringList;
  i: integer;
  Name, Value: AnsiString;
begin
  if not HasIDE_SetPrefAsString then raise Exception.Create(SCannotSave);
  S := TStringList.Create;
  try
    Settings.Save(S);
    for i := 0 to S.Count - 1 do
    begin
      Name := AnsiString(S.Names[i]);
      Value := AnsiString(S.ValueFromIndex[i]);
      IDE_SetPrefAsString(PluginID, nil, @Name[1], @Value[1]);
    end;
  finally
    FreeAndNil(S);
  end;
end;

{ Управление доступностью пункта меню }
procedure TPLSQLDevPlugIn.ShowMenuItem(AIndex: integer; AState: boolean);
begin
  if HasIDE_SetMenuVisible
    then IDE_SetMenuVisible(PluginID, AIndex, AState)
    else IDE_MenuState(PluginID, AIndex, AState);
end;

{ Форматирование текста }
procedure TPLSQLDevPlugIn.FormatText(ANewWindow: boolean);
var
  H: THandle;
  Range: TCharRange;
  Line, Col, ColShift, i: integer;
  Input, Output, Postfix: string;
begin
  { Получим хандл окна редактирования }
  Assert(HasIDE_GetEditorHandle);
  H := IDE_GetEditorHandle;
  if H = 0 then exit;
  { Найдём выделение }
  SendMessage(H, EM_EXGETSEL, 0, LPARAM(@Range));
  { Если его нет - выделим весь текст }
  if Range.cpMin = Range.cpMax then SendMessage(H, EM_SETSEL, 0, -1);
  { Найдём позицию начала выделения }
  SendMessage(H, EM_EXGETSEL, 0, LPARAM(@Range));
  Line := SendMessage(H, EM_EXLINEFROMCHAR, 0, Range.cpMin);
  Col := Range.cpMin - SendMessage(H, EM_LINEINDEX, Line, 0);
  { Возьмём форматируемый текст }
  Input := string(IDE_GetSelectedText);
  if Input.Trim = '' then exit;
  { Если выделена вся строка, а текст с отступом - учтём реальный сдвиг, чтобы при форматировании сохранился отступ }
  ColShift := 0;
  if Col = 0 then
    while (Col + ColShift < Input.Length) and (Input[Col + ColShift + 1] = ' ') do Inc(ColShift)
  { Если же выделено со сдвигом - учтём его, чтобы лексемы получили правильные позиции и не ломалась привязка комментариев }
  else if Col > 0 then
    Input := StringOfChar(' ', Col) + Input;
  { Сохраним перевод строки и отступ между выделенным фрагментом и дальнейшим текстом }
  i := Input.Length + 1;
  while (i > 1) and (Input[i - 1] = ' ') do Dec(i);
  if (i > 1) and CharInSet(Input[i - 1], [#13, #10])
    then Postfix := #13 + Input.Substring(i)
    else Postfix := Input.Substring(i);
  { Отформатируем текст }
  Settings.StartIndent := Col + ColShift;
  Controller.MakeFormatted(Input, Settings, OracleParser, Output);
  if Output = '' then exit;
  { Если сказано сделать новое окно, вернём туда результат }
  if ANewWindow then
  begin
    IDE_CreateWindow(wtSQL, @(AnsiString(Output)[1]), false);
    exit;
  end;
  { Уберём из результата первый отступ первой строки - он уже учтён в позиции вставки }
  if Col > 0 then Output := Output.Substring(Col);
  { Добавим whitespace-ы, отделяющие от дальнейшего текста }
  Output := Output + Postfix;
  { Заменим полученным текстом выделенный фрагмент }
  SendMessage(H, EM_EXGETSEL, 0, LPARAM(@Range));
  SendMessage(H, EM_REPLACESEL, WPARAM(true), LPARAM(@Output[1]));
  Range.cpMax := Range.cpMin + Output.Length;
  SendMessage(H, EM_EXSETSEL, 0, LPARAM(@Range));
end;

initialization
  Instance := TPLSQLDevPlugIn.Create;

finalization
  FreeAndNil(Instance);

end.
