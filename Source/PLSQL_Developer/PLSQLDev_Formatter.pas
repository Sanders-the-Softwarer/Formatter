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

uses SysUtils, Classes, Windows, Messages;

type
  TPLSQLDevPlugIn = class
  private
    FPluginID: integer;
    HasIDE_GetEditorHandle, HasIDE_SetMenuVisible: boolean;
  protected
    { Управление доступностью пункта меню }
    procedure ShowMenuItem(AIndex: integer; AState: boolean);
    { Форматирование текста }
    procedure FormatText(ANewWindow: boolean);
  public
    { Возврат синглтона }
    class function GetInstance: TPLSQLDevPlugIn;
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
  public
    property PluginID: integer read FPluginID;
  end;

implementation

uses PlugInIntf, RichEdit, Printer, Controller;

resourcestring
  SDescription      = 'PL/SQL Formatter by Sanders the Softwarer';
  SFormatSameWindow = 'SM Lab / Formatter';
  SFormatNewWindow  = 'SM Lab / Formatter (new window)';

{ TPLSQLDevPlugIn }

var
  Instance: TPLSQLDevPlugIn;

{ Возврат синглтона }
class function TPLSQLDevPlugIn.GetInstance: TPLSQLDevPlugIn;
begin
  Result := Instance;
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
end;

{ Активация плагина }
procedure TPLSQLDevPlugIn.Activate;
begin
  { Определим имеющиеся функции }
  HasIDE_GetEditorHandle := Assigned(IDE_GetEditorHandle);
  HasIDE_SetMenuVisible  := Assigned(IDE_SetMenuVisible);
  { Если можем работать с редактором - можем и форматировать }
  ShowMenuItem(1, HasIDE_GetEditorHandle);
  ShowMenuItem(2, HasIDE_GetEditorHandle);
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
  FormatText(AIndex = 2);
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
  Settings: TFormatSettings;
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
  try
    Settings := TFormatSettings.Default;
    Settings.StartIndent := Col + ColShift;
    Controller.MakeFormatted(Input, Settings, OracleParser, Output);
    if Output = '' then exit;
  finally
    FreeAndNil(Settings);
  end;
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
