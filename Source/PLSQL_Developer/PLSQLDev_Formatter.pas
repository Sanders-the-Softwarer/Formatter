////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                 �������� ������ ������� ��� PL/SQL Developer               //
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
    { ���������� ������������ ������ ���� }
    procedure ShowMenuItem(AIndex: integer; AState: boolean);
    { �������������� ������ }
    procedure FormatText(ANewWindow: boolean);
  public
    { ������� ��������� }
    class function GetInstance: TPLSQLDevPlugIn;
  public
    { ���������� �������������� ������� }
    procedure Identify(APluginID: integer);
    { ������� �������� ������� }
    function Description: string;
    { ������� ������ ������� ���� }
    procedure GetMenuItems(const AItems: TStrings);
    { ��������� ������� }
    procedure Activate;
    { ����������� ������� }
    procedure Deactivate;
    { ���������� ������ ���� }
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

{ ������� ��������� }
class function TPLSQLDevPlugIn.GetInstance: TPLSQLDevPlugIn;
begin
  Result := Instance;
end;

{ ���������� �������������� ������� }
procedure TPLSQLDevPlugIn.Identify(APluginID: integer);
begin
  FPluginID := APluginID;
end;

{ ������� �������� ������� }
function TPLSQLDevPlugIn.Description: string;
begin
  Result := SDescription;
end;

{ ������� ������ ������� ���� }
procedure TPLSQLDevPlugIn.GetMenuItems(const AItems: TStrings);
begin
  Assert(AItems <> nil);
  AItems.Clear;
  AItems.Add(SFormatSameWindow);
  AItems.Add(SFormatNewWindow);
end;

{ ��������� ������� }
procedure TPLSQLDevPlugIn.Activate;
begin
  { ��������� ��������� ������� }
  HasIDE_GetEditorHandle := Assigned(IDE_GetEditorHandle);
  HasIDE_SetMenuVisible  := Assigned(IDE_SetMenuVisible);
  { ���� ����� �������� � ���������� - ����� � ������������� }
  ShowMenuItem(1, HasIDE_GetEditorHandle);
  ShowMenuItem(2, HasIDE_GetEditorHandle);
end;

{ ����������� ������� }
procedure TPLSQLDevPlugIn.Deactivate;
begin
  ShowMenuItem(1, false);
  ShowMenuItem(2, false);
end;

{ ���������� ������ ���� }
procedure TPLSQLDevPlugIn.MenuItemClick(AIndex: integer);
begin
  FormatText(AIndex = 2);
end;

{ ���������� ������������ ������ ���� }
procedure TPLSQLDevPlugIn.ShowMenuItem(AIndex: integer; AState: boolean);
begin
  if HasIDE_SetMenuVisible
    then IDE_SetMenuVisible(PluginID, AIndex, AState)
    else IDE_MenuState(PluginID, AIndex, AState);
end;

{ �������������� ������ }
procedure TPLSQLDevPlugIn.FormatText(ANewWindow: boolean);
var
  H: THandle;
  Range: TCharRange;
  Line, Col, ColShift, i: integer;
  Input, Output, Postfix: string;
  Settings: TFormatSettings;
begin
  { ������� ����� ���� �������������� }
  Assert(HasIDE_GetEditorHandle);
  H := IDE_GetEditorHandle;
  if H = 0 then exit;
  { ����� ��������� }
  SendMessage(H, EM_EXGETSEL, 0, LPARAM(@Range));
  { ���� ��� ��� - ������� ���� ����� }
  if Range.cpMin = Range.cpMax then SendMessage(H, EM_SETSEL, 0, -1);
  { ����� ������� ������ ��������� }
  SendMessage(H, EM_EXGETSEL, 0, LPARAM(@Range));
  Line := SendMessage(H, EM_EXLINEFROMCHAR, 0, Range.cpMin);
  Col := Range.cpMin - SendMessage(H, EM_LINEINDEX, Line, 0);
  { ������ ������������� ����� }
  Input := string(IDE_GetSelectedText);
  if Input.Trim = '' then exit;
  { ���� �������� ��� ������, � ����� � �������� - ���� �������� �����, ����� ��� �������������� ���������� ������ }
  ColShift := 0;
  if Col = 0 then
    while (Col + ColShift < Input.Length) and (Input[Col + ColShift + 1] = ' ') do Inc(ColShift)
  { ���� �� �������� �� ������� - ���� ���, ����� ������� �������� ���������� ������� � �� �������� �������� ������������ }
  else if Col > 0 then
    Input := StringOfChar(' ', Col) + Input;
  { �������� ������� ������ � ������ ����� ���������� ���������� � ���������� ������� }
  i := Input.Length + 1;
  while (i > 1) and (Input[i - 1] = ' ') do Dec(i);
  if (i > 1) and CharInSet(Input[i - 1], [#13, #10])
    then Postfix := #13 + Input.Substring(i)
    else Postfix := Input.Substring(i);
  { ������������� ����� }
  try
    Settings := TFormatSettings.Default;
    Settings.StartIndent := Col + ColShift;
    Controller.MakeFormatted(Input, Settings, OracleParser, Output);
    if Output = '' then exit;
  finally
    FreeAndNil(Settings);
  end;
  { ���� ������� ������� ����� ����, ����� ���� ��������� }
  if ANewWindow then
  begin
    IDE_CreateWindow(wtSQL, @(AnsiString(Output)[1]), false);
    exit;
  end;
  { ����� �� ���������� ������ ������ ������ ������ - �� ��� ���� � ������� ������� }
  if Col > 0 then Output := Output.Substring(Col);
  { ������� whitespace-�, ���������� �� ����������� ������ }
  Output := Output + Postfix;
  { ������� ���������� ������� ���������� �������� }
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
