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

uses SysUtils, Classes, Windows, Messages, Printer;

type
  TPLSQLDevPlugIn = class
  private
    FPluginID: integer;
    FSettings: TFormatSettings;
    HasIDE_GetEditorHandle, HasIDE_SetMenuVisible, HasIDE_GetPrefAsString,
      HasIDE_SetPrefAsString: boolean;
  protected
    { ���������� ������������ ������ ���� }
    procedure ShowMenuItem(AIndex: integer; AState: boolean);
    { �������������� ������ }
    procedure FormatText(ANewWindow: boolean);
  public
    { ������� ��������� }
    class function GetInstance: TPLSQLDevPlugIn;
    { ��������-����������� ������� }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
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
    { �������� �������� �������������� }
    procedure LoadSettings;
    { ���������� �������� �������������� }
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
  SCannotLoad       = '������ �� ��������� ��������� � ���� ������� PL/SQL Developer. ���������� �������� ����������';
  SCannotSave       = '������ �� ��������� ��������� � ���� ������� PL/SQL Developer. ���������� �������� ����������';

{ TPLSQLDevPlugIn }

var
  Instance: TPLSQLDevPlugIn;

{ ������� ��������� }
class function TPLSQLDevPlugIn.GetInstance: TPLSQLDevPlugIn;
begin
  Result := Instance;
end;

{ ��������-����������� ������� }

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
  AItems.Add(SDivider);
  AItems.Add(SSettings);
end;

{ ��������� ������� }
procedure TPLSQLDevPlugIn.Activate;
begin
  { ��������� ��������� ������� }
  HasIDE_GetEditorHandle := Assigned(IDE_GetEditorHandle);
  HasIDE_SetMenuVisible  := Assigned(IDE_SetMenuVisible);
  HasIDE_GetPrefAsString := Assigned(IDE_GetPrefAsString);
  HasIDE_SetPrefAsString := Assigned(IDE_SetPrefAsString);
  { ���� ����� �������� � ���������� - ����� � ������������� }
  ShowMenuItem(1, HasIDE_GetEditorHandle);
  ShowMenuItem(2, HasIDE_GetEditorHandle);
  { �������� ��������� �������������� }
  LoadSettings;
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
  case AIndex of
    1, 2: FormatText(AIndex = 2);
    4: TFormSettings.ShowSettings;
  end;
end;

{ �������� �������� �������������� }
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

{ ���������� �������� �������������� }
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
  Settings.StartIndent := Col + ColShift;
  Controller.MakeFormatted(Input, Settings, OracleParser, Output);
  if Output = '' then exit;
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
