﻿// PL/SQL Developer Plug-In Interface functions
// Copyright 2009 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

unit PlugInIntf;

interface

uses
  Windows, Messages, Classes;

const // Window types
  wtNone      = 0;
  wtSQL       = 1;
  wtTest      = 2;
  wtProcEdit  = 3;
  wtCommand   = 4;
  wtPlan      = 5;
  wtReport    = 6;
  wtHTML      = 7;
  wtGenerator = 8;
  wtImporter  = 9;
  wtDiagram   = 10;
  wtGraph     = 11;
  wtTestMgr   = 12;
  wtSession   = 97;
  wtPrjFiles  = 98;
  wtProject   = 99;

var
  PlugInID: Integer;


var // Declaration of all PL/SQL Developer callback functions
  SYS_Version: function: Integer; cdecl;
  SYS_Registry: function: PAnsiChar; cdecl;
  SYS_RootDir: function: PAnsiChar; cdecl;
  SYS_OracleHome: function: PAnsiChar; cdecl;
  SYS_OCIDLL: function: PAnsiChar; cdecl;
  SYS_OCI8Mode:function : Bool; cdecl;
  SYS_XPStyle:function : Bool; cdecl;
  SYS_TNSNAMES: function(Param: PAnsiChar): PAnsiChar; cdecl;
  SYS_DelphiVersion: function: Integer; cdecl;
  // 10
  IDE_MenuState: procedure(ID, Index: Integer; Enabled: Bool); cdecl;
  IDE_Connected: function: Bool; cdecl;
  IDE_GetConnectionInfo: procedure(var Username, Password, Database: PAnsiChar); cdecl;
  IDE_GetBrowserInfo: procedure(var ObjectType, ObjectOwner, ObjectName: PAnsiChar); cdecl;
  IDE_GetWindowType: function: Integer; cdecl;
  IDE_GetAppHandle: function: Integer; cdecl;
  IDE_GetWindowHandle: function: Integer; cdecl;
  IDE_GetClientHandle: function: Integer; cdecl;
  IDE_GetChildHandle: function: Integer; cdecl;
  IDE_Refresh: procedure; cdecl;
  // 20
  IDE_CreateWindow: procedure(WindowType: Integer; Text: PAnsiChar; Execute: Bool); cdecl;
  IDE_OpenFile: function(WindowType: Integer; Filename: PAnsiChar): Bool; cdecl;
  IDE_SaveFile: function: Bool; cdecl;
  IDE_Filename: function: PAnsiChar; cdecl;
  IDE_CloseFile: procedure; cdecl;
  IDE_SetReadOnly: procedure(ReadOnly: Bool); cdecl;
  IDE_GetReadOnly: function: Bool; cdecl;
  IDE_ExecuteSQLReport: function(SQL: PAnsiChar; Title: PAnsiChar; Updateable: Bool): Bool; cdecl;
  IDE_ReloadFile: function: Bool; cdecl;
  IDE_SetFilename: procedure(Filename: PAnsiChar); cdecl;
  // 30
  IDE_GetText: function: PAnsiChar; cdecl;
  IDE_GetSelectedText: function: PAnsiChar; cdecl;
  IDE_GetCursorWord: function: PAnsiChar; cdecl;
  IDE_GetEditorHandle: function: Integer; cdecl;
  IDE_SetText: function(T: PAnsiChar): Bool; cdecl;
  IDE_SetStatusMessage: function(T: PAnsiChar): Bool; cdecl;
  IDE_SetErrorPosition: function(Line, Col: Integer): Bool; cdecl;
  IDE_ClearErrorPositions: procedure; cdecl;
  IDE_GetCursorWordPosition: function: Integer; cdecl;
  IDE_Perform: function(Param: Integer): Bool; cdecl;
  // 40
  SQL_Execute: function(SQL: PAnsiChar): Integer; cdecl;
  SQL_FieldCount: function: Integer; cdecl;
  SQL_Eof: function: Bool; cdecl;
  SQL_Next: function: Integer; cdecl;
  SQL_Field: function(Field: Integer): PAnsiChar; cdecl;
  SQL_FieldName: function(Field: Integer): PAnsiChar; cdecl;
  SQL_FieldIndex: function(Name: PAnsiChar): Integer; cdecl;
  SQL_FieldType: function(Field: Integer): Integer; cdecl;
  SQL_ErrorMessage: function: PAnsiChar; cdecl;
  // 50
  SQL_UsePlugInSession: function(PlugInID: Integer): Bool; cdecl;
  SQL_UseDefaultSession: procedure(PlugInID: Integer); cdecl;
  SQL_CheckConnection: function: Bool; cdecl;
  SQL_GetDBMSGetOutput: function: PAnsiChar; cdecl;
  SQL_SetVariable: procedure(Name, Value: PAnsiChar); cdecl;
  SQL_GetVariable: function(Name: PAnsiChar): PAnsiChar; cdecl;
  SQL_ClearVariables: procedure; cdecl;
  // 60
  IDE_GetCustomKeywords: function: PAnsiChar; cdecl;
  IDE_SetCustomKeywords: procedure(Keywords: PAnsiChar); cdecl;
  IDE_SetKeywords: procedure(ID, Style: Integer; Keywords: PAnsiChar); cdecl;
  IDE_ActivateKeywords: procedure; cdecl;
  IDE_RefreshMenus: procedure(ID: Integer); cdecl;
  IDE_SetMenuName: procedure(ID, Index: Integer; Name: PAnsiChar); cdecl;
  IDE_SetMenuCheck: procedure(ID, Index: Integer; Enabled: Bool); cdecl;
  IDE_SetMenuVisible: procedure(ID, Index: Integer; Enabled: Bool); cdecl;
  IDE_GetMenuLayout: function: PAnsiChar; cdecl;
  IDE_CreatePopupItem: procedure(ID, Index: Integer; Name, ObjectType: PAnsiChar); cdecl;
  // 70
  IDE_SetConnection: function(Username, Password, Database: PAnsiChar): Bool; cdecl;
  IDE_GetObjectInfo: procedure(AnObject: PAnsiChar; var ObjectType, ObjectOwner, ObjectName, SubObject: PAnsiChar); cdecl;
  IDE_GetBrowserItems: function(Node: PAnsiChar; GetItems: Bool): PAnsiChar; cdecl;
  IDE_RefreshBrowser: procedure(Node: PAnsiChar); cdecl;
  IDE_GetPopupObject: procedure(var ObjectType, ObjectOwner, ObjectName, SubObject: PAnsiChar); cdecl;
  IDE_GetPopupBrowserRoot: function: PAnsiChar; cdecl;
  IDE_RefreshObject: procedure(ObjectType, ObjectOwner, ObjectName: PAnsiChar; Action: Integer) cdecl;
  IDE_FirstSelectedObject: function(var ObjectType, ObjectOwner, ObjectName, SubObject: PAnsiChar): Bool; cdecl;
  IDE_NextSelectedObject: function(var ObjectType, ObjectOwner, ObjectName, SubObject: PAnsiChar): Bool; cdecl;
  IDE_GetObjectSource: function(ObjectType, ObjectOwner, ObjectName: PAnsiChar): PAnsiChar; cdecl;
  // 80
  IDE_GetWindowCount: function: Integer; cdecl;
  IDE_SelectWindow: function(Index: Integer): Bool; cdecl;
  IDE_ActivateWindow: function(Index: Integer): Bool; cdecl;
  IDE_WindowIsModified: function: Bool; cdecl;
  IDE_WindowIsRunning: function: Bool; cdecl;
  IDE_WindowPin: function(Pin: Integer): Integer; cdecl;
  // 90
  IDE_SplashCreate: procedure(ProgressMax: Integer); cdecl;
  IDE_SplashHide: procedure; cdecl;
  IDE_SplashWrite: procedure(S: PAnsiChar); cdecl;
  IDE_SplashWriteLn: procedure(S: PAnsiChar); cdecl;
  IDE_SplashProgress: procedure(Progress: Integer); cdecl;
  IDE_TemplatePath: function: PAnsiChar; cdecl;
  IDE_ExecuteTemplate: function(Template: PAnsiChar; NewWindow: Bool): Bool; cdecl;
  IDE_GetConnectAs: function: PAnsiChar; cdecl;
  IDE_SetConnectionAs: function(Username, Password, Database, ConnectAs: PAnsiChar): Bool; cdecl;
  // 100
  IDE_GetFileOpenMenu: function(MenuIndex: Integer; var WindowType: Integer): PAnsiChar; cdecl;
  IDE_CanSaveWindow: function: Bool; cdecl;
  IDE_OpenFileExternal: procedure(WindowType: Integer; Data, FileSystem, Tag, Filename: PAnsiChar); cdecl;
  IDE_GetFileTypes: function(WindowType: Integer): PAnsiChar; cdecl;
  IDE_GetDefaultExtension: function(WindowType: Integer): PAnsiChar; cdecl;
  IDE_GetFileData: function: PAnsiChar; cdecl;
  IDE_FileSaved: procedure(FileSystem, FileTag, Filename: PAnsiChar); cdecl;
  IDE_ShowHTML: function(Url, Hash, Title, ID: PAnsiChar): Bool; cdecl;
  IDE_RefreshHTML: function(Url, ID: PAnsiChar; BringToFront: Bool): Bool; cdecl;
  IDE_GetProcEditExtension: function(oType: PAnsiChar): PAnsiChar; cdecl;
  // 110
  IDE_GetWindowObject: function(var ObjectType, ObjectOwner, ObjectName, SubObject: PAnsiChar): Bool; cdecl;
  IDE_FirstSelectedFile: function (Files, Directories: Boolean): PAnsiChar; cdecl;
  IDE_NextSelectedFile: function: PAnsiChar; cdecl;
  IDE_RefreshFileBrowser: procedure; cdecl;
  // 120
  IDE_KeyPress: procedure(Key, Shift: Integer); cdecl;
  IDE_GetMenuItem: function(MenuName: PAnsiChar): Integer; cdecl;
  IDE_SelectMenu: function(MenuItem: Integer): Bool; cdecl;
  // 130
  IDE_TranslationFile: function: PAnsiChar; cdecl;
  IDE_TranslationLanguage: function: PAnsiChar; cdecl;
  IDE_GetTranslatedMenuLayout: function: PAnsiChar; cdecl;
  IDE_MainFont: function: PAnsiChar; cdecl;
  IDE_TranslateItems: function(Group: PAnsiChar): PAnsiChar; cdecl;
  IDE_TranslateString: function(ID, Default, Param1, Param2: PAnsiChar): PAnsiChar; cdecl;
  // 140
  IDE_SaveRecoveryFiles: function: Bool; cdecl;
  IDE_GetCursorX: function: Integer; cdecl;
  IDE_GetCursorY: function: Integer; cdecl;
  IDE_SetCursor: procedure(X, Y: Integer); cdecl;
  IDE_SetBookmark: function(Index, X, Y: Integer): Integer; cdecl;
  IDE_ClearBookmark: procedure(Index: Integer); cdecl;
  IDE_GotoBookmark: procedure(Index: Integer); cdecl;
  IDE_GetBookmark: function(Index: Integer; var X: Integer; var Y: Integer): Bool; cdecl;
  IDE_TabInfo:function (Index: Integer): PAnsiChar; cdecl;
  IDE_TabIndex: function(Index: Integer): Integer; cdecl;
  // 150
  IDE_CreateToolButton: procedure(ID, Index: Integer; Name: PAnsiChar; BitmapFile: PAnsiChar; BitmapHandle: Integer); cdecl;
  IDE_WindowHasEditor: function(CodeEditor: Bool): Bool; cdecl;
  // 160
  IDE_BeautifierOptions: function: Integer; cdecl;
  IDE_BeautifyWindow: function: Bool; cdecl;
  IDE_BeautifyText: function(S: PAnsiChar): PAnsiChar; cdecl;
  IDE_ObjectAction: function(Action, ObjectType, ObjectOwner, ObjectName: PAnsiChar): Bool; cdecl;
  IDE_ShowDialog: function(Dialog, Param: PAnsiChar): Bool; cdecl;
  // 170
  IDE_DebugLog: procedure(Msg: PAnsiChar); cdecl;
  IDE_GetParamString: function(Name: PAnsiChar): PAnsiChar; cdecl;
  IDE_GetParamBool: function(Name: PAnsiChar): Bool; cdecl;
  IDE_GetBrowserFilter: procedure(Index: Integer; var Name, WhereClause, OrderByClause, User: PAnsiChar; var Active: Bool); cdecl;
  // 180
  IDE_CommandFeedback: procedure(FeedbackHandle: Integer; S: PAnsiChar); cdecl;
  // 190
  IDE_ResultGridRowCount: function: Integer; cdecl;
  IDE_ResultGridColCount: function: Integer; cdecl;
  IDE_ResultGridCell: function(Col, Row: Integer): PAnsiChar; cdecl;
  // 200
  IDE_Authorized: function(Category, Name, SubName: PAnsiChar): Bool; cdecl;
  IDE_WindowAllowed: function(WindowType: Integer; ShowErrorMessage: Bool): Bool; cdecl;
  IDE_Authorization: function: Bool; cdecl;
  IDE_AuthorizationItems: function(Category: PAnsiChar): PAnsiChar; cdecl;
  IDE_AddAuthorizationItem: procedure(PlugInID: Integer; Name: PAnsiChar); cdecl;
  // 210
  IDE_GetPersonalPrefSets: function: PAnsiChar; cdecl;
  IDE_GetDefaultPrefSets: function: PAnsiChar; cdecl;
  IDE_GetPrefAsString: function(PlugInID: Integer; PrefSet, Name: PAnsiChar; Default: PAnsiChar): PAnsiChar; cdecl;
  IDE_GetPrefAsInteger: function(PlugInID: Integer; PrefSet, Name: PAnsiChar; Default: Integer): Integer; cdecl;
  IDE_GetPrefAsBool: function(PlugInID: Integer; PrefSet, Name: PAnsiChar; Default: Bool): Bool; cdecl;
  IDE_SetPrefAsString: function(PlugInID: Integer; PrefSet, Name: PAnsiChar; Value: PAnsiChar): Bool; cdecl;
  IDE_SetPrefAsInteger: function(PlugInID: Integer; PrefSet, Name: PAnsiChar; Value: Integer): Bool; cdecl;
  IDE_SetPrefAsBool: function(PlugInID: Integer; PrefSet, Name: PAnsiChar; Value: Bool): Bool; cdecl;
  IDE_GetGeneralPref: function(Name: PAnsiChar): PAnsiChar; cdecl;
  IDE_PlugInSetting: function(PlugInID: Integer; Setting, Value: PAnsiChar): Bool; cdecl;
  // 220
  IDE_GetProcOverloadCount: function(Owner, PackageName, ProcedureName: PAnsiChar): Integer; cdecl;
  IDE_SelectProcOverloading: function(Owner, PackageName, ProcedureName: PAnsiChar): Integer; cdecl;
  // 230
  IDE_GetSessionValue: function(AName: PAnsiChar): PAnsiChar; cdecl;
  IDE_CheckDBVersion: function(Version: PAnsiChar): Boolean; cdecl;
  // 240
  IDE_GetConnectionInfoEx: function(ix: Integer; var Username, Password, Database, ConnectAs: PAnsiChar): Bool; cdecl;
  IDE_FindConnection: function(Username, Database, ConnectAs: PAnsiChar): Integer; cdecl;
  IDE_AddConnection: function(Username, Password, Database, ConnectAs: PAnsiChar): Integer; cdecl;
  IDE_ConnectConnection: function(ix: Integer): Bool; cdecl;
  IDE_SetMainConnection: function(ix: Integer): Bool; cdecl;
  IDE_GetWindowConnection: function: Integer; cdecl;
  IDE_SetWindowConnection: function(ix: Integer): Bool; cdecl;
  // 250
  IDE_GetConnectionInfoEx10: function(ix: Integer; var Username, Password, Database, ConnectAs, Edition, Workspace: PAnsiChar): Bool; cdecl;
  IDE_FindConnectionEx10: function(Username, Database, ConnectAs, Edition, Workspace: PAnsiChar): Integer; cdecl;
  IDE_AddConnectionEx10: function(Username, Password, Database, ConnectAs, Edition, Workspace: PAnsiChar): Integer; cdecl;
  IDE_GetConnectionTreeEx10: function(ix: Integer; var Description, Username, Password, Database, ConnectAs, Edition, Workspace: PAnsiChar; var ID, ParentID: integer): Bool; cdecl;



procedure RegisterCallback(Index: Integer; Addr: Pointer); cdecl;

implementation

// Registration of PL/SQL Developer callback functions
procedure RegisterCallback(Index: Integer; Addr: Pointer); cdecl;
begin
  case Index of
     1 : @SYS_Version := Addr;
     2 : @SYS_Registry := Addr;
     3 : @SYS_RootDir := Addr;
     4 : @SYS_OracleHome := Addr;
     5 : @SYS_OCIDLL := Addr;
     6 : @SYS_OCI8Mode := Addr;
     7 : @SYS_XPStyle := Addr;
     8 : @SYS_TNSNAMES := Addr;
     9 : @SYS_DelphiVersion := Addr;
    10 : @IDE_MenuState := Addr;
    11 : @IDE_Connected := Addr;
    12 : @IDE_GetConnectionInfo := Addr;
    13 : @IDE_GetBrowserInfo := Addr;
    14 : @IDE_GetWindowType := Addr;
    15 : @IDE_GetAppHandle := Addr;
    16 : @IDE_GetWindowHandle := Addr;
    17 : @IDE_GetClientHandle := Addr;
    18 : @IDE_GetChildHandle := Addr;
    19 : @IDE_Refresh := Addr;
    20 : @IDE_CreateWindow := Addr;
    21 : @IDE_OpenFile := Addr;
    22 : @IDE_SaveFile := Addr;
    23 : @IDE_Filename := Addr;
    24 : @IDE_CloseFile := Addr;
    25 : @IDE_SetReadOnly := Addr;
    26 : @IDE_GetReadOnly := Addr;
    27 : @IDE_ExecuteSQLReport := Addr;
    28 : @IDE_ReloadFile := Addr;
    29 : @IDE_SetFilename := Addr;
    30 : @IDE_GetText := Addr;
    31 : @IDE_GetSelectedText := Addr;
    32 : @IDE_GetCursorWord := Addr;
    33 : @IDE_GetEditorHandle := Addr;
    34 : @IDE_SetText := Addr;
    35 : @IDE_SetStatusMessage := Addr;
    36 : @IDE_SetErrorPosition := Addr;
    37 : @IDE_ClearErrorPositions := Addr;
    38 : @IDE_GetCursorWordPosition := Addr;
    39 : @IDE_Perform := Addr;
    40 : @SQL_Execute := Addr;
    41 : @SQL_FieldCount := Addr;
    42 : @SQL_Eof := Addr;
    43 : @SQL_Next := Addr;
    44 : @SQL_Field := Addr;
    45 : @SQL_FieldName := Addr;
    46 : @SQL_FieldIndex := Addr;
    47 : @SQL_FieldType := Addr;
    48 : @SQL_ErrorMessage := Addr;
    50 : @SQL_UsePlugInSession := Addr;
    51 : @SQL_UseDefaultSession := Addr;
    52 : @SQL_CheckConnection := Addr;
    53 : @SQL_GetDBMSGetOutput := Addr;
    54 : @SQL_SetVariable := Addr;
    55 : @SQL_GetVariable := Addr;
    56 : @SQL_ClearVariables := Addr;
    60 : @IDE_GetCustomKeywords := Addr;
    61 : @IDE_SetCustomKeywords := Addr;
    62 : @IDE_SetKeywords := Addr;
    63 : @IDE_ActivateKeywords := Addr;
    64 : @IDE_RefreshMenus := Addr;
    65 : @IDE_SetMenuName := Addr;
    66 : @IDE_SetMenuCheck := Addr;
    67 : @IDE_SetMenuVisible := Addr;
    68 : @IDE_GetMenuLayout := Addr;
    69 : @IDE_CreatePopupItem := Addr;
    70 : @IDE_SetConnection := Addr;
    71 : @IDE_GetObjectInfo := Addr;
    72 : @IDE_GetBrowserItems := Addr;
    73 : @IDE_RefreshBrowser := Addr;
    74 : @IDE_GetPopupObject := Addr;
    75 : @IDE_GetPopupBrowserRoot := Addr;
    76 : @IDE_RefreshObject := Addr;
    77 : @IDE_FirstSelectedObject := Addr;
    78 : @IDE_NextSelectedObject := Addr;
    79 : @IDE_GetObjectSource := Addr;
    80 : @IDE_GetWindowCount := Addr;
    81 : @IDE_SelectWindow := Addr;
    82 : @IDE_ActivateWindow := Addr;
    83 : @IDE_WindowIsModified := Addr;
    84 : @IDE_WindowIsRunning := Addr;
    85 : @IDE_WindowPin := Addr;
    90 : @IDE_SplashCreate := Addr;
    91 : @IDE_SplashHide := Addr;
    92 : @IDE_SplashWrite := Addr;
    93 : @IDE_SplashWriteLn := Addr;
    94 : @IDE_SplashProgress := Addr;
    95 : @IDE_TemplatePath := Addr;
    96 : @IDE_ExecuteTemplate := Addr;
    97 : @IDE_GetConnectAs := Addr;
    98 : @IDE_SetConnectionAs := Addr;
   100 : @IDE_GetFileOpenMenu := Addr;
   101 : @IDE_CanSaveWindow := Addr;
   102 : @IDE_OpenFileExternal := Addr;
   103 : @IDE_GetFileTypes := Addr;
   104 : @IDE_GetDefaultExtension := Addr;
   105 : @IDE_GetFileData := Addr;
   106 : @IDE_FileSaved := Addr;
   107 : @IDE_ShowHTML := Addr;
   108 : @IDE_RefreshHTML := Addr;
   109 : @IDE_GetProcEditExtension := Addr;
   110 : @IDE_GetWindowObject := Addr;
   111 : IDE_FirstSelectedFile := Addr;
   112 : IDE_NextSelectedFile := Addr;
   113 : IDE_RefreshFileBrowser := Addr;
   120 : @IDE_KeyPress := Addr;
   121 : @IDE_GetMenuItem := Addr;
   122 : @IDE_SelectMenu := Addr;
   130 : @IDE_TranslationFile := Addr;
   131 : @IDE_TranslationLanguage := Addr;
   132 : @IDE_GetTranslatedMenuLayout := Addr;
   133 : @IDE_MainFont := Addr;
   134 : @IDE_TranslateItems := Addr;
   135 : @IDE_TranslateString := Addr;
   140 : @IDE_SaveRecoveryFiles := Addr;
   141 : @IDE_GetCursorX := Addr;
   142 : @IDE_GetCursorY := Addr;
   143 : @IDE_SetCursor := Addr;
   144 : @IDE_SetBookmark := Addr;
   145 : @IDE_ClearBookmark := Addr;
   146 : @IDE_GotoBookmark := Addr;
   147 : @IDE_GetBookmark := Addr;
   148 : @IDE_TabInfo := Addr;
   149 : @IDE_TabIndex := Addr;
   150 : @IDE_CreateToolButton := Addr;
   153 : @IDE_WindowHasEditor := Addr;
   160 : @IDE_BeautifierOptions := Addr;
   161 : @IDE_BeautifyWindow := Addr;
   162 : @IDE_BeautifyText := Addr;
   165 : @IDE_ObjectAction := Addr;
   166 : @IDE_ShowDialog := Addr;
   173 : @IDE_DebugLog := Addr;
   174 : @IDE_GetParamString := Addr;
   175 : @IDE_GetParamBool := Addr;
   180 : @IDE_CommandFeedback := Addr;
   190 : @IDE_ResultGridRowCount := Addr;
   191 : @IDE_ResultGridColCount := Addr;
   192 : @IDE_ResultGridCell := Addr;
   200 : @IDE_Authorized := Addr;
   201 : @IDE_WindowAllowed := Addr;
   202 : @IDE_Authorization := Addr;
   203 : @IDE_AuthorizationItems := Addr;
   204 : @IDE_AddAuthorizationItem := Addr;
   210 : @IDE_GetPersonalPrefSets := Addr;
   211 : @IDE_GetDefaultPrefSets := Addr;
   212 : @IDE_GetPrefAsString := Addr;
   213 : @IDE_GetPrefAsInteger := Addr;
   214 : @IDE_GetPrefAsBool := Addr;
   215 : @IDE_SetPrefAsString := Addr;
   216 : @IDE_SetPrefAsInteger := Addr;
   217 : @IDE_SetPrefAsBool := Addr;
   218 : @IDE_GetGeneralPref := Addr;
   219 : @IDE_PlugInSetting := Addr;
   220 : @IDE_GetProcOverloadCount := Addr;
   221 : @IDE_SelectProcOverloading := Addr;
   230 : @IDE_GetSessionValue := Addr;
   231 : @IDE_CheckDBVersion := Addr;
   240 : @IDE_GetConnectionInfoEx := Addr;
   241 : @IDE_FindConnection := Addr;
   242 : @IDE_AddConnection := Addr;
   243 : @IDE_ConnectConnection := Addr;
   244 : @IDE_SetMainConnection := Addr;
   245 : @IDE_GetWindowConnection := Addr;
   246 : @IDE_SetWindowConnection := Addr;
   250 : @IDE_GetConnectionInfoEx10 := Addr;
   251 : @IDE_FindConnectionEx10 := Addr;
   252 : @IDE_AddConnectionEx10 := Addr;
   253 : @IDE_GetConnectionTreeEx10 := Addr;
  end;
end;


end.
