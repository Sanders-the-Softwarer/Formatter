unit frSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Menus, Vcl.StdCtrls, Vcl.Samples.Spin, Printer;

type
  TFrameSettings = class(TFrame)
    btLoadFromFile: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    edDeclarationSingleLineParamLimit: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    edNamedArgumentSingleLineParamLimit: TSpinEdit;
    GroupBox1: TGroupBox;
    checkAlignFields: TCheckBox;
    checkAlignVariables: TCheckBox;
    checkAlignSpecialComments: TCheckBox;
    GroupBox3: TGroupBox;
    checkReplaceDefault: TCheckBox;
    Label3: TLabel;
    edMatchParamLimit: TSpinEdit;
    checkAlignCommands: TCheckBox;
    checkReplaceAsIs: TCheckBox;
    edPreferredExpressionLength: TSpinEdit;
    Label4: TLabel;
    checkAlignExpressions: TCheckBox;
    checkAlignColumns: TCheckBox;
    GroupBox2: TGroupBox;
    checkAddInAccessSpecificator: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    edPositionalArgumentSingleLineParamLimit: TSpinEdit;
    GroupBox4: TGroupBox;
    checkChangeCommentType: TCheckBox;
    checkUseSpace: TCheckBox;
    GroupBox5: TGroupBox;
    checkRemovePasswords: TCheckBox;
    checkLongOperands: TCheckBox;
    checkAlignRightComments: TCheckBox;
    checkCommentCorrectSpaces: TCheckBox;
    checkAlignFrom: TCheckBox;
    GroupBox6: TGroupBox;
    checkShiftPackageHeader: TCheckBox;
    checkShiftPackageBody: TCheckBox;
    btnSaveToFile: TButton;
    procedure UpdateRequired(Sender: TObject);
    procedure btLoadFromFileClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
  private
    LockOnChanged: boolean;
  public
    Settings: TFormatSettings;
    OnChanged: TNotifyEvent;
  public
    { Обновление настроек из интерфейса }
    procedure UpdateSettings;
    { Обновление интерфейса из настроек }
    procedure UpdateEdits;
  end;

implementation

{$R *.dfm}

{ Обновление настроек из интерфейса }
procedure TFrameSettings.UpdateSettings;
begin
  Assert(Settings <> nil);
  Settings.DeclarationSingleLineParamLimit := edDeclarationSingleLineParamLimit.Value;
  Settings.NamedArgumentSingleLineParamLimit := edNamedArgumentSingleLineParamLimit.Value;
  Settings.PositionalArgumentSingleLineParamLimit := edPositionalArgumentSingleLineParamLimit.Value;
  Settings.MatchParamLimit                 := edMatchParamLimit.Value;
  Settings.AlignVariables                  := checkAlignVariables.Checked;
  Settings.AlignFields                     := checkAlignFields.Checked;
  Settings.AlignColumns                    := checkAlignColumns.Checked;
  Settings.AlignExpressions                := checkAlignExpressions.Checked;
  Settings.AlignSpecialComments            := checkAlignSpecialComments.Checked;
  Settings.AlignCommands                   := checkAlignCommands.Checked;
  Settings.AlignUseSpace                   := checkUseSpace.Checked;
  Settings.AlignRightComments              := checkAlignRightComments.Checked;
  Settings.AlignFrom                       := checkAlignFrom.Checked;
  Settings.ReplaceDefault                  := checkReplaceDefault.Checked;
  Settings.ReplaceAsIs                     := checkReplaceAsIs.Checked;
  Settings.AddInAccessSpecificator         := checkAddInAccessSpecificator.Checked;
  Settings.ChangeCommentType               := checkChangeCommentType.Checked;
  Settings.CorrectCommentSpaces            := checkCommentCorrectSpaces.Checked;
  Settings.RemoveConnectPasswords          := checkRemovePasswords.Checked;
  Settings.BeautifyLongOperands            := checkLongOperands.Checked;
  Settings.ShiftPackageHeader              := checkShiftPackageHeader.Checked;
  Settings.ShiftPackageBody                := checkShiftPackageBody.Checked;
  Settings.PreferredExpressionLength       := edPreferredExpressionLength.Value;
end;

{ Обновление интерфейса из настроек }
procedure TFrameSettings.UpdateEdits;
begin
  Assert(Settings <> nil);
  edDeclarationSingleLineParamLimit.Value := Settings.DeclarationSingleLineParamLimit;
  edNamedArgumentSingleLineParamLimit.Value := Settings.NamedArgumentSingleLineParamLimit;
  edPositionalArgumentSingleLineParamLimit.Value := Settings.PositionalArgumentSingleLineParamLimit;
  edMatchParamLimit.Value              := Settings.MatchParamLimit;
  edPreferredExpressionLength.Value    := Settings.PreferredExpressionLength;
  checkAlignFields.Checked             := Settings.AlignFields;
  checkAlignColumns.Checked            := Settings.AlignColumns;
  checkAlignVariables.Checked          := Settings.AlignVariables;
  checkAlignSpecialComments.Checked    := Settings.AlignSpecialComments;
  checkAlignCommands.Checked           := Settings.AlignCommands;
  checkAlignExpressions.Checked        := Settings.AlignExpressions;
  checkUseSpace.Checked                := Settings.AlignUseSpace;
  checkAlignRightComments.Checked      := Settings.AlignRightComments;
  checkAlignFrom.Checked               := Settings.AlignFrom;
  checkReplaceDefault.Checked          := Settings.ReplaceDefault;
  checkReplaceAsIs.Checked             := Settings.ReplaceAsIs;
  checkAddInAccessSpecificator.Checked := Settings.AddInAccessSpecificator;
  checkChangeCommentType.Checked       := Settings.ChangeCommentType;
  checkCommentCorrectSpaces.Checked    := Settings.CorrectCommentSpaces;
  checkRemovePasswords.Checked         := Settings.RemoveConnectPasswords;
  checkLongOperands.Checked            := Settings.BeautifyLongOperands;
  checkShiftPackageHeader.Checked      := Settings.ShiftPackageHeader;
  checkShiftPackageBody.Checked        := Settings.ShiftPackageBody;
end;

procedure TFrameSettings.btLoadFromFileClick(Sender: TObject);
var S: TStringList;
begin
  if not OpenDialog.Execute then exit;
  SaveDialog.FileName := OpenDialog.FileName;
  S := TStringList.Create(dupIgnore, true, false);
  try
    LockOnChanged := true;
    S.LoadFromFile(OpenDialog.FileName);
    Settings.Load(S);
    UpdateEdits;
  finally
    FreeAndNil(S);
    LockOnChanged := false;
    UpdateRequired(Self);
  end;
end;

procedure TFrameSettings.btnSaveToFileClick(Sender: TObject);
var S: TStringList;
begin
  UpdateSettings;
  if not SaveDialog.Execute then exit;
  OpenDialog.FileName := SaveDialog.FileName;
  S := TStringList.Create(dupIgnore, true, false);
  try
    Settings.Save(S);
    S.SaveToFile(SaveDialog.FileName);
  finally
    FreeAndNil(S);
  end;
end;

procedure TFrameSettings.UpdateRequired(Sender: TObject);
begin
  if Assigned(OnChanged) and not LockOnChanged then OnChanged(Self);
end;

end.
