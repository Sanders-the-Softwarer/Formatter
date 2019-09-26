////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                    Атрибуты для описания классов проекта                   //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Attributes;

interface

uses System.Rtti;

type

  { Сообщает принтеру о том, что объект поддерживает выравнивание }
  AlignedAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  { Сообщает принтеру о необходимости печатать элемент в нижнем регистре }
  LowerCaseAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  { Сообщает принтеру о степени приоритетности пробелов слева }
  LeftSpaceAttribute = class(TCustomAttribute)
  private
    FPriority: integer;
  public
    constructor Create(APriority: integer);
    property Priority: integer read FPriority;
  end;

  { Сообщает принтеру о степени приоритетности пробелов справа }
  RightSpaceAttribute = class(TCustomAttribute)
  private
    FPriority: integer;
  public
    constructor Create(APriority: integer);
    property Priority: integer read FPriority;
  end;

  TCustomAttributeClass = class of TCustomAttribute;

{ Получение атрибута из класса }
function GetAttribute(AClass: TClass; AttributeClass: TCustomAttributeClass): TCustomAttribute; overload;
function GetAttribute(AObject: TObject; AttributeClass: TCustomAttributeClass): TCustomAttribute; overload;

{ Проверка наличия атрибута в классе }
function HasAttribute(AClass: TClass; AttributeClass: TCustomAttributeClass): boolean;

implementation

var
  Context: TRttiContext;

{ Получение атрибута из класса }
function GetAttribute(AClass: TClass; AttributeClass: TCustomAttributeClass): TCustomAttribute;
var
  RttiType: TRttiType;
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  repeat
    RttiType := Context.GetType(AClass);
    Attrs := RttiType.GetAttributes;
    for Attr in Attrs do
      if Attr.InheritsFrom(AttributeClass) then
        exit(Attr);
    AClass := AClass.ClassParent;
  until AClass = TObject;
  Result := nil;
end;

{ Получение атрибута из класса }
function GetAttribute(AObject: TObject; AttributeClass: TCustomAttributeClass): TCustomAttribute;
begin
  if Assigned(AObject)
    then Result := GetAttribute(AObject.ClassType, AttributeClass)
    else Result := nil;
end;

{ Проверка наличия атрибута в классе }
function HasAttribute(AClass: TClass; AttributeClass: TCustomAttributeClass): boolean;
var
  RttiType: TRttiType;
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  Result := false;
  RttiType := Context.GetType(AClass);
  Attrs := RttiType.GetAttributes;
  for Attr in Attrs do
    Result := Result or Attr.InheritsFrom(AttributeClass);
end;

{ AlignedAttribute }

constructor AlignedAttribute.Create;
begin
end;

{ LowerCaseAttribute }

constructor LowerCaseAttribute.Create;
begin
end;

{ LeftSpaceAttribute }

constructor LeftSpaceAttribute.Create(APriority: integer);
begin
  FPriority := APriority;
end;

{ RightSpaceAttribute }

constructor RightSpaceAttribute.Create(APriority: integer);
begin
  FPriority := APriority;
end;

initialization
  Context := TRttiContext.Create;

end.

