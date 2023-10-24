{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RnControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  RnEdit, RnButton, RnIconChar, RnShapeGradient, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RnEdit', @RnEdit.Register);
  RegisterUnit('RnButton', @RnButton.Register);
  RegisterUnit('RnShapeGradient', @RnShapeGradient.Register);
end;

initialization
  RegisterPackage('RnControls', @Register);
end.
