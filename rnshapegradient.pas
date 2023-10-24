unit RnShapeGradient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLIntf,
  Types, WSExtCtrls, Math;

type
  TGradientType = (gtRectangle, gtRoundRect);

  TGradientDirection = (gdCenter, gdVertical, gdHorizontal);

  { TRnShapeGradient }

  TRnShapeGradient = class(TGraphicControl)
  private
    FGradient: TGradientType;
    FColorBorder: TColor;
    FColorPrimary: TColor;
    FColorSecondary: TColor;
    FGradientDirection: TGradientDirection;
    FPaintOnlyBorder: Boolean;
    FRoundedSize: Smallint;
    procedure SetColorBorder(AValue: TColor);
    procedure SetGradient(Value: TGradientType);
    procedure SetColorPrimary(Value: TColor);
    procedure SetColorSecondary(Value: TColor);
    procedure SetGradientDirection(AValue: TGradientDirection);
    procedure SetPaintOnlyBorder(AValue: Boolean);
    procedure SetRoundedSize(AValue: Smallint);
  protected
    class procedure WSRegisterClass; override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property ColorBorder: TColor read FColorBorder write SetColorBorder default clNone;
    property ColorPrimary: TColor read FColorPrimary write SetColorPrimary default clBlue;
    property ColorSecondary: TColor read FColorSecondary write SetColorSecondary default clRed;
    property Constraints;
    property Direction: TGradientDirection read FGradientDirection write SetGradientDirection default gdCenter;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Gradient: TGradientType read FGradient write SetGradient default gtRectangle;
    property PaintOnlyBorder: Boolean read FPaintOnlyBorder write SetPaintOnlyBorder default False;
    property ParentShowHint;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property RoundedSize: Smallint read FRoundedSize write SetRoundedSize default 30;
    property ShowHint;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RnControls',[TRnShapeGradient]);
end;

{ TRnShapeGradient }

procedure TRnShapeGradient.SetGradient(Value: TGradientType);
begin
  if FGradient = Value then Exit;
  FGradient := Value;
  Invalidate;
end;

procedure TRnShapeGradient.SetColorBorder(AValue: TColor);
begin
  if FColorBorder=AValue then Exit;
  FColorBorder:=AValue;
  Invalidate;
end;

procedure TRnShapeGradient.SetColorPrimary(Value: TColor);
begin
  if FColorPrimary = Value then Exit;
  FColorPrimary := Value;
  Invalidate;
end;

procedure TRnShapeGradient.SetColorSecondary(Value: TColor);
begin
  if FColorSecondary = Value then Exit;
  FColorSecondary := Value;
  Invalidate;
end;

procedure TRnShapeGradient.SetGradientDirection(AValue: TGradientDirection);
begin
  if FGradientDirection=AValue then Exit;
  FGradientDirection:=AValue;
  Invalidate;
end;

procedure TRnShapeGradient.SetPaintOnlyBorder(AValue: Boolean);
begin
  if FPaintOnlyBorder=AValue then Exit;
  FPaintOnlyBorder:=AValue;
  Invalidate;
end;

procedure TRnShapeGradient.SetRoundedSize(AValue: Smallint);
begin
  if FRoundedSize=AValue then Exit;
  FRoundedSize:=AValue;
  Invalidate;
end;

class procedure TRnShapeGradient.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterShape;
end;

class function TRnShapeGradient.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 65;
  Result.CY := 65;
end;

constructor TRnShapeGradient.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  ControlStyle := ControlStyle + [csReplicatable];

  FRoundedSize := 30;
  FPaintOnlyBorder := False;
  FGradientDirection := gdCenter;
  FColorBorder := clNone;
  FColorPrimary := clBlue;
  FColorSecondary := clRed;
end;

destructor TRnShapeGradient.Destroy;
begin
  inherited Destroy;
end;

procedure TRnShapeGradient.Paint;
var
  PaintRect: TRect;

  MetadeLargura, MetadeAltura, Max, RC, GC, BC, Um_A_Um, Altura, Largura: Integer;
  Factor, RStep, GStep, BStep : Real;
  X, Red1, Green1, Blue1, Red2, Green2, Blue2 : Integer;

  Calc: Integer;
begin
  Largura := Self.Width;
  Altura := Self.Height;

  case FGradientDirection of
    gdHorizontal: Max := Largura;
    gdVertical: Max := Altura;
    gdCenter:
    begin
      MetadeLargura := Largura Div 2;
      MetadeAltura := Altura Div 2;
      if MetadeLargura > MetadeAltura then
      begin
        Max := MetadeLargura;
        Factor := MetadeAltura/MetadeLargura;
      end
      else
      begin
        Max := MetadeAltura;
        Factor := MetadeLargura/MetadeAltura;
      end;
    end;
  end;

  Red1 := GetRValue(FColorPrimary);
  Green1 := GetGValue(FColorPrimary);
  Blue1 := GetBValue(FColorPrimary);

  Red2 := GetRValue(FColorSecondary);
  Green2 := GetGValue(FColorSecondary);
  Blue2 := GetBValue(FColorSecondary);

  RStep := (Red2 - Red1) / Max;
  GStep := (Green2 - Green1) / Max;
  BStep := (Blue2 - Blue1) / Max;

  case FGradient of
    gtRectangle:
      Canvas.Pen.Width := 1;
    gtRoundRect:
    begin
      if FPaintOnlyBorder then
        Canvas.Pen.Width := 1
      else
        Canvas.Pen.Width := 2;
    end;
  end;

  X := 0;
  for Um_A_Um := Max downto 0 do
  begin
    if (Um_A_Um = Max) and (FColorBorder <> clNone) then
      Canvas.Pen.Color := FColorBorder
    else
    begin
      RC := Red1   + Ceil(X * RStep);
      GC := Green1 + Ceil(X * GStep);
      BC := Blue1  + Ceil(X * BStep);

      Canvas.Pen.Color := RGB(RC, GC, BC);
    end;

    if FPaintOnlyBorder then
      Canvas.Brush.Color := FColorPrimary;

    case FGradientDirection Of
      gdVertical:
      begin
        Canvas.MoveTo(0, Um_A_Um);
        Canvas.LineTo(Largura, Um_A_Um);
      end;
      gdHorizontal:
      begin
        Canvas.MoveTo(Um_A_Um, 0) ;
        Canvas.LineTo(Um_A_Um, Altura);
      end;
      gdCenter:
      begin
        Calc := Round(Um_A_Um*Factor);

        if MetadeLargura > MetadeAltura then
        begin
          PaintRect := Rect(MetadeLargura-Um_A_Um, MetadeAltura-Calc, MetadeLargura + Um_A_Um, MetadeAltura + Calc)
        end
        else
          PaintRect := Rect(MetadeLargura-Calc, MetadeAltura-Um_A_Um, MetadeLargura + Calc, MetadeAltura + Um_A_Um);

        case FGradient of
          gtRectangle:
            Canvas.Rectangle(PaintRect);
          gtRoundRect:
            Canvas.RoundRect(PaintRect, FRoundedSize, FRoundedSize);
        end;
      end;
    end;

    if FPaintOnlyBorder then
      Break;

    Inc(X);
  end;
  inherited Paint;
end;

end.
