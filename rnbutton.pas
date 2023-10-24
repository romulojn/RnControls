unit RnButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, GraphType, Buttons, ImgList, Types,
  LazUtilities, Controls, LMessages, Themes, RnIconChar, fpimage;

type

  { TRnCustomButton }

  TRnCustomButton = class(TCustomPanel)
  private
    FDefaultCaption: Boolean;
    FLayout: TButtonLayout;
    FMargin: Integer;
    FSpacing: Integer;
    FImageChangeLink: TChangeLink;
    FIconChar: Word;
    FIconColor: TColor;
    FIconSize: Integer;
    FIconMargin: Byte;

    FAllowDown: Boolean;
    FGroupIndex: Integer;
    FIsSelected: Boolean;
    FHoverColor: TColor;
    FDefaultColor: TColor;
    FSelectedColor: TColor;
    FSelectedHoverColor: TColor;

    function GetGlyph: TBitmap;
    function GetNumGlyphs: Integer;
    function GetImages: TCustomImageList;
    function GetImageIndex: TImageIndex;
    function GetImageWidth: Integer;

    procedure ImageListChange(Sender: TObject);
    function IsGlyphStored: Boolean;

    procedure SetHoverColor(AValue: TColor);
    procedure SetDefaultColor(AValue: TColor);
    procedure SetSelectedColor(AValue: TColor);
    procedure SetSelectedHoverColor(AValue: TColor);
    procedure SetAllowDown(AValue: Boolean);
    procedure SetIconChar(AValue: Word);
    procedure SetIconColor(AValue: TColor);
    procedure SetIconSize(AValue: Integer);
    procedure SetIconMargin(AValue: Byte);

    procedure SetLayout(AValue: TButtonLayout);
    procedure SetGlyph(AValue: TBitmap);
    procedure SetMargin(const AValue: Integer);
    procedure SetNumGlyphs(AValue: Integer);
    procedure SetSpacing(AValue: Integer);
    procedure SetDefaultCaption(const AValue: Boolean);
    procedure SetImages(const AImages: TCustomImageList);
    procedure SetImageIndex(const AImageIndex: TImageIndex);
    procedure SetImageWidth(const AImageWidth: Integer);

    procedure MouseEnter(Sender: TObject);
    procedure MouseLeave(Sender: TObject);
    procedure Click(Sender: TObject);
  protected
    FButtonGlyph: TButtonGlyph;
    procedure SetIsSelected(AValue: Boolean);
    procedure GlyphChanged(Sender: TObject);
    function IsCaptionStored: Boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure TextChanged; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure CMAppShowBtnGlyphChanged(var Message: TLMessage); message CM_APPSHOWBTNGLYPHCHANGED;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure Click; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure ResetColor;
    procedure ResetColors;
    procedure LoadGlyphFromResourceName(Instance: THandle; const AName: String);
    procedure LoadGlyphFromLazarusResource(const AName: String);
    procedure LoadGlyphFromStock(AIdButton: Integer);
  public
    property Caption stored IsCaptionStored;
    property DefaultCaption: Boolean read FDefaultCaption write SetDefaultCaption default False;
    property AllowDown: Boolean read FAllowDown write SetAllowDown default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsGlyphStored;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clDefault;
    property IconChar: Word read FIconChar write SetIconChar default 0;
    property IconColor: TColor read FIconColor write SetIconColor default clBlack;
    property IconMargin: Byte read FIconMargin write SetIconMargin default 0;
    property IconSize: Integer read FIconSize write SetIconSize default 12;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    property ImageWidth: Integer read GetImageWidth write SetImageWidth default 0;
    property DefaultColor: TColor read FDefaultColor write SetDefaultColor default clDefault;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clDefault;
    property SelectedHoverColor: TColor read FSelectedHoverColor write SetSelectedHoverColor default clDefault;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
  end;

  { TRnButton }

  TRnButton = class(TRnCustomButton)
  published
    property Align;
    property AllowDown;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BidiMode;
    property Caption stored IsCaptionStored;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DefaultColor;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property Glyph;
    property GroupIndex;
    property IconChar;
    property IconColor;
    property IconMargin;
    property IconSize;
    property Images;
    property ImageIndex;
    property ImageWidth;
    property HoverColor;
    property Layout;
    property Margin;
    property NumGlyphs;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SelectedColor;
    property SelectedHoverColor;
    property ShowHint;
    property Spacing;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property Wordwrap;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
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
    property OnUnDock;
  end;

procedure Register;

implementation

uses Math;

procedure Register;
begin
  RegisterComponents('RnControls',[TRnButton]);
end;

{ TRnCustomButton }

function TRnCustomButton.GetGlyph: TBitmap;
begin
  Result := FButtonGlyph.Glyph;
end;

function TRnCustomButton.GetNumGlyphs: Integer;
begin
  Result := FButtonGlyph.NumGlyphs;
end;

function TRnCustomButton.GetImages: TCustomImageList;
begin
  Result := FButtonGlyph.ExternalImages;
end;

function TRnCustomButton.GetImageIndex: TImageIndex;
begin
  Result := FButtonGlyph.ExternalImageIndex;
end;

function TRnCustomButton.GetImageWidth: Integer;
begin
  Result := FButtonGlyph.ExternalImageWidth;
end;

procedure TRnCustomButton.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    GlyphChanged(Sender);
end;

function TRnCustomButton.IsGlyphStored: Boolean;
begin
  Result := (FButtonGlyph.Glyph <> nil) and (not FButtonGlyph.Glyph.Empty)
      and (FButtonGlyph.Glyph.Width > 0) and (FButtonGlyph.Glyph.Height > 0);
end;

procedure TRnCustomButton.SetHoverColor(AValue: TColor);
begin
  if FHoverColor <> AValue then
  begin
    FHoverColor := AValue;
    if csDesigning in ComponentState then
      Invalidate;
  end;
end;

procedure TRnCustomButton.SetDefaultColor(AValue: TColor);
begin
  if FDefaultColor <> AValue then
  begin
    FDefaultColor := AValue;
    Color := FDefaultColor;
    if csDesigning in ComponentState then
      Invalidate;
  end;
end;

procedure TRnCustomButton.SetSelectedColor(AValue: TColor);
begin
  if FSelectedColor <> AValue then
  begin
    FSelectedColor := AValue;
    if csDesigning in ComponentState then
      Invalidate;
  end;
end;

procedure TRnCustomButton.SetSelectedHoverColor(AValue: TColor);
begin
  if FSelectedHoverColor <> AValue then
  begin
    FSelectedHoverColor := AValue;
    if csDesigning in ComponentState then
      Invalidate;
  end;
end;

procedure TRnCustomButton.SetAllowDown(AValue: Boolean);
begin
  if FAllowDown <> AValue then
    FAllowDown := AValue;
end;

procedure TRnCustomButton.SetIconChar(AValue: Word);
var
  Icon: TUTF16Conv;
begin
  if FIconChar = AValue then Exit;
  FIconChar := AValue;
  Icon.W := Word(FIconChar);

  FButtonGlyph.Glyph := nil;
  FButtonGlyph.Glyph := CreateFAIcon(Icon, FIconColor, FIconSize, FIconMargin);
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TRnCustomButton.SetIconColor(AValue: TColor);
var
  Icon: TUTF16Conv;
begin
  if FIconColor = AValue then Exit;
  FIconColor := AValue;
  Icon.W := Word(FIconChar);

  if FIconChar <> 0 then
  begin
    FButtonGlyph.Glyph := nil;
    FButtonGlyph.Glyph := CreateFAIcon(Icon, FIconColor, FIconSize, FIconMargin);
    InvalidatePreferredSize;
    AdjustSize;
  end;
end;

procedure TRnCustomButton.SetIconSize(AValue: Integer);
var
  Icon: TUTF16Conv;
begin
  if FIconSize = AValue then Exit;
  FIconSize := AValue;
  Icon.W := Word(FIconChar);

  if FIconChar <> 0 then
  begin
    FButtonGlyph.Glyph := nil;
    FButtonGlyph.Glyph := CreateFAIcon(Icon, FIconColor, FIconSize, FIconMargin);
    InvalidatePreferredSize;
    AdjustSize;
  end;
end;

procedure TRnCustomButton.SetIconMargin(AValue: Byte);
var
  Icon: TUTF16Conv;
begin
  if FIconMargin = AValue then Exit;
  FIconMargin := AValue;
  Icon.W := Word(FIconChar);

  if FIconChar <> 0 then
  begin
    FButtonGlyph.Glyph := nil;
    FButtonGlyph.Glyph := CreateFAIcon(Icon, FIconColor, FIconSize, FIconMargin);
    InvalidatePreferredSize;
    AdjustSize;
  end;
end;

procedure TRnCustomButton.SetLayout(AValue: TButtonLayout);
begin
  if FLayout = AValue then Exit;
  FLayout := AValue;
  AdjustSize;
end;

procedure TRnCustomButton.SetGlyph(AValue: TBitmap);
begin
  FButtonGlyph.Glyph := AValue;
  InvalidatePreferredSize;
  AdjustSize;

  IconChar := 0;
end;

procedure TRnCustomButton.SetImages(const AImages: TCustomImageList);
begin
  if FButtonGlyph.ExternalImages <> nil then
  begin
    FButtonGlyph.ExternalImages.UnRegisterChanges(FImageChangeLink);
    FButtonGlyph.ExternalImages.RemoveFreeNotification(Self);
  end;
  FButtonGlyph.ExternalImages := AImages;
  if FButtonGlyph.ExternalImages <> nil then
  begin
    FButtonGlyph.ExternalImages.FreeNotification(Self);
    FButtonGlyph.ExternalImages.RegisterChanges(FImageChangeLink);
  end;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TRnCustomButton.SetImageIndex(const AImageIndex: TImageIndex);
begin
  FButtonGlyph.ExternalImageIndex := AImageIndex;
end;

procedure TRnCustomButton.SetImageWidth(const AImageWidth: Integer);
begin
  FButtonGlyph.ExternalImageWidth := aImageWidth;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TRnCustomButton.SetMargin(const AValue: Integer);
begin
  if FMargin = AValue then Exit;
  FMargin := AValue;
  AdjustSize;
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TRnCustomButton.SetNumGlyphs(AValue: Integer);
begin
  if AValue < Low(TNumGlyphs) then AValue := Low(TNumGlyphs);
  if AValue > High(TNumGlyphs) then AValue := High(TNumGlyphs);

  if AValue <> FButtonGlyph.NumGlyphs then
  Begin
    FButtonGlyph.NumGlyphs := TNumGlyphs(AValue);
    if csDesigning in ComponentState then
      Invalidate;
  end;
end;

procedure TRnCustomButton.SetSpacing(AValue: Integer);
begin
  if (FSpacing = AValue) or (AValue < -1) then Exit;
  FSpacing := AValue;
  AdjustSize;
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TRnCustomButton.SetDefaultCaption(const AValue: Boolean);
begin
  if FDefaultCaption = AValue then Exit;
  FDefaultCaption := AValue;
end;

procedure TRnCustomButton.MouseEnter(Sender: TObject);
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Sender);
end;

procedure TRnCustomButton.MouseLeave(Sender: TObject);
begin
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Sender);
end;

procedure TRnCustomButton.Click(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Sender);
end;

procedure TRnCustomButton.GlyphChanged(Sender: TObject);
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

function TRnCustomButton.IsCaptionStored: Boolean;
begin
  Result := inherited IsCaptionStored and not DefaultCaption;
end;

procedure TRnCustomButton.Loaded;
begin
  inherited Loaded;
end;

procedure TRnCustomButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FButtonGlyph <> nil) and (AComponent = FButtonGlyph.ExternalImages) then
    Images := nil;
end;

procedure TRnCustomButton.TextChanged;
begin
  inherited TextChanged;
  AdjustSize;
  FDefaultCaption := False;
end;

class function TRnCustomButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 30;
end;

procedure TRnCustomButton.CMAppShowBtnGlyphChanged(var Message: TLMessage);
begin
  FButtonGlyph.Refresh;
end;

procedure TRnCustomButton.Resize;
begin
  inherited Resize;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TRnCustomButton.Paint;
var
  ARect: TRect;
  TS : TTextStyle;

  GlyphWidth: Integer;
  GlyphHeight: Integer;

  CalcTextWidth: Integer;
  CalcTextHeight: Integer;

  CalcMargin: Integer;
begin
  ARect := GetClientRect;

  GlyphWidth := IfThen(FButtonGlyph.Glyph <> nil, FButtonGlyph.Glyph.Width);
  GlyphHeight := IfThen(FButtonGlyph.Glyph <> nil, FButtonGlyph.Glyph.Height);

  CalcTextWidth := GlyphWidth + IfThen(GlyphWidth > 0, FSpacing);
  CalcTextHeight := GlyphHeight + IfThen(GlyphHeight > 0, FSpacing);

  CalcMargin := IfThen(FMargin > -1, FMargin);

  Canvas.TextRect(ARect, ARect.Left, ARect.Top, '');

  {$region atribuir textstyle}
  TS := Canvas.TextStyle;
  TS.Layout := tlCenter;
  if FMargin = -1 then
  begin
    TS.Alignment := BidiFlipAlignment(taCenter, UseRightToLeftAlignment);
    ARect.Top := (Height div 2) - (Canvas.TextHeight(Caption) div 2);
    ARect.Bottom := (Height div 2) + (Canvas.TextHeight(Caption) div 2);
    ARect.Left := (Width div 2) - (Canvas.TextWidth(Caption) div 2);
    ARect.Right := (Width div 2) + (Canvas.TextWidth(Caption) div 2);
  end
  else
  if FLayout = blGlyphLeft then
    TS.Alignment := BidiFlipAlignment(taLeftJustify, UseRightToLeftAlignment)
  else
  if FLayout = blGlyphRight then
    TS.Alignment := BidiFlipAlignment(taRightJustify, UseRightToLeftAlignment)
  else
  begin
    TS.Alignment := BidiFlipAlignment(taCenter, UseRightToLeftAlignment);
    if FLayout = blGlyphTop then
      TS.Layout := tlTop
    else
      TS.Layout := tlBottom;
  end;

  if BiDiMode <> bdLeftToRight then
    TS.RightToLeft := True;
  TS.Opaque := False;
  TS.Clipping := False;
  TS.SystemFont := Canvas.Font.IsDefault;
  TS.Wordbreak := WordWrap;
  TS.SingleLine := not Wordwrap;
  {$endregion}

  if FMargin = -1 then
  begin
    case FLayout of
      blGlyphTop:
      begin
        ARect.Top := ARect.Top + (CalcTextWidth div 2);
        ARect.Bottom := ARect.Bottom + (CalcTextWidth div 2);
      end;
      blGlyphBottom:
      begin
        ARect.Top := ARect.Top - (CalcTextWidth div 2);
        ARect.Bottom := ARect.Bottom - (CalcTextWidth div 2);
      end;
      blGlyphLeft:
      begin
        ARect.Left := ARect.Left + (CalcTextHeight div 2);
        ARect.Right := ARect.Right + (CalcTextHeight div 2);
      end;
      blGlyphRight:
      begin
        ARect.Left := ARect.Left - (CalcTextHeight div 2);
        ARect.Right := ARect.Right - (CalcTextHeight div 2);
      end;
    end;
  end
  else
  begin
    case FLayout of
      blGlyphTop:
      begin
        ARect.Top := ARect.Top + CalcTextWidth + CalcMargin;
        ARect.Bottom := ARect.Bottom + CalcTextWidth + CalcMargin;
      end;
      blGlyphBottom:
      begin
        ARect.Top := ARect.Top - CalcTextWidth - CalcMargin;
        ARect.Bottom := ARect.Bottom - CalcTextWidth - CalcMargin;
      end;
      blGlyphLeft:
      begin
        ARect.Left := ARect.Left + CalcTextHeight + CalcMargin;
        ARect.Right := ARect.Right + CalcTextHeight + CalcMargin;
      end;
      blGlyphRight:
      begin
        ARect.Left := ARect.Left - CalcTextHeight - CalcMargin;
        ARect.Right := ARect.Right - CalcTextHeight - CalcMargin;
      end;
    end;
  end;

  if Caption <> '' then
  begin
    if not Enabled then
      if ThemeServices.ThemesEnabled then
        Canvas.Font.Color := clGrayText
      else
      begin
        Canvas.Font.Color := clBtnHighlight;
        OffsetRect(ARect, 1, 1);
        Canvas.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
        Canvas.Font.Color := clBtnShadow;
        OffsetRect(ARect, -1, -1);
      end
    else
      Canvas.Font.Color := Font.Color;

    Canvas.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
  end;

  if FButtonGlyph.Glyph <> nil then
  begin
    case FLayout of
      blGlyphTop: Canvas.Draw((Width div 2) - (GlyphWidth div 2), ARect.Top - FSpacing - GlyphWidth, FButtonGlyph.Glyph);
      blGlyphBottom: Canvas.Draw((Width div 2) - (GlyphWidth div 2), ARect.Bottom + FSpacing, FButtonGlyph.Glyph);
      blGlyphLeft: Canvas.Draw(ARect.Left - FSpacing - GlyphHeight, (Height div 2) - (GlyphHeight div 2), FButtonGlyph.Glyph);
      blGlyphRight: Canvas.Draw(ARect.Right + FSpacing, (Height div 2) - (GlyphHeight div 2), FButtonGlyph.Glyph);
    end;

//    if Caption = '' then
//    begin
//      Self.Width := FButtonGlyph.Glyph.Width;
//      Self.Height := FButtonGlyph.Glyph.Height;
//    end;
  end;

  if Assigned(OnPaint) then OnPaint(Self);
end;

procedure TRnCustomButton.Click;
var
  I: Integer;
begin
  if FAllowDown then
  begin
    SetIsSelected(True);

    Self.Color := FSelectedHoverColor;

    for I := 0 to Pred(Parent.ControlCount) do
    begin
      if (Parent.Controls[I] is TRnCustomButton) and
         (TRnCustomButton(Parent.Controls[I]).GroupIndex > 0) then
      begin
        if (TRnCustomButton(Parent.Controls[I]).GroupIndex = Self.GroupIndex) and
           (TRnCustomButton(Parent.Controls[I]).Name <> Self.Name) then
          TRnCustomButton(Parent.Controls[I]).SetIsSelected(False);
      end;
    end;
  end;

  inherited Click;
end;

procedure TRnCustomButton.MouseEnter;
begin
  if FIsSelected then
    Self.Color := FSelectedHoverColor
  else
    Self.Color := FHoverColor;

  inherited MouseEnter;
end;

procedure TRnCustomButton.MouseLeave;
begin
  if FIsSelected then
    Self.Color := FSelectedColor
  else
    Self.Color := FDefaultColor;

  inherited MouseLeave;
end;

procedure TRnCustomButton.ResetColor;
begin
  SetIsSelected(False);
end;

procedure TRnCustomButton.ResetColors;
var
  I: Integer;
begin
  for I := 0 to Pred(Parent.ControlCount) do
  begin
    if (Parent.Controls[I] is TRnCustomButton) then
    begin
      if (TRnCustomButton(Parent.Controls[I]).GroupIndex = Self.GroupIndex) then
        TRnCustomButton(Parent.Controls[I]).SetIsSelected(False);
    end;
  end;
end;

procedure TRnCustomButton.LoadGlyphFromResourceName(Instance: THandle;
  const AName: String);
begin
  Buttons.LoadGlyphFromResourceName(FButtonGlyph, Instance, AName);
end;

procedure TRnCustomButton.LoadGlyphFromLazarusResource(const AName: String);
begin
  Buttons.LoadGlyphFromLazarusResource(FButtonGlyph, AName);
end;

procedure TRnCustomButton.LoadGlyphFromStock(AIdButton: Integer);
begin
  Buttons.LoadGlyphFromStock(FButtonGlyph, AIdButton);
end;

procedure TRnCustomButton.SetIsSelected(AValue: Boolean);
begin
  FIsSelected := AValue;
  if FIsSelected then
    Self.Color := FSelectedHoverColor
  else
    Self.Color := FDefaultColor;
end;

constructor TRnCustomButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  BevelOuter := bvNone;
  Color := clDefault;

  FDefaultCaption := False;
  FLayout := blGlyphLeft;
  FMargin := -1;
  FSpacing := 4;
  FAllowDown := False;

  FIconChar := 0;
  FIconColor := clBlack;
  FIconMargin := 0;
  FIconSize := 12;

  FGroupIndex := 0;
  FHoverColor := clDefault;
  FDefaultColor := clDefault;
  FSelectedColor := clDefault;
  FSelectedHoverColor := clDefault;

  FButtonGlyph := TButtonGlyph.Create;
  FButtonGlyph.NumGlyphs := 1;
  FButtonGlyph.Glyph.OnChange := @GlyphChanged;
  FButtonGlyph.IsDesigning := csDesigning in ComponentState;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
end;

destructor TRnCustomButton.Destroy;
begin
  FreeThenNil(FButtonGlyph);
  FreeAndNil(FImageChangeLink);

  inherited Destroy;
end;

end.
