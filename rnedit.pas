unit RnEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls, Messages, ExtCtrls,
  RnButton;

type
  TStringEvent = procedure(Sender: TObject; var s: String) of object;

  TInputType = (itDefault, itDate, itPhone);

type

  { TRnCustomEdit }

  TRnCustomEdit = class(TCustomEdit)
  private
    FShape: TShape;
    FLine: TShape;
    FOnBeforePaste: TStringEvent;
    FOnAfterPaste: TNotifyEvent;
    FCanModify: Boolean;
    FColor: TColor;
    FColorOnFocus: TColor;
    FRnButton: TRnButton;
    FInputType: TInputType;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure SetRnButton(Value: TRnButton);
    procedure KeyDownCtrl(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyUpCtrl(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormatTextDate(Sender: TObject; Key: Word);
    function FormatTextDate(Str: String): String;
    procedure FormatTextPhone(Sender: TObject; Key: Word);
    function FormatTextPhone(Str: String): String;
  protected
    procedure InitializeWnd; override;
    procedure Resize; override;
    procedure SetColor(Value: TColor); override;
    procedure SetColorOnFocus(Value: TColor);
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property CanModify: Boolean read FCanModify write FCanModify default True;
    property ColorOnFocus: TColor read FColorOnFocus write FColorOnFocus default clDefault;
    property InputType: TInputType read FInputType write FInputType default itDefault;
    property OnBeforePaste: TStringEvent read FOnBeforePaste write FOnBeforePaste;
    property OnAfterPaste: TNotifyEvent read FOnAfterPaste write FOnAfterPaste;
    property RnButton: TRnButton read FRnButton write SetRnButton;
  end;

  { TRnEdit }

  TRnEdit = class(TRnCustomEdit)
  public
    property AutoSelected;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CanModify;
    property CharCase;
    property Color;
    property ColorOnFocus;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property InputType;
    property MaxLength;
    property NumbersOnly;
    property ParentBidiMode;
    property OnAfterPaste;
    property OnBeforePaste;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property RnButton;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Text;
    property TextHint;
    property Visible;
  end;

procedure Register;

implementation

uses
  Clipbrd, Math, windows;

procedure Register;
begin
  RegisterComponents('RnControls',[TRnEdit]);
end;

{ TRnEdit }

procedure TRnCustomEdit.WMPaste(var Message: TMessage);
var
  strClpText: String;
begin
  if not FCanModify then
    Exit;

  // Check if the OnBeforePaste event is assigned and check if the clipboard contains plain text.
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    // Save the clipboard text into a local variable
    strClpText := ClipBoard.AsText;
    // Fire the OnBeforePaste event

    if FInputType = itDefault then
    begin
      if Assigned(FOnBeforePaste) then
        FOnBeforePaste(Self, strClpText);
    end
    else
    if FInputType = itDate then
      strClpText := FormatTextDate(strClpText)
    else
      strClpText := FormatTextPhone(strClpText);

    // Clear the clipboard and replace it with the new text
    Clipboard.Clear;
    Clipboard.AsText := strClpText;
  end;

  // Perform the actual paste
  inherited;

  // if the OnAfterPaste event is assigned, fire it.
  if Assigned(FOnAfterPaste) then
    FOnAfterPaste(Self);
end;

procedure TRnCustomEdit.SetRnButton(Value: TRnButton);
begin
  FRnButton := Value;
  if Assigned(FRnButton) and Assigned(FRnButton.Glyph) then
  begin
    FRnButton.DefaultColor := Color;
    FRnButton.HoverColor := Color;

    FRnButton.BringToFront;
  end;
end;

procedure TRnCustomEdit.KeyDownCtrl(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Sender, Key, Shift);
end;

procedure TRnCustomEdit.KeyUpCtrl(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Sender, Key, Shift);
end;

procedure TRnCustomEdit.FormatTextDate(Sender: TObject; Key: Word);
var
  T, E: String;
  I, S: Integer;
begin
  T := TRnCustomEdit(Sender).Text;
  S := TRnCustomEdit(Sender).SelStart;
  //L := TRnCustomEdit(Sender).SelLength;

  if (S = Length(T)) and (S > 0) and ((Length(T) = 2) or (Length(T) = 5)) and (Key = 8) then
    Delete(T, Length(T), 1);

  T := StringReplace(T, '/', '', [rfReplaceAll]);

  E := '';
  for I := 1 to Length(T) do
  begin
    E := E + T[I];
    if (Length(E) = 2) or (Length(E) = 5) then
      E := E + '/';
  end;

  TRnCustomEdit(Sender).Text := E;
  TRnCustomEdit(Sender).SelStart := Length(E);
end;

function TRnCustomEdit.FormatTextDate(Str: String): String;
begin
  Result := StringReplace(Str, '/', '', [rfReplaceAll]);

  if Length(Result) <> 8 then
    Result := String.Empty
  else
  try
    Result := FormatDateTime('dd/MM/yyyy', EncodeDate(
      StrToInt(Copy(Result, 5, 4)),
      StrToInt(Copy(Result, 3, 2)),
      StrToInt(Copy(Result, 1, 2))));
  except
    Result := String.Empty;
  end;
end;

procedure TRnCustomEdit.FormatTextPhone(Sender: TObject; Key: Word);
var
  T, E: String;
  I, S: Integer;
begin
  T := TEdit(Sender).Text;

  S := TEdit(Sender).SelStart;
  //L := TEdit(Sender).SelLength;

  if (S = Length(T)) and (S > 0) and (Key = 8) then
    if Length(T) = 4 then
      Delete(T, Length(T)-1, 2)
    else
    if Length(T) = 9 then
      Delete(T, Length(T), 1);

  T := StringReplace(T, '(', '', [rfReplaceAll]);
  T := StringReplace(T, ')', '', [rfReplaceAll]);
  T := StringReplace(T, '-', '', [rfReplaceAll]);
  T := StringReplace(T, ' ', '', [rfReplaceAll]);

  E := '';
  for I := 1 to Length(T) do
  begin
    if I = 1 then
      E := E + '(';
    E := E + T[I];
    if I = 2 then
      E := E + ') ';
    if I = 6 then
      E := E + '-';
    if I = 11 then
    begin
      Delete(E, 10, 1);
      Insert('-', E, 11);
    end;
  end;

  TEdit(Sender).Text := E;
  TEdit(Sender).SelStart := Length(E);
end;

function TRnCustomEdit.FormatTextPhone(Str: String): String;
var
  E: String;
  I: Integer;
begin
  Result := Str;

  Result := StringReplace(Result, '(', '', [rfReplaceAll]);
  Result := StringReplace(Result, ')', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
  Result := StringReplace(Result, ' ', '', [rfReplaceAll]);

  if StrToFloatDef(Result, 0) = 0 then
    Result := ''
  else
  if (Length(Result) < 10) or (Length(Result) > 11) then
    Result := ''
  else
  begin
    E := '';
    for I := 1 to Length(Result) do
    begin
      if I = 1 then
        E := E + '(';
      E := E + Result[I];
      if I = 2 then
        E := E + ') ';
      if I = 6 then
        E := E + '-';
      if I = 11 then
      begin
        Delete(E, 10, 1);
        Insert('-', E, 11);
      end;
    end;
    Result := E;
  end;
end;

procedure TRnCustomEdit.InitializeWnd;
begin
  inherited InitializeWnd;

  FShape := TShape.Create(Self);
  FShape.Brush.Color := Self.Color;
  FShape.Pen.Style := psClear;
  Parent.InsertControl(FShape);

  FLine := TShape.Create(Self);
  FLine.Height := 1;
  FLine.Pen.Color := clSilver;
  Parent.InsertControl(FLine);
end;

procedure TRnCustomEdit.Resize;
begin
  inherited Resize;

  if Assigned(FShape) then
  begin
    FShape.Top := Self.Top - 5;
    FShape.Height := Self.Height + 10;
    FShape.Left := Self.Left - 5;
    FShape.Width := Self.Width + 10;

    FLine.Left := FShape.Left;
    FLine.Top := FShape.Top + FShape.Height - 2;
    FLine.Width := FShape.Width;
  end;
end;

procedure TRnCustomEdit.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  if Assigned(FShape) then
    FShape.Brush.Color := Value;
end;

procedure TRnCustomEdit.SetColorOnFocus(Value: TColor);
begin
  if FColorOnFocus = Value then
    FColorOnFocus := Value;
end;

procedure TRnCustomEdit.DoEnter;
begin
  inherited DoEnter;

  FColor := Self.Color;
  Color := FColorOnFocus;

  if Assigned(FRnButton) then
  begin
    FRnButton.DefaultColor := Color;
    FRnButton.HoverColor := Color;
  end;
end;

procedure TRnCustomEdit.DoExit;
begin
  Self.Color := FColor;

  inherited DoExit;

  if Assigned(FRnButton) then
  begin
    FRnButton.DefaultColor := Color;
    FRnButton.HoverColor := Color;
  end;
end;

procedure TRnCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  T, N, E: String;
  I, S, L: Integer;
  IsValidKey: Boolean;
begin
  if FCanModify then
  begin
    if FInputType = itDefault then
    begin
      inherited KeyDown(Key, Shift);
      Exit;
    end;

    if (Shift = []) and (Key in [48..57, VK_NUMPAD0..VK_NUMPAD9]) then
    begin
      T := TRnCustomEdit(Self).Text;
      if (FInputType = itDate) and (Length(T) = 10) and (TRnCustomEdit(Self).SelLength <> 10) then
      begin
        Key := 0;
        Exit;
      end
      else
      if (FInputType = itPhone) and (Length(T) = 15) and (TRnCustomEdit(Self).SelLength <> 15) then
      begin
        Key := 0;
        Exit;
      end;

      case Key of
        VK_NUMPAD0: Key := 48;
        VK_NUMPAD1: Key := 49;
        VK_NUMPAD2: Key := 50;
        VK_NUMPAD3: Key := 51;
        VK_NUMPAD4: Key := 52;
        VK_NUMPAD5: Key := 53;
        VK_NUMPAD6: Key := 54;
        VK_NUMPAD7: Key := 55;
        VK_NUMPAD8: Key := 56;
        VK_NUMPAD9: Key := 57;
      end;

      S := TRnCustomEdit(Self).SelStart;
      L := TRnCustomEdit(Self).SelLength;
      N := Copy(T, 1, S) + Chr(Key) + Copy(T, S+L+1, Length(T)-(S+L));

      if FInputType = itDate then
      begin
        N := StringReplace(N, '/', '', [rfReplaceAll]);

        E := '';
        for I := 1 to Length(N) do
        begin
          E := E + N[I];
          if (Length(E) = 2) or (Length(E) = 5) then
            E := E + '/';
        end;
      end
      else
      if FInputType = itPhone then
      begin
        N := StringReplace(N, '(', '', [rfReplaceAll]);
        N := StringReplace(N, ')', '', [rfReplaceAll]);
        N := StringReplace(N, '-', '', [rfReplaceAll]);
        N := StringReplace(N, ' ', '', [rfReplaceAll]);

        E := '';
        for I := 1 to Length(N) do
        begin
          if I = 1 then
            E := E + '(';
          E := E + N[I];
          if I = 2 then
            E := E + ') ';
          if I = 6 then
            E := E + '-';
          if I = 11 then
          begin
            Delete(E, 10, 1);
            Insert('-', E, 11);
          end;
        end;
      end;

      TRnCustomEdit(Self).Text := E;
      TRnCustomEdit(Self).SelStart := Length(E);

      Key := 0;
    end
    else
    begin
      IsValidKey := False;
      if Key in [8,9,13,16,17,18,19,20,33,34,35,36,37,38,39,40,45,46,67,112,113,114,115,116,117,118,119,120,121,122,123,144,145] then
        IsValidKey := True;
      if (Key = 67) and (Shift = [ssCtrl]) then
        IsValidKey := True;
      if (Key = 86) and (Shift = [ssCtrl]) then
        IsValidKey := True;
      if (Key = 88) and (Shift = [ssCtrl]) then
        IsValidKey := True;

      if not IsValidKey then
        Key := 0;
    end;

    inherited KeyDown(Key, Shift);
  end
  else
  begin
    IsValidKey := False;

    if Key in [8,9,13,16,17,18,19,20,33,34,35,36,37,38,39,40,45,46,67,112,113,114,115,116,117,118,119,120,121,122,123,144,145] then
      IsValidKey := True;
    if (Key = 67) and (Shift = [ssCtrl]) then
      IsValidKey := True;

    inherited KeyDown(Key, Shift);

    if (not IsValidKey) or (Key in [8, 46]) then
      Key := 0;
  end;
end;

procedure TRnCustomEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if FCanModify then
  begin
    if FInputType = itDefault then
      inherited KeyUp(Key, Shift)
    else
    begin
      if Key in [8, 46] then //Backspace, Delete
      begin
        if FInputType = itDate then
          FormatTextDate(Self, Key)
        else
        if FInputType = itPhone then
          FormatTextPhone(Self, Key);
      end;
    end;
  end;
end;

constructor TRnCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;

  Color := clWhite;
  FColorOnFocus := clWhite;
  FCanModify := True;
  FInputType := itDefault;
end;

destructor TRnCustomEdit.Destroy;
begin
  FreeAndNil(FShape);
  FreeAndNil(FLine);

  inherited Destroy;
end;

end.
