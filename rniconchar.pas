unit RnIconChar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LazFreeTypeIntfDrawer, EasyLazFreeType,
  IntfGraphics, Forms, fpimage, GraphType, LazFreeTypeFontCollection;

type
  IconException = class(Exception);

  // retirado de : https://forum.lazarus.freepascal.org/index.php?topic=37887.0
  // em: 22/08/2022
  TUTF16Conv = record
  case Byte of
    0: (C: UnicodeChar);
    1: (W: Word);
  end;

function CreateFAIcon(AIcon: TUTF16Conv; AColor: TColor = clBlack;
  ASize: Byte = 12; const AMargin: Byte = 0): TBitmap;

implementation

uses Math;

function CreateFAIcon(AIcon: TUTF16Conv; AColor: TColor = clBlack;
  ASize: Byte = 12; const AMargin: Byte = 0): TBitmap;
var
  img: TLazIntfImage;
  d: TIntfFreeTypeDrawer;
  f: TFreeTypeFont;
  h, w: Single;
  RMode: TFPURoundingMode;
begin
  Result := TBitmap.Create;

  f := TFreeTypeFont.Create;
  try
    f.Name := Application.Location + 'fa-brands-400.ttf';
    if f.CharIndex[AIcon.W] = 0 then
    begin
      f.Name := Application.Location + 'fa-regular-400.ttf';
      if f.CharIndex[AIcon.W] = 0 then
      begin
        f.Name := Application.Location + 'fa-solid-900.ttf';
        if f.CharIndex[AIcon.W] = 0 then
          raise IconException.Create(IntToStr(AIcon.W) + ' is not a valid IconChar');
      end;
    end;
    f.SizeInPixels := ASize;
    f.Hinted := True;
    f.ClearType := True;
    f.Quality := grqHighQuality;
    f.SmallLinePadding := False;

    RMode := GetRoundMode;

    w := f.TextWidth(AnsiString(AIcon.C));

    if (w - Round(w)) > 0 then
    begin
      SetRoundMode(rmUp);
      w := Round(w);
    end;

    SetRoundMode(RMode);

    h := f.TextHeight(AnsiString(AIcon.C));
    if (h - Round(h)) > 0 then
    begin
      SetRoundMode(rmUp);
      h := Round(h);
    end;

    SetRoundMode(RMode);

    img := TLazIntfImage.Create(Trunc(w) + AMargin, Trunc(h), [riqfRGB, riqfAlpha]);
    try
      d := TIntfFreeTypeDrawer.Create(img);
      try
        d.FillPixels(colTransparent);
        d.DrawTextRect(AnsiString(AIcon.C), f, AMargin, 0, Trunc(w) + AMargin, Trunc(h), TColorToFPColor(AColor), []);
      finally
        d.Free;
      end;
      Result.LoadFromIntfImage(img);
    finally
      img.Free;
    end;
  finally
    f.Free;
  end;
end;

end.

