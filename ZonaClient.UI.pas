unit ZonaClient.UI;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Layouts, FMX.Controls.Presentation, FMX.MultiView, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, ZonaAPI, ZonaAPI.Types;

type
  TForm2 = class(TForm)
    MultiView1: TMultiView;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListView1: TListView;
    stylbk1: TStyleBook;
    lbi1: TListBoxItem;
    procedure FormShow(Sender: TObject);
    procedure ListView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure OnColumnClick(const Sender: TObject; const Column: Integer; const X, Y: Single; const AItem: TListViewItem; const DrawebleName: string);
    procedure ListBoxItem1Click(Sender: TObject);
    procedure ListView1UpdatingObjects(const Sender: TObject; const AItem: TListViewItem; var AHandled: Boolean);
    procedure lbi1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FZona: TZonaAPI;
    FCatalog: TznCategory;
    procedure ReLoadLV;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  ZonaClient.UI.Filter, System.Threading, System.Math, System.Net.HttpClient;

const
  colTitle = 'title';
  colBitmap = 'bitmap';
  colLogo = 'logo';
  colLoading = 'loading';
  ColumnStartIndex = 1;
{$R *.fmx}

function getRealIndex(const Row, Column, Columns: Integer): Integer;
begin
  Result := (((Row + 1) * Columns) - 1) - (Columns - Column);
end;

procedure LoadBitmapFromURL(const aURL: string; aBitmap: TBitmap);
// System.Net.HTTPClient
var
  LThread: TThread;
begin
  LThread := TThread.CreateAnonymousThread(
    procedure
    var
      Http: THTTPClient;
      Result: TMemoryStream;
    begin
      Result := TMemoryStream.Create;
      Http := THTTPClient.Create;
      try
        try
          Http.HandleRedirects := true;
          Http.Get(aURL, Result);
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            var
              aSourceBmp: TBitmap;
            begin
              aSourceBmp := TBitmap.Create;
              aSourceBmp.LoadFromStream(Result);
              if not aSourceBmp.IsEmpty then
              begin
                aBitmap.Clear(TAlphaColorRec.White);
                aBitmap.SetSize(aSourceBmp.Width, aSourceBmp.Height);
                aBitmap.CopyFromBitmap(aSourceBmp);
              end;
              FreeAndNil(aSourceBmp);
            end);
        except
          FreeAndNil(Result);
        end;
      finally
        FreeAndNil(Result);
        FreeAndNil(Http);
      end;
    end);
  LThread.FreeOnTerminate := true;
  LThread.start;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FZona := TZonaAPI.Create;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  ListView1.ColumnWidth := 100;
  ListView1.AutoColumns := true;
  ListView1.ItemAppearance.ItemHeight := 180;
  ListView1.StyleLookup := 'listviewstyle_panel';
  ListView1.OnColumnClick := OnColumnClick;
  ListView1.ShowScrollBar := false;
  ListView1.EnableTouchAnimation(false);
  TTask.Run(
    procedure
    begin
      FCatalog := FZona.GetSeries;
      TThread.Synchronize(nil,
        procedure
        begin
          Tag := 0;
          ReLoadLV;
          Caption := FCatalog.title_h1;
        end);
    end);

end;

procedure TForm2.lbi1Click(Sender: TObject);
var
  X: TForm7;
begin
  X := TForm7.Create(Self);
  X.Filter := FZona.Filter;
  X.Show;
end;

procedure TForm2.ListBoxItem1Click(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      FCatalog := FZona.GetSeries;
      TThread.Synchronize(nil,
        procedure
        begin
          Tag := 0;
          ReLoadLV;
        end);
    end);
end;

procedure TForm2.ListView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  I, J, RowColumns: Integer;
  iBitmap: TListItemImage;
  aFirst, aLast: Integer;
begin
  if ListView1.Items.Count <= 0 then
    exit;

  aFirst := Max(0, ListView1.getFirstVisibleItemIndex);
  aLast := aFirst + ListView1.getVisibleCount;

  for I := aFirst to aLast do
  begin
    if InRange(I, 0, ListView1.Items.Count - 1) then
    begin
      // Label1.Text := string.Join(':', [I, ListView1.getFirstVisibleItemIndex,
      // ListView1.getVisibleCount, ListView1.getLastVisibleItemindex]);

      RowColumns := ListView1.Items[I].Tag;
      for J := 1 to RowColumns do
      begin
        iBitmap := ListView1.Items[I].Objects.FindObjectT < TListItemImage > (colBitmap + IntToStr(J));

        if Assigned(iBitmap) then
        begin
          if Assigned(iBitmap.Bitmap) and (ListView1.Items[I].Data[colLoading + IntToStr(J)].AsInteger = 1) then
          begin
            ListView1.Items[I].Data[colLoading + IntToStr(J)] := 0;
            LoadBitmapFromURL(ListView1.Items[I].Data[colLogo + IntToStr(J)].AsString, iBitmap.Bitmap);
          end;
        end;
      end;
    end
    else
      break;
  end;
end;

procedure TForm2.ListView1UpdatingObjects(const Sender: TObject; const AItem: TListViewItem; var AHandled: Boolean);
var
  iTitle: TListItemText;
  iBitmap: TListItemImage;
  aPos: Single;
  I: Integer;
//  realIndex: Integer;
begin
  for I := 1 to ListView1.Columns do
  begin
    if not InRange(I, ColumnStartIndex, AItem.Tag) then
      continue;

    aPos := (((ListView1.ColumnWidth * I) - ListView1.ColumnWidth) - 6) + Trunc(ListView1.ColumnOffset * I);

    iBitmap := AItem.Objects.FindObjectT<TListItemImage>(colBitmap + IntToStr(I));
    if iBitmap = nil then
      iBitmap := TListItemImage.Create(AItem);
    iBitmap.Name := colBitmap + IntToStr(I);
    iBitmap.Width := ListView1.ColumnWidth - 8;
    iBitmap.Height := ListView1.ColumnWidth - 8;
    iBitmap.ScalingMode := TImageScalingMode.Stretch;
    iBitmap.PlaceOffset.X := aPos;
    iBitmap.PlaceOffset.Y := 4;
    iBitmap.OwnsBitmap := true;
    if (iBitmap.Bitmap = nil) then
      iBitmap.Bitmap := TBitmap.Create;

    // çàãîëîâîê
    iTitle := AItem.Objects.FindObjectT<TListItemText>(colTitle + IntToStr(I));
    if iTitle = nil then
      iTitle := TListItemText.Create(AItem);
    iTitle.Name := colTitle + IntToStr(I);
    iTitle.TextAlign := TTextAlign.Center;
    iTitle.TextVertAlign := TTextAlign.Center;
    iTitle.SelectedTextColor := TAlphaColorRec.Black;
    iTitle.TextColor := TAlphaColorRec.Black;
    iTitle.Font.Size := 14;
    iTitle.WordWrap := true;
    iTitle.Width := iBitmap.Width - 8;
    iTitle.PlaceOffset.X := aPos + 8;
    iTitle.PlaceOffset.Y := iBitmap.Height;
    iTitle.Height := ListView1.ItemAppearance.ItemHeight - iTitle.PlaceOffset.Y;
    iTitle.Text := AItem.Data[colTitle + IntToStr(I)].AsString;
  end;

  AHandled := true;

end;

procedure TForm2.OnColumnClick(const Sender: TObject; const Column: Integer; const X, Y: Single; const AItem: TListViewItem; const DrawebleName: string);
begin
  //
end;

procedure TForm2.ReLoadLV;
var
  J, realIndex, ColumnInRow, RowCount: Integer;
  AItem: TListViewItem;
  iBitmap: TListItemImage;
begin
  ListView1.BeginUpdate;
  try
    ListView1.OnPaint := nil;
    // AniIndicator1.Enabled := true;
    while ListView1.Items.Count > 0 do
    begin
      for J := 1 to ListView1.Items[0].Tag do
      begin
        iBitmap := ListView1.Items[0].Objects.FindObjectT < TListItemImage > (colBitmap + IntToStr(J));
        if (Assigned(iBitmap) and iBitmap.OwnsBitmap) then
        begin
          iBitmap.Bitmap.Free;
          iBitmap.Bitmap := nil;
        end;
      end;
      ListView1.Items.Delete(0);
    end;
    if Length(FCatalog.Items) < 0 then
      exit;
    RowCount := System.Math.Ceil(Length(FCatalog.Items) / ListView1.Columns);
    realIndex := -1;
    for J := 0 to RowCount - 1 do
    begin
      inc(realIndex);
      AItem := ListView1.Items.Add;
      with AItem do
      begin
        ColumnInRow := ColumnStartIndex;
        while realIndex < Length(FCatalog.Items) do
        begin
          Data[colTitle + IntToStr(ColumnInRow)] := FCatalog.Items[realIndex].name_rus;
          Data[colLogo + IntToStr(ColumnInRow)] := FCatalog.Items[realIndex].cover;
          Data[colLoading + IntToStr(ColumnInRow)] := 1;
          Tag := ColumnInRow;
          if ColumnInRow mod ListView1.Columns = 0 then
            break;
          inc(realIndex);
          inc(ColumnInRow);
        end;
      end;
      ListView1.Adapter.ResetView(AItem);
    end;
    // AniIndicator1.Enabled := false;
    ListView1.OnPaint := ListView1Paint;
  finally
    ListView1.EndUpdate;
    ListView1.Repaint;
  end;
end;

end.

