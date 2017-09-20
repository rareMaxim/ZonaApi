unit ZonaClient.UI.Filter;

interface

uses
  ZonaAPI.FilterProcessor, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm7 = class(TForm)
    grp1: TGroupBox;
    cbbGenre: TComboBox;
    procedure cbbGenreChange(Sender: TObject);
  private
    FFilter: TznFilterProcessor;
    procedure SetFilter(const Value: TznFilterProcessor);
    { Private declarations }
  public
    { Public declarations }
    property Filter: TznFilterProcessor read FFilter write SetFilter;
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

procedure TForm7.cbbGenreChange(Sender: TObject);
begin
  Filter.Genre := cbbGenre.Selected.Text;
end;

procedure TForm7.SetFilter(const Value: TznFilterProcessor);
var
  I: Integer;
begin
  FFilter := Value;
  cbbGenre.BeginUpdate;
  try
    for I := 0 to Value.Genres.Count - 1 do
      cbbGenre.Items.Add(Value.Genres.Names[I]);
  finally
    cbbGenre.EndUpdate;
  end;
end;

end.

