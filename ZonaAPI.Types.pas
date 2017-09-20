unit ZonaAPI.Types;

interface

uses
  DJSON.Attributes,
  System.Generics.Collections;

type
  TznItem = class
  public
    cover: string;
    id: Integer;
    mobi_link_id: Integer;
    name_eng: string;
    name_id: string;
    name_rus: string;
    rating: Single;
    rating_imdb_count: single;
    rating_kinopoisk: Single;
    rating_kinopoisk_count: Integer;
    serial: Boolean;
    serial_ended: Boolean;
    serial_end_year: Integer;
    year: Integer;
  end;

  TznPagination = class
  public
    current_page: Integer;
    limit: Integer;
    offset: Integer;
    total_items: Integer;
    total_pages: Integer;
    prev_url: string;
    next_url: string;
  end;

  TznCategory = class
  public
    [djName('items')]
    Items: TArray<TznItem>;
    pagination: TznPagination;
    title_h1: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation


{ TznCategory }

constructor TznCategory.Create;
begin
 // Items := TObjectList<TznItem>.Create;
  pagination := TznPagination.Create;
end;

destructor TznCategory.Destroy;
begin
//  Items.Free;
  pagination.Free;
  inherited;
end;

end.

