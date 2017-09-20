unit ZonaAPI;

interface

uses
  ZonaAPI.Types,
  DJSON,
  System.Net.HttpClient,
  DJSON.Params;

type
  TznGenres = ();

  TznFilter = class
  end;

  TznCatalog = class
  end;

  TZonaAPI = class
  private
    function SetupDjson: IdjParams;
    function ZonaAPI(const AUrl: string): TznCategory;
    procedure SetupHttp(AHttp: THTTPClient);
  public
    function GetSeries: TznCategory;
  end;

implementation

{ TZonaAPI }

function TZonaAPI.GetSeries: TznCategory;
begin
  Result := ZonaAPI('https://zona.mobi/tvseries');
end;

function TZonaAPI.SetupDjson: IdjParams;
begin
  Result := dj.DefaultByFields;
  Result.Engine := TdjEngine.eJDO;
end;

procedure TZonaAPI.SetupHttp(AHttp: THTTPClient);
begin
  AHttp.Accept := 'application/json, text/javascript, */*; q=0.01';
  AHttp.CustomHeaders['Upgrade-Insecure-Requests'] := '1';
  AHttp.CustomHeaders['X-Requested-With'] := 'XMLHttpRequest';
  AHttp.UserAgent := 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.91 Safari/537.36 Vivaldi/1.93.955.36';
end;

function TZonaAPI.ZonaAPI(const AUrl: string): TznCategory;
var
  LHttp: THTTPClient;
  LResponse: IHTTPResponse;
begin
  LHttp := THTTPClient.Create;
  SetupHttp(LHttp);
  try
    LResponse := LHttp.Get(AUrl);
    Result := dj.FromJson(LResponse.ContentAsString(), SetupDjson).&To < TznCategory > ;
  finally
    LHttp.Free;
  end;
end;

end.

