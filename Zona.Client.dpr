program Zona.Client;

uses
  System.StartUpCopy,
  FMX.Forms,
  ZonaClient.UI in 'ZonaClient.UI.pas' {Form2},
  ZonaAPI.FilterProcessor in 'ZonaAPI.FilterProcessor.pas',
  ZonaAPI in 'ZonaAPI.pas',
  ZonaAPI.Types in 'ZonaAPI.Types.pas',
  FMX.ListView in 'FMX.ListView.pas',
  ZonaClient.UI.Filter in 'ZonaClient.UI.Filter.pas' {Form7},
  ZonaClient.UI.ItemView in 'ZonaClient.UI.ItemView.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
