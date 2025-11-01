# CRT-Dipumba
![R Project CI](https://github.com/nickvanreet/CRT-Dipumba/actions/workflows/ci.yml/badge.svg)

## GRID3 gezondheidszones offline gebruiken

De Shiny-app zoekt nu automatisch naar een lokaal GRID3-bestand voordat er een poging
wordt gedaan om de (gedeeltelijk afgeschermde) ArcGIS-endpoints aan te spreken. Plaats
een GeoPackage, RDS of GeoJSON met de gezondheidszones in een van de volgende locaties en
de kaart wordt zonder internetverbinding geladen:

- `data/grid3_health_zones.rds`
- `data/grid3_health_zones.gpkg`
- `data/grid3_health_zones.geojson`
- `www/grid3_health_zones.rds`
- `www/grid3_health_zones.gpkg`
- `www/grid3_health_zones.geojson`

Gebruik bij voorkeur de meest recente GRID3-export en zorg dat de kolommen
`province`, `zonesante` en `zs_uid` aanwezig zijn. Een RDS-bestand moet een `sf`-object
bevatten (zoals de output van `saveRDS(hz, "data/grid3_health_zones.rds")`). Een
alternatief pad kan ook worden opgegeven via de omgevingsvariabele
`GRID3_HEALTH_ZONES_FILE`, bijvoorbeeld:

```r
Sys.setenv(GRID3_HEALTH_ZONES_FILE = "C:/Users/nvanreet/OneDrive - ITG/Documenten/GitHub/data/grid3_health_zones.rds")
```
