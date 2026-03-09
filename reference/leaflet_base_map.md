# a leaflet base map

a leaflet base map

## Usage

``` r
leaflet_base_map(
  sf_obj = NULL,
  leaflet_provider_base,
  leaflet_provider_secondary,
  overlay_group = "IceSat2 1-degree",
  overlay_polylines = "Polylines",
  width = NULL,
  height = NULL,
  popup = NULL,
  collapse_layer_control = TRUE
)
```

## Arguments

- sf_obj:

  a simple features object which must include the columns 'date' and
  'size_MB' if the popup parameter is set to TRUE

- leaflet_provider_base:

  a leaflet provider object

- leaflet_provider_secondary:

  a leaflet provider object

- overlay_group:

  a character string specifying the overlay group

- overlay_polylines:

  a character string specifying the Polylines group

- width:

  a numeric value specifying the width of the leaflet map

- height:

  a numeric value specifying the height of the leaflet map

- popup:

  a boolean specifying if the popup should be shown or not

- collapse_layer_control:

  a boolean specifying if the layer-control should be collapsed or not

## Value

a leaflet object (map)
