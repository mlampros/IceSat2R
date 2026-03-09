# Get the data based on the API URL

Get the data based on the API URL

## Usage

``` r
get_URL_data(
  URL,
  outputFormat = "csv",
  download_method = "curl",
  file_path_zip = NULL,
  verbose = FALSE
)
```

## Arguments

- URL:

  a character string specifying the API command

- outputFormat:

  a character string specifying the output format of the downloaded
  data. One of 'csv', 'json' or 'zip'

- download_method:

  a character string specifying the download method to use. Can be one
  of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or
  'auto'. For more information see the documentation of the
  'utils::download.file()' function

- file_path_zip:

  either NULL or a character string specifying a valid path to the
  output .zip file. This parameter will normally be a valid path if the
  'outputFormat' parameter is set to 'zip'. If it's NULL and the
  'outputFormat' parameter is 'zip' then the downloaded '.zip' file will
  be converted and returned as a data.table object

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

if the 'file_path_zip' parameter is NULL it returns either a data.table
(if outputFormat is 'csv') or a nested list (if outputFormat is 'json')
else the file path where the .zip file is saved. In case that the
'outputFormat' is set to 'zip' and the 'file_path_zip' parameter to NULL
then a data.table will be returned.
