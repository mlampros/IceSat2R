# Customized function to download files

Customized function to download files

## Usage

``` r
download_file(url, destfile, download_method, verbose = FALSE)
```

## Arguments

- url:

  a character string specifying a valid url

- destfile:

  a character string specifying a valid path where the output file will
  be saved

- download_method:

  a character string specifying the download method to use. Can be one
  of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or
  'auto'. For more information see the documentation of the
  'utils::download.file()' function

- verbose:

  a boolean. If TRUE then information will appear in the console

## References

https://github.com/mlverse/torchdatasets/blob/master/R/utils.R#L20

## Examples

``` r
if (FALSE) { # \dontrun{

require(IceSat2R)

# the default timeout is 60 and we set it to 600
options(timeout = 600)

# we specify a URL and a temporary file
default_url = "https://icesat-2.gsfc.nasa.gov/sites/default/"
url_downl = glue::glue('{default_url}files/page_files/IS2RGTscycle12datetime.zip')
tmp_f = tempfile(fileext = '.zip')

# then we download the file
downl_f = download_file(url = url_downl,
                        destfile = tmp_f, 
                        download_method = 'curl',
                        verbose = TRUE)
} # }
```
