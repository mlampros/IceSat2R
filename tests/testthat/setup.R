
#..............................
# Parameter setting (and Url's)
#..............................

minx = 140
miny = -6.641235
maxx = 145
maxy = -1.641235

date = "2021-02-11"
date_start = '2020-01-01'
date_end = '2020-01-01'
trackId = 756
beamName = 'gt1l'
product = 'atl08'
outputFormat = 'csv'
client = 'portal'
sampling = FALSE
sample_rgts = c("24", "32")

INVALID_URL = "https://openaltimetry.org/INVALID/"
VALID_URL_a3 = glue::glue("https://openaltimetry.org/data/api/icesat2/{product}?minx={minx}&miny={miny}&maxx={maxx}&maxy={maxy}&trackId={trackId}&beamName={beamName}&outputFormat={outputFormat}&date={date}&client={client}&sampling={tolower(sampling)}")
VALID_URL_remaining = glue::glue("https://openaltimetry.org/data/api/icesat2/{product}?minx={minx}&miny={miny}&maxx={maxx}&maxy={maxy}&trackId={trackId}&outputFormat={outputFormat}&date={date}&client={client}&beamName={beamName}")
ORBITS_URL = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/arcticallorbits.zip'
WKT = 'POLYGON ((-14.765 18.979, -11.25 18.979, -11.25 21.943, -14.765 21.943, -14.765 18.979))'
