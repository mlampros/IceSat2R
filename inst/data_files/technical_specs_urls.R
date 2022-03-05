
#...............................................
# R script using Github Actions on per day basis
# to update the 'technical_specs_urls.csv' file
#...............................................

pth_orb = file.path('inst', 'data_files', 'technical_specs_urls.csv')
upd_orbs = IceSat2R::latest_orbits(verbose = TRUE)
data.table::fwrite(x = upd_orbs, file = pth_orb, row.names = F)
