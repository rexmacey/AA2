
# Create data file in which to store the Excel ranges related to RAFI asset allocation file
rafi_xlranges <- list( return = "B4:L32",
                    corr = "C4:AD32",
                    cov = "C35:AD63",
                    date = "C50:C50")
usethis::use_data(rafi_xlranges, overwrite = TRUE)
rm(rafi_xlranges)
data("rafi_xlranges")
