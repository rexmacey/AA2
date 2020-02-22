# Create data file in which to store the Excel ranges related to RAFI asset allocation file
rafi_xlranges_default <- list( return = "B4:L32",
                    corr = "C4:AD32",
                    cov = "C35:AD63",
                    date = "C50:C50")
# map between Black Diamond (bd) segments names and rtnames (which are associated with RAFI assumptions)
acMap <- data.frame(stringsAsFactors = FALSE,
                    rt = c("Munis", "STUSTsy", "LTUSTsy", "IntUSTsy", "USTIPs", "USCash", "BankLoans", "USTIPs",
                           "BankLoans", "Munis", "HiYld", "EMLocalDebt", "EMNonLclDebt", "GlobalCore", "GlobalCore",
                           "HiYld", "EMCurrency", NA, NA, NA, "REIT", "Commodities", "USCash", NA, NA, "USLarge",
                           "USSmall", "EMEquity", "EAFEEquity", "GlblEquity", "USCash", "LTUSTsy", "Munis", "USTIPs",
                           "USCoreBonds","USCoreBonds"),
                    bd = toupper(c("Tax-Exempt", "U.S. Short Taxable", "U.S. Long Term", "U.S. Int. Taxable", "Inflation Protected",
                                   "Cash & Equiv", "Floating Rate Note", "Global ILBs", "Bank Loans", "High Yield Tax-Exemp",
                                   "High Yield Taxable", "EM (Local) Debt", "EM (Non-Local) Debt", "Foreign Bonds", "Global Bonds",
                                   "Preferred", "Emerging Currency", "Conservative", "Moderate", "Aggressive", "REITs",
                                   "Commodity", "Alternatives", "Other", "To Be Classified", "U.S. Large", "U.S. Small",
                                   "Emerging", "International", "Global", "Cash & Equivalents", "U.S. Long Term Taxable",
                                   "High Yield Tax-Exempt", "U.S. TIPS", "U.S. Short/Intermediate Taxable",
                                   "U.S. Core Taxable")))

usethis::use_data(rafi_xlranges_default, acMap, overwrite = TRUE, internal = TRUE)
rm(rafi_xlranges_default, acMap)
AA2:::rafi_xlranges_default
AA2:::acMap
