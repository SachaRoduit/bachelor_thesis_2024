DF = read_xlsx("DF_Main.xlsx")

saveRDS(DF, file="DF_INV.RData")

DF_Big = read_xlsx("DF_Big.xlsx")

saveRDS(DF_Big, file="DF_startup.RData")

DF_Small = read_xlsx("DF_Small.xlsx")
