Race_zip_raw <- read.csv("High_Schoolers_by_Zip_Race_data.csv")
nrow(Race_zip_raw)
names(Race_zip_raw)
colnames(Race_zip_raw) <- c("zip", "white", "hispanic", "black", "native", "asian", "hawaiianPI")
asianPI <- Race_zip_raw$asian + Race_zip_raw$hawaiianPI
Race_zip <- data.frame(Race_zip_raw[, 1:5], asianPI)
names(Race_zip)
