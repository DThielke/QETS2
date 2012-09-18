# Homework 2: Working with CRSP Data

# read the data
crsp <- read.csv("data/crsp.csv", header=TRUE)

# separate YYYYMMDD dates into years, months and days
crsp$year <- as.numeric(substr(as.character(crsp$DATE), 1, 4))
crsp$month <- as.numeric(substr(as.character(crsp$DATE), 5, 6))
crsp$day <- as.numeric(substr(as.character(crsp$DATE), 7, 8))
crsp$DATE <- NULL

# keep only stocks listed on NYSE (1), AMEX (2) or NASDAQ (3)
crsp <- crsp[crsp$EXCHCD >= 1 & crsp$EXCHCD <= 3,]

# keep only ordinary stocks (share code 11)
crsp <- crsp[crsp$SHRCD == 11,]

# keep only non-financial stocks
crsp <- crsp[crsp$SICCD < 6000 | crsp$SICCD > 6999,]

# remove any duplicate observations for a given stock in a given month
crsp <- crsp[!duplicated(crsp[, c("month", "year", "PERMNO")]),]

# count the number of stocks
crsp$PERMNO <- factor(crsp$PERMNO)
stocks <- levels(crsp$PERMNO)
nstock <- length(stocks)

# remove any characters from key values
destring <- function(x, columns=names(crsp)) {
    tmp <- x
    tmp[, columns] <- suppressWarnings(lapply(lapply(x[, columns], as.character), as.numeric))
    return (tmp)
}
crsp <- destring(crsp, c("RET", "RETX", "DLRET", "DLRETX", "PRC", "VOL", "SHROUT"))

# replace missing returns with delisting returns
crsp$RET[is.na(crsp$RET)] <- crsp$DLRET[is.na(crsp$RET)]
crsp$RETX[is.na(crsp$RETX)] <- crsp$DLRETX[is.na(crsp$RETX)]

# remove unused data
crsp$DLRET <- NULL
crsp$DLRETX <- NULL
crsp$SHRCD <- NULL
crsp$EXCHCD <- NULL
crsp$SICCD <- NULL

# remove observations with missing returns
crsp <- crsp[!is.na(crsp$RET) & !is.na(crsp$RETX),]

# calculate portfolio dates
crsp$pyear <- ifelse(crsp$month < 7, crsp$year - 1, crsp$year)
crsp$pmonth <- ifelse(crsp$month < 7, crsp$month + 6, crsp$month - 6)

# calculate market capitalization
junes <- crsp$month == 6
size <- data.frame(PERMNO=crsp$PERMNO[junes], pyear=crsp$year[junes], mktcap=(crsp$SHROUT[junes] * abs(crsp$PRC[junes])))
crsp <- merge(crsp, size, by=c("PERMNO", "pyear"))

# sort the cleaned data and remove unused variables
crsp.clean <- crsp[order(crsp$PERMNO, crsp$year, crsp$month),]
rm(crsp, size, junes)

# given an array of returns, computes the compounded returns using a 
# sliding window which runs from (t-from) to (t-to)
trailing.compound.return <- function(ret, from, to) {
    nper <- length(ret)
    # take advantage of cumsum to greatly speed up calculations
    cum.ret <- cumsum(log(1 + ret))
    compound.ret <- vector(mode="numeric", length=nper)
    compound.ret[1:from] <- NA
    compound.ret[from + 1] <- cum.ret[from - to + 1]
    # use indexing to subtract cumulative sums rather than looping
    compound.ret[(from + 2):nper] <- cum.ret[(from - to + 2):(nper - to)] - cum.ret[1:(nper - from - 1)]
    return (exp(compound.ret) - 1)
}

# calculate compound momentum and reversal returns
tic <- proc.time()
for (i in stocks) {
    crsp.clean$momentum[crsp.clean$PERMNO == i] <- trailing.compound.return(crsp.clean$RET[crsp.clean$PERMNO == i], 12, 2)
    crsp.clean$reversal[crsp.clean$PERMNO == i] <- trailing.compound.return(crsp.clean$RET[crsp.clean$PERMNO == i], 60, 13)
}
toc <- proc.time()
print(toc - tic) # time momentum/reversal calculations

# save the results
save(crsp.clean, file="smr.Rdata")
