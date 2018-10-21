library(acs)
source('~/juvenile-arrests/scripts/acsHelpers.R')

# ACS B01001
# Get geography object for CT and subcounty divisions
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2016,
    table = "B01001"
)

pops <- data.table()
for (data in acsdata) {
    year <- data@endyear

    pop.total <- acsSum(data, c(3:6, 27:30), "Total")
    pop.under10 <- acsSum(data, c(3, 4, 27, 28), "0 to 9 years")
    pop.10to14 <- acsSum(data, c(5, 29), "10 to 14 years")
    pop.15to17 <- acsSum(data, c(6, 30), "15 to 17 years")

    datafips <- data.table(fips = getACSFips(data))

    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(pop.total),
        estimate(pop.under10),
        estimate(pop.10to14),
        estimate(pop.15to17)
    )

    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(pop.under10) * 1.645,
        standard.error(pop.total) * 1.645,
        standard.error(pop.10to14) * 1.645,
        standard.error(pop.15to17) * 1.645
    )

    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )

    setkey(estimates, FIPS, Year, `Age Range`)
    setkey(moes, FIPS, Year, `Age Range`)

    pops <- rbind(pops, estimates[moes])
}

pops <- pops[pops$FIPS != "0900100000",]

# Write to File
write.table(
    pops,
    file.path(getwd(), "raw", "populations.csv"),
    sep = ",",
    row.names = F
)
