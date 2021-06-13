library(stringi)

makeColNamesUserFriendly <- function(ds) {
  # Convert any number of consecutive dots to a single space.
  names(ds) <- gsub(x = names(ds),
                    pattern = "(\\.)+",
                    replacement = "_")
  
  # Drop the trailing spaces.
  names(ds) <- gsub(x = names(ds),
                    pattern = "( )+$",
                    replacement = "")
  ds
}

dat <-
  read.csv(file = "World Indicators_Full Data.csv",
           header = TRUE,
           check.names = FALSE)
# dat <- makeColNamesUserFriendly(dat)

names(dat) <-
  c(
    "Threshold",
    "Birth_Rate_Bin",
    "Country",
    "Ease_Of_Business_Cluster",
    "GDP_Per_Capita_Bin",
    "Region",
    "Year",
    "Birth_Rate",
    "Business_Tax_Rate",
    "CO2_Emission",
    "Days_to_Start_Business",
    "Ease_Of_Business",
    "Energy_Usage",
    "GDP",
    "GDP_Per_Capita",
    "Health_Exp_GDP",
    "Health_Exp_Per_Capita",
    "Hours_to_do_Tax",
    "Infant_Mortality_Rate",
    "Internet_Usage",
    "Lending_Interest",
    "Life_Expectancy",
    "Life_Expectancy_Female",
    "Life_Expectancy_Male",
    "Mobile_Phone_Usage",
    "Number_of_Records",
    "Population_0_to_14",
    "Population_15_to_64",
    "Population_above_65",
    "Population_Total",
    "Population_Urban",
    "Tourism_Inbound",
    "Tourism_Outbound"
  )

pct_to_number <- function(x) {
  if (!is.numeric(x)) {
    x_replace_pct <- sub("%", "", x)
    return(as.numeric(x_replace_pct) / 100)
  } else {
    return(x)
  }
}

usd_to_number <- function(x) {
  if (!is.numeric(x)) {
    x_replace <- sub("^.", "", x)
    x_replace <- sub(",", "", x_replace)
    amt <- as.numeric(gsub("[A-Z]", "", x_replace))
    multiplier <- substring(x_replace, nchar(x_replace))
    multiplier <- dplyr::case_when(multiplier == "M" ~ 1e6,
                                   multiplier == "B" ~ 1e9,
                                   TRUE ~ 1)
    return(as.numeric(amt * multiplier))
  } else {
    return(x)
  }
}

year_extract <-
  function(x) {
    stringi::stri_extract_last_regex(x, "\\d{4}")
  }

preprocess_columns <- function(ds) {
  tryCatch({
    names(ds) <- tolower(names(ds))
    if ("year" %in% names(ds)) {
      if (is.character(ds["year"][1, ]) & nchar(ds["year"][1, ]) > 7)
        ds["year"] <- sapply(ds["year"], year_extract)
    }
    ds["birth_rate"] <- sapply(ds["birth_rate"], pct_to_number)
    ds["health_exp_gdp"] <-
      sapply(ds["health_exp_gdp"], pct_to_number)
    ds["internet_usage"] <-
      sapply(ds["internet_usage"], pct_to_number)
    ds["mobile_phone_usage"] <-
      sapply(ds["mobile_phone_usage"], pct_to_number)
    ds["population_urban"] <-
      sapply(ds["population_urban"], pct_to_number)
    
    ds["gdp"] <- sapply(ds["gdp"], usd_to_number)
    ds["gdp_per_capita"] <-
      sapply(ds["gdp_per_capita"], usd_to_number)
    ds["tourism_inbound"] <-
      sapply(ds["tourism_inbound"], usd_to_number)
    ds["tourism_outbound"] <-
      sapply(ds["tourism_outbound"], usd_to_number)
    ds["health_exp_per_capita"] <-
      sapply(ds["health_exp_per_capita"], usd_to_number)
    
    ds["population_total"] <-
      sapply(ds["population_total"], usd_to_number)
  })
  names(ds) <- toupper(names(ds))
  return(ds)
}

data("GNI2014", package = "treemap")

data <- preprocess_columns(dat)
data <- merge(data, GNI2014, by.x = 'COUNTRY', by.y = 'country', all.x = T)
data[,35:37] <- NULL
data = data %>% mutate(COUNTRY_CODE = iso3)
save(data, file = 'processed_data.Rdata')
