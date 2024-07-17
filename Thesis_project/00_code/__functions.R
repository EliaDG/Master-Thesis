
# Function to map NUTS2 codes to countries
mapping_nuts <- function(nuts_code) {
  if (grepl("^AL", nuts_code)) {
    return("Albania")
  } else if (grepl("^AT", nuts_code)) {
    return("Austria")
  } else if (grepl("^BA", nuts_code)) {
    return("Bosnia and Herzegovina")
  } else if (grepl("^BE", nuts_code)) {
    return("Belgium")
  } else if (grepl("^BG", nuts_code)) {
    return("Bulgaria")
  } else if (grepl("^CY", nuts_code)) {
    return("Cyprus")
  } else if (grepl("^CZ", nuts_code)) {
    return("Czech Republic")
  } else if (grepl("^DE", nuts_code)) {
    return("Germany")
  } else if (grepl("^DK", nuts_code)) {
    return("Denmark")
  } else if (grepl("^EE", nuts_code)) {
    return("Estonia")
  } else if (grepl("^EL", nuts_code)) {
    return("Greece")
  } else if (grepl("^ES", nuts_code)) {
    return("Spain")
  } else if (grepl("^FI", nuts_code)) {
    return("Finland")
  } else if (grepl("^FR", nuts_code)) {
    return("France")
  } else if (grepl("^HR", nuts_code)) {
    return("Croatia")
  } else if (grepl("^HU", nuts_code)) {
    return("Hungary")
  } else if (grepl("^IE", nuts_code)) {
    return("Ireland")
  } else if (grepl("^IT", nuts_code)) {
    return("Italy")
  } else if (grepl("^LT", nuts_code)) {
    return("Lithuania")
  } else if (grepl("^LU", nuts_code)) {
    return("Luxembourg")
  } else if (grepl("^LV", nuts_code)) {
    return("Latvia")
  } else if (grepl("^MD", nuts_code)) {
    return("Moldova")
  } else if (grepl("^ME", nuts_code)) {
    return("Montenegro")
  } else if (grepl("^MK", nuts_code)) {
    return("North Macedonia")
  } else if (grepl("^MT", nuts_code)) {
    return("Malta")
  } else if (grepl("^NL", nuts_code)) {
    return("Netherlands")
  } else if (grepl("^PL", nuts_code)) {
    return("Poland")
  } else if (grepl("^PT", nuts_code)) {
    return("Portugal")
  } else if (grepl("^RO", nuts_code)) {
    return("Romania")
  } else if (grepl("^RS", nuts_code)) {
    return("Serbia")
  } else if (grepl("^SE", nuts_code)) {
    return("Sweden")
  } else if (grepl("^SI", nuts_code)) {
    return("Slovenia")
  } else if (grepl("^SK", nuts_code)) {
    return("Slovakia")
  } else if (grepl("^TR", nuts_code)) {
    return("Turkey")
  } else if (grepl("^UK", nuts_code)) {
    return("United Kingdom")
  } else if (grepl("^XK", nuts_code)) {
    return("Kosovo")
  } else {
    return(NA)
  }
}
