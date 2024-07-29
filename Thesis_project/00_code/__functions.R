
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


merge_geometries <- function(data, nuts, new_nuts, new_name) {
  combined_geometry <- data %>%
    filter(NUTS %in% nuts) %>%
    st_union()
  
  new_row <- data.frame(
    NUTS = new_nuts,
    Name = new_name,
    geometry = st_sfc(combined_geometry)
  )
  
  new_row <- st_as_sf(new_row, sf_column_name = "geometry", crs = st_crs(data))
  
  data <- data %>%
    filter(!NUTS %in% nuts) %>%
    bind_rows(new_row)
  
  return(data)
}

update_w_queen <- function(W.queen, region_pairs, value_pairs) {
  # Ensure the lengths of region_pairs and value_pairs match
  if (length(region_pairs) != length(value_pairs)) {
    stop("The lengths of region_pairs and value_pairs must match.")
  }
  
  for (i in seq_along(region_pairs)) {
    region1 <- region_pairs[[i]][1]
    region2 <- region_pairs[[i]][2]
    value <- value_pairs[i]
    
    row_index1 <- which(rownames(W.queen) == region1)
    col_index1 <- which(colnames(W.queen) == region2)
    row_index2 <- which(rownames(W.queen) == region2)
    col_index2 <- which(colnames(W.queen) == region1)
    
    if (length(row_index1) > 0 && length(col_index1) > 0 && length(row_index2) > 0 && length(col_index2) > 0) {
      W.queen[row_index1, col_index1] <- value
      W.queen[row_index2, col_index2] <- value
    } else {
      warning(paste("One or both regions not found in the matrix:", region1, region2))
    }
  }
  
  return(W.queen)
}
