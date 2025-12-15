# Functions for cleaning data
# sourced to Quarto doc for main work

library(dplyr) 
library(lubridate)
library(pdftools)
library(tidyverse)
library(dplyr)
library(here)

# Function to drop columns that are all NA
drop_na_columns <- function(df) {
  df[, colSums(is.na(df)) < nrow(df)]
}



# Function to drop rows where all values except first column are NA
drop_all_na_rows <- function(df) {
  # Check if all columns except first are NA for each row
  keep_rows <- !apply(df[, -1], 1, function(row) all(is.na(row)))
  
  # Return filtered dataframe
  df[keep_rows, ]
}




# Function to drop rows with fewer than 25 non-NA values
# written for WDI data set
drop_sparse_rows <- function(df, min_values = 25, exclude_cols = 1) {
  # Select columns to check (all except excluded)
  check_cols <- setdiff(1:ncol(df), exclude_cols)
  
  # Count non-NA values in each row
  non_na_counts <- apply(df[, check_cols, drop = FALSE], 1, function(row) sum(!is.na(row)))
  
  # Keep rows with at least min_values non-NA values
  keep_rows <- non_na_counts >= min_values
  
  # Return filtered dataframe
  df[keep_rows, ]
}





# Function to get all weather data for a station with automatic retry
get_station_data <- function(station_id, start_date, end_date, island_name, max_retries = 3) {
  
  # Convert dates
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  
  # Create date ranges (1 year chunks to avoid hitting limit)
  date_ranges <- seq(start, end, by = "1 year")
  if (tail(date_ranges, 1) != end) {
    date_ranges <- c(date_ranges, end)
  }
  
  all_data <- list()
  failed_ranges <- list()
  
  # Loop through date ranges
  for (i in 1:(length(date_ranges) - 1)) {
    
    chunk_start <- as.character(date_ranges[i])
    chunk_end <- as.character(date_ranges[i + 1] - 1)
    
    cat("Fetching data for", island_name, "from", chunk_start, "to", chunk_end, "\n")
    
    tryCatch({
      weather <- ncdc(
        datasetid = "GHCND",
        stationid = station_id,
        datatypeid = c("PRCP", "TMAX", "TMIN"),
        startdate = chunk_start,
        enddate = chunk_end,
        limit = 1000
      )
      
      if (!is.null(weather$data)) {
        all_data[[i]] <- weather$data %>%
          mutate(island = island_name)
      }
      
      # API rate limit - pause between requests
      Sys.sleep(0.2)
      
    }, error = function(e) {
      cat("Error fetching data:", e$message, "\n")
      # Store failed range for retry
      failed_ranges[[length(failed_ranges) + 1]] <- list(
        start = chunk_start,
        end = chunk_end
      )
    })
  }
  
  # Bind initial data
  if (length(all_data) > 0) {
    combined_data <- bind_rows(all_data)
  } else {
    combined_data <- NULL
  }
  
  # Check for missing years and retry
  if (!is.null(combined_data)) {
    start_year <- year(as.Date(start_date))
    end_year <- year(as.Date(end_date))
    
    data_years <- combined_data %>%
      mutate(year = year(date)) %>%
      pull(year) %>%
      unique() %>%
      sort()
    
    all_years <- start_year:end_year
    missing_years <- setdiff(all_years, data_years)
    
    if (length(missing_years) > 0) {
      cat("\nMissing years detected:", paste(missing_years, collapse = ", "), "\n")
      cat("Retrying missing years...\n\n")
      
      # Retry missing years
      retry_data <- list()
      
      for (attempt in 1:max_retries) {
        if (length(missing_years) == 0) break
        
        cat("Retry attempt", attempt, "of", max_retries, "\n")
        
        for (year in missing_years) {
          chunk_start <- paste0(year, "-01-01")
          chunk_end <- paste0(year, "-12-31")
          
          cat("Retrying", island_name, "for year", year, "\n")
          
          tryCatch({
            weather <- ncdc(
              datasetid = "GHCND",
              stationid = station_id,
              datatypeid = c("PRCP", "TMAX", "TMIN"),
              startdate = chunk_start,
              enddate = chunk_end,
              limit = 1000
            )
            
            if (!is.null(weather$data)) {
              retry_data[[as.character(year)]] <- weather$data %>%
                mutate(island = island_name)
              cat("Success for", year, "\n")
            }
            
            Sys.sleep(0.5)  # Longer pause for retries
            
          }, error = function(e) {
            cat("Error for", year, ":", e$message, "\n")
          })
        }
        
        # Check which years are still missing
        if (length(retry_data) > 0) {
          retry_combined <- bind_rows(retry_data)
          combined_data <- bind_rows(combined_data, retry_combined)
          
          data_years <- combined_data %>%
            mutate(year = year(date)) %>%
            pull(year) %>%
            unique() %>%
            sort()
          
          missing_years <- setdiff(all_years, data_years)
        }
        
        if (length(missing_years) == 0) {
          cat("\nAll missing years recovered!\n")
          break
        } else if (attempt < max_retries) {
          cat("\nStill missing:", paste(missing_years, collapse = ", "), "\n")
          cat("Waiting before next retry attempt...\n")
          Sys.sleep(2)
        }
      }
      
      if (length(missing_years) > 0) {
        cat("\nWarning: Could not retrieve data for years:", 
            paste(missing_years, collapse = ", "), "\n")
      }
    } else {
      cat("\nAll years successfully retrieved!\n")
    }
    
    # Final summary
    cat("\nData Summary for", island_name, ":\n")
    cat("Date range:", min(combined_data$date), "to", max(combined_data$date), "\n")
    cat("Total records:", nrow(combined_data), "\n")
    
    year_counts <- combined_data %>%
      mutate(year = year(date)) %>%
      count(year)
    
    cat("Years with data:", nrow(year_counts), "\n\n")
  }
  
  return(combined_data)
}





# Function to get storm events with automatic retry for missing years
get_storm_events <- function(station_id, start_year, end_year, island_name, max_retries = 3) {
  
  all_events <- list()
  
  # Initial data collection
  for (year in start_year:end_year) {
    
    start_date <- paste0(year, "-01-01")
    end_date <- paste0(year, "-12-31")
    
    cat("Fetching storm events for", island_name, "in", year, "\n")
    
    tryCatch({
      events <- ncdc(
        datasetid = "GHCND",
        stationid = station_id,
        datatypeid = c("WT01", "WT03", "WT04", "WT05", "WT06", "WT11", "WT16", "WT18"),
        startdate = start_date,
        enddate = end_date,
        limit = 1000
      )
      
      if (!is.null(events$data)) {
        all_events[[as.character(year)]] <- events$data
      }
      
      Sys.sleep(0.2)
      
    }, error = function(e) {
      cat("Error for", year, ":", e$message, "\n")
    })
  }
  
  # Bind initial data
  if (length(all_events) > 0) {
    combined_data <- bind_rows(all_events)
  } else {
    combined_data <- NULL
  }
  
  # Check for missing years and retry
  if (!is.null(combined_data)) {
    data_years <- combined_data %>%
      mutate(year = year(date)) %>%
      pull(year) %>%
      unique() %>%
      sort()
    
    all_years <- start_year:end_year
    missing_years <- setdiff(all_years, data_years)
    
    if (length(missing_years) > 0) {
      cat("\nMissing years detected for", island_name, ":", paste(missing_years, collapse = ", "), "\n")
      cat("Retrying missing years...\n\n")
      
      retry_data <- list()
      
      for (attempt in 1:max_retries) {
        if (length(missing_years) == 0) break
        
        cat("Retry attempt", attempt, "of", max_retries, "for", island_name, "\n")
        
        for (year in missing_years) {
          start_date <- paste0(year, "-01-01")
          end_date <- paste0(year, "-12-31")
          
          cat("Retrying", island_name, "for year", year, "\n")
          
          tryCatch({
            events <- ncdc(
              datasetid = "GHCND",
              stationid = station_id,
              datatypeid = c("WT01", "WT03", "WT04", "WT05", "WT06", "WT11", "WT16", "WT18"),
              startdate = start_date,
              enddate = end_date,
              limit = 1000
            )
            
            if (!is.null(events$data)) {
              retry_data[[as.character(year)]] <- events$data
              cat("Success for", year, "\n")
            }
            
            Sys.sleep(0.5)
            
          }, error = function(e) {
            cat("Error for", year, ":", e$message, "\n")
          })
        }
        
        # Check which years are still missing
        if (length(retry_data) > 0) {
          retry_combined <- bind_rows(retry_data)
          combined_data <- bind_rows(combined_data, retry_combined)
          
          data_years <- combined_data %>%
            mutate(year = year(date)) %>%
            pull(year) %>%
            unique() %>%
            sort()
          
          missing_years <- setdiff(all_years, data_years)
        }
        
        if (length(missing_years) == 0) {
          cat("\nAll missing years recovered for", island_name, "!\n")
          break
        } else if (attempt < max_retries) {
          cat("\nStill missing for", island_name, ":", paste(missing_years, collapse = ", "), "\n")
          cat("Waiting before next retry attempt...\n")
          Sys.sleep(2)
        }
      }
      
      if (length(missing_years) > 0) {
        cat("\nWarning: Could not retrieve storm event data for", island_name, 
            "in years:", paste(missing_years, collapse = ", "), "\n")
      }
    } else {
      cat("\nAll years successfully retrieved for", island_name, "!\n")
    }
    
    # Final summary
    cat("\nStorm Events Summary for", island_name, ":\n")
    if (nrow(combined_data) > 0) {
      cat("Date range:", min(combined_data$date), "to", max(combined_data$date), "\n")
      cat("Total events:", nrow(combined_data), "\n")
      
      year_counts <- combined_data %>%
        mutate(year = year(date)) %>%
        count(year)
      
      cat("Years with events:", nrow(year_counts), "\n\n")
    } else {
      cat("No events found\n\n")
    }
  } else {
    cat("\nNo storm event data found for", island_name, "\n\n")
  }
  
  return(combined_data)
}

# Function to extract tourism data from a single PDF
extract_tourism_data <- function(pdf_path, data_type) {
  
  cat("Processing:", basename(pdf_path), "\n")
  
  # Read PDF text
  text <- pdf_text(pdf_path)
  page1 <- text[1]
  
  # Split into lines
  lines <- str_split(page1, "\n")[[1]]
  
  # Extract years from title line
  year_line <- lines[str_detect(lines, regex("January\\s+\\d{4}\\s+to\\s+December\\s+\\d{4}", ignore_case = TRUE))]
  
  if (length(year_line) == 0) {
    cat("  WARNING: Could not find years\n")
    return(NULL)
  }
  
  years <- as.numeric(str_extract_all(year_line[1], "\\d{4}")[[1]])
  cat("  Years:", years[1], "to", years[2], "\n")
  
  # Define months
  months <- c("January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December")
  
  # Extract data for each month
  data_list <- list()
  
  for (month in months) {
    # Find the line with this month (but not the title line)
    # Look for month followed by a space and numbers
    month_pattern <- paste0("^\\s*", month, "\\s+\\d")
    month_lines <- lines[str_detect(lines, regex(month_pattern, ignore_case = TRUE))]
    
    if (length(month_lines) == 0) {
      next
    }
    
    month_line <- month_lines[1]
    
    # Extract all numbers including decimals and negative signs
    numbers <- str_extract_all(month_line, "-?[\\d,]+\\.?\\d*")[[1]]
    numbers <- as.numeric(str_remove_all(numbers, ","))
    
    # Skip if first number is a year (happens with January sometimes)
    if (length(numbers) > 0 && numbers[1] >= 1990 && numbers[1] <= 2030) {
      next
    }
    
    # Expected pattern (when all data present):
    # Month | STT_Y1 | STT_Y2 | %chg | STX_Y1 | STX_Y2 | %chg | Total_Y1 | Total_Y2 | %chg
    # Index:    0        1        2       3        4        5       6          7          8
    
    if (length(numbers) >= 9) {
      # Full data - both years, all islands
      data_list[[length(data_list) + 1]] <- tibble(
        year = years[1],
        month = month,
        st_thomas_st_john = numbers[1],
        st_croix = numbers[4],
        usvi_total = numbers[7],
        arrival_type = data_type
      )
      
      data_list[[length(data_list) + 1]] <- tibble(
        year = years[2],
        month = month,
        st_thomas_st_john = numbers[2],
        st_croix = numbers[5],
        usvi_total = numbers[8],
        arrival_type = data_type
      )
    } else if (length(numbers) >= 2) {
      # Partial data - handle missing values
      # Sometimes St. Croix has dashes for missing data
      
      # Try to extract what we have
      stt_y1 <- if (length(numbers) >= 1) numbers[1] else NA_real_
      stt_y2 <- if (length(numbers) >= 2) numbers[2] else NA_real_
      stx_y1 <- if (length(numbers) >= 4) numbers[4] else NA_real_
      stx_y2 <- if (length(numbers) >= 5) numbers[5] else NA_real_
      total_y1 <- if (length(numbers) >= 7) numbers[7] else NA_real_
      total_y2 <- if (length(numbers) >= 8) numbers[8] else NA_real_
      
      data_list[[length(data_list) + 1]] <- tibble(
        year = years[1],
        month = month,
        st_thomas_st_john = stt_y1,
        st_croix = stx_y1,
        usvi_total = total_y1,
        arrival_type = data_type
      )
      
      data_list[[length(data_list) + 1]] <- tibble(
        year = years[2],
        month = month,
        st_thomas_st_john = stt_y2,
        st_croix = stx_y2,
        usvi_total = total_y2,
        arrival_type = data_type
      )
    }
  }
  
  if (length(data_list) > 0) {
    return(bind_rows(data_list))
  } else {
    return(NULL)
  }
}
