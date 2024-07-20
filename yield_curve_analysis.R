library(rvest)
library(xml2)
library(readxl)
library(ggplot2)
library(viridis)

rfr_data_path <- "data/rfr/"

# Download data

## get html contents
url <- "https://www.eiopa.europa.eu/tools-and-data/risk-free-interest-rate-term-structures_en#risk-free-rates-previous-releases-and-preparatory-phase"
webpage <- read_html(url)

div_ids <- c(
  "paragraph-165-0-content", # 2024
  "paragraph-165-1-content"  # 2023
)

## get all download links
download_links <- vector(mode="character")
for (div_id in div_ids){
  section <- html_node(webpage, css = paste0("#", div_id))  # Replace with the correct CSS selector
  links <- html_nodes(section, "a")
  download_links <- c(download_links, html_attr(links, "href"))
}
download_links

## download all files
base_url <- "https://www.eiopa.europa.eu/"
for (file_link in download_links){
  file_name <- strsplit(file_link, "filename=")[[1]][2]
  download.file(
    paste0(base_url, file_link), 
    paste0(rfr_data_path, file_name), 
    mode = "wb"
  )
}

# check whether names are correct - if not rename file manually
for (file in list.files(rfr_data_path)){
  if(!startsWith(file, "EIOPA_RFR_") | !endsWith(file, ".zip")){
    print(paste(file, "does not comply with naming convention."))
  }
}

# check whether number of files is plausible
n_dates <- 1 + 12 + 6
stopifnot(length(list.files(rfr_data_path)) == n_files) # 2022 + 2023 + 2024

# unzip all files
for (file in list.files(rfr_data_path, full.names = T)){
  print(file)
  unzip(file, exdir=rfr_data_path)
}


# extract country and tenor wise interest rates (RFR without volatility adjustment)

process_rfr_df <- function(df){
  # remove rows not needed
  df <- df[-c(1:8),]
  row.names(df) <- NULL
  colnames(df)[1] <- "Tenor"
  
  # date column
  date_str <- strsplit(file_name, "_")[[1]][3]
  df[,"Date"] <- as.Date(date_str, format="%Y%m%d")
  
  # put date column first
  df <- df[,c(ncol(df), 1:ncol(df)-1)]

  return(df)
}

# load excels, extract sheet, process sheet and concat to one comprehensive data frame 
df <- data.frame()
for (file_name in list.files(rfr_data_path, full.names = T)){
  if(grepl("_Term_Structures.xlsx", file_name)){
    print(paste("Loading", basename(file_name)))
    df_sub <- data.frame(
      read_excel(file_name, sheet="RFR_spot_no_VA")
    )
    
    print(paste("Processing", basename(file_name)))
    df_sub <- process_rfr_df(df_sub)
    
    df <- rbind(df, df_sub)
  }
}

# set data types
df$Tenor <- as.integer(df$Tenor)
cols.rates <- c(3:dim(df)[2])
df[cols.rates] <- sapply(df[cols.rates], as.double)

# check if all files were concatenated
stopifnot(length(unique(df$Date)) == n_dates)
# check if all Tenors are there
stopifnot(nrow(df) == n_dates * length(unique(df$Tenor)))
# check if we have NAs for the locations
stopifnot(sum(is.na(df))==0)
# check if RFRs make sense
stopifnot(all(df[,cols.rates]>-0.01) & all(df[,cols.rates] < 1))

# check extreme values
df[which(df<0, arr.ind = T)] # check negative yields
max(df[cols.rates])


# Visualize the changes in US and Euro yield curve across tenors since 2022
plot_yield_curves <- function(region){
  ggplot(df, aes(x=Tenor, y=df[,region], color=as.factor(Date))) + 
    geom_line(aes(color = as.factor(Date)), size = 1) + 
    scale_color_viridis_d() +
    labs(title = paste(region, "Yield Curves"),
         x = "Maturity (Years)",
         y = "Yield",
         color = "Date") +
    theme_minimal() +
    theme(legend.position = "bottom")
}
plot_yield_curves("Euro")
plot_yield_curves("United.States")
