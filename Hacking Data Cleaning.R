#============================ Country Cleaning ============================
# Convert to lowercase, trim spaces, and handle missing values
data$Country <- data$Country %>%
  str_trim() %>%
  str_to_lower() %>%   
  na_if("unknown") %>%
  na_if("")

# Correct ALL misspellings and map regions/non-countries to NA
data <- data %>%
  mutate(Country = case_when(
    Country %in% c("africa", "asia", "europe", "eastern europe", "western europe", 
                   "south america", "middle east", "oceania", "america", 
                   "asia/pacific region", "oseania", "southamerica", "middleeast", "westeuro", "easteuro") ~ NA,
    Country == "european uni" | Country == "european union" ~ NA,  
    Country == "anonymous proxy" ~ NA,  
    Country == "satellite provider" ~ NA,
    Country == "ascension island" | Country == "ascensionisland" ~ "St. Helena",
    Country == "french polyn" ~ "french polynesia",
    Country == "united kingd" ~ "united kingdom",
    Country == "united state" ~ "united states",
    Country == "new caledoni" | Country == "caledonia" ~ "new caledonia",
    Country == "micronesia" ~ "federated states of micronesia",
    Country == "virgin islan" | Country == "virgin islands" ~ "virgin islands (u.s.)",  
    TRUE ~ Country  # Keep other valid country names unchanged
  ))

# Apply `countrycode()` only on valid country names
data$Country <- countrycode(data$Country, origin = "country.name.en", destination = "country.name.en")

# Capitalize the first letter of each word for consistency
data$Country <- str_to_title(data$Country)


# Imputing missing values

data <- data %>%
  mutate(Country_Code = str_extract(URL, "\\.[a-z]{2}$") %>% str_remove("^\\."))

# Converting country codes to uppercase because countrycode is case-sensitive
data <- data %>%
  mutate(Country_Code = toupper(Country_Code))

# Handle special cases manually before mapping
data <- data %>%
  mutate(Country_Code = case_when(
    Country_Code == "AC" ~ "SH",
    Country_Code == "BK" ~ NA,
    Country_Code == "DB" ~ NA,    # These are not country codes
    Country_Code == "GO" ~ NA,    
    Country_Code == "JS" ~ NA,
    Country_Code == "TX" ~ NA,
    Country_Code == "UK" ~ "GB",  # UK should be GB in ISO 3166-1
    Country_Code == "SU" ~ "RU",  # Soviet Union mapped to Russia
    Country_Code == "EU" ~ NA,    # EU is not a country, set as NA
    Country_Code == "AN" ~ "CW",  # Netherlands Antilles is now Curacao
    Country_Code == "YU" ~ "RS",  # Yugoslavia is now Serbia
    TRUE ~ Country_Code 
  ))

# Automatically map country codes to country names
data <- data %>%
  mutate(Country = ifelse(
    is.na(Country) & Country_Code %in% countrycode::codelist$iso2c,
    countrycode(Country_Code, "iso2c", "country.name"),
    Country
  ))

# Remove temporary column
data <- dplyr::select(data, -Country_Code)

#============================ Operating System Cleaning ============================
data_clean <- data %>%
  drop_na(OS, Loss) %>%  # Remove rows with missing OS or Loss
  mutate(OS = tolower(OS)) %>%  # Convert OS to lowercase
  mutate(OS = str_trim(OS)) %>%  # Trim whitespace
  mutate(OS = case_when(  # Standardize OS names
    str_detect(OS, "win|windows") ~ "Windows",             # Group all Windows variants
    str_detect(OS, "mac|osx") ~ "MacOS",                   # Group all MacOS variants
    str_detect(OS, "linux|ubuntu|debian|fedora") ~ "Linux",# Group all Linux variants
    str_detect(OS, "android") ~ "Android",                # Group Android
    str_detect(OS, "ios") ~ "iOS",                        # Group iOS
    OS == "" | OS == "unknown" ~ "Unknown",               # Treat empty or unknown as 'Unknown'
    TRUE ~ OS
  )) %>%
  # Filter Invalid OS Values
  filter(OS %in% c("Windows", "MacOS", "Linux", "Android", "iOS", "Unknown")) %>%
  # Group rare OS types into "other" (if fewer than 500 occurrences)
  mutate(OS = ifelse(OS %in% names(table(OS)[table(OS) >= 500]), OS, "other"))

# Convert OS to factor
data_clean$OS <- as.factor(data_clean$OS)

# Convert Loss to Numeric
data_clean$Loss <- as.numeric(data_clean$Loss)

# Handle Negative Loss Values
data_clean <- data_clean %>%
  filter(Loss >= 0)  # Filter out rows with negative Loss values

# Optional: Handle Outliers in Loss (Capping)
quantiles <- quantile(data_clean$Loss, probs = c(0.01, 0.99), na.rm = TRUE)
data_clean <- data_clean %>%
  mutate(Loss = pmin(pmax(Loss, quantiles[1]), quantiles[2]))

#============================ Web Server Cleaning ============================
data <- data %>%
  mutate(WebServer = str_trim(tolower(WebServer))) %>%
  mutate(WebServer = case_when(
    str_detect(WebServer, "apache") ~ "Apache",
    str_detect(WebServer, "nginx") ~ "Nginx",
    str_detect(WebServer, "microsoft-iis|iis") ~ "IIS",
    str_detect(WebServer, "tomcat") ~ "Apache Tomcat",
    str_detect(WebServer, "litespeed") ~ "LiteSpeed",
    WebServer == "" | WebServer == "unknown" | is.na(WebServer) ~ NA,
    TRUE ~ WebServer
  ))

# Identify rare web servers and group them under "Other", keeping top 10
top_web_servers <- names(sort(table(data$WebServer), decreasing = TRUE)[1:10])
data <- data %>%
  mutate(WebServer = ifelse(WebServer %in% top_web_servers, WebServer, "Other"))

# Handling Unknown values
data <- data %>%
  mutate(WebServer = case_when(
    is.na(WebServer) | WebServer == "Unknown" ~ case_when(
      str_detect(OS, "Linux") ~ "Apache",
      str_detect(OS, "Win") ~ "IIS",
      str_detect(OS, "Unix") ~ "Nginx",
      TRUE ~ "Imputed"
    ),
    TRUE ~ WebServer
  ))

data <- data %>% mutate(WebServer = ifelse(is.na(WebServer), "Imputed", WebServer))

#============================ Ransom & Loss Cleaning ============================
data <- data %>%
  # Convert Ransom and Loss to numeric (removing non-numeric characters)
  mutate(Ransom = suppressWarnings(as.numeric(gsub("[^0-9.]", "", Ransom))),
         Loss = suppressWarnings(as.numeric(gsub("[^0-9.]", "", Loss)))) %>%
  # Remove negative values
  filter(Ransom >= 0, Loss >= 0)


# Outlier detection using IQR
Q1 <- quantile(data$Ransom, 0.25, na.rm = TRUE)
Q3 <- quantile(data$Ransom, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Mark outliers
data <- data %>% mutate(Ransom_Outlier = ifelse(Ransom < lower_bound | Ransom > upper_bound, TRUE, FALSE))

# Remove outliers (optional)
data <- data %>% filter(Ransom_Outlier == FALSE)

