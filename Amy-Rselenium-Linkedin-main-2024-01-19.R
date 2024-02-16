# load the required packages
library(RSelenium)
library(tidyverse)
library(rvest)
library(tidygeocoder)
library(leaflet)
library(rnaturalearth)
library(sf)
library(udpipe)
library(textrank)
library(wordcloud)
library(stringr)
library(wdman)
library(netstat)
library(xml2)

get_url <- function(key_word, location) {
  # Generate URL from job title and location
  linkedin <- 'https://www.linkedin.com/jobs/search/?keywords=%s&location=%s&refresh=true'
  url <- sprintf(linkedin, URLencode(key_word), URLencode(location))
  return(url)
}

get_record <- function(page){
  # Job title
  job_title <- page %>%
    html_elements(css = ".job-card-list__title") %>%
    html_text2()
  
  # URL for job post 
  job_url <- page %>%
    html_elements(css = ".job-card-list__title")%>%
    html_attr('href') %>%
    paste0("https://www.linkedin.com", .)
  
  # Company name
  company_name <- page %>%
    html_elements(css = ".job-card-container__primary-description ")%>%
    html_text2()
  
  # Location
  company_location <- page %>%
    html_elements(css = ".job-card-container__metadata-item")%>%
    html_text2()
  
  if (length(company_location) > 25){
    # Identify indices of elements containing "$" (salary)
    salary_index <- grep("\\$", company_location)
    # # salary
    # salary = rep(NA, 25)
    # salary[salary_index] <- company_location[salary_index]
    
    # Remove salary from location
    company_location <- company_location[-salary_index]
  }
  
  # # Post Date
  # post_date <- page %>%
  #   html_elements("time") %>%
  #   html_attr("datetime")
  
  # Job posts in the same format
  df <- data.frame(job_title, company_name, company_location, job_url)
}

# start the Selenium server
rdriver <- rsDriver(browser = "firefox", # browser name
                    port = free_port(), # port number
                    chromever = NULL, # chrome browser version
                    verbose=F)

# creating a client object and opening the browser
remDr <- rdriver$client

# For navigating to the url
key_word <- "data analyst"
location = "Canada"

url = get_url(key_word,location)
remDr$navigate(url)

# LinkedIn login
username <- readLines("/Volumes/Share/DSWE Project/Web Scraping/Linkedin/username.txt", n = 1)
password <- readLines("/Volumes/Share/DSWE Project/Web Scraping/Linkedin/password.txt", n = 1)

remDr$findElement(using = 'class', 'nav__button-secondary')$clickElement()
username_box <- remDr$findElement(using = 'id', 'username')
password_box <- remDr$findElement(using = 'id', 'password')
username_box$sendKeysToElement(list(username,key = 'tab'))
password_box$sendKeysToElement(list(password,key = 'enter'))

# Date filter
remDr$findElement(using = 'id', 'searchFilter_timePostedRange')$clickElement()
remDr$findElement(using = 'xpath', '//*[text()="Past week"]')$clickElement()
remDr$findElement(using = 'class', 'artdeco-button--primary')$clickElement()

# Scroll down until the end is reached
i = 0
while (i <= 6) {
  # Send the "down_arrow" key to the element
  element_to_scroll <- remDr$findElement(using = 'class', 'jobs-search-results-list')
  element_to_scroll$sendKeysToElement(list(key = "page_down"))
  # You may need to introduce a delay to give the page time to load
  i <- i + 1
}

page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()

df <- get_record(page)

for (i in 2:9) {
  print(paste('page',i))
  xpath_expression <- sprintf('//*[@aria-label="Page %d"]', i)
  remDr$findElement(using = 'xpath', xpath_expression)$clickElement()
  # Scroll down until the end is reached
  i = 0
  while (i <= 6) {
    # Send the "down_arrow" key to the element
    element_to_scroll <- remDr$findElement(using = 'class', 'jobs-search-results-list')
    element_to_scroll$sendKeysToElement(list(key = "page_down"))
    Sys.sleep(1)
    # You may need to introduce a delay to give the page time to load
    i <- i + 1
  }
  Sys.sleep(1)
  page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
  Sys.sleep(1)
  df <- rbind(df, get_record(page))
}

# job description
# Execute JavaScript to open a new window
remDr$executeScript('window.open("", "_blank");')
# Get all window handles
window_handles <- remDr$getWindowHandles()
# Switch to the second window (index 2)
remDr$switchToWindow(window_handles[[2]])

job_description0 <- list()
job_description <- list()

for (i in 1:length(df$job_url)){
  remDr$navigate(df$job_url[i])
  Sys.sleep(2)
  page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
  job_description0 <- page %>%
    html_nodes(xpath = '//*[@id="job-details"]') %>%
    html_children() %>%
    # html_nodes('.jobs-box__html-content') %>%
    html_text2()
    # paste(collapse = " ")
  job_description[i] <- job_description0[2]
  # Wait for 5 seconds
  Sys.sleep(3)
}

df$job_description <- sapply(job_description, paste, collapse = ",")

file_name = paste("linkedin",key_word,Sys.time(),sep="-")
write.csv(df, paste("/Volumes/Share/DSWE Project/Web Scraping/Linkedin/", file_name, ".csv"), row.names = FALSE)
write.table(df, file = paste("/Volumes/Share/DSWE Project/Web Scraping/Linkedin/", file_name, ".tsv"), sep = "\t", row.names = FALSE)