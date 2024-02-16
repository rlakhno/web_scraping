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
library(httr)

get_url <- function(key_word, location) {
  # Generate URL from job title and location
  indeed <- 'https://ca.indeed.com/jobs?q=%s&l=%s&start='
  url <- sprintf(indeed, URLencode(key_word), URLencode(location))
  return(url)
}

get_record <- function(page){
  # Job title
  job_title <- page %>%
    html_elements(css = ".mosaic-provider-jobcards .result") %>%
    html_elements(css = ".resultContent") %>%
    html_element("h2") %>%
    html_text2() %>%
    str_replace(".css.*;\\}", "")
  
  # URL for job post 
  job_url <- page %>%
    html_elements(css = ".mosaic-provider-jobcards .result")%>%
    html_elements(css = ".resultContent") %>%
    html_element("h2") %>%
    html_element("a") %>%
    html_attr('href') %>%
    paste0("https://ca.indeed.com", .)
  
  # Company name
  company_name <- page %>%
    html_elements(css = ".mosaic-provider-jobcards .result")%>%
    html_elements(css = ".resultContent")%>%
    html_elements(xpath = '//*[@data-testid="company-name"]') %>%
    html_text()
  
  # Location
  company_location <- page %>%
    html_elements(css = ".mosaic-provider-jobcards .result")%>%
    html_elements(css = ".resultContent")%>%
    html_elements(xpath = '//*[@data-testid="text-location"]') %>%
    html_text()
  
  # Data about salary (when indicated)
  salary_hour <- page %>%
    html_elements(css = ".mosaic-provider-jobcards .result .resultContent")%>%
    html_element(css = ".salaryOnly") %>%
    html_element(".metadata.salary-snippet-container [data-testid='attribute_snippet_testid']") %>%
    html_text()
  
  # Job posts in the same format
  df <- data.frame(job_title, company_name, company_location,salary_hour, job_url)
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

page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()

# Date filter
url <- page %>%
  html_element(xpath = '//*[text()="Last 7 days"]') %>%
  html_attr('href') %>%
  paste0('https://ca.indeed.com', .)
remDr$navigate(url)

page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()

df <- get_record(page)

while (TRUE) {
  tryCatch({
    next_page <- page %>%
      html_element(xpath = '//*[@aria-label="Next Page"]') %>%
      html_attr('href') %>%
      paste0('https://ca.indeed.com', .)
    
    # Check if there is a "Next Page" link
    if (is.na(next_page)) {
      # No more pages, exit the loop
      break
    }
    
    url <- next_page
    remDr$navigate(url)
    page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
    df <- rbind(df, get_record(page))
  }, error = function(err) {
    cat("An error occurred:", conditionMessage(err), "\n")
    # You may want to add additional error handling logic here
    # For example, consider logging the error or breaking out of the loop
    break
  })
}

# job description
# Execute JavaScript to open a new window
remDr$executeScript('window.open("", "_blank");')
# Get all window handles
window_handles <- remDr$getWindowHandles()
# Switch to the second window (index 2)
remDr$switchToWindow(window_handles[[2]])

job_description <- list()
for (i in 1:length(df$job_url)){
  remDr$navigate(df$job_url[i])
  page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
  job_description[i] <- page %>%
    html_element(xpath = '//*[@id="jobDescriptionText"]') %>%
    html_text2()
  # Wait for 5 seconds
  Sys.sleep(5)
}

df$job_description <- sapply(job_description, paste, collapse = ",")

file_name = paste("indeed",key_word,Sys.time(),sep="-")
write.csv(df,paste("/Volumes/Share/DSWE Project/Web Scraping/Indeed/",file_name,".csv"), row.names = FALSE)