
# Import and extract race information -------------------------------------
getURL <- function(api, name) {
  root <- "http://www.name-prism.com/api_token/eth/csv/"
  u <- paste0(root, api, "/", name)
  return(URLencode(u))
}

start=1
for (i in start:nrow(mdppas.names)) {
  first.name <- mdppas.names$firstname[i]
  last.name <- mdppas.names$lastname[i]
  name <- paste(first.name, str_replace(last.name, "/", ""), sep=" ")
  url <- getURL(api=nameprism.key, name)
  json.dat <- GET(url)
  data.pull <- str_replace(rawToChar(json.dat$content), "2PRACE", "RACE2")
  new.row <- as_tibble(data.pull) %>%
    mutate(firstname=first.name, lastname=last.name)
  write_csv(new.row,file="data/output/nameprism-mdppas.csv",append=TRUE)
  print(i)
  Sys.sleep(0.5)  
}