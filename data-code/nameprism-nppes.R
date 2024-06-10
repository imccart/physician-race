
# Import and extract race information -------------------------------------
getURL <- function(api, name) {
  root <- "http://www.name-prism.com/api_token/eth/csv/"
  u <- paste0(root, api, "/", name)
  return(URLencode(u))
}

start=1
for (i in start:nrow(nppes.names)) {
  first.name <- nppes.names$firstname[i]
  last.name <- nppes.names$lastname[i]
  last.name <- str_replace(last.name, "04/21/1970","")
  last.name <- str_replace(last.name, "D/B/A MASSAGE MATTERS","")
  name <- paste(first.name, str_replace(last.name, "/", ""), sep=" ")
  url <- getURL(api=nameprism.key, name)
  json.dat <- GET(url)
  data.pull <- str_replace(rawToChar(json.dat$content), "2PRACE", "RACE2")
  new.row <- as_tibble(data.pull) %>%
    mutate(firstname=first.name, lastname=last.name)
  write_csv(new.row,file="data/output/nameprism-nppes.csv",append=TRUE)
  print(i)
  Sys.sleep(0.5)  
}