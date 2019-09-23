library(RSelenium)
library(rvest)
library(purrr)
library(dplyr)
library(stringr)

rD <- rsDriver(chromever="77.0.3865.40") # got closes match to actual chrome version
remDr <- rD[["client"]]

# Go to main page for PD2M program
remDr$navigate("https://aiche.confex.com/aiche/2019/meetingapp.cgi/Program/2595")
innerHTML = remDr$executeScript("return document.body.innerHTML")

# Get list of sessions and their links
sessions_html = read_html(innerHTML[[1]])
sessions_info = sessions_html %>% html_nodes(".SessionListTitle")
sessions_title = map_chr(sessions_info, function(x) x %>% html_text())
sessions_num = map_chr(sessions_info, function(x) x %>% html_nodes("span") %>% html_text())
sessions_link = map_chr(sessions_info, function(x) x %>% html_nodes("a") %>% html_attr("href"))
sessions_link = paste0("https://aiche.confex.com/aiche/2019/meetingapp.cgi/", sessions_link)

# cleanup of session title
sessions_title = map_chr(sessions_title, function(x) gsub("[\t|\n]", "", x))
sessions_title = map_chr(1:length(sessions_title), function(i) gsub(sessions_num[i], "", sessions_title[i]))

sessions_df = tibble(title = sessions_title, link = sessions_link)
write.csv(sessions_df, "sessions.csv", row.names = FALSE)

# get talks for each session along with their info
sessions_df = read.csv("sessions.csv", stringsAsFactors = FALSE)
for(i in 1:nrow(sessions_df)) {
    print(i)
    picklink = sessions_df$link[i]
    remDr$navigate(picklink)
    Sys.sleep(60)
    innerHTML = remDr$executeScript("return document.body.innerHTML")
    cat(innerHTML[[1]],file = paste0("tmpPages/session_", i, ".html"))
}

# get chairs of sessions
sessionfiles = list.files("tmpPages/")
sessionfiles = sessionfiles[grepl("session", sessionfiles)]

sessChairL = list()
for(i in 1:length(sessionfiles)){
    print(i)
    picksession_html = read_html(paste0("tmpPages/", sessionfiles[i]))
    sessionnum = str_extract_all(sessionfiles[i], "[0-9]+")[[1]]
    sessChair = picksession_html %>% html_nodes(".RoleListItem a") %>% html_text() %>% str_trim()
    sessChairAffil = picksession_html %>% html_nodes(".roleAffiliation li") %>% html_text() %>% str_trim()
    if(length(sessChairAffil) == 0) sessChairAffil = rep(NA, length(sessChair))
    sessChairL[[i]] = tibble(sessionnum = sessionnum, chair = sessChair, affil = sessChairAffil)
}
sessChairdf = bind_rows(sessChairL)

sessChairdf %>% count(affil, sort = TRUE)
sessChairdf %>% filter(grepl("Georgia", affil))

