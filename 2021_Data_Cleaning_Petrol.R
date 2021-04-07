#=========================== Load Libraries ================================#
library(tidyverse)
library(janitor)
library(xml2)
library(rvest)
library(ggthemes)
library(httr)
library(htmltools)
library(pdftools)
library(lubridate)
library(tidytext)
library(extrafont)
theme_set(theme_clean())

#========================== Set-Up HTML ====================================#
PP_90_99 <- rvest::read_html(x="http://www.energy.gov.za/files/esources/petroleum/history_petrol_price.html") %>%
  rvest::html_nodes("tbody") %>% 
  html_table()


PP_90_99 <- lapply(PP_90_99,data.frame) %>% 
  bind_rows()

PP_90_99 <- PP_90_99[-c(1:3),]

names(PP_90_99) <- PP_90_99[1,]

PP_90_99 <- PP_90_99[-1,]

PP_90_99 <- clean_names(PP_90_99)

Year_20_2010 <- cbind(data.frame(year= 2000:2010),
                      data.frame(path = replicate(11,
                                                  "http://www.energy.gov.za/files/esources/petroleum/Achieves_Petrol/"))) %>%
  mutate(file = paste0(path,"petroleum_",year,".html"))

Year_20_2010 <- as.data.frame(Year_20_2010)

#============================= Extract Tables ============================#
Documents <- as.list(list.files(pattern = ".html",
                                full.names = TRUE))

Per_Year <- lapply(Documents,str_extract,pattern="\\d{4}")

Per_Year <- lapply(Per_Year,data.frame) %>% 
  bind_rows()
names(Per_Year) <- "year"
Download_Prices <- lapply(Documents,read_html)

find_tables <- function(html_documents){
  number_of_files <- 1:length(html_documents)
  Files <- list()
  for(i in seq_along(number_of_files)){
    Files[[i]] <- html_documents[[i]] %>% 
      html_elements("tbody") %>% 
      html_table()
    message("Tables found, proceeding to the next document.")
  }
return(Files)}

PP_20_2010 <- find_tables(html_documents = Download_Prices)

clean_prices <- function(x){
  number_of_files <- 1:length(x)
  Files <- list()
  for(i in seq_along(number_of_files)){
    Files[[i]] <- x[[i]][[6]]
    Files[[i]] <- Files[[i]][,1:8] %>% 
      mutate(year=Per_Year[i,1])
  }
return(Files)}

PP_20_2010 <- clean_prices(x=PP_20_2010)

PP_20_2010 <- lapply(PP_20_2010,data.frame) %>% 
  bind_rows()

PP_20_2010 <- as.data.frame(sapply(PP_20_2010,
                                   str_squish))
PP_20_2010 <- as.data.frame(sapply(PP_20_2010,
                                   str_trim))

Irrelevant <- data.frame(X1 =grep("In bond landed cost",
     PP_20_2010$X1,
     value = TRUE))


PP_20_2010 <- anti_join(PP_20_2010,
           Irrelevant)

Irrelevant <- data.frame(X1 =grep("^Y",
                                  PP_20_2010$X1,
                                  value = TRUE))

PP_20_2010 <- anti_join(PP_20_2010,
                        Irrelevant)

#=========================== Clean Columns ================================#
rm(list = c("Irrelevant","Download_Prices","Per_Year",
            "Year_20_2010","Documents"))

PP_20_2010 <- split(PP_20_2010,
                    PP_20_2010$year)

PP_20_2010 <- lapply(PP_20_2010,na.omit)

PP_20_2010[[10]] <- PP_20_2010[[10]][-1,]
PP_20_2010[[11]] <- PP_20_2010[[11]][-1,]

find_colnames <- function(x){
  per_year <- 1:length(x)
  y = list()
  for(i in seq_along(per_year)){
    y[[i]] <- x[[i]][1,]
    colnames(y[[i]]) <- y[[i]]
    y[[i]][,9] <- "year"
  }
return(y)}

columns <-find_colnames(PP_20_2010)

new_columns <- function(pp,col){
  list_sum <- 1:length(pp)
  for(i in seq_along(pp)){
    colnames(pp[[i]]) <- col[[i]] %>% 
      clean_names() %>% 
      na.omit()
    pp[[i]] <- pp[[i]][-1,]
    colnames(pp[[i]])[1] <-"month"
    pp[[i]] <- pp[[i]] %>% 
      clean_names()
    pp[[i]] <- pp[[i]] %>% 
      select(starts_with("month"),
             starts_with("petrol"),
             starts_with("year"))
  }
return(pp)}

PP_20_2010 <- new_columns(pp=PP_20_2010,
            col = columns)
rm("columns")

PP_20_2010 <- lapply(PP_20_2010,data.frame) %>% 
  bind_rows()

#====================== PDF Documents =====================================#
Results <- data.frame(year = 2011:2021,
           url = replicate(11,
                           "http://www.energy.gov.za/files/esources/petroleum/December2012/Basic_Fuel_Price.pdf"))

Results$url[1] <- "http://www.energy.gov.za/files/esources/petroleum/Dec2011/BasicFuelPrice.pdf"
Results$url[2] <- "http://www.energy.gov.za/files/esources/petroleum/December2012/Basic_Fuel_Price.pdf"
Results$url[3] <- "http://www.energy.gov.za/files/esources/petroleum/Dec-2013/Basic-Fuel-Price.pdf"
Results$url[4] <- "http://www.energy.gov.za/files/esources/petroleum/December2014/Basic-Fuel-Price.pdf"
Results$url[5] <- "http://www.energy.gov.za/files/esources/petroleum/December2015/Basic-Fuel-Price.pdf"
Results$url[6] <- "http://www.energy.gov.za/files/esources/petroleum/December2016/Basic-Fuel-Price.pdf"
Results$url[7] <- "http://www.energy.gov.za/files/esources/petroleum/December2017/Basic-Fuel-Price.pdf"
Results$url[8] <- "http://www.energy.gov.za/files/esources/petroleum/December2018/Basic-Fuel-Price.pdf"
Results$url[9] <- "http://www.energy.gov.za/files/esources/petroleum/December2019/Basic-Fuel-Price.pdf"
Results$url[10] <- "http://www.energy.gov.za/files/esources/petroleum/December2020/Basic-Fuel-Price.pdf"
Results$url[11] <- "http://www.energy.gov.za/files/esources/petroleum/April2021/Basic-Fuel-Price.pdf"

write_reports <- function(x){
  number_of_reports <- 1:length(x[,1])
  reports <- x[,2]
  for(i in seq_along(number_of_reports)){
   try(download.file(url = reports[i],
                  destfile = paste0(x[i,1],"_Petrol_Prices.pdf"),
                  method = "wininet",
                  mode="wb"))
    message(paste0(x[i,1])," report downloaded.")
  }
}

write_reports(x=Results)

Data_21_2020 <- lapply(as.list(list.files(pattern=".pdf")),
                    pdftools::pdf_data)

PP_21_2020 <- lapply(as.list(list.files(pattern = ".pdf")),
                     pdftools::pdf_text)

names(PP_21_2020) <- Results$year

change_to_df <- function(x){
  x <- data.frame(report = x) 
  return(x)
}

PP_21_2020 <- lapply(PP_21_2020,change_to_df)

expand_rows <- function(x){
  number_of_reports <- 1:length(x)
  Files <- list()
  for(i in seq_along(number_of_reports)){
    Files[[i]] <- x[[i]] %>% 
      unnest_lines(input =report,
                   output="per_row",
                   to_lower = FALSE) %>% 
    mutate(year = names(x)[[i]])}
return(Files)}

PP_21_2020 <- expand_rows(x=PP_21_2020)

PP_21_2020 <- lapply(PP_21_2020,
                     data.frame) %>% 
  bind_rows()


Relevant <- data.frame(per_row = grep("^[[:upper:]]{3}",
                          PP_21_2020$per_row,
                          value = TRUE))

PP_21_2020 <- semi_join(PP_21_2020,
          Relevant)

rm("Relevant")

Year_2011_2015 <- PP_21_2020 %>% 
  filter(year >= 2010,
         year <= 2015)

Year_2016_2020 <- anti_join(PP_21_2020,Year_2011_2015)

Year_2011_2015 <- Year_2011_2015 %>% 
  separate(per_row,
           into = c("month","petrol_93_unleaded","petrol_95_unleaded",
                    "diesel_0_05_sulphur","diesel_0_005_sulphur",
                    "illum_paraffin","exchange_rate_rand_dollar",
                    "average_dated_brent_crude"),
           sep = "\\s{1,}")

Year_2016_2020 <- Year_2016_2020 %>% 
  separate(per_row,
           into = c("month","petrol_93_unleaded","petrol_95_unleaded",
                    "diesel_0_05_sulphur","diesel_0_005_sulphur",
                    "illum_paraffin","maximum_refinery_gate_price","exchange_rate_rand_dollar",
                    "average_dated_brent_crude"),
           sep = "\\s{1,}")

Year_2016_2020 <- subset(Year_2016_2020,
                         !is.na(Year_2016_2020$petrol_93_unleaded))

PP_21_2020 <- bind_rows(Year_2011_2015 %>% 
                          select(c(month,petrol_93_unleaded,
                                   petrol_95_unleaded,year)),
                        Year_2016_2020 %>% 
                          select(c(month,petrol_93_unleaded,
                                   petrol_95_unleaded,
                                   year)))
rm(list= c("Year_2011_2015","Year_2016_2020",
           "Data_21_2020","Results"))


PP_20_2010 <- PP_20_2010 %>% 
  mutate(petrol_93_unleaded = as.double(petrol_93_unleaded),
         petrol_93_unleaded = ifelse(is.na(petrol_93_unleaded)==TRUE,
                                     petrol_93,petrol_93_unleaded),
         petrol_95_unleaded = ifelse(is.na(petrol_95_unleaded)==TRUE,
                                     petrol_95,petrol_95_unleaded)) %>% 
  select(c(month,contains("unleaded"),
           year))

PP_20_2010[96,2] = 441.632

PP_20_2021 <- PP_20_2010 %>% 
  bind_rows(PP_21_2020)

rm(list=ls()[-8])

PP_20_2021 <- inner_join(PP_20_2021,
data.frame(month = unique(PP_20_2021$month),
           value = 1:12))

PP_20_2021 <- PP_20_2021 %>% 
  mutate(new_date = ymd(paste0(year,"-",
                           value,"-",
                           1)),
         new_date = ceiling_date(new_date,
                                 "month")- days(1))

PP_20_2021 <- PP_20_2021 %>% 
  select(-c(month,year,value))

write_csv(x=PP_20_2021,
          file ="2000_2021_Basic_Fuel_Prices.csv")

PP_20_2021 <- PP_20_2021 %>% 
  mutate(petrol_93_unleaded = as.double(petrol_93_unleaded),
         petrol_95_unleaded = as.double(petrol_95_unleaded))

PP_20_2021 <- PP_20_2021 %>% 
  mutate(petrol_93_unleaded = round(petrol_93_unleaded/100,2),
         petrol_95_unleaded = round(petrol_95_unleaded/100,2))

Levies <- data.frame(extra_levies_tax = c("Fuel_Tax","Customs_Excise",
                                "RAF","Transport_Costs",
                                "Petroleum_Products_Levy",
                                "Wholesale_Margin","Secondary_Storage",
                                "Retail_Margin","DSML"),
           value = c(round(383.000/100,2),
                     round(4.000/100,2),round(218.000/100,2),
                     round(64.900/100,2),round(0.330/100,2),
                     round(40.500/100,2),round(27.200/100,2),
                     round(221.600/100,2),round(10.000/100,2)))
#================== Visualisation ========================================#
PP_20_2021 %>% 
  ggplot()+
  geom_line(aes(new_date,petrol_95_unleaded,group=1),
            color="#0f204b",alpha=0.8)