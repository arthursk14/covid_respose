###################################################
## database
###################################################
  
  # Packages
  
      require(tidyverse)
      require(readxl)
      require(SciViews)
  
  # Database
      
      OWID <- as.tibble(read.csv("owid-covid-data.csv"))
      stringency  <- as.tibble(read.csv("covid-stringency-index.csv"))
      retail <- as.tibble(read.csv("change-visitors-retail-recreation.csv"))
      
      IMF <- as.tibble(read_excel("IMF.xlsx"))
      
  # Filter
      
      filter <- OWID %>% filter(date=="2021-05-31",
                                !is.na(total_deaths_per_million))
      
      stringency <- stringency %>% filter(as.Date(Day) <= "2021-05-31" & as.Date(Day) >= "2020-03-01")
      stringency <- stringency %>% group_by(Code) %>% summarise(stringency_index = mean(stringency_index))
  
      retail <- retail %>% filter(as.Date(Day) <= "2021-05-31" & as.Date(Day) >= "2020-03-01")
      retail <- retail %>% group_by(Code) %>% summarise(retail_and_recreation = mean(retail_and_recreation))
      
  # Join ISO code
      
      join <- inner_join(filter,IMF, by=c("iso_code"="ISO")) %>% 
              inner_join(.,stringency, by=c("iso_code"="Code")) %>%
              inner_join(.,retail, by=c("iso_code"="Code"))
        
  # Utilized columns
      
      database <- join %>% dplyr::select(iso_code, 
                                  total_cases_per_million,
                                  total_deaths_per_million, 
                               
                                  delta_GDP,
                                  delta_GGD,
                               
                                  median_age,
                                  aged_65_older,
                               
                                  stringency_index.y,
                                  retail_and_recreation,
                                  people_fully_vaccinated_per_hundred
                               )
      
    # People fully vaccinated
      
      median(database$people_fully_vaccinated_per_hundred[which(!is.na(database$people_fully_vaccinated_per_hundred))])
      max(database$people_fully_vaccinated_per_hundred[which(!is.na(database$people_fully_vaccinated_per_hundred))])
      quantile(database$people_fully_vaccinated_per_hundred[which(!is.na(database$people_fully_vaccinated_per_hundred))],0.75)
      
      database <- database %>% dplyr::select(iso_code, 
                                  total_cases_per_million,
                                  total_deaths_per_million, 
                                  
                                  delta_GDP,
                                  delta_GGD,
                                  
                                  median_age,
                                  aged_65_older,
                                  
                                  stringency_index.y,
                                  retail_and_recreation
                               )
     
  # Last filter
      
      data <- database %>% filter(total_cases_per_million>=500, !(delta_GDP==0 & delta_GGD==0), !(iso_code %in% c("ZWE", "NIC", "HTI", "JOR", "KWT")))
      