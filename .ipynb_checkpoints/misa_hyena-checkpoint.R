library(rvest)
library(tidyverse)

misa_hyena <- function(yyyymm, golden_time = T, ignore_past = F){

    uri_all <- paste0("https://www.hanam.go.kr/www/selectMisaParkResveWeb.do?key=4062&yyyymm=",yyyymm,"&misaParkCode=TS0",1:4,"&listType=D#n")
    
    df_list <- list()
    
    for(court in 1:4){
        
        tmp_uri <- uri_all[court]
        tmp_html <- read_html(tmp_uri, encoding = "UTF-8")
        tmp_tab <- tmp_html %>% html_nodes(".p-table") %>% html_table()
        tmp_round_time_df <- do.call(rbind.data.frame, strsplit(tmp_tab[[1]]$이용시간, "회"))
        names(tmp_round_time_df) <- c("회차", "이용시간")
        refined_df <- cbind(subset(tmp_tab[[1]], select = c(-이용시간)),
                              tmp_round_time_df) %>% select("일자","회차","이용시간","예약단체","신청하기")
        refined_df$요일 <- substring(tmp_tab[[1]]$일자, nchar(tmp_tab[[1]]$일자)-1, nchar(tmp_tab[[1]]$일자)-1)
        refined_df$일자 <- as.Date(substring(tmp_tab[[1]]$일자, 1, 10))
        refined_df$court <- court
        refined_df$회차 <- as.numeric(refined_df$회차)
        
        df_list[[paste0(court,'코트')]] <- refined_df
        
    }
    
    if(golden_time){
        golden_days <- c("일","월","화","수","목")
        golden_rounds <- c("7","8")
        
        golden_df_list <- list()
        
        for(court in 1:4){
            
            if(ignore_past){
                golden_df_list[[paste0(court,'코트')]] <- df_list[[court]] %>% filter(요일 %in% golden_days, 회차 %in% golden_rounds, 일자 >= Sys.Date()) %>% select("일자","요일","회차","이용시간","예약단체","신청하기")
            } else {
                golden_df_list[[paste0(court,'코트')]] <- df_list[[court]] %>% filter(요일 %in% golden_days, 회차 %in% golden_rounds) %>% select("일자","요일","회차","이용시간","예약단체","신청하기")
            }
            
        }
        
        res_list <- golden_df_list
        
        
    } else {
        
        res_list <- list()
        
        for(court in 1:4){
            
            if(ignore_past){
                res_list[[paste0(court,'코트')]] <- df_list[[court]] %>% filter(일자 >= Sys.Date()) %>% select("일자","요일","회차","이용시간","예약단체","신청하기")
            } else {
                res_list[[paste0(court,'코트')]] <- df_list[[court]] %>% select("일자","요일","회차","이용시간","예약단체","신청하기")
            }
            
        }
        
    }
    
    final_df <- res_list[[1]] %>% select("일자","요일","이용시간")
    for(court in 1:4){
        final_df[,paste0(court,"코트")]<- ifelse(res_list[[court]]$신청하기 == "신청가능", "신청가능","")
    }
    
    return(final_df)
    
    
}

