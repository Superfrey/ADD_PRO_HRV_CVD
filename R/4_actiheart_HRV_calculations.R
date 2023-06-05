files <- list.files(here("XXX"), pattern = "*.CSV", full.names = T)
data <- vroom(here("XXXX.csv"))

#Loading IBI + time generation functions

source(here::here("R/actiheart_ibi_time_data_generation.R"))
source(here::here("R/hrv_function_est.R"))
source(here:here("R/ibi_diff_est.R"))


#############
#Function for extracting ID number from file name
extract_prono <- function(paths){
    paths %>%
        fs::path_file() %>%
        stringr::str_extract("#.*#") %>%
        stringr::str_to_lower() %>%
        stringr::str_remove_all("#|pro")
}



    ##


    vroom_hrv <- function(filepath) {
        data <- vroom(filepath)
        pro_no <- extract_prono(filepath)

        ibi_hour <- data  %>%
            mutate(time_hour = as.numeric(nrow(data))/2/60)

        ibi <-  data %>%
          actiheart_time_day() %>%
          ibi_function()

        ibi_diff <- ibi_diff_data(data)

        # week HRV
        ibi_diff_week <- acti_ibi_diff_week(ibi_diff)
        # ibi_diff_week <- ibi_diff_week %>%
        #    select(mean_ibi_diff, median_ibi_diff, mean_ratio_ibi_diff)
        ibi_diff_week<- cbind(pro_no, ibi_diff_week)


        hrv_time_domain_week <- rhrv_file_time_domain_week(ibi)
        hrv_time_domain_week <- cbind(pro_no, hrv_time_domain_week)

        # day HRV
        ibi_diff_day <- acti_ibi_diff_day(ibi_diff)
        #ibi_diff_day <- ibi_diff_day %>%
        #   select(mean_ibi_diff, median_ibi_diff, mean_ratio_ibi_diff)
        ibi_diff_day <- cbind(pro_no, ibi_diff_day)

        hrv_time_domain_day <- rhrv_file_time_domain_day(ibi)
        hrv_time_domain_day <- cbind(pro_no, hrv_time_domain_day)

        # hour HRV
        ibi_diff_hour <- acti_ibi_diff_hour(ibi_diff)
        ibi_diff_hour<- cbind(pro_no, ibi_diff_hour)


        hrv_time_domain_hour <- rhrv_file_time_domain_hour(ibi)
        hrv_time_domain_hour <- cbind(pro_no, hrv_time_domain_hour)

        # circadian HRV
        ibi_diff_circ <- acti_ibi_diff_circadian(ibi_diff)
        ibi_diff_circ <- cbind(pro_no, ibi_diff_circ)


        hrv_time_domain_circ <- rhrv_file_time_domain_circadian(ibi)
        hrv_time_domain_circ <- cbind(pro_no, hrv_time_domain_circ)

        # circ per day
        ibi_diff_circ_day <- acti_ibi_diff_circ_day(ibi_diff)
        ibi_diff_circ_day <- cbind(pro_no, ibi_diff_circ_day)

        hrv_time_domain_circ_day <- rhrv_file_time_domain_circ_day(ibi)
        hrv_time_domain_circ_day <- cbind(pro_no, hrv_time_domain_circ_day)

        # hour HRV per day
        ibi_diff_hour_day <- acti_ibi_diff_hour_day(ibi_diff)
        ibi_diff_hour_day<- cbind(pro_no, ibi_diff_hour_day)



        hrv_time_domain_hour_day <- rhrv_file_time_domain_hour_day(ibi)
        hrv_time_domain_hour_day <- cbind(pro_no, hrv_time_domain_hour_day)


        hrv_data <- list(week = hrv_time_domain_week, daily = hrv_time_domain_day, hourly = hrv_time_domain_hour, circadian = hrv_time_domain_circ, hourly_per_day = hrv_time_domain_hour_day,
                         circadian_per_day = hrv_time_domain_circ_day, ibi_week = ibi_diff_week, ibi_day = ibi_diff_day , ibi_hour = ibi_diff_hour,
                         ibi_circ = ibi_diff_circ, ibi_circ_day = ibi_diff_circ_day, ibi_hour_day = ibi_diff_hour_day)
        return(hrv_data)
    }


    values1 <- vroom_hrv(files[6])
    values2 <- vroom_hrv(files[3])


## Function looping multiple csv files and row binding each individual into a complete data frame

hrv_loop_folder <- function(folder){

    data_hrv <- map(folder, possibly(vroom_hrv))

   # data_hrv <- do.call(Map, c(f = rbind, data_hrv))

    return(data_hrv)
}


################################################# For check

hrv_loop_save_folder<- function(folder){

    Error_messages <- c()


    for (i in folder){
     tryCatch({

            data_hrv <- vroom_hrv(i)

            id_pro <- extract_prono(i)
            pro_no_safe <- paste0("data/hrv/hrv_id/","pro_no_",id_pro,".rds")

            saveRDS(data_hrv, file = here::here(pro_no_safe))




      }, error = function(e) {

    Error_messages <- c(Error_messages,
                       paste("Hello Buddy Error encountered at csv file", i,":",
                             e$message))

    next
    })
    }

    error_df <- data.frame(Error_massage = Error_messages)

    write.csv(error_df, file = here::here("data/hrv/error_files/error_messages.csv"), row.names = FALSE)


}

####################################### For the long run

hrv_loop_save_folder<- function(folder){

    Error_messages <- c()
    Completed_iterations <- 0


    for (i in folder){
        tryCatch({

            data_hrv <- vroom_hrv(i)

             # check if completed
            Completed__iteration <- Completed__iteration + 1




            id_pro <- extract_prono(i)
            pro_no_safe <- paste0("data/hrv/hrv_id/","pro_no_",id_pro,".rds")

            saveRDS(data_hrv, file = here::here(pro_no_safe))



        }, error = function(e) {

            Error_messages <- c(Error_messages,
                                paste("Error encountered at csv file", i,":",
                                      e$message))

            next
        })

        if (i %% 1 == 0) {
            tryCatch({
                error_df <- data.frame(
                    Iterations = 1:lenght(Error_messages),
                    error_message = Error_messages,
                    Completed_iterations = Completed__iteration
                )

            write.csv(error_df, file = here::here("data/hrv/error_files/error_messages.csv"), row.names = FALSE)

        }, error =function(e) {
            cat("error occurred while saving error data:", e$message, "\n")
        })

    }
}

 final_error_df <- data.frame(Iterations = 1:lenght(Error_messages),
                        error_message = Error_messages,
                        Completed_iterations = Completed__iteration
                        )

write.csv(final_error_df, file = here::here("data/hrv/error_files/error_messages.csv"), row.names = FALSE)

}

# 1- 350 saved

hrv_loop_save_folder(files[351:400])

## Run function for the folder
hrv_data_sim1_td <- hrv_loop_folder(files[1:50]) #
saveRDS(hrv_data_sim1_td, file = here("data/hrv_50t.rds"))




# load and combine ID HRV

hrv_files <- list.files(here("data/hrv/hrv_id"), pattern = "*.rds", full.names = T)


HRV_combine <- function(hrv_path) {
    hrv_df <- map(hrv_path, readRDS)

    neasted_hrv_df <- do.call(Map, c(f=rbind, hrv_df))

    return(neasted_hrv_df)

}


data_hrv_nested <- HRV_combine(hrv_files)

### Data frames

hrv_week_df <- data_hrv_nested$week
ibi_week_df <- data_hrv_nested$ibi_week
hrv_week_df <- merge(hrv_week_df, ibi_week_df, by = "pro_no", all.x = TRUE)

hrv_day_df <- data_hrv_nested$daily %>%
    select(c(1:14))

ibi_day_df <- data_hrv_nested$ibi_day
hrv_day_df <- merge(hrv_day_df, ibi_day_df, by = "pro_no", all.x = TRUE, no.dups = FALSE)


hrv_hour_day_df <- data_hrv_nested$hourly_per_day
ibi_hour_day_df <- data_hrv_nested$ibi_hour_day
hrv_hour_day_df <- merge(hrv_hour_day_df, ibi_hour_day_df, by = "pro_no" , all.x = TRUE)


hrv_circadian_day_df <- data_hrv_nested$circadian_per_day
ibi_circadian_day_df <- data_hrv_nested$ibi_circ_day
hrv_circadian_day_df <- merge(hrv_circadian_day_df, ibi_circadian_day_df, by = "pro_no" , all.x = TRUE)

