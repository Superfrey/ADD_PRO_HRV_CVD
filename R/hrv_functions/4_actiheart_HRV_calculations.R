files <- list.files(here("XXX"), pattern = "*.CSV", full.names = T)
data <- vroom(here("XXXX.csv"))

#Loading IBI + time generation functions

source(here::here("R/hrv_function/1_generate_IBI_data.R"))
source(here::here("R/hrv_function/2_hrv_indices_calculations_functions.R"))


#############
#Function for extracting ID number from file name
extract_prono <- function(paths){
    paths %>%
        fs::path_file() %>%
        stringr::str_extract("#.*#") %>%
        stringr::str_to_lower() %>%
        stringr::str_remove_all("#|pro")
}


## Calculating HRV measures for individual and include id number


    vroom_hrv <- function(filepath) {
        data <- vroom(filepath)
        pro_no <- extract_prono(filepath)


        ibi <-  data %>%
          create_time_day_variables() %>%
          calculate_ibi()

        # week HRV

        hrv_time_domain_week <- rhrv_time_domain_week(ibi)
        hrv_time_domain_week <- cbind(pro_no, hrv_time_domain_week)

        # day HRV

        hrv_time_domain_day <- rhrv_time_domain_day(ibi)
        hrv_time_domain_day <- cbind(pro_no, hrv_time_domain_day)

        # circ per day

        hrv_time_domain_circ_day <- rhrv_time_domain_circ_day(ibi)
        hrv_time_domain_circ_day <- cbind(pro_no, hrv_time_domain_circ_day)

        # hour HRV per day

        hrv_time_domain_hour_day <- rhrv_time_domain_hour_day(ibi)
        hrv_time_domain_hour_day <- cbind(pro_no, hrv_time_domain_hour_day)


        hrv_data <- list(week = hrv_time_domain_week, daily = hrv_time_domain_day, hourly_per_day = hrv_time_domain_hour_day,
                         circadian_per_day = hrv_time_domain_circ_day)
        return(hrv_data)
    }

#########################

    hrv_id_file <- function(filepath) {
        data_hrv <- vroom_hrv(filepath)
        id_pro <- extract_prono(filepath)
        pro_no_safe <- paste0("data/hrv/hrv_id_2023/","pro_no_",id_pro,".rds")

        saveRDS(data_hrv, file = here::here(pro_no_safe))

        }


## Function looping multiple csv files and row binding each individual into a complete data frame

hrv_loop_folder <- function(folder){

        plan(multisession, workers = 10)

    furrr::future_map(folder,hrv_id_file,
    .options = furrr::furrr_options(seed = TRUE))
}

hrv_data <- hrv_loop_folder(files[10])


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

hrv_day_df <- data_hrv_nested$daily

hrv_hour_day_df <- data_hrv_nested$hourly_per_day

hrv_circadian_day_df <- data_hrv_nested$circadian_per_day
