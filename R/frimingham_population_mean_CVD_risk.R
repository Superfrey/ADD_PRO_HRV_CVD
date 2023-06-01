install.packages("CVrisk")

library(CVrisk)

#Example on model
ascvd_10y_accaha(race = "aa", gender = "male", age = 65,
                 totchol = 213, hdl = 40, sbp = 140,
                 bp_med = 1, smoker=0, diabetes=0)

?ascvd_10y_accaha

# function
# REMEMBER: addapt function to your dataset variables

frimingham_risk_data <- function(data){

    df <- data %>%
        select(id, race, gender, age,
               totchol, hdl, sbp, bp_med, smoker, diabetes) # rename so it fits you dataset

    id_cvd_risk <- lapply(df$id, function(id_nr)) {

        id_cvd <- df %>%
            filter(id== "id_nr")


        cvd_risk <- ascvd_10y_accaha(race = id_cvd$race, gender = id_cvd$gender, age = id_cvd$age,
                         totchol = id_cvd$totchol, hdl = id_cvd$hdl, sbp = id_cvd$sbp,
                         bp_med = id_cvd$bp_med, smoker = id_cvd$smoker, diabetes = id_cvd$diabetes)

        rbind(id_nr, cvd_risk)

    }
    mean_CVD_risk_df  <-  do.call(rbind, id_cvd_risk)

    return(mean_CVD_risk_df)

}
