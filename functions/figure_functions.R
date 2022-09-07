##
# Tables

format_table <- function(dt){
    year_cols <- c("birth_ym", "IBORN_YM", "ISURVEY_YM")
    setorder(dt, COUNTRY)
    dt[, c(year_cols) := lapply(.SD, year), .SDcols = year_cols]
    dt[,
        mother_by := paste0(min(IBORN_YM),"-", max(IBORN_YM)),
        by = "COUNTRY"
    ][,
        child_by := paste0(min(birth_ym),"-", max(birth_ym)),
        by = "COUNTRY"
    ][,
        survey_years := paste0(min(ISURVEY_YM),"-", max(ISURVEY_YM)),
        by = "COUNTRY"
    ][,
        n_kids := .N,
        by = "COUNTRY"
    ]

   tab <-  unique(dt[, c("COUNTRY", "survey_years", "mother_by", "child_by", "n_kids")])
   datasummary_df(tab, "latex_tabular", booktabs = TRUE, fmt = identity)
}


