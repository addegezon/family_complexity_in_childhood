##
# Tables

format_table <- function(dt){
    library(modelsummary)

    setorder(dt, COUNTRY)

    # Format dates to year
    year_cols <- c("birth_ym", "IBORN_YM", "ISURVEY_YM")
    dt[, c(year_cols) := lapply(.SD, year), .SDcols = year_cols]

    # Create table variables
    dt[,
        ':='(
            mother_min = min(IBORN_YM),
            mother_max = max(IBORN_YM),
            child_min = min(birth_ym),
            child_max = max(birth_ym),
            svy_min = min(ISURVEY_YM),
            svy_max = max(ISURVEY_YM),
            n_kids = .N
        ),
        by = "COUNTRY"
    ]


    collapsed <-  unique(dt[, 
        c(
            "COUNTRY",
            "svy_min",
            "svy_max",
            "mother_min",
            "mother_max",
            "child_min",
            "child_max",
            "n_kids"
        )
    ])

    # Split country and survey
    survey_components <- "(^[A-z]+)(\\sRepublic)?(\\s)(.+)"
    collapsed[,
        ':=' (
                survey = gsub(survey_components, "\\4" , COUNTRY),
                COUNTRY = gsub(survey_components, "\\1\\2", COUNTRY)
            )
    ]

    # Add Total row
    total_row <- data.table(
        "COUNTRY" = "Total",
        "svy_min" = collapsed[, min(svy_min)],
        "svy_max" = collapsed[, max(svy_min)],
        "mother_min" = collapsed[, min(mother_min)],
        "mother_max" = collapsed[, max(mother_max)],
        "child_min" = collapsed[, min(child_min)],
        "child_max" = collapsed[, max(child_max)],
        "n_kids" = collapsed[, sum(n_kids)],
        "survey" = ""
    )

    collapsed <- rbindlist(list(collapsed, total_row))

    # Rename columns
    table <- collapsed[, .(
        "{Country}" = COUNTRY,
        "{Survey}" = survey,
        "{Survey years}" = paste0(svy_min, " - ", svy_max),
        "{Mother cohorts}" = paste0(mother_min, " - ", mother_max),
        "{Child cohorts}" = paste0(child_min, " - ", child_max),
        "{N. children}" = format(n_kids)
        ) 
    ]

    # Create table
    tab <- datasummary_df(
        table,
        "latex_tabular",
        booktabs = TRUE,
        fmt = identity,
        align = "llcccr",
        hrule = nrow(table)
    )

    return(tab)
}


