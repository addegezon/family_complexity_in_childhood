##
# Data preparation

# Remove unused variables, convert to proper date format and 
# generate new partner variables
format_data <- function(dt) {
    
    # Define variables to keep
    keep_vars <- c(
        "RESPID",
        "COUNTRY",
        "YEAR_S",
        "IMONTH_S",
        "SEX",
        "BORN_Y",
        "IBORN_M",
        grep("^UNION_[0-9]+?", colnames(dt), value = TRUE),
        grep("^SEP_[0-9]+?", colnames(dt), value = TRUE),
        grep("^MARR_[0-9]+?", colnames(dt), value = TRUE),
        grep("^DIV_[0-9]+?", colnames(dt), value = TRUE),
        grep("^UNION_Y", colnames(dt), value = TRUE),
        grep("^IUNION_M", colnames(dt), value = TRUE),
        grep("^SEP_Y", colnames(dt), value = TRUE),
        grep("^ISEP_M", colnames(dt), value = TRUE),
        grep("^MARR_Y", colnames(dt), value = TRUE),
        grep("^IMARR_M", colnames(dt), value = TRUE),
        grep("^DIV_Y", colnames(dt), value = TRUE),
        grep("^IDIV_M", colnames(dt), value = TRUE),
        grep("^KID", colnames(dt), value = TRUE),
        grep("^IKID", colnames(dt), value = TRUE),
        "EDU_3",
        "NATIVE"
    )

    # Keep only selected variables
    dt <- dt[, ..keep_vars]

    # Convert all missings to NA
    dt <- zap_missing(dt)

    # Convert haven_labelled to factor:
    # First those where to keep the defaults
    label_cols <- c("SEX", "COUNTRY", "EDU_3", "NATIVE")
    dt[,
        (label_cols) := lapply(
                            .SD,
                            as_factor,
                            levels = "default"
                        ), 
        .SDcols = label_cols
    ]
    # Then those where to keep the values
    value_cols <- setdiff(colnames(dt), label_cols)
    dt[,
        (value_cols) := lapply(
                            .SD,
                            zap_labels
                        ), 
        .SDcols = value_cols
    ]

    # Drop countries
    drop_countries <- grep(
        "^Canada|^Germany|^Moldova|^Russia|^UK|^USA|^Uruguay|^Kazakhstan|^Georgia",
        levels(dt[,COUNTRY]),
        value = TRUE
    )

    dt <- dt[!COUNTRY %in% drop_countries]
    
    # Separate COUNTRY from survey
    survey_components <- "(^[A-z]+)(\\sRepublic)?(\\s)(.+)"
    dt[,
        ':=' (
                survey = gsub(survey_components, "\\4" , COUNTRY),
                COUNTRY = gsub(survey_components, "\\1\\2", COUNTRY)
            )
    ]
    
    # Define regions
    dt[
        COUNTRY == "France" |
        COUNTRY == "Netherlands" |
        COUNTRY == "Belgium" |
        COUNTRY == "Austria",
        region := "Western Europe"
    ]
    dt[
        COUNTRY == "Bulgaria" |
        COUNTRY == "Belarus" |
        COUNTRY == "Czech Republic" |
        COUNTRY == "Estonia" |
        COUNTRY == "Lithuania" |
        COUNTRY == "Hungary" |
        COUNTRY == "Romania" |
        COUNTRY == "Poland",
        region := "Post-socialist"
    ]
    dt[
        COUNTRY == "Norway" |
        COUNTRY == "Sweden",
        region := "Scandinavia" 
    ]
    dt[
        COUNTRY == "Spain" |
        COUNTRY == "Italy",
        region := "Southern Europe"
    ]

    # Merge all year and month columns into one date column
    # For partnership history:
    format_date(dt, c("UNION_", "SEP_", "MARR_", "DIV_"), 1:10)
    # For child history:
    format_date(dt, c("KID_", "KID_D", "KID_L"), 1:16)
    # For time of interview and birth:
    dt[
        survey == "SFS 2018",
        ISURVEY_YM := as.Date(
            paste(YEAR_S, "01", "1"),
            format = "%Y %m %d"
        )
    ]
    dt[
        survey != "SFS 2018", 
        ISURVEY_YM := as.Date(
            paste(YEAR_S, IMONTH_S, "1"),
            format = "%Y %m %d"
        )    
    ]
    dt[, IBORN_YM := as.Date(
                        paste(BORN_Y, IBORN_M, "1"),
                        format = "%Y %m %d"
                    )]

    # Remove old date cols
    old_date_cols <- grep(
                        "Y[0-9]+?$|[^Y]M[0-9]+?$",
                        colnames(dt),
                        value = TRUE
                    )
    dt[,
        (old_date_cols) := NULL
    ]

    # Generate partner status and dates of splitting up,
    # disregarding marriage status
    for (i in 1:10){
        union <- paste0("UNION_", i)
        union_ym <- paste0("IUNION_YM", i)
        marriage <- paste0("MARR_", i)
        marriage_ym <- paste0("IMARR_YM", i)
        partner <- paste0("PARTNER_", i)
        partner_ym <- paste0("IPARTNER_YM", i)
        divorce <- paste0("DIV_", i)
        divorce_ym <- paste0("IDIV_YM", i)
        separation <- paste0("SEP_", i)
        separation_ym <- paste0("ISEP_YM", i)
        split <- paste0("SPLIT_", i)
        split_ym <- paste0("ISPLIT_YM", i)

        dt[ # Those with no partner i
            (get(union) == 0 & (get(marriage) == 0 | is.na(get(marriage)))) |
                (get(marriage) == 0 & (get(union) == 0 | is.na(get(union)))),
            eval(partner) := 0
        ][  # Those with partner i
            get(union) == 1 | get(marriage) == 1,
            eval(partner) := 1
        ][ # Those with no separation i
            (get(separation) == 0 & (get(divorce) == 0 | is.na(get(divorce)))) |
                (get(divorce) == 0 & (get(separation) == 0 | is.na(get(separation)))),
            eval(split) := 0
        ][  # Those with separation i
            get(separation) == 1 | get(divorce) == 1,
            eval(split) := 1
        ][  # Date of partnership formation with partner i
            !is.na(get(union_ym)) | !is.na(get(marriage_ym)),
            eval(partner_ym) := pmin(get(union_ym), get(marriage_ym), na.rm = TRUE)
        ][  # Date of separation with partner i
            !is.na(get(separation_ym)) | !is.na(get(divorce_ym)),
            eval(split_ym) := pmin(get(separation_ym), get(divorce_ym), na.rm = TRUE)
        ][  # If there is date of split but no split dummy, set it to 1
            !is.na(get(split_ym)),
            eval(split) := 1
        ][  # If there is no indicator of union but a date, set it to 1
            !is.na(get(partner_ym)),
            eval(partner) := 1

        ][  # If there is a partner but no split, set split to 0
            is.na(get(split)) & get(partner) == 1,
            eval(split) := 0
        ]
    }

    # Create variable coding for which partner is the parent of the child
    # If child is born within the union, the partner is considered to be the parent
    for (kid in 1:16){
        parent_kid <- paste0("PARENT_KID_", kid)
        kid_ym <- paste0("IKID_YM", kid)
        child <- paste0("KID_", kid)
        
        for (par in 1:10){
                partner_ym <- paste0("IPARTNER_YM", par)
                split_ym <- paste0("ISPLIT_YM", par)
                split <- paste0("SPLIT_", par)

            dt[
                (get(kid_ym) >= get(partner_ym)) &
                    ((get(kid_ym) <= get(split_ym)) | get(split) == 0) &
                    get(child) == 1,
                eval(parent_kid) := par
            ]
        }

        # If kid exists but parent is unknown, set parent to 0
        dt[
            get(child) == 1 & is.na(get(parent_kid)),
            eval(parent_kid) := 0
        ]
    }

    return(dt)
}

# Function for combining year and month columns into single date column
format_date <- function (dt, columns, range) {

    for(var in columns){
        for (i in range) {
            year_col <- paste0(var, "Y", i)
            month_col <- paste0("I", var, "M", i)
            dt[,
                paste0("I", var, "YM", i) :=
                    ymd(
                        paste(
                            get(year_col),
                            get(month_col),
                            "1"
                        )
                    )
            ]
        }
    }

    return(dt) 
}

# Reshape data.table to use children as anchor
melt_children <- function(dt){

    # Create copy of data wide data for use at the end of the function
    child_info <- copy(dt)

    # Columns to melt
    child_exists <- paste0("KID_", 1:16)
    gender <- paste0("KID_S", 1:16)
    dead <- paste0("KID_D", 1:16)
    left_home <- paste0("KID_L", 1:16)
    birth_ym <- paste0("IKID_YM", 1:16)
    death_ym <- paste0("IKID_DYM", 1:16)
    left_ym <- paste0("IKID_LYM", 1:16)
    parent_kid <- paste0("PARENT_KID_", 1:16)

    dt   <- melt(
                dt,
                measure = list(
                    child_exists,
                    gender,
                    dead,
                    left_home,
                    birth_ym,
                    death_ym,
                    left_ym,
                    parent_kid
                ),
                value.name = c(
                    "child_exists",
                    "gender",
                    "dead",
                    "left_home",
                    "birth_ym",
                    "death_ym",
                    "left_ym",
                    "parent"
                )
            )

    # Only keep those rows representing an existing child
    dt <- dt[child_exists == 1]

    # Rename "variable" which shows which KID_X is used
    setnames(dt, "variable", "child")

    # Generate child ID
    dt[, childID := .I]

    # Merge information on siblings
    child_cols <- c("RESPID", grep("KID", colnames(child_info), value = TRUE))
    child_info[, ..child_cols]
    merged <- merge(
        dt,
        child_info[, ..child_cols],
        by = "RESPID"
    )

    # Set to NA those cells representing the anchor child
    for (i in 1:16){
        anchor_cols <- grep(
                        paste0("KID.+", i, "$"),
                        colnames(merged),
                        value = TRUE
                        )
        merged[
            child == i,
            (eval(anchor_cols)) := NA
        ]

    }


    return(merged)
}

# Tag parents to be dropped
tag_children <- function(dt){
    library(lubridate)
   

    # Mark missing birth date
    dt[
        is.na(birth_ym),
        ':='(drop = TRUE, drop_reason = "Missing birth date")
    ]

    # Mark those born before 1980
    dt[
        year(birth_ym) < 1980,
        ':='(drop = TRUE, drop_reason = "Born before 1980")
    ]

    # Mark those born before mother 
    dt[
        birth_ym < IBORN_YM,
        ':='(drop = TRUE, drop_reason = "Incorrectly reported birth date")
    ]

    # Mark those younger than 15
    dt[
        int_length(
            interval(
                birth_ym,
                ISURVEY_YM
            ) %/% months(1)
        ) < 180 & is.na(drop),
        ':='(drop = TRUE, drop_reason = "Younger than 15")
    ]

    # Mark those who died before 15
    dt[
        int_length(
            interval(
                birth_ym,
                death_ym
            ) %/% months(1)
        ) < 180 & is.na(drop),
        ':='(drop = TRUE, drop_reason = "Died before age 15")
    ]

    # Mark those who left home before 15 
        dt[
        int_length(
            interval(
                birth_ym,
                left_ym
            ) %/% months(1)
        ) < 180 & is.na(drop),
        ':='(drop = TRUE, drop_reason = "Left home before age 15")
    ]

    return(dt)

}

# Tag parents to be dropped
tag_parents <- function(dt) {

    # Mark men
    dt <- dt[
        SEX == "Male",
        ':='(
            drop = TRUE,
            drop_reason = "Remove men"
        )
    ]

    # Mark childless women
    kid_cols <- paste0("KID_", 1:16)
    dt[, kid.tmp := rowSums(.SD, na.rm = TRUE), .SDcols = kid_cols]
    dt <- dt[
            kid.tmp == 0 & is.na(drop),
            ':='(drop = TRUE, drop_reason = "Remove childless")
            ][,
                kid.tmp := NULL
            ]

    # Mark missing birth information
    # dt <- dt[
    #         is.na(IBORN_YM),
    #         ':='(drop = TRUE, drop_reason = "Missing time of birth")
    #     ]

    # Mark missing partnership dates
    for (i in 1:10){
        partner <- paste0("PARTNER_", i)
        partner_ym <- paste0("IPARTNER_YM", i)
        split <- paste0("SPLIT_", i)
        split_ym <- paste0("ISPLIT_YM", i)

        # Mark those with missing info on date of partnership formation
        dt <- dt[
                !is.na(get(partner)) & 
                get(partner) == 1 & 
                is.na(get(partner_ym)) &
                is.na(drop),
                ':='(drop = TRUE, drop_reason = "Missing partnership dates")
            ]
        # Mark those with missing info on date of partnership dissolution
        dt <- dt[
                !is.na(get(partner)) & 
                    get(split) == 1 & 
                    is.na(get(split_ym)) &
                    is.na(drop),
                ':='(
                    drop = TRUE,
                    drop_reason = "Missing partnership dates"
                )
            ]
    }

    # Mark those with overlapping relationships
    for (i in 1:9) {
        next_partner <- paste0("IPARTNER_YM", i + 1)
        split <- paste0("SPLIT_", i)
        split_ym <- paste0("ISPLIT_YM", i)
        
        dt[
            (get(split_ym) > get(next_partner)) & 
                get(split) == 1 & 
                is.na(drop),
            ':='(
                drop = TRUE,
                drop_reason = "Overlapping relationships"
            )
        ]

    }

    # Mark those with inconsistent ordering of partnership formation and dissolution
    for (i in 1:10){
        partner_ym <- paste0("IPARTNER_YM", i)
        split_ym <- paste0("ISPLIT_YM", i)

        dt[
            get(split_ym) < get(partner_ym) & is.na(drop),
            ':='(drop = TRUE, drop_reason = "Relationship reported to end before start")
        ]
    }

    # Mark those where relationships are reported to begin before birth
    for (i in 1:10){

        dt[
            IBORN_YM > get(partner_ym) & is.na(drop),
            ':='(drop = TRUE, drop_reason = "Relationship started before birth")
        ]
    }
    return(dt)

}

##
# Sequence generation

# Generate partnership history
gen_union_states <- function(dt){
    library(TraMineR)
    library(lubridate)
    # Cycle through each month
    for (month in seq(0, 179, by = 3)){
        
        # Print which month is being processed for trackin progress
        print(
            paste("Processing month", month)
        )
        
        # Define name of partner state variable for the month
        partner_state <- paste0("PARTNER_STATE", month)

        # Cycle through each potential partner
        for (par in 1:10){
            partner_ym <- paste0("IPARTNER_YM", par)
            split <- paste0("SPLIT_", par)
            split_ym <- paste0("ISPLIT_YM", par)
            
            # Set partner state for the month = 1 if partnered with par
            # during the month
            dt[
                birth_ym + months(month) >= get(partner_ym) &
                    (birth_ym + months(month) < get(split_ym) | get(split) == 0 | is.na(split)),
                eval(partner_state) := par  
                
            ]
        }

        # Convert NA to single 
        dt[
            is.na(get(partner_state)),
            eval(partner_state) := 0
        ]

        # Generate family state columns
        family_state <- paste0("FAMILY_STATE", month)
        dt[
            get(partner_state) == PARTNER_STATE0,
            eval(family_state) := "OP"
        ][
            get(partner_state) != 0 & get(partner_state) != PARTNER_STATE0,
            eval(family_state) := "Step"
        ][
            get(partner_state) == 0,
            eval(family_state) := "Single"
        ]
    }

    # # Add family state DSS column
    # sequence_columns <- grep(
    #                         "FAMILY_STATE",
    #                         colnames(dt),
    #                         value=TRUE
    #                     )
    # # Define sequence object
    # dt.seq <- seqdef(dt, var = sequence_columns)

    # # Save sequence as DSS-string
    # dss <- as.data.table(
    #         seqformat(
    #             dt.seq,
    #             to ="DSS",
    #             compress=TRUE
    #         )
    #     )

    # # Bind the DSS to the data.table
    # dt <- cbind(dt, dss)

    return(dt)
}

gen_sibling_states <- function(dt){

    for (month in seq(0, 179, by = 3)){
        print(
            paste("Processing month", month)
        )

        fullsibling_state <- paste0("FULLSIBLING_STATE", month)
        dt[, eval(fullsibling_state) := 0]

        halfsibling_state <- paste0("HALFSIBLING_STATE", month)
        dt[, eval(halfsibling_state) := 0]

        for(i in 1:16) {
            sibling_ym <- paste0("IKID_YM", i)
            parent_sibling <- paste0("PARENT_KID_", i)

            dt[
                birth_ym + months(month) >= get(sibling_ym) &
                   PARTNER_STATE0 == get(parent_sibling) &
                   PARTNER_STATE0 != 0,
                eval(fullsibling_state) := get(fullsibling_state) + 1  
                
            ][
                birth_ym + months(month) >= get(sibling_ym) &
                    (PARTNER_STATE0 != 
                        get(parent_sibling) | PARTNER_STATE0 == 0),
                eval(halfsibling_state) := get(halfsibling_state) + 1
            ]
        }

    }

    return(dt)
}

##
# Analysis functions

# Find intact family trajectories
define_subsamples <- function(dt, size = 10000){

    # Full sample
    dt[,
        full_sample := TRUE
    ]

    # Complex families
    partner_cols <- paste0("FAMILY_STATE", seq(0, 179, by = 3))
    dt[,
        complex_sample := apply(
            .SD,
            1,
            function(x) {
                if (any(x != "OP")) {
                    TRUE
                } else FALSE
            }
        ),
        .SDcols = partner_cols
    ]

    # Random subsample
    dt[
        sample(.N, size),
        random_sample := TRUE
    ][
        is.na(random_sample),
        random_sample := FALSE
    ]

    return(dt)

}