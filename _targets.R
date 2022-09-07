library(targets)
source("functions/data_functions.R")
source("functions/figure_functions.R")


tar_option_set(
    packages = c(
        "data.table",
        "fst",
        "haven",
        "lubridate"
    )
)

list(
    
    ##
    # Data processing

    # Load Harmonized histories .dta file and save as fst_dt
    tar_target(
        f_hh_dta,
        file.path("data", "HARMONIZED-HISTORIES_ALL_GGSaccess.dta"),
        format = "file"
    ),
    tar_target(
        df_hh,
        read_dta(f_hh_dta, encoding = "latin1"),

    ),
    tar_target(
        dt_hh,
        as.data.table(df_hh)
    ),

    # Remove unused variables
    tar_target(
        hh_processed,
        format_data(dt_hh),
        format = "fst_dt",
    ),

    # Tag which observation to filter out
    tar_target(
        parents_tagged,
        tag_parents(hh_processed),
        format = "fst_dt"
    ),

    # Filter out adult not to be included in analysis
    tar_target(
        parents_filtered,
        parents_tagged[is.na(drop)],
        format = "fst_dt"
    ),

    # Reformat data to use each child as anchor
    tar_target(
        child_anchor,
        melt_children(parents_filtered),
        format = "fst_dt"
    ),

    # Tag which children to filter out
    tar_target(
        children_tagged,
        tag_children(child_anchor),
        format = "fst_dt"
    ),

    # Filter out children not be included in analysis
    tar_target(
        children_filtered,
        children_tagged[is.na(drop)],
        format = "fst_dt"
    ),

    ## Sequence generation
    #

    # Generate union states
    tar_target(
        children_unions,
        gen_union_states(children_filtered[20000:50000]),
        format = "fst_dt"
    ),

    tar_target(
        children_siblings,
        gen_sibling_states(children_unions),
        format = "fst_dt"
    )

    ##
    # Descriptive statistics
)