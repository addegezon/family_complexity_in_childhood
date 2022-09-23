library(targets)
library(tarchetypes)
source("functions/data_functions.R")
source("functions/figure_functions.R")


tar_option_set(
    packages = c(
        "data.table",
        "fst",
        "haven",
        "lubridate",
        "TraMineR",
        "cluster"
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

    ## 
    # Sequence generation

    # Generate union states
    tar_target(
        children_unions,
        gen_union_states(children_filtered),
        format = "fst_dt"
    ),

    # tar_target(
    #     children_siblings,
    #     gen_sibling_states(children_unions),
    #     format = "fst_dt"
    # ),

    # Generate subsamples
    tar_target(
        final_data,
        define_subsamples(children_unions),
        format = "fst_dt"
    ),

    # Define family sequence and distance matrix for 
    # full, complex and random subsample
    tarchetypes::tar_map(
        values = list(
            sample = c("full_sample", "complex_sample", "random_sample"),
            sample_name = c("full", "complex", "random")
        ),
        names = sample_name,

        # Define family sequence
        tar_target(
            family_sequence,
            seqdef(
                final_data[get(sample) == TRUE],
                var = paste0("FAMILY_STATE", 0:179),
                id = final_data[get(sample) == TRUE, childID],
                start = 0
            )
        ),

        # Create distance matrix
        tar_target(
            family_om,
            seqdist(
                family_sequence,
                method ="DHD"
            )
        )
    ),

    ##
    # Clustering and analysis
    
    # Create clusters
    tarchetypes::tar_map(
        values = list(
            family_om = rlang::syms(
                c("family_om_complex", "family_om_random")
            ),
            sample_name = c("complex", "random")
        ),
        names = sample_name,
        # Cluster using Ward
        tar_target(
            family_clusters,
            agnes(
                family_om,
                diss = TRUE,
                method ="ward")
        )
    ),

    # Generate sequence plots
    tar_target(
        p_cluster_random,
        joint_plot(
            final_data[random_sample == TRUE],
            family_sequence_random,
            family_om_random,
            groups = cutree(family_clusters_random, k = 6)
        )
    ),

    tar_target(
        p_cluster_complex,
        joint_plot(
            final_data[complex_sample == TRUE],
            family_sequence_complex,
            family_om_complex,
            groups = cutree(family_clusters_complex, k = 7)
        )
    ),

    # Define analytical groups
    tar_target(
        family_groups,
        define_analytical_groups(final_data),
        format = "fst_dt"
    ),

    tar_target(
        p_analytical_random,
        joint_plot(
            final_data[full_sample == TRUE],
            family_sequence_full,
            family_om_full,
            groups = family_groups[full_sample == TRUE, family_group]
        )
    ),

    ##
    # Descriptive statistics

    # Table 1
    tar_target(
        table_1,
        format_table(children_filtered)
    )

    # # Drops
    # tar_target(
    #     p_drops,
    #     plot_drops(parents_tagged, children_tagged)
    # ),

    ##
    # Render thesis
    # tar_quarto(
    #     thesis,
    #     path = file.path("writing", "thesis.qmd")
    # )
)