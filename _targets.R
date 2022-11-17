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
        "cluster",
        "WeightedCluster",
        "ggplot2"
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

    tar_target(
        children_siblings,
        gen_sibling_states(children_unions),
        format = "fst_dt"
    ),

    # Generate subsamples
    tar_target(
        final_data,
        define_subsamples(children_siblings),
        format = "fst_dt"
    ),

    # Define family sequence
    tar_target(
        family_sequence,
        seqdef(
            final_data[full_sample == TRUE],
            var = paste0("FAMILY_STATE", 0:179),
            id = final_data[full_sample == TRUE, childID],
            start = 0
        )
    ),

    # Create distance matrix
    tar_target(
        family_diss,
        seqdist(
            family_sequence,
            method = "DHD"
        )
    ),

    # Cluster using Ward
    tar_target(
        family_clusters,
        hclust(
            as.dist(family_diss),
            method = "ward.D2"
        )
    ),

    # Computing cluster quality measures
    tar_target(
        cluster_quality,
        as.clustrange(
            family_clusters,
            diss = family_diss,
            ncluster = 10
        )
    ),

    # Compute cluster quality measure for the null model
    tar_target(
        bootstrap_cluster_quality,
        seqnullcqi(
            family_sequence,
            cluster_quality,
            R = 10,
            model = c("combined"),
            seqdist.args = list(method = "DHD"),
            hclust.method = "ward.D2"
        )
    ),

    # Compute cluster quality measure for the null model
    tar_target(
        bootstrap_cqi_r100,
        seqnullcqi(
            family_sequence,
            cluster_quality,
            R = 100,
            model = c("combined"),
            seqdist.args = list(method = "DHD"),
            hclust.method = "ward.D2"
        )
    ),

    # Generate CQI plots
    tar_target(
        p_cluster_asw,
        cowplot::plot_grid(
            ggseqnullcqiplot(
                bootstrap_cqi_r100,
                stat ="ASW",
                type ="line"
            ) + plot_theme(),
            ggseqnullcqiplot(
                bootstrap_cqi_r100,
                stat ="ASW",
                type ="line",
                standardized = TRUE
            ) + plot_theme()
        )
    ),
    tar_target(
        p_cluster_hc,
        cowplot::plot_grid(
            ggseqnullcqiplot(
                bootstrap_cqi_r100,
                stat ="HC",
                type ="line"
            ) + plot_theme(),
            ggseqnullcqiplot(
                bootstrap_cqi_r100,
                stat ="HC",
                type ="line",
                standardized = TRUE
            ) + plot_theme()
        )
    ),

    # Generate sequence plots
    tar_target(
        p_clusters,
        joint_plot(
            final_data,
            family_sequence,
            family_diss,
            groups = cutree(family_clusters, k = 7)
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