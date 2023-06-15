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

    # Aggregate similar sequences
    tar_target(
        final_data_agg,
        wcAggregateCases(
            final_data[,
                .SD,
                .SDcols = paste0(
                    "FAMILY_STATE",
                    seq(0, 179, by = 3)
                )
            ]
        )
    ),

    tar_target(
        unique_sequences,
        final_data[
            final_data_agg$aggIndex, 
            .SD,
            .SDcols = paste0(
                "FAMILY_STATE",
                seq(0, 179, by = 3)
            )
        ]
    ),

    # Define family sequence
    tar_target(
        family_sequence,
        seqdef(
            unique_sequences,
            weights = final_data_agg$aggWeights,
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
            method = "ward.D2",
            members = final_data_agg$aggWeights
        )
    ),

    # Computing cluster quality measures
    tar_target(
        cluster_quality,
        as.clustrange(
            family_clusters,
            diss = family_diss,
            weights = final_data_agg$aggWeights,
            ncluster = 10
        )
    ),

    # Compute cluster quality measure for the null model
    tar_target(
        bootstrap_cqi_r50,
        seqnullcqi(
            family_sequence,
            cluster_quality,
            R = 50,
            model = c("combined"),
            seqdist.args = list(method = "DHD"),
            hclust.method = "ward.D2"
        )
    ),

    # Generate CQI plots
    tar_target(
        p_cluster_cqi,
        cowplot::plot_grid(
            ggseqnullcqiplot(
                bootstrap_cqi_r50,
                stat = "ASW",
                type = "line",
                standardized = TRUE
            ) + plot_theme(),
            ggseqnullcqiplot(
                bootstrap_cqi_r50,
                stat = "HC",
                type = "line",
                standardized = TRUE
            ) + plot_theme()
        )
    ),

    tar_target(
        group_labels,
        c(
            "Intact original family",
            "Original family to stepfamily",
            "Early separation",
            "Single mother",
            "Single mother to stepfamily"
        )
    ),

    # Generate sequence plots
    tar_target(
        p_clusters_5,
        joint_plot(
            family_sequence,
            family_diss,
            groups = cutree(family_clusters, k = 5),
            weights = final_data_agg$aggWeights,
            group_labels = group_labels
        )
    ),

    # Alternative sequence plots for appendix
    tar_target(
        p_clusters_7,
        joint_plot(
            family_sequence,
            family_diss,
            groups = cutree(family_clusters, k = 7),
            weights = final_data_agg$aggWeights,
            group_labels = 1:7
        )
    ),

    tar_target(
        p_clusters_10,
        joint_plot(
            family_sequence,
            family_diss,
            groups = cutree(family_clusters, k = 10),
            weights = final_data_agg$aggWeights,
            group_labels = 1:10
        )
    ),

    # Append cluster to final data
    tar_target(
        children_clusters,
        final_data[, 
            cluster := factor(
                cluster_quality$clustering$cluster5[final_data_agg$disaggIndex],
                levels = 1:5,
                labels = group_labels
        )
        ]
    ),

    ##
    # Descriptive statistics

    # Table 1
    tar_target(
        table_1,
        format_table(children_filtered)
    ),

    # Proportion family types per country
    tar_target(
        tab_proportion_family_type,
        tab_cluster_proportions(
            children_clusters
        )
    ),

    # Plot complex cluster proportions
    tar_target(
        p_cluster_proportions,
        plot_cluster_proportions(
            children_clusters
        )
    ),

    # Proportion of sibling relations per region
    tar_target(
        p_sibling_proportion,
        plot_sibling_proportions(
            children_clusters
        )
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