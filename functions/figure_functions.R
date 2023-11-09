##
# Tables

format_table <- function(dt, format = "latex", booktabs = TRUE){
    library(modelsummary)
    library(kableExtra)
    library(forcats)

    setorder(dt, COUNTRY)

    # Format dates to year
    year_cols <- c("birth_ym", "IBORN_YM", "ISURVEY_YM")
    dt[, c(year_cols) := lapply(.SD, data.table::year), .SDcols = year_cols]

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
        by = .(COUNTRY, survey)
    ]

    # Load and match response rates
    rr <- as.data.table(
            read.csv(
                file.path("data", "response_rates.csv"))
    )

    dt <- merge(dt, rr, by = c("COUNTRY", "survey"), all.x = TRUE)

    collapsed <-  unique(dt[, 
        c(
            "COUNTRY",
            "region",
            "svy_min",
            "svy_max",
            "mother_min",
            "mother_max",
            "child_min",
            "child_max",
            "n_kids",
            "survey",
            "RR"
        )
    ])

    # Add Total row
    total_row <- data.table(
        "COUNTRY" = "Total",
        "region" = " ",
        "svy_min" = collapsed[, min(svy_min)],
        "svy_max" = collapsed[, max(svy_min)],
        "mother_min" = collapsed[, min(mother_min)],
        "mother_max" = collapsed[, max(mother_max)],
        "child_min" = collapsed[, min(child_min)],
        "child_max" = collapsed[, max(child_max)],
        "n_kids" = collapsed[, sum(n_kids)],
        "survey" = "",
        "RR" = NA_character_
    )

    collapsed <- rbindlist(list(collapsed, total_row))


    # Rename columns
    table <- collapsed[, .(
        "Country" = COUNTRY,
        "Region" = region,
        "Survey" = survey,
        "Survey years" = paste0(svy_min, " - ", svy_max),
        "Response rate" = lapply(
            RR, 
            function (x) {
                if (!is.na(x)) {
                    paste0(x, "%")
                } else {
                    " "
                }

            }
        ),
        "Child cohorts" = paste0(child_min, " - ", child_max),
        "N. children" = prettyNum(n_kids, big.mark = ",")
        )
    ]

    setorder(table,Region, Country)

    # Create table
    tab <- knitr::kable(
        table[,!'Region'],
        booktabs = booktabs,
        format = format
    ) |> 
        row_spec(1, hline_after = T) |> 
        pack_rows(index = table(fct_inorder(table$Region))) 

    return(tab)
}

tab_cluster_proportions <- function(dt, format = "latex", booktabs = TRUE) {
    library(kableExtra)
    library(forcats)

    dt <- copy(dt)

    # # Generate no. of observations per country
    dt[, country_n := .N, by = "COUNTRY"]

    # Sum dt by number in each cluster and number of observations by country
    dt <- unique(
        dt[, 
            .(
                number = .N,
                country_n = country_n, 
                region = region
            ), 
            by = .(COUNTRY, cluster)
        ]
    )

    # Calculate mean and confidence intervals
    dt <- dt[, 
        .(
            CI = prop.test(
                    number,
                    country_n,
                    conf.level = .95
            )[c('estimate','conf.int')]
        ),
        by = .(COUNTRY, cluster, region)
    ]
    
    # Arrange it for tabbing
    dt[
        seq_len(nrow(dt)) %% 2 == 0,
        c('lower', 'upper') := data.table::transpose(CI)
    ]
    dt[
        seq_len(nrow(dt)) %% 2 == 1,
        mean := data.table::transpose(CI)
    ]
    dt[,
        mean := as.character(round(mean*100,1))
    ]
    dt[
        is.na(mean),
        mean := paste0(
            "[",
            round(lower*100,1),
            ", ",
            round(upper*100,1),
            "]"
        )
    ]
    
    # To wide format
    dt_mean <- dcast(
        dt[seq_len(nrow(dt)) %% 2 == 1],
        COUNTRY + region ~ cluster,
        value.var = "mean"
    )

    dt_conf <- dcast(
        dt[seq_len(nrow(dt)) %% 2 == 0],
        COUNTRY + region~ cluster,
        value.var = "mean"
    )

    # Bind together the tables
    table <- rbindlist(list(dt_mean, dt_conf))
    setorder(table, region, COUNTRY, "Intact original family")
    table[
        seq_len(nrow(table)) %% 2 == 0,
        COUNTRY := " "
    ]

    # Make final table
    setnames(table, "COUNTRY", "Country")
    tab <- knitr::kable(
        table[,!'region'],
        format = format,
        booktabs = booktabs,
        linesep = if (booktabs) c('', '\\addlinespace') else '\\hline'
    ) |> pack_rows(index = table(fct_inorder(table$region)))
    return(tab)
}

##
# Plots

# Color scheme definition
color_scheme <- function(index = 1:8) {

    library(prismatic)

    col_scheme <- c(
            "#1E00BE",
            "#8D90F5",
            "#3CA651",
            "#70DC69",
            "#91289B",
            "#F0C3E6",
            "#FFBE2D",
            "#FFDC82"
        ) |> 
        clr_rotate(10) |>
        clr_alpha(alpha = 1)
    
    return(col_scheme[index])

}

# Colors for sequence plots
plot_colors <- function(scale_type = "fill"){

    named_scheme <- c(
        "OP" = color_scheme(1),
        "Single" = color_scheme(6),
        "Step" = color_scheme(3)
    )


    if (scale_type == "fill") {
        return(scale_fill_manual(values = named_scheme, drop = FALSE))
    }
    else if (scale_type == "color") {
        return(scale_color_manual(values = named_scheme, drop = FALSE))
    }
}
    

# Theme for all sequence plots
plot_theme <- function(){

    library(ggplot2)

    p_theme = theme(
        text = element_text(family="Helvetica", colour = "#5D00BBFF"),
        axis.text = element_text(family="Helvetica", colour = "#5D00BBFF"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "#5D00BBFF", fill = NA),
        strip.background = element_rect(color = "#5D00BBFF", fill = NA),
        strip.text.y.right = element_text(angle = 0, color = "#5D00BBFF")
    )

    return(p_theme)
}

# Create medoid plot
plot_medoid <- function(sequence, medoids){
    library(ggplot2)

    # Generate data.table for medoid plot
    plot_dt = list(
        month = seq(0, 179, by = 3),
        value = rep(1,60),
        state = as.matrix(sequence[medoids,])[1,]
    )
    setDT(plot_dt)

    # Create medoid plot
    medoid_plot <- ggplot(
        plot_dt, 
        aes(
            month,
            value,
            fill = state
        )
    ) + 
        geom_raster() +
        scale_x_continuous(
            expand = c(0, 0),
            breaks = c(0, 12*3, 12*6, 12*9, 12*12, 12*15),
            labels = c("0", "3","6" , "9", "12", "15"),
            name = "Age (years)"
        ) + 
        scale_y_continuous(expand = c(0, 0)) +
        plot_theme() + 
        plot_colors("fill") +
        theme(
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            panel.grid = element_blank(),
            plot.margin = margin(c(0,0,5,0))
        ) 

    return(medoid_plot)

}

# Create three sequence plots
triple_plot <- function(sequence, medoids, groups, index, weights, group_labels){

    library(ggtext)

    # Create data.table of sequence information
    dt_sequence <- as.data.table(sequence)
    dt_sequence[, weight := weights]
    dt_sequence[, group := groups]


    #Create medoid plot
    medoid_plot <- plot_medoid(sequence, medoids[index])

    # Create chronogram
    chronogram <- ggseqdplot(sequence[groups == index,]) +
        plot_theme() + 
        plot_colors() +
        theme(
            legend.position = "none",
            axis.ticks.x=element_blank(),
            axis.title.x.bottom = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = margin(c(0,10,0,10))
        )

    # Create modal plot
    modal <- ggseqmsplot(sequence[groups == index,]) +
        scale_y_continuous(
            breaks = c(0, 0.5, 1),
            labels = c(0, 0.5, 1),
            limits = c(0,1)
        ) +
        plot_theme() + 
        plot_colors("fill") +
        theme(
            legend.position = "none",
            axis.ticks.x=element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            plot.margin = margin(c(5,10,0,10))
        )
        
    # Create index plot
    index_plot <- ggseqiplot(
        sequence[groups == index,],
        sortv = "from.start"
    ) +
        ggtitle(
                paste0(
                    group_labels[index],
                    "\n(n = ",
                    sum(dt_sequence[group == index, weight]),
                    ")"
                )
            ) +
        plot_theme() +
        plot_colors("fill") + 
        plot_colors("color") +
        theme(
            plot.title = element_text(size = 8),
            legend.position = "none",
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            plot.margin = margin(c(10,0,0,0))
        )

    return(
        list(index_plot, chronogram, medoid_plot)
    )
}

# Function for joining together the three main sequence plots
joint_plot <- function(
                    sequence,
                    diss,
                    groups,
                    weights = NULL,
                    group_labels
                ){
    library(egg)
    library(cowplot)
    library(ggseqplot)
    library(TraMineR)
    library(cluster)
   
    # Find medoids
    medoids <- disscenter(
        diss,
        group = groups,
        medoids.index = "first",
        weights = weights
    )

    grp_order <- sort(unique(groups))

    # Loop over cluster groups
    p_list <- lapply(
        grp_order,
        function(index){
            triple_plot(
                sequence,
                medoids,
                groups,
                index,
                weights,
                group_labels
            )
        }
    )

    # Arrange the three plots for each cluster
    arranged_list <- lapply(p_list, 
        function(list){
            ggarrange(
                plots = list,
                heights = c(3, 2, 1)
            )
        }) 

    # Get legend and reverse it
    legend <- get_legend(
        p_list[[1]][[2]] + 
        theme(legend.position = "bottom") +
        guides(
            fill = guide_legend(
                reverse = TRUE,
                nrow = 1
            )
        )
    )

    grid_arranged <- plot_grid(plotlist = arranged_list, scale = 1)

    grid_legend <- plot_grid(
        grid_arranged,
        legend,
        ncol = 1,
        rel_heights = c(1, 0.1)
    )

    return(grid_legend)
}

# Plot sibling proportions
plot_sibling_proportions <- function (dt) {

    dt <- copy(dt)

    # Create sibling indicator
    dt[
        HALFSIBLING_STATE177 > 0,
        has_halfsibling := 1
    ][
        HALFSIBLING_STATE177 == 0,
        has_halfsibling := 0
    ]

    dt[
        FULLSIBLING_STATE177 > 0,
        has_fullsibling := 1
    ][
        FULLSIBLING_STATE177 == 0,
        has_fullsibling := 0
    ]

    dt[,
        has_full_and_half := has_fullsibling * has_halfsibling
    ]

    # Reshape to long, using type of sibling
    dt <- melt(
        dt,
        measure.vars = c(
            "has_halfsibling",
            "has_fullsibling",
            "has_full_and_half"
        ),
        variable.name = "sibling_type",
        value.name = "has_sibling"
    )

    # Caclulate CI and mean
    dt[,
        upper_ci_sib := pmin(
            confint(lm(has_sibling ~ 1), level=0.95)[2],
            1
        ),
        by = .(COUNTRY, cluster, sibling_type)
    ]

    dt[,
        lower_ci_sib := pmax(
            confint(lm(has_sibling ~ 1), level=0.95)[1],
            0
        ),
        by = .(COUNTRY, cluster, sibling_type)
    ]

    dt[,
        mean_sib := mean(has_sibling),
        by = .(COUNTRY, cluster, sibling_type)
    ]

    # Get number of observations by Country*Cluster
    dt[,
        obs_100 := .N >= 100,
        by = .(COUNTRY, cluster)
    ]

    # Keep only unique values
    dt <- unique(
        dt[,
            .(
                COUNTRY,
                cluster,
                sibling_type,
                upper_ci_sib,
                lower_ci_sib,
                mean_sib,
                obs_100
            )
        ]
    )

    # Plot it!
    plot <- ggplot(
        dt[sibling_type != "has_full_and_half"],
        aes(
            x = COUNTRY,
            y = mean_sib,
            group = sibling_type,
            color = sibling_type,
            alpha = obs_100
        )
    ) + 
    geom_pointrange(
        aes(
            ymin = lower_ci_sib,
            ymax = upper_ci_sib
        )
    ) +
    facet_grid(vars(cluster)) +
    xlab("Country") + 
    ylab("Proportion of children with full- or half-siblings by country and cluster") +
    plot_theme() +
    scale_y_continuous(limits = c(-0.01,1.01)) +
    scale_colour_manual(
        name = "Sibling type",
        values = c(
            "has_fullsibling" = color_scheme(1),
            "has_halfsibling" = color_scheme(3)
        ),
        labels = c(
           "has_fullsibling" =  "At least one full-sibling",
            "has_halfsibling" = "At least one half-sibling"
        ),
        guide = guide_legend(
            reverse = TRUE,
        ) 
    ) +
    scale_alpha_discrete(
        range = c(0.20, 0.9),
        guide = "none"
    ) +
    theme(legend.key = element_rect(fill = NA))

    return(plot)

}

sort_by_intact <- function(dt) {

      # We want to order by proportion intact family
    dt[, 
        avg_intact := mean(
                cluster == "Intact original family"
        ),
        by = "COUNTRY"
    ]
   
    # Change country order to account for region
    factor <- unique(dt[,.(COUNTRY, region, avg_intact)])
    dt[, COUNTRY := as.factor(COUNTRY)]
    dt[, 
        COUNTRY := factor(
            COUNTRY, 
            levels = factor[order(region, -avg_intact)]$COUNTRY
        )
    ]

    return(dt)

}

# Plot cluster proportions
plot_cluster_proportions <- function(dt) {

     dt <- sort_by_intact(dt)

    # Plot it!
    plot <- ggplot(
        dt,
        aes(
            x = COUNTRY,
            fill = cluster
        )
    ) +
        geom_bar(position = "fill") +
        labs(
            x = "Country", 
            y = "Proportion of family type"
        ) + 
        coord_flip() + 
        facet_grid(
            region ~ ., 
            scales = "free", 
            space = "free"
        ) + 
        plot_theme() + 
        scale_fill_manual(
            values = color_scheme(c(1, 2, 3, 5, 6, 7)),
            guide = guide_legend(reverse = TRUE)
        ) +
        theme(
            legend.position = "bottom",
            legend.box = "vertical",
            legend.margin = margin(),
            legend.title = element_blank()
        ) + 
        guides(
            fill = guide_legend(nrow = 2)
        )

    return(plot)
}

# Plot cluster proportions
plot_complex_proportions <- function(dt) {

    dt <- sort_by_intact(dt)
   

    dt <- dt[cluster != "Intact original family"]


    # Plot it!
    plot <- ggplot(
        dt,
        aes(
            x = COUNTRY,
            fill = cluster
        )
    ) +
        geom_bar(position = "fill") +
        geom_hline(
            yintercept = 0.5,
            linetype = "dashed",
            color = "white"
        ) +
        labs(
            x = "Country", 
            y = "Proportion of family type"
        ) + 
        coord_flip() + 
        facet_grid(
            region ~ ., 
            scales = "free", 
            space = "free"
        ) + 
        plot_theme() + 
        scale_fill_manual(
            values = color_scheme(c(2, 3, 5, 6, 7)),
            guide = guide_legend(reverse = TRUE)
        ) +
        theme(
            legend.position = "bottom",
            legend.box = "vertical",
            legend.margin = margin(),
            legend.title = element_blank()
        ) + 
        guides(
            fill = guide_legend(nrow = 2)
        )

    return(plot)
}

combine_proportion_plots <- function(p1, p2) {

    library(ggplot2)
    library(cowplot)

    p1_m <- p1 + 
    ylab(element_blank()) +
    theme(
        strip.background = element_blank(), 
        strip.text.y.right  = element_blank(),
    )
    p2_m <- p2 + 
    xlab(element_blank()) + 
    ylab(element_blank()) + 
    theme(
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        strip.text.y.right = element_text(
        angle = 270,
        size = 6
        )
    )

    grid <- plot_grid(
        p1_m + theme(legend.position = "none"),
        p2_m + theme(legend.position = "none"),
        rel_widths = c(1.1,1)
    )

    legend <- get_legend(
        p1 + 
            guides(color = guide_legend(nrow = 1)) +
            theme(legend.position = "bottom")
    )

    plot <- plot_grid(
        grid,
        legend, 
        ncol = 1, 
        rel_heights = c(1, .1)
    )

    return(plot)
}

# Plot drops during data cleaning
plot_drops <- function(dt_par, dt_kid){


    return(plot)
}


########
## SEQNULLCQI - WORK IN PROGRESS

ggcqdensity <- function(
    bcq,
    stat,
    quant = NULL,
    norm = FALSE,
    seg = TRUE,
    fill_col = "#E69F00",
    ...
){

    # Get CQI measures
    cq <- bcq$clustrange

    # Get distribution
    sumcqi <- normstatcqi(
        bcq,
        stat = stat,
        norm = norm
    )

    # Calculate range of CQI values
    allrange <- range(
        c(
            sumcqi$origstat,
            sumcqi$alldatamax
        )
    )

    # Get clusters
    kvals <- cq$kvals

    # Calculate density
    dens <- density(sumcqi$alldatamax)

    # Initiate empty density plot
    density_plot <- ggplot(
        data.frame(x = sumcqi$alldatamax)
    ) + 
        aes(x = x) + 
        scale_x_continuous(limits = allrange)

    # Add line segments if seg == TRUE
    if(seg) {
        adj <- (seq_along(kvals) - 1) / (length(kvals) - 1) * 0.6 + 0.2

        # Create data.frame holding line segment values
        segments <- data.frame(
            x = sumcqi$origstat,
            y = rep(0, length(kvals)),
            x_end = sumcqi$origstat,
            y_end  = max(dens$y) * adj,
            lbl = kvals
        )

        # Add line segments to the density plot as well as
        # corresponding number of clusters in solution
        density_plot <- density_plot +
            geom_segment(
                data = segments,
                aes(
                    x = x,
                    y = y,
                    xend = x_end,
                    yend  = y_end
                ),
                alpha = 0.5
            ) +
            ggrepel::geom_text_repel(
                data = segments,
                aes(
                    x = x,
                    y = y_end,
                    label = lbl
                )
            )
    } 

    # Fill confidence interval under density curve if value has been
    # supplied for quant
    if(!is.null(quant)){

        # Get the minimum and maximum x-values
        minmax <- confcqi(
            sumcqi$alldatamax,
            quant,
            bcq$R
        )

        # Create data.frame with values to fill
        fill_values <- data.frame(x = dens$x, y = dens$y) |>
            dplyr::mutate(
                fill = ifelse(
                    (x >= minmax[1] & x <= minmax[2]),
                    TRUE,
                    FALSE
                )
            )

        # Fill in confidence interval
        density_plot <- density_plot + 
            geom_ribbon(
                data = subset(fill_values, fill),
                aes(x = x, ymax = y),
                ymin = 0,
                fill = fill_col,
                alpha = 0.5
            )

        # Print the CI
        print(minmax)
    }

    # Add density line as final layer and theming
    density_plot <- density_plot +
        geom_density(trim = TRUE) +
        theme_minimal() +
        ylab("Density") +
        xlab(
            paste0(
                "N = ",
                dens$n,
                "\n Bandwidth = ",
                round(dens$bw, 2)
            )
        ) + 
        theme(
            axis.title.y = element_text(vjust = +3),
            panel.grid.major.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.margin = margin(-0.2, 0, 0, -0.2, unit = "cm"),
            axis.line.x = element_line(linewidth = .3),
            axis.ticks = element_line(linewidth = .3)
        )

    return(density_plot)
}

map_cluster_proportions <- function(dt) {

    world <- map_data("world")

    europe <- subset(world, region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                                        "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                        "Croatia", "Cyprus", "Czech Republic","Denmark","Estonia","Finland", 
                                        "France","Georgia", "Germany", "Greece","Hungary","Iceland", 
                                        "Ireland", "Italy", "Kosovo", "Latvia","Liechtenstein", 
                                        "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                        "Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                                        "Russia","San Marino","Serbia","Slovakia","Slovenia","Spain",
                                        "Sweden","Switzerland","Turkey","Ukraine","UK","Vatican"))

    europe <- as.data.table(europe)
    europe <- europe[subregion != "Svalbard" | is.na(subregion)]
    europe[, COUNTRY := region]

    clusters <- levels(dt$cluster)

    all_combinations <- CJ(
    COUNTRY = unique(europe$COUNTRY),
    cluster = clusters
    )

    europe <- merge(europe, all_combinations, by = "COUNTRY", allow.cartesian = TRUE )


    dt[, prop_cluster := as.double(.N), by = .(COUNTRY, cluster)]
    dt[, prop_cluster := prop_cluster/.N, by = .(COUNTRY)]
    
    
    dt[, eu_mean_cluster := as.double(.N), by = .(cluster)]
    dt[, eu_mean_cluster := eu_mean_cluster/.N, ]
    dt[, rr_cluster := prop_cluster/eu_mean_cluster]

    map_dat <- merge(
        europe, 
        unique(dt[,.(COUNTRY, region,cluster, prop_cluster, rr_cluster)]), 
        by = c("COUNTRY", "cluster"), 
        all.x = TRUE, 
        allow.cartesian = TRUE
    ) 



    #map_dat[, prop_cluster := scale(prop_cluster), by = .(cluster)]


    plot <- ggplot(
        data = map_dat, 
        aes(x = long, y = lat, group = group)
    ) + 
        geom_polygon(
            fill = alpha("gray", 0.4),
            color = color_scheme(1),
            size = 0.01
        ) +
        geom_polygon(
            aes(
                fill = rr_cluster
            ), 
            color = color_scheme(1)
        ) +
        coord_fixed(
            ratio=1.6,
            xlim = c(-10, 32), 
            ylim = c(36, 72)
        ) +   
        scale_fill_gradient2(
            low = alpha(color_scheme(5),1),
            high = alpha(color_scheme(3),1),
            na.value = alpha(color_scheme(1), 0),
            midpoint = 1
        ) +
        facet_wrap(~cluster) + 
        plot_theme() +
        theme(
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title = element_blank(),
            strip.text = element_text(color = color_scheme(1)),
            legend.position="bottom"
        )


    return(plot)
}

map_educational_representation <- function(dt) {

    world <- map_data("world")

    europe <- subset(world, region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                                        "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                        "Croatia", "Cyprus", "Czech Republic","Denmark","Estonia","Finland", 
                                        "France","Georgia", "Germany", "Greece","Hungary","Iceland", 
                                        "Ireland", "Italy", "Kosovo", "Latvia","Liechtenstein", 
                                        "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                        "Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                                        "Russia","San Marino","Serbia","Slovakia","Slovenia","Spain",
                                        "Sweden","Switzerland","Turkey","Ukraine","UK","Vatican"))

    europe <- as.data.table(europe)

    europe <- europe[subregion != "Svalbard" | is.na(subregion)]
    europe[, COUNTRY := region]

    ## Educational standardization
    dt <- dt[!is.na(EDU_3)]

    # Group by country, educational level, and occupational sector, and count the number of individuals in each group
    data_grouped <- dt[, .(count = .N), by = .(COUNTRY, EDU_3, cluster)]

    # Calculate the total count of individuals in each educational level within each country
    total_education_counts <- data_grouped[, .(total_count_edu = sum(count)), by = .(COUNTRY, EDU_3)]

    # Calculate the total count of individuals in each occupational sector within each country
    total_sector_counts <- data_grouped[, .(total_count_clust = sum(count)), by = .(COUNTRY, cluster)]

    # Join the total counts back to the grouped data
    data_grouped <- merge(data_grouped, total_education_counts, by = c("COUNTRY", "EDU_3"), all.x = TRUE)
    data_grouped <- merge(data_grouped, total_sector_counts, by = c("COUNTRY", "cluster"), all.x = TRUE)

    data_grouped[, total_pop := sum(count), by = "COUNTRY"]
    #data_grouped[, relative_risk := (count/total_count_clust)/(total_count_edu/total_pop)]

    data_grouped[, relative_risk := (count/total_count_edu)/(total_count_clust/total_pop)]

    clusters <- levels(dt$cluster)
    EDU_3 <- levels(dt$EDU_3)

    all_combinations <- CJ(
    COUNTRY = unique(europe$COUNTRY),
    cluster = clusters,
    EDU_3 = EDU_3
    )

    europe <- merge(
    europe, 
    all_combinations, 
    by = "COUNTRY", 
    allow.cartesian = TRUE
    )


    map_dat <- merge(
    europe,
    data_grouped, 
    by = c("COUNTRY", "cluster", "EDU_3"), 
    all.x = TRUE, 
    allow.cartesian = TRUE
    )

    map_dat[, EDU_3 := factor(EDU_3, levels = c("Low", "Medium", "High"))]

    plot <- ggplot(
        data = map_dat[cluster != "Intact original family"], 
        aes(x = long, y = lat, group = group)
    ) + 
        geom_polygon(
            fill = alpha("gray", 0.4),
            color = color_scheme(1),
            size = 0.01
        ) +
        geom_polygon(
            aes(
                fill = relative_risk
            ), 
            color = color_scheme(1)
        ) +
        coord_fixed(
            ratio=1.6,
            xlim = c(-10, 32), 
            ylim = c(36, 72)
        ) +   
        scale_fill_gradient2(
            low = alpha(color_scheme(5),1),
            high = alpha(color_scheme(3),1),
            na.value = alpha("gray", 0),
            midpoint = 1
        ) +
        facet_grid(cluster ~ EDU_3) + 
        plot_theme() +
        theme(
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title = element_blank(),
            strip.text = element_text(color = color_scheme(1)),
            strip.text.y.right = element_text(angle = 270),
            legend.position="bottom"
        )

    return(plot)
}


ggseqnullcqiplot <- function(
    bcq,
    stat,
    type = "line",
    quant = 0.95,
    norm = TRUE,
    legendpos="topright",
    alpha=.2,
    standardized = FALSE,
    ...
){
    # Density plot
    if(type == "density") {
        return(
            ggcqdensity(
                bcq,
                stat,
                quant = quant,
                norm = norm,
                ...
            )
        )
    }

    # Sequence density plot
    if(type == "seqdplot") {
        return(
            ggseqdplot(bcq$seqdata,  ...)
        )
    }

    # Get cluster quality and solution number
    cq <- bcq$clustrange
    kvals <- cq$kvals

    if(stat == "all") {

        alls <- list()

        for(ss in colnames(cq$stats)) {

            statz <- data.frame(bcq$stats[[ss]])
            colnames(statz) <- kvals
            statz <- tidyr::gather(
                statz,
                kval,
                index,
                colnames(statz),
                factor_key=TRUE
            )

            for(i in seq_along(nstat)){
                nstat[i] <- (nstat[i]-mean(allstat[, i]))/sd(allstat[, i])
            }

            alls[[ss]] <- nstat
        }

        for(ss in colnames(cq$stats)) {

            nstat <- cq$stats[, ss]
            allstat <- rbind(nstat, bcq$stats[[ss]])

            for(i in seq_along(nstat)){
                nstat[i] <- (nstat[i]-mean(allstat[, i]))/sd(allstat[, i])
            }

            alls[[ss]] <- nstat
        }
		
		plot(kvals,nstat, type="n", main="Standardized quality measure", ylab="Normalized quality measure", xlab="Number of clusters", ylim=range(unlist(alls)))
		for(ss in seq_along(alls)){
			lines(kvals, alls[[ss]], type="b", col=ss)
		}
		
		legend(legendpos, fill=seq_along(alls), legend=names(alls))
		return(invisible(NULL))
    }
    
    origstat <- cq$stats[, stat]
    nullstat <- bcq$stats[[stat]]
    
    internalplot <- function(kvals, origstat, nullstat, main, ylab, ...){

        allstat <- rbind(origstat, nullstat)
        allrange <- range(allstat)
        alpha <- (1-quant)/2

        if(type != "boxplot"){

            # Initiate empty plot
            cqi_plot <- ggplot(
                data.frame(x = kvals),
                aes(x = x)
            ) +  
                scale_x_continuous(breaks = kvals) +
                scale_y_continuous(limits = allrange) +
                labs(
                    x = "Number of clusters",
                    y = ylab,
                    title = main
                ) +
                theme(
                    axis.title.y = element_text(vjust = +3),
                    panel.grid.major.x = element_blank(),
                    legend.position = "bottom",
                    legend.title = element_blank(),
                    legend.margin = margin(-0.2, 0, 0, -0.2, unit = "cm"),
                    axis.line.x = element_line(linewidth = .3),
                    axis.ticks = element_line(linewidth = .3),
                    plot.margin = margin(10,10,10,15),
                    panel.border =  element_rect(colour = "black", fill = NA)
                )

            # for(i in 1:nrow(nullstat)){
            # 	lines(kvals, nullstat[i,], col=gray(.5, alpha=alpha), lwd=1)
            # }

            if(!is.null(quant)){

                # Create data frame for geom_ribbon
                minmax <- sapply(
                    seq_along(kvals),
                    function(x) quantile(
                        nullstat[, x],
                        c(alpha, 1 - alpha)
                    )
                )

                ribbon_data <- data.frame(
                    x = kvals,
                    ymin = minmax[1,],
                    ymax = minmax[2,]
                )

                # Fill in confidence interval in plot
                cqi_plot <- cqi_plot + 
                    geom_ribbon(
                        data = ribbon_data,
                        aes(
                            x = x,
                            ymax = ymax,
                            ymin = ymin
                        ),
                        fill = "#F4C2E1FF",
                        alpha = 0.8
                )
            }

        } else {
            nn <- as.vector(nullstat)
            kk <- rep(kvals, each=nrow(nullstat))
            boxplot(nn~kk, ylim=allrange)
            lines(seq_along(unique(kk)), origstat, lwd=2, col="black", type="b")
        }
        # Add line showing quality threshold
        if(!is.null(quant)){

            threshold <- ifelse(stat == "HC", alpha, c(1 - alpha))

            if(stat == "HC") {

                overallmaxq <- quantile(apply(nullstat, 1, min), threshold)

            } else {

                overallmaxq <- quantile(apply(nullstat, 1, max), threshold)

            }

            cqi_plot <- cqi_plot +
                geom_hline(
                    yintercept = overallmaxq,
                    linetype = "dashed",
                    col = "#00A862FF"
                )
        }

         # Add origstat line
        cqi_plot <- cqi_plot +
            geom_line(
                data = data.frame(x = kvals, y = origstat),
                aes(x = x, y = y),
                color = "#5D00BBFF"
            ) + 
            geom_point(
                data = data.frame(x = kvals, y = origstat),
                aes(x = x, y = y),
                color = "#5D00BBFF"
            )

        return(cqi_plot)
    }

    if (standardized == FALSE) {
        plot <- internalplot(
            kvals,
            origstat,
            nullstat,
            main = paste("Raw", stat),
            ylab = stat
        )
    }

    if (standardized == TRUE) {
        # Calculate standardized values
        normstat <- origstat
        normnullstat <- nullstat 
        for(i in seq_along(normstat)) {

            mn <- mean(nullstat[, i])
            sdn <- sd(nullstat[, i])
            normstat[i] <- (origstat[i] - mn) / sdn
            normnullstat[, i] <- (normnullstat[, i] - mn) / sdn
        }

        plot <- internalplot(
            kvals,
            normstat,
            normnullstat,
            main = paste("Standardized", stat),
            ylab = paste("Standardized", stat)
        )
    }

    return(plot)

}



normstatcqi <- function(bcq, stat, norm=TRUE){
	
	origstat <- bcq$clustrange$stats[, stat]
	nullstat <- bcq$stats[[stat]]
	
	if(norm){
		for(i in seq_along(origstat)){

			mx <- mean(nullstat[, i])
			sdx <- sd(nullstat[, i])
			nullstat[ , i] <- (nullstat[, i]-mx)/sdx
			origstat[i] <- (origstat[i]-mx)/sdx
		}
	}
	alldatamax <- apply(nullstat, 1, max)
	sumcqi <- list(origstat=origstat, nullstat=nullstat, alldatamax=alldatamax)
	return(sumcqi)

}


confcqi <- function(nullstat, quant, n){
	alpha <- (1-quant)/2
	#calpha <- alpha+(alpha-1)/n
	#print(c(calpha, alpha))
	#minmax <- quantile(nullstat, c(calpha, 1-calpha))
	minmax <- quantile(nullstat, c(alpha, 1 - alpha))
	return(minmax)
}
