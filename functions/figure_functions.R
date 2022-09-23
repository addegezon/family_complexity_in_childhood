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

##
# Plots

# Colors for sequence plots
plot_colors <- function(){
    # library(wesanderson)
    # cols <- c(
    #     "OP" = wes_palette("Royal2")[3],
    #     "Single" = wes_palette("Royal2")[4],
    #     "Step" = wes_palette("Royal2")[5]
    # )

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

    named_scheme <- c("OP" = col_scheme[1], "Single" = col_scheme[2], "Step" = col_scheme[3])

    return(scale_fill_manual(values = named_scheme))
}

# Theme for all sequence plots
plot_theme <- function(){
    library(ggthemes)
    library(ggplot2)
    p_theme = theme(
        text = element_text(family="Helvetica", colour = "#5D00BBFF"),
        axis.text = element_text(family="Helvetica", colour = "#5D00BBFF"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "#5D00BBFF", fill = NA)
    )

    return(p_theme)
}

# Create medoid plot
plot_medoid <- function(dt, medoids){
    library(ggplot2)

    # Generate data.table for medoid plot
    plot_dt = list(
        month = 0:179,
        value = rep(1, 180),
        state = as.matrix(
            dt[
                medoids,
                .SD, 
                .SDcols = grep(
                    "FAMILY_STATE",
                    colnames(dt),
                    value = TRUE
                )
            ]
        )[1,]
    )
    setDT(plot_dt)

    # Create medoid plot
    medoid_plot <- ggplot(plot_dt, aes(month, value, fill = state)) + 
        geom_raster() +
        scale_x_continuous(
            expand = c(0, 0),
            breaks = c(0, 12*3, 12*6, 12*9, 12*12, 12*15),
            labels = c("0", "3","6" , "9", "12", "15"),
            name = "Age (years)"
        ) + 
        scale_y_continuous(expand = c(0, 0)) +
        plot_theme() + 
        plot_colors() +
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
triple_plot <- function(dt, sequence, medoids, groups, index){

    #Create medoid plot
    medoid_plot <- plot_medoid(dt, medoids[index])

    # Create chronogram
    chronogram <- ggseqdplot(sequence[groups == index,]) + 
        ggtitle(
            paste0(
                index,
                " (n = ",
                nrow(sequence[groups == index,]),
                ")"
            )
        ) +
        plot_theme() + 
        plot_colors() +
        theme(
            title = element_text(size = 8),
            legend.position = "none",
            axis.ticks.x=element_blank(),
            axis.title.x.bottom = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = margin(c(5,0,0,0))
        )

    # Create modal plot
    modal <- ggseqmsplot(sequence[groups == index,]) +
        scale_y_continuous(
            breaks = c(0, 0.5, 1),
            labels = c(0, 0.5, 1),
            limits = c(0,1)
        ) +
        plot_theme() + 
        plot_colors() +
        theme(
            legend.position = "none",
            axis.ticks.x=element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            plot.margin = margin(c(5,10,0,10))
        )

    return(
        list(chronogram, modal, medoid_plot)
    )
}

# Function for joining together the three main sequence plots
joint_plot <- function(
                    dt,
                    sequence,
                    diss,
                    groups,
                    no_of_clusters = 6
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
        medoids.index="first"
    )

    if ("Only OP" %in% groups){
        grp_order <- c(
            "Only OP",
            "Early Single",
            "Late Step",
            "Mid Single",
            "Late Single",
            "Early Step"
        )
    } else {
        grp_order <- sort(unique(groups))
    }

    # Loop over cluster groups
    p_list <- lapply(
        grp_order,
        function(index){
            triple_plot(dt, sequence, medoids, groups, index)
        }
    )

    # Arrange the three plots for each cluster
    arranged_list <- lapply(p_list, 
        function(list){
            ggarrange(plots=list, heights = c(3, 1.5, 1))
        })

    # Get legend and reverse it
    legend <- get_legend(
        p_list[[1]][[2]] + 
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom") +
        guides(
            fill = guide_legend(
                override.aes = list(reverse = TRUE)
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


# Plot drops during data cleaning
plot_drops <- function(dt_par, dt_kid){


    return(plot)
}