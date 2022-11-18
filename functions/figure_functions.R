##
# Tables

format_table <- function(dt){
    library(modelsummary)
    library(kableExtra)

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

    collapsed[
        COUNTRY == "Czech Republic GGS wave 1" |
        COUNTRY == "France GGS wave1" |
        COUNTRY == "Netherlands FFS" |
        COUNTRY == "Netherlands OG 2013" |
        COUNTRY == "Belgium GGS wave1",
        region := "Central Europe"
    ][
        COUNTRY == "Bulgaria GGS wave1" |
        COUNTRY == "Belarus GGS wave 1" |
        COUNTRY == "Georgia GGS wave1" |
        COUNTRY == "Hungary GGS wave1" |
        COUNTRY == "Poland GGS wave1",
        region := "Eastern Europe"
    ][
        COUNTRY == "Estonia GGS wave1" |
        COUNTRY == "Lithuania GGS wave1" |
        COUNTRY == "Norway GGS wave1" |
        COUNTRY == "Sweden GGS wave 1",
        region := "Scandinavia and Baltics" 
    ][
        COUNTRY == "Spain SFS 2006" |
        COUNTRY == "Romania GGS wave1",
        region := "Southern Europe"
    ]

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
        "COUNTRY" = "",
        "svy_min" = collapsed[, min(svy_min)],
        "svy_max" = collapsed[, max(svy_min)],
        "mother_min" = collapsed[, min(mother_min)],
        "mother_max" = collapsed[, max(mother_max)],
        "child_min" = collapsed[, min(child_min)],
        "child_max" = collapsed[, max(child_max)],
        "n_kids" = collapsed[, sum(n_kids)],
        "region" = "Total",
        "survey" = ""
    )

    collapsed <- rbindlist(list(collapsed, total_row))




    # Rename columns
    table <- collapsed[, .(
        "Country" = COUNTRY,
        "Survey" = survey,
        "Survey years" = paste0(svy_min, " - ", svy_max),
        # "{Mother cohorts}" = paste0(mother_min, " - ", mother_max),
        "Child cohorts" = paste0(child_min, " - ", child_max),
        "N. children" = format(n_kids),
        region = region
        ) 
    ]

    # Create table
    tab <- knitr::kable(
        table[order(region)][, !"region"],
        booktabs = TRUE,
        format = "latex"
    ) |> pack_rows(index = table(forcats::fct_inorder(table$region))) |>
        row_spec(16, hline_after = T)

    return(tab)
}

tab_cluster_proportions <- function(dt, clusters) {

    dt <- copy(dt)

    dt[grep("Netherlands", COUNTRY), COUNTRY := "Netherlands"]

    # Attach clusters to data.table and label them
    dt[, 
        cluster := factor(
            cutree(clusters, k = 7),
            levels = 1:7,
            labels = c(
                "Intact original family",
                "Mid-childhood stepfamily",
                "Single mother",
                "Late childhood separation",
                "Early childhood separation",
                "Early childhood stepfamily",
                "Late childhood stepfamily"
            )
    )
    ]

    # Generate no. of observations per country
    dt[, country_n := .N, by = "COUNTRY"]

    dt <- unique(
        dt[, 
            .(proportion = round(.N / country_n, 2)), 
            by = .(COUNTRY, cluster)
        ]
    )

    # To wide format
    dt <- dcast(
        dt,
        COUNTRY ~ cluster,
        value.var = "proportion"
    )

    tab <- knitr::kable(
        dt,
        format = "latex",
        booktabs = TRUE
    )

    return(tab)
}

##
# Plots

# Colors for sequence plots
plot_colors <- function(){

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
            axis.line.x = element_line(size = .3),
            axis.ticks = element_line(size = .3)
        )

    return(density_plot)
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
                    axis.line.x = element_line(size = .3),
                    axis.ticks = element_line(size = .3),
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
