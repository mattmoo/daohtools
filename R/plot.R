#' Plot DAOH in a histogram
#'
#' @param input.dt The DAOH data (with daoh column)
#' @param by.group The group (string) by which to separate the histograms.
#' @param xlimits Length two numeric vector of xlimits..
#' 
#' @return ggplot2 plot
#'
#' @export plot.daoh.histogram
plot.daoh.histogram = function(input.dt,
                               daoh.col.name = 'daoh',
                               by.group = NA_character_,
                               xlimits = c(-0.5, 90.5)) {
  
  
  #Graph display properties
  figureTextSize = 12
  
  
  y.scale.labels = function(x)
    sprintf("%0.2g%%", round(x * 100, digits = 5))
  ybreaks =  c(0,
               0.001,
               0.0025,
               0.005,
               0.01,
               0.02,
               0.03,
               0.04,
               0.05,
               0.075,
               seq(from = 0.1, to = 1, by = 0.05))
  
  transformed.y.scale = scale_y_sqrt(
    labels = y.scale.labels,
    minor_breaks = NULL,
    breaks = ybreaks,
    expand = c(0.001, 0.001),
    limits = c(0, .25)
  )
  
  x.breaks =  seq(-600, 600, by = 15)
  x.scale = ggplot2::scale_x_continuous(breaks = x.breaks,
                                        expand = c(0.01, 0.1),
                                        limits = xlimits)
  
  
  if (!is.na(by.group)) {
    p = ggplot2::ggplot(input.dt, ggplot2::aes(
      x = get(daoh.col.name), 
      y = after_stat(density),
      fill = get(by.group),
      colour = get(by.group),
      linetype = get(by.group)
    )) +
      ggplot2::geom_histogram(position = "identity",
                              binwidth = 1,
                              size = 0.75)
    # ggplot2::geom_point()
  } else {
    p = ggplot2::ggplot(input.dt, ggplot2::aes(x = get(daoh.col.name), y = after_stat(density))) +
      ggplot2::geom_histogram(alpha = .75,
                              position = "identity",
                              binwidth = 1)
  }
  
  p = p +
    ggplot2::labs(title="Frequency for Overall DAOH") +
    ggplot2::labs(x="Days alive and out of hospital", y="Density") +
    x.scale +
    transformed.y.scale +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  return(p)
}

#' Plot DAOH in a frequency polygon
#'
#' @param input.dt The DAOH data (with daoh column)
#' @param by.group The group (string) by which to separate the histograms.
#' @param xlimits Length two numeric vector of xlimits..
#' 
#' @return ggplot2 plot
#'
#' @export plot.daoh.freqpoly
plot.daoh.freqpoly = function(input.dt,
                              daoh.col.name = 'daoh',
                              by.group = NA_character_,
                              xlimits = c(-0.5, 90.5)) {
  
  #Graph display properties
  figureTextSize = 12
  
  
  y.scale.labels = function(x)
    sprintf("%0.2g%%", round(x * 100, digits = 5))
  ybreaks =  c(0,
               0.001,
               0.0025,
               0.005,
               0.01,
               0.02,
               0.03,
               0.04,
               0.05,
               0.075,
               seq(from = 0.1, to = 1, by = 0.05))
  
  transformed.y.scale = scale_y_sqrt(
    labels = y.scale.labels,
    minor_breaks = NULL,
    breaks = ybreaks,
    expand = c(0.001, 0.001),
    limits = c(0, .25)
  )
  
  x.breaks =  seq(-600, 600, by = 15)
  x.scale = ggplot2::scale_x_continuous(breaks = x.breaks,
                                        expand = c(0.01, 0.1),
                                        limits = xlimits)
  
  
  if (!is.na(by.group)) {
    p = ggplot2::ggplot(input.dt, ggplot2::aes(
      x = daoh, 
      y = after_stat(density),
      colour = get(by.group),
      linetype = get(by.group)
    )) +
      ggplot2::geom_freqpoly(alpha = .75,
                             position = "identity",
                             binwidth = 1,
                             size = 0.75)
    # ggplot2::geom_point()
  } else {
    p = ggplot2::ggplot(input.dt, ggplot2::aes(x = daoh, y = ((..count..) /
                                                                sum(..count..)))) +
      ggplot2::geom_freqpoly(alpha = .75,
                             position = "identity",
                             binwidth = 1)
  }
  
  p = p +
    ggplot2::labs(title="Frequency for Overall DAOH") +
    ggplot2::labs(x="Days alive and out of hospital", y="Density") +
    x.scale +
    transformed.y.scale +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    # scale_colour_grey(start = 0.1, end = 0.5) +
    # scale_fill_manual(values = as.vector(yarrr::piratepal(palette = "google"))[c(4,2)], name = by.group) +
    # scale_linetype_manual(values = c( "dashed", "solid"), name = by.group)
  return(p)
}

#' Plot DAOH in a density chart
#'
#' @param input.dt The DAOH data (with daoh column)
#' @param daoh.col.name Name of DAOH column
#' @param by.group The group (string) by which to separate the histograms.
#' @param xlimits Length two numeric vector of xlimits..
#' 
#' @return ggplot2 plot
#'
#' @export plot.daoh.density
plot.daoh.density = function(input.dt, 
                             daoh.col.name = 'daoh',
                             by.group = NA_character_, 
                             xlimits = c(-0.5,90.5), 
                             adjust = 1.5) {
  
  
  # ybreaks =  c(0, 0.001, 0.0025,0.005,0.01,0.02,0.03,0.04,0.05,0.075,seq(from=0.1, to = .5, by = 0.05),seq(from=0.6, to = 1, by = 0.1))
  # NRPercent <- function(x) {
  #   paste0(sapply(x, scales::percent_format(accuracy = 0.1)))
  # }
  # transformed.y.scale = ggplot2::scale_y_continuous(trans=scales::trans_new("mysqrt",
  #                                                                           transform = base::sqrt,
  #                                                                           inverse = function(x) ifelse(x<0, 0, x^2),
  #                                                                           domain = c(0, Inf)),
  #                                                   label = NRPercent,
  #                                                   minor_breaks = NULL,
  #                                                   breaks = ybreaks,
  #                                                   expand = c(0.001, 0.001))
  # 
  # x.breaks =  seq(xlimits[1],xlimits[2],by=15)
  # x.scale = ggplot2::scale_x_continuous(breaks = x.breaks, expand = c(0.01, 0.1), limits = xlimits)
  # 
  # if (!is.na(by.group)) {
  #   p = ggplot2::ggplot(input.dt, ggplot2::aes(x=daoh, fill=stringr::str_to_title(get(by.group)), colour = stringr::str_to_title(get(by.group)))) +
  #     ggplot2::geom_density(aes(y=..density.., linetype = stringr::str_to_title(get(by.group))), alpha=0.03, adjust = adjust, size = 0.5) +
  #     scale_colour_brewer(palette = 'Set1', direction = -1, name = by.group) +
  #     scale_fill_brewer(palette = 'Set1', direction = -1, name = by.group) +
  #     scale_linetype_manual(values = c("dashed", "solid"), name = by.group)
  # } else {
  #   p = ggplot2::ggplot(input.dt, ggplot2::aes(x=daoh, y=2*(..density..)/sum(..density..))) +
  #     ggplot2::geom_density(aes(y=..density..), alpha=0.03, adjust = adjust, size = 0.5)
  # }
  # 
  # p = p +
  #   x.scale +
  #   transformed.y.scale + 
  #   labs(x = 'Days alive and out of hospital', y = 'Density (square-root transform)') + 
  #   guides(fill=guide_legend(title=stringr::str_to_title(by.group)),
  #          linetype=guide_legend(title=stringr::str_to_title(by.group)),
  #          colour=guide_legend(title=stringr::str_to_title(by.group)))
  
  ybreaks =  c(
    0,
    0.001,
    0.0025,
    0.005,
    0.01,
    0.02,
    0.03,
    0.04,
    0.05,
    0.075,
    seq(from = 0.1, to = .5, by = 0.05),
    seq(from = 0.6, to = 1, by = 0.1)
  )
  transformed.y.scale = ggplot2::scale_y_continuous(trans=scales::trans_new("mysqrt",
                                                                            transform = base::sqrt,
                                                                            inverse = function(x) ifelse(x<0, 0, x^2),
                                                                            domain = c(0, Inf)),
                                                    labels = scales::percent_format(accuracy = 0.01),
                                                    minor_breaks = NULL,
                                                    breaks = ybreaks,
                                                    expand = c(0.001, 0.001))
  
  x.breaks =  seq(0,720,by=10)
  x.scale = ggplot2::scale_x_continuous(breaks = x.breaks, expand = c(0.01, 0.1), limits = xlimits)
  
  p = ggplot(input.dt,
             aes(
               x = get(daoh.col.name),
               y = stat(density))) +
    
    x.scale +
    transformed.y.scale +
    labs(x = 'Days alive and out of hospital (90 days)', y = 'Percentage of group')
  
  if (!is.na(by.group)) {
    p = p +
      geom_histogram(
        aes(
          fill = get(by.group),
          colour = get(by.group),
          linetype = get(by.group)
        ),
        alpha = .1,
        position = 'identity',
        size = 0,
        binwidth = 1
      ) +
      scale_colour_manual(values = yarrr::transparent(as.vector(yarrr::piratepal(palette = "google"))[c(4, 2)],
                                                      trans.val = 0.5),
                          name = by.group) +
      ggnewscale::new_scale_colour() +
      geom_density(
        aes(
          fill = get(by.group),
          colour = get(by.group),
          linetype = get(by.group)
        ),
        adjust = adjust,
        size = .75,
        alpha = 0.25
      ) +
      scale_colour_manual(values = as.vector(yarrr::piratepal(palette = "google"))[c(4,2)], name = by.group) +
      scale_fill_manual(values = as.vector(yarrr::piratepal(palette = "google"))[c(4,2)], name = by.group) +
      scale_linetype_manual(values = c("solid", "dashed"), name = by.group)
      
  } else {
    p = p +
      geom_histogram(
        alpha = .5,
        position = 'identity',
        size = 0,
        binwidth = 1
      ) +
      geom_density(
        adjust = adjust,
        size = .75,
        alpha = 0.25
      )
  }
  
    # scale_colour_discrete(palette = 'Dark2',
    # direction = 1,
    #                     name = by.group) +
    # scale_fill_brewer(palette = 'Set2',
    #                   direction = 1,
    #                   name = by.group)
  
  return(p)
}




#' Plot DAOH barplot.
#'
#' Plots a bar plot using ggplot2. Input to this function must be summarised, it
#' will not count for you.
#'
#' @param input.summary.dt A data.table with DAOH summarised by value (and by
#'   some other group if desired).
#' @param daoh.col.name Name of the DAOH column
#' @param by.group Name of the stratifying group column, none if NULL (e.g., an
#'   intervention) (Default: NULL)
#' @param xlimits Limits on the x-axis of the plot (Default: c(-0.5, 90.5))
#' @param y.aes What's on the y-axis. "prop" for proportion per group, "N" for
#'   frequency. (Default:  c('prop', "N")[1])
#'
#' @return A ggplot2 plot.
#' @export plot.daoh.barplot
#'
#' @examples
plot.daoh.barplot = function(input.summary.dt,
                             daoh.col.name = 'daoh',
                             by.group = NULL,
                             xlimits = c(-0.5, 90.5),
                             y.aes = c('prop', "N")[1]) {
  
  
  y.lab = ''
  if (y.aes == 'prop') {
    y.lab = 'Percentage of group'
    
    y.scale.labels = function(x)
      sprintf("%0.2g%%", round(x * 100, digits = 5))
    ybreaks =  c(0,
                 0.001,
                 0.0025,
                 0.005,
                 0.01,
                 0.02,
                 0.03,
                 0.04,
                 0.05,
                 0.075,
                 seq(from = 0.1, to = 1, by = 0.05))
    
    transformed.y.scale = scale_y_sqrt(
      name = y.lab,
      labels = y.scale.labels,
      minor_breaks = NULL,
      breaks = ybreaks,
      expand = c(0.001, 0.001),
      limits = c(0, .25)
    )
  }
  if (y.aes == 'N') {
    y.lab = 'Frequency'
    
    ybreaks =  c(0,
                 10,
                 100,
                 250,
                 500,
                 750,
                 1000,
                 1500,
                 2000,
                 2500,
                 seq(from = 3000, to = 100000, by = 1000))
    
    transformed.y.scale = scale_y_sqrt(
      name = y.lab,
      breaks = ybreaks,
      expand = c(0.001, 0.001)
    )
  }

  x.major.breaks =  seq(-600, 600, by = 15)
  x.minor.breaks =  seq(-600, 600, by = 5)
  x.scale = ggplot2::scale_x_continuous(breaks = x.major.breaks,
                                        minor_breaks = x.minor.breaks,
                                        expand = c(0.01, 0.1),
                                        limits = xlimits)
  
  if (!is.null(by.group)) {
    p = ggplot(data = input.summary.dt, aes(x = get(daoh.col.name), 
                                             y = get(y.aes), 
                                             fill = get(by.group),
                                             colour = get(by.group),
                                             linetype = get(by.group))) +
      geom_bar(width = 1, stat = "identity", position = "identity")  +
      
      scale_colour_grey(start = 0.5, end = 0.1, name = by.group) +
      scale_fill_manual(values = c(rgb(0, 0, 0, .35), rgb(0, 0, 0, 0)), name = by.group) +
      scale_linetype_manual(values = c("blank", "solid"), name = by.group)
    
  } else {
    
    p = ggplot2::ggplot(input.summary.dt, ggplot2::aes(x = get(daoh.col.name),
                                                       y = get(y.aes))) +
      geom_bar(width = 1, stat = "identity", position = "identity", alpha = 0.75) 
  }
  
  p = p +
    ggplot2::labs(x = "Days alive and out of hospital") +
    x.scale +
    transformed.y.scale +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  return(p)
}

#' Summarise a data.table
#'
#' Plots a bar plot using ggplot2. Input to this function must be summarised, it
#' will not count for you.
#'
#' @param input.summary.dt A data.table with DAOH summarised by value (and by
#'   some other group if desired).
#' @param daoh.col.name Name of the DAOH column
#' @param by.group Name of the stratifying group column, none if NA (e.g., an
#'   intervention) (Default: NULL)
#' @param weight.col.name Name of a column to weight observations by, for
#'   risk-adjustment purposes. If provided, it will be summed instead of the
#'   number of instance (Default: NULL)
#' @return data.table with totals and proportions per group of DAOH values
#'   
#' @export
calculate.summary.dt = function(input.dt,
                                daoh.col.name = "daoh",
                                by.group = NULL,
                                weight.col.name = NULL) {
  
  summarise.by.cols = c(daoh.col.name, by.group)
  
  if (is.null(weight.col.name)) {
    summary.dt = input.dt[, .(N = .N), by = summarise.by.cols]
    summary.dt[, prop := N/sum(N), by = by.group]
  } else {
    summary.dt = input.dt[, .(N = sum(get(weight.col.name))), by = summarise.by.cols]
    summary.dt[, prop := N/sum(N), by = by.group]
  }
  setorderv(summary.dt, summarise.by.cols)
  
  return(summary.dt)
  
}
