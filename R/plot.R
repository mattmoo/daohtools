#' Plot DAOH in a histogram
#'
#' @param input.dt The DAOH data (with daoh column)
#' @param by.group The group (string) by which to separate the histograms.
#' 
#' @return ggplot2 plot
#'
#' @export
plot.daoh.histogram = function(input.dt, by.group = NA_character_) {


  #Graph display properties
  figureTextSize = 12
  histogramBins = 90
  NRPercent <- function(x) {
    paste0(sapply(x, scales::percent_format(accuracy = 0.1)))
  }

  ybreaks =  c(0, 0.001, 0.0025,0.005,0.01,0.02,0.03,0.04,0.05,0.075,seq(from=0.1, to = 1, by = 0.05))
  transformed.y.scale = ggplot2::scale_y_continuous(trans=scales::trans_new("mysqrt",
                                                                            transform = base::sqrt,
                                                                            inverse = function(x) ifelse(x<0, 0, x^2),
                                                                            domain = c(0, Inf)),
                                                    labels = NRPercent,
                                                    minor_breaks = NULL,
                                                    breaks = ybreaks,
                                                    expand = c(0.001, 0.001))

  x.breaks =  seq(0,90,by=15)
  x.scale = ggplot2::scale_x_continuous(breaks = x.breaks, expand = c(0.01, 0.1))


  if (!is.na(by.group)) {
    p = ggplot2::ggplot(input.dt, ggplot2::aes(x=daoh, fill=get(by.group), y=4*(..density..)/sum(..density..))) +
      ggplot2::geom_histogram(alpha=.35,
                              position="identity",
                              bins=histogramBins)
  } else {
    p = ggplot2::ggplot(input.dt, ggplot2::aes(x=daoh, y=2*(..density..)/sum(..density..))) +
      ggplot2::geom_histogram(alpha=.75,
                              position="identity",
                              bins=histogramBins)
  }

  p = p +
    ggplot2::labs(title="Frequency for Overall DAOH") +
    ggplot2::labs(x="Days alive and out of hospital", y="Frequency") +
    x.scale +
    transformed.y.scale +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    # theme(text = element_text(size=figureTextSize))
  return(p)
}

#' Plot DAOH in a density chart
#'
#' @param input.dt The DAOH data (with daoh column)
#' @param by.group The group (string) by which to separate the histograms.
#' 
#' @return ggplot2 plot
#'
#' @export
plot.daoh.density = function(input.dt, by.group = NA_character_, xlimits = c(0,90), adjust = 1.5) {
  
  
  ybreaks =  c(0, 0.001, 0.0025,0.005,0.01,0.02,0.03,0.04,0.05,0.075,seq(from=0.1, to = .5, by = 0.05),seq(from=0.6, to = 1, by = 0.1))
  transformed.y.scale = ggplot2::scale_y_continuous(trans=scales::trans_new("mysqrt",
                                                                            transform = base::sqrt,
                                                                            inverse = function(x) ifelse(x<0, 0, x^2),
                                                                            domain = c(0, Inf)),
                                                    # label = NRPercent,
                                                    minor_breaks = NULL,
                                                    breaks = ybreaks,
                                                    expand = c(0.001, 0.001))
  
  x.breaks =  seq(xlimits[1],xlimits[2],by=15)
  x.scale = ggplot2::scale_x_continuous(breaks = x.breaks, expand = c(0.01, 0.1), limits = xlimits)
  
  if (!is.na(by.group)) {
    p = ggplot2::ggplot(input.dt, ggplot2::aes(x=daoh, fill=stringr::str_to_title(get(by.group)), colour = stringr::str_to_title(get(by.group)))) +
      ggplot2::geom_density(aes(y=..density.., linetype = stringr::str_to_title(get(by.group))), alpha=0.03, adjust = adjust, size = 0.5) +
      scale_colour_brewer(palette = 'Set1', direction = -1, name = by.group) +
      scale_fill_brewer(palette = 'Set1', direction = -1, name = by.group) +
      scale_linetype_manual(values = c("dashed", "solid"), name = by.group)
  } else {
    p = ggplot2::ggplot(input.dt, ggplot2::aes(x=daoh, y=2*(..density..)/sum(..density..))) +
      ggplot2::geom_density(aes(y=..density..), alpha=0.03, adjust = adjust, size = 0.5)
  }
  
  p = p +
    x.scale +
    transformed.y.scale + 
    labs(x = 'Days alive and out of hospital', y = 'Density (square-root transform)') + 
    guides(fill=guide_legend(title=stringr::str_to_title(by.group)),
           linetype=guide_legend(title=stringr::str_to_title(by.group)),
           colour=guide_legend(title=stringr::str_to_title(by.group)))
}

#' Plot the distribution by drawing from it heaps.
#' 
#' @param x The values in the distribution
#' @param prob The probability that the value will be drawn.
#' @param n The number of points to sample. It actually has to be pretty high
#'   for DAOH, about 1e8 (10,000,000)
#' 
#' @return ggplot2 plot
#'
#' @export
plot.distribution = function(x, prob, n = 1e8) {
  daoh = sample(x = x, prob = prob, size = n, replace = T)
  p = plot.daoh.histogram(data.table(daoh = daoh))
}
