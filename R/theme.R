# plotting theme from https://github.com/bcgov/BC-demography

theme_BCStats <- function() {
  theme_bw() +
    theme(
      panel.border = element_rect(colour="white"),
      axis.line = element_line(colour="black"),
      legend.position=c(1,0), 
      legend.justification=c(1,0),
      legend.title = element_text(size=12),
      legend.text = element_text(size=11),
      axis.title = element_text(size=16),
      axis.text = element_text(size=16),
      plot.title = element_text(size=18)
    )
}
  
#

# colour palette for BC Stats charts (use with scale_colour_manual)
palette_BCStats <- function() {
  c("#234275", "#E3A82B", "#26BDEF", "#11CC33", "#D3E2ED", "8A8A8A")
}

# grayscale for fill (use with scale_fill_manual)
palette_BCStats_fill <- function() {
  c("#3F3F3F", "#ABABAB", "#DFDFDF", "#969696", "#838383", "8A8A8A")
}

