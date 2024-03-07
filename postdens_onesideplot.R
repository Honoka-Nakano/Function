## postdens_onesideplot.R
##
## Create a density plot on one side.
##
## Created: 2024-02-19 Keisuke Nakakno
## Modified: 2024-03-08 Keisuke Nakano
##
## Dependent: tidyverse

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

postdens_onesideplot <- function(post_sample,
                                 cutpoint = NULL, adjust = 1,
                                 include = TRUE, reverse = FALSE,
                                 from = min(post_sample), to = max(post_sample),
                                 linecolor = 'black', fillcolor = 'dodgerblue',
                                 xlab = NULL, ylab = NULL, title = NULL) {
  ## function to create a posterior density plot with values greater than the cutpoint.
  ## params:
  ##  post_sample = posterior sample (must be a vector)
  ##  adjust      = adjust bandwidth for smoothing. See the help of density()
  ##  cutpoint    = boundary value for colored area
  ##  include     = if TRUE, colored area include cutpoint
  ##  reverse     = if TRUE, colored area is created with values smaller than cutpoint.
  ##  from        = the lower bound of density estimation
  ##  to          = the upper bound of density estimation
  
  xlab <- ifelse(is.null(xlab), 'parameter', xlab)
  ylab <- ifelse(is.null(ylab), 'density', ylab)
  title <- ifelse(is.null(title), '', title)
  dens <- density(x = post_sample, adjust = 1,
                  from = from, to = to)
  df <- tibble(parameter = dens$x,
               density   = dens$y)
  
  if (reverse) {
    if (include) {
      df_ribbon <- df |> filter(parameter <= cutpoint)
      prob <- mean(df$parameter <= cutpoint)
    } else {
      df_ribbon <- df |> filter(parameter < cutpoint)
      prob <- mean(df$parameter < cutpoint)
    }
  } else {
    if (include) {
      df_ribbon <- df |> filter(parameter >= cutpoint)
      prob <- mean(df$parameter >= cutpoint)
    } else {
      df_ribbon <- df |> filter(parameter > cutpoint)
      prob <- mean(df$parameter > cutpoint)
    }
  }

  p <- ggplot(df, aes(x = parameter, y = density)) +
    geom_hline(yintercept = 0, color = 'gray') +
    geom_ribbon(data = df_ribbon,
                aes(x = parameter, ymin = 0, ymax = density),
                fill = fillcolor) +
    geom_line(color = linecolor) +
    labs(x = xlab, y = ylab, title = title)

  cat('prob:', prob)
  return(p)
}
