self_document <- function(expr) {
#  https://www.tjmahr.com/self-titled-ggplot2-plots/
 
  monofont <- ifelse(
    extrafont::choose_font("Consolas") == "", 
    "mono", 
    "Consolas"
  )
  
  p <- rlang::enexpr(expr)
  title <- rlang::expr_text(p) |> 
    grkstyle::grk_style_text() |> 
    paste0(collapse = "\n")
  
  patchwork::wrap_elements(eval(p)) + 
    patchwork::plot_annotation(
      title = title, 
      theme = theme(
        plot.title = element_text(
          family = monofont, hjust = 0, size = rel(.9), 
          margin = margin(0, 0, 5.5, 0, unit = "pt")
        )
      )
    )
}

# Example:  

#install.packages("grkstyle")
#devtools::install_github("gadenbuie/grkstyle")

library(ggplot2)
source("functions/self_document.R")

# Parameters of plot
color1="blue"
color2="yellow"

self_document(
  ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(bins = 20, color = !! color1, fill = "orange") +
    labs(title = "A basic histogram") +
    theme_bw()
)  


