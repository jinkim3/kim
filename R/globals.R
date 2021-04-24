# set global variables, mostly for functions in ggplot2
utils::globalVariables(unlist(strsplit(gsub("\n", "",
 "%+replace% aes annotate coord_cartesian element_blank element_line
 element_text geom_bar geom_errorbar geom_errorbarh geom_histogram
 geom_line geom_point geom_smooth geom_text geom_vline ggplot ggtitle
 labs margin position_jitter scale_linetype_manual scale_size
 scale_x_continuous scale_y_continuous scale_y_discrete stat theme
 theme_classic unit xlab ylab"), " ")))
