# Code for simplex plots used in this presentation:
# https://www.youtube.com/watch?v=GxoVfZRv6Ms
# Pages 56 to 63 in this PDF: https://mariobecerra.github.io/files/2021-10-22_RSSB_conference_Liege_v3.pdf

library(opdesmixr)
library(tidyverse)
library(xtable)
library(here)

source(here("code/utils.R"))


designs_folder = here("out/cocktail_cornell_designs/")


out_folder = here("out/cocktail_cornell_plots_and_tables/rssb_conference/")
dir.create(out_folder, showWarnings = F)





# Cocktail experiment -----------------------------------------------------
############################################################################
############################################################################
############################################################################




#### Load designs for cocktail experiment

beta0 = c(1.36, 1.57, 2.47, -0.43, 0.50, 1.09)

# x1, mango juice, 0.3
# x2, blackcurrant syrup, 0.15
# x3, lemon juice, 0.1
lower_bounds_cocktail = c(0.3, 0.15, 0.1)
sum_lower_bounds = sum(lower_bounds_cocktail)

cocktail_d_opt_filename = paste0(designs_folder, "cocktail_d_optimal.rds")
cocktail_i_opt_filename = paste0(designs_folder, "cocktail_i_optimal.rds")

cocktail_D_opt = readRDS(cocktail_d_opt_filename)
cocktail_I_opt = readRDS(cocktail_i_opt_filename)

# opdesmixr package must be installed for this to work
ruseckaite_cocktail_designs = read_csv(system.file("extdata", "ruseckaite_cocktail_designs.csv", package = "opdesmixr", mustWork = T))


ruseckaite_cocktail_beta0_design = mnl_design_dataframe_to_array(
  ruseckaite_cocktail_designs %>%
    filter(prior == "beta_0 and sigma_0") %>%
    select(-prior)
)





###############################
### Cocktail design plots
###############################

levels_cocktail = c(0, .5625, 1.125, 1.6875, 2.25)
utilities_cocktail_bayesian_plot = opdesmixr:::scheffe_order3_q3_utilities(beta0, 100000) %>%
  mutate(utility_fact = cut(utility, levels_cocktail, right = F)) %>%
  mutate(utility_int = as.integer(utility_fact)) %>%
  mutate(utility_avg = case_when(
    utility_int == 1 ~ (levels_cocktail[1] + levels_cocktail[2] - levels_cocktail[1])/2,
    utility_int == 2 ~ (levels_cocktail[2] + levels_cocktail[3] - levels_cocktail[2])/2,
    utility_int == 3 ~ (levels_cocktail[3] + levels_cocktail[4] - levels_cocktail[3])/2,
    utility_int == 4 ~ (levels_cocktail[4] + levels_cocktail[5] - levels_cocktail[4])/2
  ))



# Color palette that goes from yellow to red
color_palette_cocktail = tibble(cutoffs = unique(utilities_cocktail_bayesian_plot$utility_fact)) %>%
  mutate(g = 1 - seq(0, 1, length.out = nrow(.)), r = 1, b = 0) %>%
  mutate(color = rgb(r, g, b))




# Settings for plots in PNG files
width_cocktail_simplex = 10
height_cocktail_simplex = 10
utility_point_size_cocktail = 0.01
utility_point_shape_cocktail = "circle"





choice_sets_to_show = c(1, 3, 9, 12, 15, 16)

for(i in 0:(length(choice_sets_to_show) + 1)){
  
  if(i == length(choice_sets_to_show) + 1){
    choice_sets_i = 1:length(unique(X_final_tbl_D$choice_set))
  } else{
    if(i == 0){
      choice_sets_i = NULL
    } else{
      choice_sets_i = choice_sets_to_show[1:i]
    }
    
  }
  
  X_final_tbl_D = mnl_design_array_to_dataframe(cocktail_D_opt$X)
  X_final_tbl_I = mnl_design_array_to_dataframe(cocktail_I_opt$X)
  
  J = dim(cocktail_D_opt$X)[2]
  
  plot_d = ggtern::ggtern(data = utilities_cocktail_bayesian_plot) +
    ggplot2::geom_point(
      aes(x = x1,
          y = x2, z = x3, color = factor(utility_int)),
      size = utility_point_size_cocktail,
      shape = utility_point_shape_cocktail,
      alpha = 1
    ) +
    ggplot2::scale_color_manual(values = color_palette_cocktail$color) +
    ggplot2::geom_point(
      data = X_final_tbl_D %>% 
        filter(choice_set %in% choice_sets_i) %>% 
        mutate(choice_set = as.character(as.integer(gl(length(choice_sets_i), J)))), 
      ggtern::aes(c1, c2, c3, shape = choice_set), color = "black", size = 2, stroke = 1.5, inherit.aes = F, alpha = 1) +
    ggtern::theme_nomask() +
    scale_shape_manual(values = 1:length(unique(X_final_tbl_D$choice_set))) +
    theme(legend.position = "none", legend.box = "vertical")
  
  
  
  plot_i = ggtern::ggtern(data = utilities_cocktail_bayesian_plot) +
    ggplot2::geom_point(
      aes(x = x1,
          y = x2, z = x3, color = factor(utility_int)),
      size = utility_point_size_cocktail,
      shape = utility_point_shape_cocktail,
      alpha = 1
    ) +
    ggplot2::scale_color_manual(values = color_palette_cocktail$color) +
    ggplot2::geom_point(
      data = X_final_tbl_I %>% 
        filter(choice_set %in% choice_sets_i) %>% 
        mutate(choice_set = as.character(as.integer(gl(length(choice_sets_i), J)))), 
      ggtern::aes(c1, c2, c3, shape = choice_set), color = "black", size = 2, stroke = 1.5, inherit.aes = F, alpha = 1) +
    ggtern::theme_nomask() +
    scale_shape_manual(values = 1:length(unique(X_final_tbl_I$choice_set))) +
    theme(legend.position = "none", legend.box = "vertical")
  
  
  
  ### One by one
  ggtern::ggsave(
    filename = paste0(out_folder, "res_cocktail_design_db_simplex_", i, ".png"),
    plot = plot_d,
    width = width_cocktail_simplex,
    height = height_cocktail_simplex,
    units = "cm"
  )


  ggtern::ggsave(
    filename = paste0(out_folder, "res_cocktail_design_ib_simplex_", i, ".png"),
    plot = plot_i,
    width = width_cocktail_simplex,
    height = height_cocktail_simplex,
    units = "cm"
  )
  
  ### Side by side
  
  plot_both = ggtern::grid.arrange(
    plot_d, plot_i, ncol = 2
  )
  
  ggtern::ggsave(
    filename = paste0(out_folder, "res_cocktail_design_both_simplex_", i, ".png"),
    plot = plot_both,
    width = 2*width_cocktail_simplex,
    height = height_cocktail_simplex,
    units = "cm"
  )
  
  
}




