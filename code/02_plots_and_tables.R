
library(opdesmixr)
library(tidyverse)
library(xtable)
library(here)

source(here("code/utils.R"))


designs_folder = here("out/cocktail_cornell_designs/")

out_folder = here("out/cocktail_cornell_plots_and_tables/")
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
#### Print designs for Latex
###############################

col_names_cocktail_table = c("Choice set", "x1", "x2", "x3", "a1", "a2", "a3")

cat("Writing table for cocktail Bayesian D-optimal design...")
transform_tibble_from_pseudocomp_to_comp(mnl_design_array_to_dataframe(cocktail_D_opt$X), lower_bounds_cocktail, 1:3) %>%
  select(4, 1:3, 5:7) %>%
  set_names(col_names_cocktail_table) %>%
  xtable::xtable(.,
                 caption = "Bayesian D-optimal design for cocktail experiment",
                 label = "tab:cocktail_exp_d_optimal_des") %>%
  print(., include.rownames = F, file = paste0(out_folder, "res_cocktail_table_d_opt_design.tex"))
cat("Done\n\n")



cat("Writing table for cocktail Bayesian I-optimal design...")
transform_tibble_from_pseudocomp_to_comp(mnl_design_array_to_dataframe(cocktail_I_opt$X), lower_bounds_cocktail, 1:3) %>%
  select(4, 1:3, 5:7) %>%
  set_names(col_names_cocktail_table) %>%
  xtable::xtable(.,
                 caption = "Bayesian I-optimal design for cocktail experiment",
                 label = "tab:cocktail_exp_i_optimal_des") %>%
  print(., include.rownames = F, file = paste0(out_folder, "res_cocktail_table_i_opt_design.tex"))
cat("Done\n\n")



###############################
#### Plot utility balance
###############################

(
  get_product_of_choice_probabilities(cocktail_I_opt$X, cocktail_I_opt$beta, order = 3, transform_beta = T) %>%
    mutate(Design = "Bayesian\nI-optimal") %>%
    bind_rows(
      get_product_of_choice_probabilities(cocktail_D_opt$X, cocktail_D_opt$beta, order = 3, transform_beta = T) %>%
        mutate(Design = "Bayesian\nD-optimal")
    ) %>%
    ggplot() +
    geom_boxplot(aes(x = Design, y = prods)) +
    theme_bw() +
    ylab("Product of choice probabilities") +
    ylim(0, 0.25)
) %>%
  ggplot2::ggsave(
    filename = paste0(out_folder, "res_cocktail_choice_probs_plot.png"),
    plot = .,
    width = 8,
    height = 9,
    units = "cm"
  )



###############################
#### Plot distances between alternatives within each choice set
###############################

(
  get_distances_within_choice_set(cocktail_I_opt$X) %>%
    mutate(Design = "Bayesian\nI-optimal") %>%
    bind_rows(
      get_distances_within_choice_set(cocktail_D_opt$X) %>%
        mutate(Design = "Bayesian\nD-optimal")
    ) %>%
    ggplot() +
    geom_boxplot(aes(x = Design, y = dist)) +
    theme_bw() +
    ylab("Distance between alternatives")
) %>%
  ggplot2::ggsave(
    filename = paste0(out_folder, "res_cocktail_distances_within_choice_set.png"),
    plot = .,
    width = 8,
    height = 9,
    units = "cm"
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

# # Plot cutoffs to create legend (not useful in the end because I did it in Latex)
# color_palette_cocktail %>%
#   ggplot() +
#   geom_rect(
#     aes(xmin = r,
#         xmax = r + resolution(r),
#         ymin = g,
#         ymax = g + resolution(g),
#         fill = rgb(r, g, b)), color = "white", size = 0.1) +
#   scale_fill_identity() +
#   geom_text(aes(x = r + resolution(r)/2, y = g + resolution(g)/2, label = cutoffs)) +
#   theme_void()

# Settings for plots in PNG files
width_cocktail_simplex = 10
height_cocktail_simplex = 10
utility_point_size_cocktail = 0.01
utility_point_shape_cocktail = "circle"

ggtern::ggsave(
  filename = paste0(out_folder, "res_cocktail_design_db_simplex.png"),
  plot = plot_choice_set_utility_discrete_palette(
    cocktail_D_opt,
    utilities_cocktail_bayesian_plot,
    utility_point_size = utility_point_size_cocktail,
    utility_point_shape = utility_point_shape_cocktail,
    color_palette = color_palette_cocktail$color
  ) +
    theme(legend.position = "none") +
    labs(x = "x_1", y = "x_2", z = "x_3") +
    ggtern::theme_latex(TRUE),
  width = width_cocktail_simplex,
  height = height_cocktail_simplex,
  units = "cm"
)

ggtern::ggsave(
  filename = paste0(out_folder, "res_cocktail_design_ib_simplex.png"),
  plot = plot_choice_set_utility_discrete_palette(
    cocktail_I_opt,
    utilities_cocktail_bayesian_plot,
    utility_point_size = utility_point_size_cocktail,
    utility_point_shape = utility_point_shape_cocktail,
    color_palette = color_palette_cocktail$color
  ) +
    theme(legend.position = "none") +
    labs(x = "x_1", y = "x_2", z = "x_3") +
    ggtern::theme_latex(TRUE),
  width = width_cocktail_simplex,
  height = height_cocktail_simplex,
  units = "cm"
)


###############################
### Cocktail FDS plots
###############################

n_points_per_alternative_cocktail = 2000

pred_vars_cocktail = mnl_get_fds_simulations(
  design_array = cocktail_I_opt$X,
  beta = cocktail_I_opt$beta,
  order = 3,
  n_points_per_alternative = n_points_per_alternative_cocktail,
  transform_beta = F,
  verbose = 1) %>%
  mutate(Design = "Bayesian I-optimal") %>%
  bind_rows(
    mnl_get_fds_simulations(
      design_array = cocktail_D_opt$X,
      beta = cocktail_D_opt$beta,
      order = 3,
      n_points_per_alternative = n_points_per_alternative_cocktail,
      transform_beta = F,
      verbose = 1) %>%
      mutate(Design = "Bayesian D-optimal")
  ) %>%
  bind_rows(
    mnl_get_fds_simulations(
      design_array = ruseckaite_cocktail_beta0_design,
      beta = cocktail_D_opt$beta,
      order = 3,
      n_points_per_alternative = n_points_per_alternative_cocktail,
      transform_beta = F,
      verbose = 1) %>%
      mutate(Design = "Ruseckaite et al.")
  )



cocktail_fds_plots = pred_vars_cocktail %>%
  ggplot() +
  geom_vline(xintercept = 0.5, linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = pred_vars_cocktail %>%
               group_by(Design) %>%
               summarize(
                 med = median(pred_var),
                 mean = mean(pred_var)) %>%
               pull(med),
             linetype = "dashed", size = 0.2) +
  geom_line(aes(fraction, pred_var, color = Design), size = 0.8) +
  xlab("Fraction of design space") +
  ylab("Prediction variance") +
  # ggtitle("Cocktail experiment") +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("red", "blue", "dark green"))


width_cocktail_fds = 18
height_cocktail_fds = 10

ggplot2::ggsave(
  filename = paste0(out_folder, "res_cocktail_fds_db_vs_ib_plot.png"),
  plot = cocktail_fds_plots,
  width = width_cocktail_fds,
  height = height_cocktail_fds,
  units = "cm"
)
























# Cornell's experiment ----------------------------------------------------
############################################################################
############################################################################
############################################################################

# opdesmixr package must be installed for this to work
ruseckaite_cornell_designs = read_csv(system.file("extdata", "ruseckaite_cornell_designs.csv", package = "opdesmixr", mustWork = T))
kappas = c(0.5, 5, 10, 30)

beta_2 = c(0.86, 0.21, 0, 3.07, 2.34, 3.24, -20.59)
beta_2_prime = c(0.86, 0.21, 3.07, 2.34, 3.24, -20.59)

cornell_designs_basefilename_transf = paste0(designs_folder, "cornell_experiment_transformed_betas_maxit20")
cornell_designs_basefilename_untransf = paste0(designs_folder, "cornell_experiment_untransformed_betas_maxit20")
cornell_designs_basefilename_analytic_transf = paste0(designs_folder, "cornell_experiment_analytic_transformed_betas_maxit20")









##############################################################
##############################################################
##### Analytically transformed betas
##############################################################
##############################################################


## Read designs with analytic_transformed betas and save them in a list
cornell_designs_analytic_transf = lapply(kappas, function(k){

  cat("kappa =", k, "\n")

  d_opt_filename = paste0(cornell_designs_basefilename_analytic_transf, "_kappa", k, "_Dopt.rds")
  i_opt_filename = paste0(cornell_designs_basefilename_analytic_transf, "_kappa", k, "_Iopt.rds")



  if(file.exists(d_opt_filename)){
    cat("\tD optimal file exists. Loading.\n")
    cornell_beta_2_bayesian_d_opt = readRDS(d_opt_filename)
  }else{
    stop("\tD optimal file does not exist.\n")

  }

  if(file.exists(i_opt_filename)){
    cat("\tI optimal file exists. Loading.\n")
    cornell_beta_2_bayesian_i_opt = readRDS(i_opt_filename)
  }else{
    stop("\tI optimal file does not exist.\n")
  }

  return(list(
    d_opt = cornell_beta_2_bayesian_d_opt,
    i_opt = cornell_beta_2_bayesian_i_opt,
    kappa = k
  ))

})






###############################
### D-optimalities of our design and Ruseckaite's
###############################

cornell_d_optimalities_analytic_transf = lapply(
  seq_along(cornell_designs_analytic_transf), function(i){

    kappa_i = cornell_designs_analytic_transf[[i]]$kappa
    cat("\n\n\nkappa:", kappa_i, format(Sys.time(),'(%H:%M:%S)'), "\n")

    # beta_prior_draws_cornell = get_halton_draws(beta_2_prime, sd = sqrt(kappa_i), ndraws = 100)
    Sigma_prime = transform_varcov_matrix(kappa_i*diag(7), 3)
    beta_prior_draws_cornell = get_correlated_halton_draws(beta = beta_2_prime, sigma = Sigma_prime, n_draws = 512)

    our_d_opt_design = cornell_designs_analytic_transf[[i]]$d_opt$X

    ruseckaite_d_opt_design = ruseckaite_cornell_designs %>%
      filter(k == kappa_i) %>%
      select(-k) %>%
      mnl_design_dataframe_to_array()

    our_d_optimality = mnl_get_opt_crit_value(X = our_d_opt_design, beta = beta_prior_draws_cornell, order = 3, opt_crit = "D", transform_beta = F)
    rus_d_optimality = mnl_get_opt_crit_value(X = ruseckaite_d_opt_design, beta = beta_prior_draws_cornell, order = 3, opt_crit = "D", transform_beta = F)

    return(
      tibble(
        our_d_optimality = our_d_optimality,
        rus_d_optimality = rus_d_optimality,
        kappa = kappa_i
      )
    )

  }) %>%
  bind_rows()


write_csv(cornell_d_optimalities_analytic_transf, file = paste0(out_folder, "res_cornell_d_optimalities_analytic_transf.csv"))



###############################
#### Print designs for Latex
###############################

col_names_cornell_analytic_transf_table = c("Choice set", "x1", "x2", "x3")
cornell_analytic_transf_tables_filename = paste0(out_folder, "res_cornell_analytic_transf_table_designs.tex")
if(file.exists(cornell_analytic_transf_tables_filename)){
  stop("Tex file already exists!")
} else{
  for(i in seq_along(cornell_designs_analytic_transf)){

    kappa_i = cornell_designs_analytic_transf[[i]]$kappa

    cat("% Saving optimal design tables for Cornell's experiment, kappa =", kappa_i, "\n")


    # # Cornell's Bayesian D-optimal design
    # cat("%Cornell's Bayesian D-optimal design\n")
    mnl_design_array_to_dataframe(cornell_designs_analytic_transf[[i]]$d_opt$X) %>%
      select(4, 1:3) %>%
      set_names(col_names_cornell_analytic_transf_table) %>%
      xtable::xtable(
        .,
        caption = paste0("Bayesian D-optimal design for artificial sweetener experiment, $\\kappa = ", kappa_i, "$"),
        label = paste0("tab:cornell_exp_d_optimal_des_kappa_", kappa_i)
      ) %>%
      print(.,
            include.rownames = F,
            file = cornell_analytic_transf_tables_filename,
            append = T)
    # cat("\n\n")



    # # Cornell's Bayesian I-optimal design
    # cat("%Cornell's Bayesian I-optimal design\n")
    mnl_design_array_to_dataframe(cornell_designs_analytic_transf[[i]]$i_opt$X) %>%
      select(4, 1:3) %>%
      set_names(col_names_cornell_analytic_transf_table) %>%
      xtable::xtable(
        .,
        caption = paste0("Bayesian I-optimal design for artificial sweetener experiment, $\\kappa = ", kappa_i, "$"),
        label = paste0("tab:cornell_exp_i_optimal_des_kappa_", kappa_i)
      ) %>%
      print(.,
            include.rownames = F,
            file = cornell_analytic_transf_tables_filename,
            append = T)
    # cat("\n\n\n\n\n\n\n")

  }
}






###############################
## Choice probability plots
###############################

cornell_analytic_trans_choice_probs = lapply(seq_along(cornell_designs_analytic_transf), function(i){

  kappa = cornell_designs_analytic_transf[[i]]$kappa

  get_product_of_choice_probabilities(cornell_designs_analytic_transf[[i]]$i_opt$X,
                                      cornell_designs_analytic_transf[[i]]$i_opt$beta,
                                      order = 3, transform_beta = F) %>%
    mutate(Design = "Bayesian\nI-optimal") %>%
    bind_rows(
      get_product_of_choice_probabilities(cornell_designs_analytic_transf[[i]]$d_opt$X,
                                          cornell_designs_analytic_transf[[i]]$d_opt$beta,
                                          order = 3, transform_beta = F) %>%
        mutate(Design = "Bayesian\nD-optimal")
    ) %>%
    mutate(kappa = kappa)
}) %>%
  bind_rows()


cornell_analytic_trans_choice_probs_plot = cornell_analytic_trans_choice_probs %>%
  mutate(kappa2 = paste0("kappa == ", kappa)) %>%
  mutate(kappa2 = fct_reorder(kappa2, kappa)) %>%
  ggplot() +
  geom_boxplot(aes(x = Design, y = prods)) +
  facet_wrap(~kappa2, ncol = 4, labeller = label_parsed) +
  ylab("Product of choice probabilities") +
  ylim(0, 0.25) +
  theme_bw()




ggplot2::ggsave(
  filename = paste0(out_folder, "res_cornell_analytic_transf_choice_probs_plot.png"),
  plot = cornell_analytic_trans_choice_probs_plot,
  width = 21,
  height = 6,
  units = "cm"
)







###############################
## Distance plots
###############################

cornell_analytic_trans_distances_within_choice_set = lapply(seq_along(cornell_designs_analytic_transf), function(i){

  kappa = cornell_designs_analytic_transf[[i]]$kappa

  get_distances_within_choice_set(cornell_designs_analytic_transf[[i]]$i_opt$X) %>%
    mutate(Design = "Bayesian\nI-optimal") %>%
    bind_rows(
      get_distances_within_choice_set(cornell_designs_analytic_transf[[i]]$d_opt$X) %>%
        mutate(Design = "Bayesian\nD-optimal")
    )  %>%
    mutate(kappa = kappa)


}) %>%
  bind_rows()


cornell_analytic_trans_distances_within_choice_set_plot = cornell_analytic_trans_distances_within_choice_set %>%
  mutate(kappa2 = paste0("kappa == ", kappa)) %>%
  mutate(kappa2 = fct_reorder(kappa2, kappa)) %>%
  ggplot() +
  geom_boxplot(aes(x = Design, y = dist)) +
  theme_bw() +
  ylab("Distance between alternatives") +
  facet_wrap(~kappa2, ncol = 4, labeller = label_parsed)


ggplot2::ggsave(
  filename = paste0(out_folder, "res_cornell_analytic_transf_distances_within_choice_set.png"),
  plot = cornell_analytic_trans_distances_within_choice_set_plot,
  width = 21,
  height = 6,
  units = "cm"
)





###############################
#### Plot designs for Cornell's experiment
###############################


# Parameters for saving plots in PNG
width_cornell_simplex = 10
height_cornell_simplex = 10
utility_point_size_cornell = 0.01
utility_point_shape_cornell = "circle"

width_cornell_fds = 20
height_cornell_fds = 12

# Number of sampled points for FDS plot
# fds_n_points_per_alternative_cornell_analytic_transf = 100
fds_n_points_per_alternative_cornell_analytic_transf = 1000

# Create utility contours
levels_cornell_bayesian = c(0, 0.375, 0.75, 1.125, 1.34)
utilities_cornell_bayesian_plot = opdesmixr:::scheffe_order3_q3_utilities(beta_2_prime, 100000) %>%
  mutate(utility_fact = cut(utility, levels_cornell_bayesian, right = F)) %>%
  mutate(utility_int = as.integer(utility_fact)) %>%
  mutate(utility_avg = case_when(
    utility_int == 1 ~ (levels_cornell_bayesian[1] + levels_cornell_bayesian[2] - levels_cornell_bayesian[1])/2,
    utility_int == 2 ~ (levels_cornell_bayesian[2] + levels_cornell_bayesian[3] - levels_cornell_bayesian[2])/2,
    utility_int == 3 ~ (levels_cornell_bayesian[3] + levels_cornell_bayesian[4] - levels_cornell_bayesian[3])/2,
    utility_int == 4 ~ (levels_cornell_bayesian[4] + levels_cornell_bayesian[5] - levels_cornell_bayesian[4])/2
  ))


# Color palette that goes from yellow to red
color_palette_cornell = tibble(cutoffs = unique(utilities_cornell_bayesian_plot$utility_fact)) %>%
  mutate(g = 1 - seq(0, 1, length.out = nrow(.)), r = 1, b = 0) %>%
  mutate(color = rgb(r, g, b))



for(i in seq_along(cornell_designs_analytic_transf)){

  kappa = cornell_designs_analytic_transf[[i]]$kappa
  cat("Doing kappa =", kappa, "\n")

  d_opt_plot_analytic_transf_i = plot_choice_set_utility_discrete_palette(
    cornell_designs_analytic_transf[[i]]$d_opt$X, utilities_cornell_bayesian_plot,
    utility_point_size = utility_point_size_cornell,
    utility_point_shape = utility_point_shape_cornell,
    legend.position = "none",
    color_palette = color_palette_cornell$color
  ) +
    labs(x = "x_1", y = "x_2", z = "x_3") +
    ggtern::theme_latex(TRUE) +
    theme(plot.margin = unit(c(-5000000, -0.5, -5000000, -0.5), "cm")) # top, right, bottom, and left margins

  i_opt_plot_analytic_transf_i = plot_choice_set_utility_discrete_palette(
    cornell_designs_analytic_transf[[i]]$i_opt$X, utilities_cornell_bayesian_plot,
    utility_point_size = utility_point_size_cornell,
    utility_point_shape = utility_point_shape_cornell,
    legend.position = "none",
    color_palette = color_palette_cornell$color
  ) +
    labs(x = "x_1", y = "x_2", z = "x_3") +
    ggtern::theme_latex(TRUE) +
    theme(plot.margin = unit(c(-5000000, -0.5, -5000000, -0.5), "cm")) # top, right, bottom, and left margins

  plot_filename_basename = paste0("res_cornell_analytic_transf_simplex_kappa_", sprintf("%03d", kappa*10), "_")

  ggtern::ggsave(
    filename = paste0(out_folder, plot_filename_basename, "D_opt.png"),
    plot = d_opt_plot_analytic_transf_i,
    width = width_cornell_simplex,
    height = height_cornell_simplex,
    units = "cm"
  )

  ggtern::ggsave(
    filename = paste0(out_folder, plot_filename_basename, "I_opt.png"),
    plot = i_opt_plot_analytic_transf_i,
    width = width_cornell_simplex,
    height = height_cornell_simplex,
    units = "cm"
  )
}



###############################
##### FDS plots
###############################



pred_vars_cornell_analytic_transf = lapply(
  seq_along(cornell_designs_analytic_transf), function(i){

    kappa_i = cornell_designs_analytic_transf[[i]]$kappa
    cat("\n\n\nkappa:", kappa_i, format(Sys.time(),'(%H:%M:%S)'), "\n")

    # beta_prior_draws_cornell = get_halton_draws(beta_2_prime, sd = sqrt(kappa_i), ndraws = 128)
    Sigma_prime = transform_varcov_matrix(kappa_i*diag(7), 3)
    beta_prior_draws_cornell = get_correlated_halton_draws(beta = beta_2_prime, sigma = Sigma_prime, n_draws = 128)

    pred_vars_cornell_analytic_transf_i = mnl_get_fds_simulations(
      design_array = cornell_designs_analytic_transf[[i]]$i_opt$X,
      beta = beta_prior_draws_cornell,
      order = 3,
      n_points_per_alternative = fds_n_points_per_alternative_cornell_analytic_transf,
      transform_beta = F,
      verbose = 1) %>%
      mutate(Design = "Bayesian I-optimal") %>%
      bind_rows(
        mnl_get_fds_simulations(
          design_array = cornell_designs_analytic_transf[[i]]$d_opt$X,
          beta = beta_prior_draws_cornell,
          order = 3,
          n_points_per_alternative = fds_n_points_per_alternative_cornell_analytic_transf,
          transform_beta = F,
          verbose = 1) %>%
          mutate(Design = "Bayesian D-optimal")
      ) %>%
      bind_rows(
        mnl_get_fds_simulations(
          design_array = ruseckaite_cornell_designs %>%
            filter(k == kappa_i) %>%
            select(-k) %>%
            mnl_design_dataframe_to_array(),
          beta = beta_prior_draws_cornell,
          order = 3,
          n_points_per_alternative = fds_n_points_per_alternative_cornell_analytic_transf,
          transform_beta = F,
          verbose = 1) %>%
          mutate(Design = "Ruseckaite et al.")
      )

    return(pred_vars_cornell_analytic_transf_i %>%
             mutate(kappa = kappa_i))

  }) %>%
  bind_rows()



cornell_fds_plots_analytic_transf = pred_vars_cornell_analytic_transf %>%
  left_join(
    pred_vars_cornell_analytic_transf %>%
      group_by(Design, kappa) %>%
      summarize(
        med = median(pred_var),
        mean = mean(pred_var))
  ) %>%
  mutate(kappa2 = paste0("kappa == ", kappa)) %>%
  mutate(kappa2 = fct_reorder(kappa2, kappa)) %>%
  ggplot() +
  geom_vline(xintercept = 0.5, linetype = "dashed", size = 0.2) +
  geom_hline(aes(yintercept = med), linetype = "dashed", size = 0.2) +
  geom_line(aes(fraction, pred_var, color = Design), size = 0.8) +
  xlab("Fraction of design space") +
  ylab("Prediction variance") +
  # ggtitle("Cornell's experiment") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~kappa2, ncol = 2, scales = "free_y", labeller = label_parsed) +
  scale_color_manual(values = c("red", "blue", "dark green"))




ggplot2::ggsave(
  filename = paste0(out_folder, "res_cornell_analytic_transf_fds_db_vs_ib_plot.png"),
  plot = cornell_fds_plots_analytic_transf,
  width = width_cornell_fds,
  height = height_cornell_fds,
  units = "cm"
)




















