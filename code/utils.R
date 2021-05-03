

##########################################################################################
#### Functions
##########################################################################################


mnl_get_choice_probabilities = function(X, beta, order, transform_beta = T){
  # Returns a tibble with the choice probabilities of each choice set.
  # The output is a tibble in which each row is a choice set and the first J columns are the probability of choosing that option.
  dim_X = dim(X)
  S = dim_X[3]
  
  probs_df = as.data.frame(matrix(rep(NA_real_, dim_X[2]*S), nrow = S)) %>%
    set_names(paste0("p_", 1:dim_X[2]))
  
  for(s in 1:S){
    probs_df[s,] = mnl_get_Ps(X, beta, s, 3, T)
  }
  
  probs_df %>%
    as_tibble() %>%
    mutate(choice_set = 1:nrow(.)) %>%
    return()
  
}




get_product_of_choice_probabilities = function(design_array, beta, order = 3, transform_beta = T){
  # Returns a tibble with the choice probabilities of each choice set. and its product.
  # The output is a tibble in which each row is a choice set and the first J columns are the probability of choosing that option.
  # The (J+1)-th column denotes the choice set, and the last column contains the product of the choice probabilities in each choice set.
  
  choice_probs = mnl_get_choice_probabilities(design_array, beta, order, transform_beta)
  
  prods = Reduce(`*`, select(choice_probs, all_of(1:(ncol(choice_probs)-1))))
  
  choice_probs %>%
    mutate(prods = prods) %>%
    return()
  
}




get_distances_within_choice_set = function(design_array){
  # Computes the Euclidean distances between the two alternatives within a choice set for a design.
  
  dim_array = dim(design_array)
  J = dim_array[2]
  if(J != 2) stop("This function only works for designs with two alternatives within each choice set.")
  
  S = dim_array[3]
  distances = tibble(dist = rep(NA_real_, S))
  
  for(s in 1:S){
    square_diff = (design_array[,1,s] - design_array[,2,s])^2
    distances[s,] = sqrt(sum(square_diff))
  }
  
  distances %>%
    mutate(choice_set = 1:nrow(.)) %>%
    return()
  
}





# Plotting function
plot_choice_set_utility_discrete_palette = function(
  design_object, utility_data, design_point_size = 2, design_point_alpha = 1,
  utility_point_size = 0.3, utility_point_shape = "square", utility_point_alpha = 1.0,
  legend.position = "bottom", legend.box = "vertical",
  color_palette = NULL) {
  
  if (class(design_object) == "list") {
    dim_X = dim(design_object$X)
    q = dim_X[1]
    S = dim_X[3]
    if (q != 3)
      stop("Design must be of 3 ingredients.")
    X_final_tbl = mnl_design_array_to_dataframe(design_object$X)
  }
  else {
    if (inherits(design_object, "array")) {
      dim_X = dim(design_object)
      q = dim_X[1]
      S = dim_X[3]
      if (q != 3)
        stop("Design must be of 3 ingredients.")
      X_final_tbl = mnl_design_array_to_dataframe(design_object)
    }
    else {
      if (inherits(design_object, "data.frame")) {
        q = ncol(design_object) - 1
        if (q != 3)
          stop("Design must be of 3 ingredients.")
        X_final_tbl = design_object %>% set_names(c("c1",
                                                    "c2", "c3", "choice_set")) %>% mutate(choice_set = as.character(choice_set))
      }
      else {
        stop("Unknown type of design")
      }
    }
  }
  
  if(is.null(color_palette)){
    # Create palette that goes from yellow to red
    green_levels = 1 - seq(0, 1, length.out = length(unique(utilities_cocktail_bayesian_plot$utility_int)))
    
    color_palette = tibble(g = green_levels) %>%
      mutate(r = 1, b = 0) %>%
      mutate(color = rgb(r, g, b)) %>%
      pull(color)
  }
  
  
  out_plot = ggtern::ggtern(data = utility_data) +
    ggplot2::geom_point(
      aes(x = x1,
          y = x2, z = x3, color = factor(utility_int)),
      size = utility_point_size,
      shape = utility_point_shape,
      alpha = utility_point_alpha
    ) +
    ggplot2::scale_color_manual(values = color_palette) +
    ggplot2::geom_point(data = X_final_tbl, ggtern::aes(c1, c2, c3, shape = choice_set), color = "black", size = design_point_size, stroke = 1.5, inherit.aes = F, alpha = design_point_alpha) +
    ggtern::theme_nomask() +
    scale_shape_manual(values = 1:length(unique(X_final_tbl$choice_set))) +
    theme(legend.position = legend.position, legend.box = legend.box)
  return(out_plot)
}






# Transforms from pseudocomponent x to original component a.
# x is a scalar or a vector, l is a scalar denoting the lower bound.
# sum_l is a scalar denoting the sum of the lower bounds.
transform_from_pseudocomp_to_comp = function(x, l, sum_l){
  a = l + (1 - sum_l)*x
  return(a)
}


transform_tibble_from_pseudocomp_to_comp = function(design_tibble, lower_bounds, var_indices){
  # Returns a tibble with p + q columns and n rows with the transformation from pseudo-components to the original components in the mixture. Here, q is the number of ingredietns proportions, p is the number of original columns in design_tibble, and n is the number of rows in design_tibble.
  sum_lower_bounds = sum(lower_bounds)
  q = length(lower_bounds)
  
  if(q != length(var_indices)) stop("Incompatible sizes of lower_bounds and var_indices")
  
  transformed_df = as_tibble(matrix(rep(NA_real_, nrow(design_tibble)*q), ncol = q)) %>%
    set_names(paste0(names(design_tibble)[var_indices], "_original"))
  
  for(i in seq_along(var_indices)){
    transformed_df[[i]] = transform_from_pseudocomp_to_comp(design_tibble[[var_indices[i]]], lower_bounds[i], sum_lower_bounds)
  }
  
  return(bind_cols(design_tibble, transformed_df))
}



