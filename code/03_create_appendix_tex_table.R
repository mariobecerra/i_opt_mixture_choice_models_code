library(tidyverse)
library(here)

tables_folder = "out/cocktail_cornell_plots_and_tables/"


initial_text = '
\\begin{appendices}


\\section{Design tables of cocktail experiment}

In both tables, $x_1$, $x_2$, and $x_3$ denote the pseudocomponents that range from $0$ to $1$, while $a_1$, $a_2$, and $a_3$ are the ingredient proportions with their original lower bounds.

'




middle_text = '

\\clearpage

\\section{Design tables of artificial sweetener experiment}

'



end_text = '\\end{appendices}'







file_cont_cocktail_dopt = readLines(here(tables_folder, "res_cocktail_table_d_opt_design.tex"))

clean_file_cont_cocktail_dopt = sapply(file_cont_cocktail_dopt, function(x){
  out = x %>% 
    gsub(x = ., pattern = "x1", replacement = "$x_1$", fixed = T) %>% 
    gsub(x = ., pattern = "x2", replacement = "$x_2$", fixed = T) %>% 
    gsub(x = ., pattern = "x3", replacement = "$x_3$", fixed = T) %>% 
    gsub(x = ., pattern = "a1", replacement = "$a_1$", fixed = T) %>% 
    gsub(x = ., pattern = "a2", replacement = "$a_2$", fixed = T) %>% 
    gsub(x = ., pattern = "a3", replacement = "$a_3$", fixed = T)
  
  return(out)
})




file_cont_cocktail_iopt = readLines(here(tables_folder, "res_cocktail_table_i_opt_design.tex"))

clean_file_cont_cocktail_iopt = sapply(file_cont_cocktail_iopt, function(x){
  out = x %>% 
    gsub(x = ., pattern = "x1", replacement = "$x_1$", fixed = T) %>% 
    gsub(x = ., pattern = "x2", replacement = "$x_2$", fixed = T) %>% 
    gsub(x = ., pattern = "x3", replacement = "$x_3$", fixed = T) %>% 
    gsub(x = ., pattern = "a1", replacement = "$a_1$", fixed = T) %>% 
    gsub(x = ., pattern = "a2", replacement = "$a_2$", fixed = T) %>% 
    gsub(x = ., pattern = "a3", replacement = "$a_3$", fixed = T)
  
  return(out)
})






file_cont_res_cornell = readLines(here(tables_folder, "res_cornell_analytic_transf_table_designs.tex"))

clean_file_cont_res_cornell = sapply(file_cont_res_cornell, function(x){
  out = x %>% 
    gsub(x = ., pattern = "x1", replacement = "$x_1$", fixed = T) %>% 
    gsub(x = ., pattern = "x2", replacement = "$x_2$", fixed = T) %>% 
    gsub(x = ., pattern = "x3", replacement = "$x_3$", fixed = T)
  
  return(out)
})




final_ordering = c(initial_text, clean_file_cont_cocktail_dopt, clean_file_cont_cocktail_iopt, middle_text, clean_file_cont_res_cornell, end_text)

file_con = file(here(tables_folder, "appendix.tex"))
writeLines(final_ordering, con = file_con)
close(file_con)




