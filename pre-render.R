make_simpleyml <- function(renderfile) {
  simple_yaml <- list()
  simple_yaml$project <- list()
  simple_yaml$project$render <- list(renderfile)
  yaml::write_yaml(simple_yaml, '_quarto-singlefile.yml')
}

envs <- Sys.getenv()
write.csv(as.data.frame(envs, row.names = names(envs)), 'out.csv')

make_simpleyml('website_notes/quarto_profiles.qmd')

