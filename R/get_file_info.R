
# Great function!
get_code_file_info <- function(path_vector, file_type = c(".R", ".qmd", "Rmd")){

  pattern_file_types <- paste0(map(file_type, ~paste0("(\\", .x, "$)")), collapse = "|")

  vec_code_files <- path_vector %>%
    map(~list.files(.x, recursive = TRUE, full.names = TRUE, pattern = pattern_file_types)) %>%
    unlist()

  tibble(
    file_path = vec_code_files
  ) %>%
    mutate(file_info = file.info(file_path)) %>%
    unnest(file_info) %>%
    select(file_path,
           modified_time = "mtime",
           created_time = "ctime",
           access_time = "atime"
    )
}



# Extract code
