

overall_self_rating_table <- function(data, start_col) {

  data %>%
    select(full_name, team, {{ start_col }}:how_minor_or_major_an_accomplishment_was_this_for_you) %>%
    set_names(c("Name", "Team", "Satisfied with Team", "My contribution", "Challenge 1", "Rating of Challenge 1", "Accomplishment 1", "Rating of Accomplishment 1")) %>%
    reactable(filterable = TRUE)

}


