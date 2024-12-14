# Load packages manager ----
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# Load packages ----
p_load(tidyverse, janitor, readxl)

# Load the data ----
my_data <- read_xlsx("REVISED EXAM TT 2ND-3RD & 4TH YR SEM I 2024-2025 AY.xlsx",
  col_types = "text"
) |>
  janitor::row_to_names(1) |>
  janitor::clean_names() |>
  janitor::remove_empty() |>
  mutate(
    day_date = if_else(
      !is.na(as.numeric(day_date)),
      janitor::excel_numeric_to_date(as.numeric(day_date)),
      dmy(day_date)
    )
  )


clean <- my_data |>
  dplyr::transmute(
    time = "0900-1100HRS",
    day_date,
    course_code_and_title,
    no_of_students,
    venue,
    invigilators
  ) |>
  bind_rows(
    my_data |>
      transmute(
        time = "1200-1400HRS",
        day_date,
        course_code_and_title = course_code_and_title_2,
        no_of_students = no_of_students_2,
        venue = venue_2,
        invigilators = invigilators_2
      )
  ) |>
  bind_rows(
    my_data |>
      transmute(
        time = "1500-1700HRS",
        day_date,
        course_code_and_title = course_code_and_title_3,
        no_of_students = no_of_students_3,
        venue = venue_3,
        invigilators = invigilators_3
      )
  ) |>
  dplyr::filter(
    !is.na(course_code_and_title)
  ) |>
  mutate(
    code = str_extract(
      course_code_and_title,
      "^[a-zA-Z]{3}\\s*[0-9]{3}"
    )
  ) |>
  mutate(
    course_title = str_extract(
      course_code_and_title, "^[^()]+"
    )
  ) |>
  mutate(
    course_title = str_remove(
      course_title, "^[a-zA-Z]{3}\\s*[0-9]{3}:\\s*"
    )
  ) |>
  mutate(
    code = str_trim(code) |> str_squish() |> str_to_upper(),
    course_title = str_trim(course_title) |> str_squish() |> str_to_upper()
  )


##################
submitted <- readxl::read_xlsx("24_25 Sem_One Examinations submssion list.xlsx") |>
  rename(no = `...1`) |>
  dplyr::filter(!is.na(no)) |>
  janitor::clean_names() |>
  mutate(
    code = str_extract(
      course_code_and_title,
      "^[a-zA-Z]{3}\\s*[0-9]{3}"
    )
  ) |>
  mutate(
    code = str_trim(code) |> str_squish() |> str_to_upper()
  ) |>
  pull(code) |>
  unique()

# Exams
covers <- clean |>
  dplyr::filter(
    code %in% submitted
  ) |>
  mutate(
    year_of_study = clean %>%
      dplyr::filter(
        code %in% submitted
      ) |>
      pull(code) |>
      stringr::str_extract("\\d{3}") %>% # Extract the 3-digit number
      as.numeric() %>% # Convert to numeric
      (\(x) x %/% 100)() %>% # Integer division to get the year
      dplyr::recode( # Map to labels
        `1` = "FIRST YEAR",
        `2` = "SECOND YEAR",
        `3` = "THIRD YEAR",
        `4` = "FOURTH YEAR"
      )
  ) |>
  mutate(
    program = case_when(
      str_detect(code, "BHR") ~ "BACHELOR OF HUMAN RESOURCES MANAGEMENT",
      str_detect(code, "PPM|BPM") ~ "BACHELOR OF PROJECT PLANNING & MANAGEMENT",
      str_detect(code, "ECO") ~ "BACHELOR OF ECONOMICS",
      str_detect(code, "MBM") ~ "MASTER OF BUSINESS MANAGEMENT",
      .default = "BACHELOR OF BUSINESS MANAGEMENT"
    )
  )

############################
# Create a list of parameter sets for each row
params_list <- covers %>%
  dplyr::mutate(
    year_of_study_text = year_of_study, # Ensure correct column name
    month = as.character(day_date) # Ensure consistent parameter names
  ) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(
    params = list(
      list(
        month = month,
        year_of_study_text = year_of_study_text,
        course_code = code,
        course_title = course_title,
        mytime = time,
        degree = program
      )
    )
  ) %>%
  dplyr::pull(params)

# Define the rendering function
render_exam <- function(params) {
  # Define the output file name based on the course code
  output_file <- paste0("exam_", params$course_code, ".pdf")

  # Render the Quarto template with the current parameters
  quarto::quarto_render(
    input = "bbm300.qmd",
    execute_params = params,
    output_file = output_file
  )

  # Return the output file name for logging
  return(output_file)
}

# Use purrr::map to apply the rendering function to each parameter set
output_files <- purrr::map(params_list, render_exam)

# Log the generated files
print(output_files)

# Style the file ----
styler::style_file("mydata.R")
