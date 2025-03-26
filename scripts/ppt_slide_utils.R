library(officedown)
library(officer)

update_title_slide <- function(template_path, report_title, report_author) {
  template_path %>%
    read_pptx() %>%
    ph_remove(
      ph_label = ph_location_label(ph_label = "Title 1")
    ) %>%
    ph_with(
      value = report_title,
      location = ph_location_label(ph_label = "Title 1")
    ) %>%
    ph_remove(
      ph_label = ph_location_label(ph_label = "Subtitle 2")
    ) %>%
    ph_with(
      value = report_author,
      location = ph_location_label(ph_label = "Subtitle 2")
    )
}

add_singlet_slide <- function(
    x, title, sbody, slide_num
) {
  x %>%
    add_slide(
      layout = "Title and Content", master = "Office Theme"
    ) %>%
    ph_with(
      value = title,
      location = ph_location_type(type = "title")
    ) %>%
    ph_with(
      value = "", location = ph_location_type(type = "ftr")
    ) %>%
    ph_remove(type = "ftr") %>%
    ph_with(
      value = format(Sys.Date()),
      location = ph_location_type(type = "dt")
    ) %>%
    ph_with(
      value = paste("Slide", slide_num, sep = " "),
      location = ph_location_type(type = "sldNum")
    ) %>%
    ph_with(
      value = sbody,
      location = ph_location_type(type = "body")
    ) 
}

add_quad_slide <- function(
    x, title, sbody1, sbody2, sbody3, sbody4, slide_num
) {
  x %>%
    add_slide(
      layout = "Four Content", master = "Office Theme"
    ) %>%
    ph_with(
      value = title,
      location = ph_location_type(type = "title")
    ) %>%
    ph_with(
      value = "", location = ph_location_type(type = "ftr")
    ) %>%
    ph_remove(type = "ftr") %>%
    ph_with(
      value = format(Sys.Date()),
      location = ph_location_type(type = "dt")
    ) %>%
    ph_with(
      value = "",# paste("Slide", slide_num, sep = ""),
      location = ph_location_type(type = "sldNum")
    ) %>%
    ph_with(
      value = sbody1,
      location = ph_location_label(
        ph_label = "Content Placeholder 2"
      )
    ) %>%
    ph_with(
      value = sbody2,
      location = ph_location_label(
        ph_label = "Content Placeholder 3"
      )
    ) %>%
    ph_with(
      value = sbody3,
      location = ph_location_label(
        ph_label = "Content Placeholder 9"
      )
    ) %>%
    ph_with(
      value = sbody4,
      location = ph_location_label(
        ph_label = "Content Placeholder 13"
      )
    )
}

add_untemplated_slide <- function(
    x, title, slide_num
) {
  x %>%
    add_slide(
      layout = "Title and Content", master = "Office Theme"
    ) %>%
    ph_with(
      value = title,
      location = ph_location_type(type = "title")
    ) %>%
    ph_with(
      value = "", location = ph_location_type(type = "ftr")
    ) %>%
    ph_remove(type = "ftr") %>%
    ph_with(
      value = format(Sys.Date()),
      location = ph_location_type(type = "dt")
    ) %>%
    ph_with(
      value = "",# paste("Slide", slide_num, sep = ""),
      location = ph_location_type(type = "sldNum")
    ) %>%
    ph_with(
      value = "",
      location = ph_location_type(type = "body")
    ) %>%
    ph_remove(ph_label = ph_location_label(ph_label = "Content Placeholder 2"))
}

add_freestyle_shape_on_slide <- function(x, sbody, location, ...) {
  ph_with(x, sbody, location = location, ...)
}
