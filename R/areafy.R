
areafy2 <- function(blockbuster_initial_state, unit_area_methods = "PDS", input_checks = TRUE) {
  # This function is an improvement on the original areafy which was found to contain some errors.
  # This areafy2 will use the dplyr case_when approach to improve ease of reading.
  # We should go from the most specific (element, sub_element, const_type) required to assign
  # unit_area calculation method, to the most general (just element required to assign)
  # case_when is still experimental so we may prefer to keep areafy() and fix.
  # The unit_area methods are based on expert opinion used to help quantify cost of repairs in PDS buildings.

  if (input_checks == TRUE) {
    # INPUT CHECKS ------------------------------------------------------------
    if (unit_area_methods != "PDS") stop("This function currently only supports unit area estimation methods
                                         from the Property Data Survey.")

    message('Checking blockbuster_initial_state is a data.frame or tibble...')
    #if (!is.data.frame(blockbuster_initial_state)) stop("blockbuster_initial_state must be a data.frame")

    # Check necessary building component columns exist for unit_area method specification

    f <- function(variable_to_check) {
      #  DNRY, lots of similar column name checks, let's make a function
      message(paste("Checking blockbuster_initial_state contains a",
                    variable_to_check, "column..."))

      if (!variable_to_check %in% colnames(blockbuster_initial_state)) stop(paste("blockbuster_initial_state must contain",
                                                                                  variable_to_check, "column..."))

    }

    variables_to_check <- c("element", "sub_element", "const_type")
    lapply(variables_to_check, f)

    # Check necessary columns exist for unit_area calculation
    variables_to_check <- c("gifa", "ground_gifa", "block_perimeter",
                            "block_perimeter", "height", "windows_doors",
                            "site_area_exc_field", "boundary_length",
                            "field_area", "swimming_pool", "composition",
                            "number_lifts")
    lapply(variables_to_check, f)

  } #  if input_checks = FALSE skip to here
  # CALL DPLYR --------------------------------------------------------------
  # require(tidyverse)

  # INSPECT BUILDING COMPONENT THEN CALCULATE UNIT AREA ---------------------------------------------------------------

  areafyed <- blockbuster_initial_state %>%
    dplyr::mutate_(unit_area = ~dplyr::case_when(

      # External Areas and five distinctions
      .$sub_element == "Boundary walls and fences" ~ .$boundary_length,
      .$sub_element == "Other walls, fences and barriers, including around tennis courts, MUGAs etc" ~ .$site_area_exc_field - .$ground_gifa,
      .$sub_element == "Swimming Pools - Plant" | .$sub_element == "Swimming Pools - Structure" ~ as.double(.$swimming_pool),
      .$sub_element == "Drainage - Other" | .$sub_element == "Drainage - Treatment plant" ~ .$gifa ,
      # if the site area is zero this leads to negative area, so assume gifa is area
      .$element == "External Areas" ~ ifelse(.$site_area_exc_field != 0, .$site_area_exc_field - .$ground_gifa, .$ground_gifa),

      # Floors and Stairs, three distinctions

      #  match on alpha numeric on strings with dodgy punctuation e.g. weird hyphen en-dash Alt + 0150
      #  This does exist but it's not being detected due to different encodings?
      #  y[grep("^Suspended floors", y$sub_element), ]  #  possible fix
      iconv(.$sub_element, from = "UTF-8", to = "ASCII", sub = "byte") ==
        paste0("Suspended floors ", "<96>", " Structure") ~ .$gifa - .$ground_gifa,

      #  this incorporates the suspended floors (matching fails) and ground / bearing hollow floors
      .$element == "Floors and Stairs" & .$const_type == "Generally" ~ .$ground_gifa,

      #  all other floors and stairs
      .$element == "Floors and Stairs" ~ .$gifa,

      #  Windows and doors two distinctions
      .$element == "External Walls, Windows and Doors" & .$sub_element == "Windows and doors" ~ .$windows_doors,
      # There can be more than one wall listed in a block and this removes window area from each.  It makes more sense to
      # ignore windows when computing wall area.
      #.$element == "External Walls, Windows and Doors" ~ (.$block_perimeter * .$height) - .$windows_doors,
      .$element == "External Walls, Windows and Doors" ~ (.$block_perimeter * .$height),

      #  Electrical Services have two distinctions
      .$sub_element == "Lifts" ~ .$number_lifts,
      .$element == "Electrical Services" ~ .$gifa,

      #  Roofs are all estimated as being ground floor gifa
      .$element == "Roofs" ~ .$ground_gifa,

      #  Code self explanatory

      .$element == "Playing Fields" ~ .$field_area,

      .$element == "Redecorations" ~ .$gifa,

      .$element == "Fixed Furniture and Fittings" ~ .$gifa,

      .$element == "Ceilings" ~ .$gifa,

      .$element == "Internal Walls and Doors" ~ .$gifa,

      .$element == "Sanitary Services" ~ .$gifa,

      .$element == "Mechanical Services" ~ .$gifa,

      TRUE ~ 0  #  if no method default to zero
    )
    )


  # TESTER ------------------------------------------------------------------
  #  sum(areafy2(y)$unit_area == 0)
  #  filter(areafy2(y), element == "Fixed Furniture and Fittings")[, c("unit_area", "gifa", "composition")]

  # CONSIDER COMPOSITION ----------------------------------------------------
  areafyed <- dplyr::mutate_(areafyed,
                             unit_area = ~(unit_area * composition))  #  composition relates to const_type not grade

  return(areafyed)

}
