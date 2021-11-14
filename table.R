
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(gt)
library(glue)
library(rsvg)


# Data Reading & Wrangling ------------------------------------------------
# Icons Directories Paths
grapes_dir <- here::here("Data", "Grapes Varieties") 
foods_dir <- here::here("Data", "Foods Icons")
flags_dir <- here::here("Data","Countries Flags")
columns_icons_dir <- here::here("Data","Columns Icons")

default_grape_path <- here::here("Data","grapes_params.svg")
food_icons_path <- here::here("Data","food_icons.csv")

#' Wrangle Data Scraped 
#'
#' @param wines_df 
#'
#' @return
#' @export
#'
#' @examples
wrangle_wines_df <- function(wines_df) { 
  wines_df %>%  
    extract(wine_name, into = c("wine_name", "wine_year"), regex = "(.*)\\s(\\d{4}|N.V)") %>% 
    mutate( wine_rating = wine_rating /10,
            wine_rating_nb = str_remove(wine_rating_nb, "notes") %>% str_trim()) %>% 
    mutate(
      # Translation to EN
      allergens = str_replace(allergens, "sulfites", "sulphites"), 
      allergens = str_replace(allergens, "allergènes\\s\\(œufs\\)", "eggs"),
      allergens = str_replace(allergens, "allergènes\\s\\(lait\\)", "milk")
    ) %>% 
    mutate(bottle_price = parse_number(str_remove_all(bottle_price,"\\s"), locale = locale("fr", decimal_mark = ",")), 
           bottle_price_dollar = bottle_price * 1.16068, 
           bottle_price_dollar = ceiling(bottle_price_dollar/5)*5, 
           alcohol_content = str_remove(alcohol_content, "%"), 
           alcohol_content = parse_number(alcohol_content, local = locale("fr", decimal_mark = "."))
    )  %>% 
    drop_na(alcohol_content, bottle_price_dollar)
}


wines <- read_csv(here::here("Data/wines_top100.csv")) %>% 
  wrangle_wines_df()

wines_bg <- read_csv(here::here("Data/wines_burgundy_under_40_top100.csv")) %>% 
  wrangle_wines_df()

wines_usa <- read_csv(here::here("Data/wines_usa_under_40_top50.csv")) %>% 
  wrangle_wines_df()

# Five Stars Rating -------------------------------------------------------

#' Convert to a fancy rating block
#'  
#' @param  rating wine avg rating 
#' @param  nb_botes number of reviews
#' @param  star_height stars height 
#' @param  rating_size rating text font size 
#' @param  review_size review text font size
#' 
#' @return html of fancy stars rating
rating_combo <- function(rating, nb_notes, star_height = 32, rating_size = "4.5em", review_size = "1.1em") {
  
  (nb_fstars <- rating  %/% 1)
  (nb_vstars <- (5 - rating) %/% 1)
  (off_1 <- (rating %% 1) * 100)
  (off_2 <- 100 - off_1)
  off_1 <- round(off_1)
  off_2 <- round(off_2)
  stars_label <-
    glue(
      '<span style = "text-align:center; line-height: .85;font-size: {rating_size};">{rating}</span>'
    )
  star <-
    glue(
      '<svg style="width: 0; height: 0;" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {star_height} {star_height}">
    <defs>
      <linearGradient id="half" x1="0" x2="100%" y1="0" y2="0">
        <stop offset="{off_1}%" stop-color="#fed94b"></stop>
        <stop offset="{off_2}%" stop-color="#f7f0c3"></stop>
      </linearGradient>

      <symbol xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" id="star">
        <path d="M31.547 12a.848.848 0 00-.677-.577l-9.427-1.376-4.224-8.532a.847.847 0 00-1.516 0l-4.218 8.534-9.427 1.355a.847.847 0 00-.467 1.467l6.823 6.664-1.612 9.375a.847.847 0 001.23.893l8.428-4.434 8.432 4.432a.847.847 0 001.229-.894l-1.615-9.373 6.822-6.665a.845.845 0 00.214-.869z" />
      </symbol>
    </defs>
  </svg>'
    )
  filled_star <- glue(' <svg style="width: var(--size, {star_height}px);
height: var(--size, {star_height}px);
fill: #fed94b;
  stroke: grey;"  width="{star_height}" height="{star_height}" viewBox="0 0 {star_height} {star_height}">
    <use xlink:href="#star"></use>
</svg>')
  void_star <- glue('<svg style="width: var(--size, {star_height}px);
height: var(--size, {star_height}px);
fill: #f7f0c3;
  stroke: grey;"  width="{star_height}" height="{star_height}" viewBox="0 0 {star_height} {star_height}">
    <use xlink:href="#star"></use>
</svg>')
  divided_star <- glue::glue(
    '
    <svg style="width: var(--size, {star_height}px);
  height: var(--size, {star_height}px);
  fill: #fed94b;
  stroke: grey;"  width="{star_height}" height="{star_height}" viewBox="0 0 {star_height} {star_height}">
  <use xlink:href="#star" fill="url(#half)"></use>
  </svg>
'
  )
  notes_label <- glue::glue('<div style="font-size:{review_size};">{nb_notes} reviews</div>')
  stars <-
    str_c(
      "<div>",
      star,
      str_c(rep(filled_star, nb_fstars), collapse = ""),
      ifelse(off_1 != 0, divided_star, ""),
      str_c(rep(void_star, nb_vstars), collapse = ""),
      "</div>",
      collapse = ""
    )
  # Collapse Complete Rating Block and return it
  rating_block <- str_c(stars_label, stars, notes_label, collapse = "")
  rating_block
}

# Grapes Varieties Rows ---------------------------------------------------

#' Slugify a string
#'
#' @param x 
#' @param alphanum_replace 
#' @param space_replace 
#' @param tolower 
#'
#' @return
#' @export
#'
#' @examples
slugify <-  function(x, alphanum_replace = "", space_replace = "_", tolower = TRUE) {
  
  x <- gsub("[^[:alnum:] ]", alphanum_replace, x)
  x <- gsub(" ", space_replace, x)
  if (tolower) {
    x <- tolower(x)
  }
  
  return(x)
}

#' Generate Grape Png 
#'
#' @param grape_color 
#' @param grape_variety 
#'
#' @return
#' @export
#'
#' @examples
generate_grape_svg <- function(grape_color, grape_variety) {
  
  # Create Grapes Directory  if not exists
  if(!dir.exists(grapes_dir)) {
    dir.create(grapes_dir)
  }
  
  #  Build Grape path 
  path <- here::here(grapes_dir, paste0(slugify(grape_variety), ".png"))
  # Read Default Grape SVG 
  grape_params <- read_file(default_grape_path)
  # Render Grape SVG 
  temp <- tempfile(fileext = ".svg")
  write_lines(whisker::whisker.render(grape_params), file = temp)
  # Convert to PNG 
  rsvg_png(temp, path)
  # Annotate PNG with Grape Variety
  pic <-  magick::image_read(path)
  pic <-
    magick::image_annotate(
      pic,
      str_wrap(str_to_upper(grape_variety), 10),
      gravity = "south",
      size = 20.5,
      weight  = 600,
      color = "#111111",
      font = "Lato"
    )
  magick::image_write(pic, path = path, format = "png")
  unlink(temp)
}

# Grape Variety with ~ hex color code
grapes_colors <- tribble(
  ~ grape_variety, ~ hex_color,
  "Cabernet Franc", "#270010",
  "Cabernet Sauvignon", "#230411",
  "Sémillon", "#d4af37",
  "Merlot", "#7F171F",
  "Chardonnay", "#FFCE7B",
  "Sauvignon Blanc", "#b18276",
  "Petit Verdot", "#3C191D",
  "Tempranillo", "#722F37",
  "Pinot Noir", "#605258",
  "Pinot Grigio", "#706278",
  "Malbec", "#734150",
  "Tinto Cao", "#1a1618",
  "Tinta Barroca", "#722F37",
  "Cinsault", "#58181F",
  "Touriga Nacional", "#1a1618", 
  "Touriga Franca" , "#1A0709", 
  "Tinta Roriz", "#722F37",
  "Corvina","#181115",
  "Rondinella", "#181115",
  "Pedro Ximenez","#bbcc99",
  "Zinfandel", "#5c2935",
  "Shiraz/Syrah", "#93787b",
  "Petite Sirah", "9E0E40",
  "Pinot Blanc", "#f1f285",
  "Others", "#9D2690"
)

# Generate Grapes Image 
grapes_colors %>%
  pwalk(~generate_grape_svg(.y, .x))

#' Generate Grapes Icons Rows from a string 
#'
#' @param grapes Grapes String
#' @param height Grapes Icons height 
#'
#' @return Html of Grapes Icons Rows
#' @export
#'
#' @examples
generate_grapes_rows <- function(grapes, height) {
  
  # Get Grape Image from string
  get_grape_image <- function(grape_variety) {
    # Default Grape image 
    grape_path <-  here::here(grapes_dir, "others.png")
    if (file.exists(here::here(grapes_dir, paste0(slugify(grape_variety), ".png")))) {
      grape_path <-
        here::here(grapes_dir, paste0(slugify(grape_variety), ".png"))
    }
    # Return Grape Image
    local_image(file  = grape_path, height = px(height))
  }
  
  grapes %>%
    str_remove_all("\\d{2,3}\\s?%\\s") %>%
    str_trim() %>%
    str_split(",\\s") %>%
    pluck(1) %>%
    head(4) %>%
    map_chr(get_grape_image) %>%
    str_c(collapse = " ")
}


# Food Icons --------------------------------------------------------------
foods_icons <- read_csv(here::here(food_icons_path)) %>% 
  # Translation 
  mutate(
    foods_name_en = case_when(
      foods_name == "Agneau" ~ "Lamb",
      foods_name == "Apéritif" ~ "Aperitif",
      foods_name == "Apéritif et snacks" ~ "Aperitif and snacks",
      foods_name == "Bœuf" ~ "Beef",
      foods_name == "Champignons" ~ "Champignons",
      foods_name == "Crustacés" ~ "Shellfish",
      foods_name == "Desserts fruités" ~ "Fruity desserts",
      foods_name == "Desserts sucrés" ~ "Sweet desserts",
      foods_name == "Fromage affiné et à pâte dure" ~ "Ripened and hard cheese",
      foods_name == "Fromage bleu" ~ "Blue cheese",
      foods_name == "Fromage de chèvre" ~ "Goat cheese",
      foods_name == "Fromage doux et à pâte molle" ~ "Mild and soft cheese",
      foods_name == "Gibier" ~ "Game (deer, roe deer, etc.)",
      foods_name == "Mets épicés" ~ "Spicy dishes",
      foods_name == "N'importe quelle camelote alimentaire fera l'affaire" ~ "Any junk food",
      foods_name == "Poisson gras" ~ "Fatty fish (salmon, tuna, etc.)",
      foods_name == "Poisson maigre" ~ "Lean fish",
      foods_name == "Porc" ~ "Pork",
      foods_name == "Pâtes" ~ "Pasta",
      foods_name == "Veau" ~ "Calf",
      foods_name == "Viande assaisonnée" ~ "Seasoned meat",
      foods_name == "Volaille" ~ "poultry",
      foods_name == "Végétarien" ~ "Vegetarian",
      TRUE ~ foods_name)
  )

#  Add xmnls tag to svg 
foods_icons <- foods_icons %>%
  mutate(foods_svg = str_replace(foods_svg, "<svg", '<svg xmlns="http://www.w3.org/2000/svg"'))

# Generate foods Icons
if (!dir.exists(foods_dir)) dir.create(foods_dir) # Create Foods Dir 
foods_icons %>%
  pwalk(~ write_lines(.y, here::here(
    foods_dir, paste0(slugify(.x), ".svg")
  )))

#' Convert a string of food pairings to a string of icons 
#'
#' @param pairing 
#' @param icon_height  Icon height
#'
#' @return
#' @export
#'
#' @examples
convert_pairing_to_icon <- function(pairing, icon_height) {
  # Rounded Border 
  my_style <- glue(
    "border: solid 1px #111111; border-radius: 50%;
    color: #111111;
    height :{icon_height}px;
    background-color: #FFFFFF;
    margin: 2px;
    padding: 5px;
  vertical-align:middle"
  )
  pairing <- pairing %>% 
    str_remove_all("(\\(.*\\))") %>% 
    str_split(",") %>%
    pluck(1) %>%
    map_chr(~.) %>% 
    str_trim() %>% 
    map_chr(slugify)
  
  pairing[!is.na(pairing)]%>%
    map_chr(
      ~ local_image(
        file  = here::here(foods_dir,paste0(.x, ".svg")),
        height = px(icon_height)
      ) %>%
        str_replace(glue::glue("height:{icon_height}px;"), my_style)
    ) %>%
    str_c(collapse = "")
}

# Countries Icons ---------------------------------------------------------
# Countries icons  Paths DF 
countries_df <- tribble(
  ~ name_fr,~ name_en,~ flag_path,
  "Portugal","Portugal",here::here(flags_dir,"portugal.png"),
  "États-Unis","USA",here::here(flags_dir,"usa.png"),
  "Italie","Italy",here::here(flags_dir,"italy.png"),
  "France","France",here::here(flags_dir,"france.png"),
  "North Macedonia","North Macedonia",here::here(flags_dir,"macedonia.png"),
  "Espagne","Spain",here::here(flags_dir,"spain.png"),
  "Roumanie","Roumanie",here::here(flags_dir,"romania.png")
)

#' Generate Regions Block from a string
#'
#' @param regions wine region string
#'
#' @return a fancy region block
#' @export
#'
#' @examples
fancy_regions <- function(regions) {
  
  #' Split Regions String
  split_region <- function(region)  {
    region %>%
      str_split("/") %>%
      pluck(1) %>%
      str_trim()
  }
  
  #' Generate Regions Block from country and subregions strings
  #' @param country wine country
  #' @param splitted_regions wine Sub Regions
  #' @param flag_path  wine country flag path 
  fancy_region <- function(country, splitted_regions, flag_path) {
    flag_image <- local_image(flag_path, height = px(37.5))
    subregion_1 <- splitted_regions %>%
      pluck(2)
    subregion_2 <-
      str_c(as.character(splitted_regions[-(1:2)]), collapse = " - ") %>%
      str_wrap(width = 25) %>%
      str_replace_all(pattern = "\n", "<br>") %>% 
      str_replace_all("-","·")
    fancy_region <- glue::glue(
      '{flag_image}
        <div style = "line-height:20px;font-size: 17px;font-weight:600;">{str_to_upper(country)}</div>
        <div style="line-height:18px;font-size: 14px;">{subregion_1}</div>
        <div style="font-family:Mercury;line-height:14px;font-size: 12.5px; font-weight: 500;">{subregion_2}</div>'
    )
  }
  
  # Split Wine Country with subregions
  tibble (region = regions) %>%
    mutate(
      splitted_regions = map(region, split_region),
      country = map_chr(splitted_regions, ~ pluck(., 1))
    ) %>%
    left_join(countries_df, by = c("country" = "name_fr")) %>%
    mutate(fancy_region = pmap_chr(
      list(country = name_en, splitted_regions, flag_path),
      fancy_region
    )) %>%
    select(region, country, fancy_region) %>%
    mutate(fancy_region = map(fancy_region , gt::html)) %>% 
    pull(fancy_region)
}


# Combine All -------------------------------------------------------------
# Columns Combos
combo_name_style <- function(wine_name, wine_style){
  glue::glue(
    "<div style='line-height:20px'><span style='font-weight:600;color:#111111;font-size:15px'>{str_replace_all(str_wrap(str_to_upper(wine_name),25),'\n','<br/>')}</div>
        <div style='line-height:15.5px'><span style ='font-family:Mercury;font-weight:500;color:grey25;font-size:13px'>{ wine_style}</span></div>"
  )
}

combo_domain_year <- function(domain, wine_year){
  glue::glue(
    "<div style='line-height:18.5px'><span style='font-weight:600;font-size:15px'>{str_replace_all(str_wrap(str_to_upper(domain),25),'\n','<br/>')}</div>
        <div style='line-height:18px'><span style ='font-weight:500;color:#777777;font-size:14px'>{wine_year}</span></div>"
  )
}

fancy_bottle_price <- function(price){
  glue::glue(
    "<div style='line-height:45px'><span style='font-weight:bold;font-size:35px'>${price}</div>
        "
  )
}

# Foods Descriptions 
icons_description <- foods_icons %>% 
  rowwise() %>% 
  mutate(icon = paste0(convert_pairing_to_icon(foods_name, 25), foods_name_en)) %>% 
  pull(icon) %>% 
  str_c(collapse = " ")
icons_des <- "<div style=\"text-align:center;\">"
icons_des <- paste0(icons_des,"<span><b>Foods Descriptions</b></span>","<br>",icons_description)
icons_des <- paste0(icons_des,"<br><br><span>  Credits to <b>Flaticon</b> for Countries Flags Icons<span>")
icons_des <- paste0(icons_des,"<br><br><span> Data scraped on <b>vivino.com</b><span>")
icons_des <- paste0(icons_des,"<br><br><span> Table by <b>Abdoul ISSA BIDA</b><span>")
icons_des <- paste0(icons_des,"</div>")



#' Styling columns with Combos
#'
#' @param wines_df 
#'
#' @return
#' @export
#'
#' @examples
format_wines_df <- function(wines_df){
  wines_df %>% 
    mutate(
      alcohol_content_color = cut_interval(alcohol_content, 5, labels = c("#C55A79","#AD466C","#91345B","#832C53","#672044")),
      fancy_alcohol_content = glue("<span style='color:{alcohol_content_color}'>{alcohol_content}%</span>"),
    ) %>% 
    arrange(desc(wine_rating), desc(bottle_price_dollar)) %>% 
    mutate(
      wine_style = map(wine_style, ~ if(is.na(.)) {""} else {.}),
      wine_name_style = combo_name_style(wine_name, wine_style),
      wine_year = map(wine_year, ~ if(. == "N.V") {""} else{.}),
      wine_domain_year = combo_domain_year(viticultural_domain, wine_year),
      fancy_regions = fancy_regions(region),
      wine_name_style = map(wine_name_style, gt::html),
      wine_domain_year = map(wine_domain_year, gt::html),
      grape_variety = map(grape_variety, ~(generate_grapes_rows(., 75))),
      grape_variety = map(grape_variety, gt::html),
      pairing_icons =   map(food_pairing, ~ convert_pairing_to_icon(., 40)),
      pairing_icons = map(pairing_icons, gt::html),
      rating_combo = map2(wine_rating, wine_rating_nb, ~rating_combo(.x,.y, 24, "3em","0.9em")),
      rating_combo = map(rating_combo, gt::html), 
      bottle_price_dollar = map(bottle_price_dollar, fancy_bottle_price), 
      bottle_price_dollar = map(bottle_price_dollar, gt::html),
      fancy_alcohol_content = map(fancy_alcohol_content, gt::html)
    ) %>% 
    select(bottle_image, wine_name_style, wine_domain_year, fancy_regions, grape_variety, pairing_icons, rating_combo, bottle_price_dollar, allergens, fancy_alcohol_content)
}


#' Table Theming 
#'
#' @param grammar_tab 
#'
#' @return
#' @export
#'
#' @examples
tab_theme <- function(grammar_tab){ 
  grammar_tab %>% 
    text_transform(
      locations = cells_body(columns = bottle_image),
      fn = function(x) {
        web_image(url = x,
                  height = px(85))
      }
    ) %>%    
    cols_label(
      bottle_image = "", 
      wine_name_style = gt::html(paste0(local_image(filename = here::here(columns_icons_dir, "wine_glass.svg"),height = 50), "<br>","Wine name<br>Style" )),
      wine_domain_year = gt::html(paste0(local_image(filename = here::here(columns_icons_dir, "domain.svg"), height = 50), "<br>","Domaine<br>Year")),
      fancy_regions = gt::html(paste0(local_image(filename = here::here(columns_icons_dir, "region.svg"), height = 50), "<br><br>","Region")),
      grape_variety = gt::html(paste0(local_image(filename = here::here(columns_icons_dir, "grape.svg"), height = 50), "<br>","Grapes <br>Varieties")),
      pairing_icons = gt::html(paste0(local_image(filename = here::here(columns_icons_dir, "foods_pairing.svg"), height = 50), "<br>","Food <br> Pairings")),
      rating_combo = gt::html(paste0(local_image(filename = here::here(columns_icons_dir, "star.svg"), height = 50), "<br><br>","Ratings")),
      bottle_price_dollar = gt::html(paste0(local_image(filename = here::here(columns_icons_dir, "price.svg"), height = 50), "<br><br>","Price")),
      allergens = gt::html(paste0(local_image(filename = here::here(columns_icons_dir, "allergen.svg"), height = 50), "<br><br>","Allergens")),
      fancy_alcohol_content = gt::html(paste0(local_image(filename = here::here(columns_icons_dir, "alcohol_content.svg"), height = 50), "<br>","Alcohol <br>Content"))
    ) %>%
    cols_width(
      bottle_image ~ px(75), 
      grape_variety ~ px(300),
      rating_combo ~px(175),
      c(bottle_price_dollar,fancy_alcohol_content) ~px(150), 
      allergens ~ px(100)
    ) %>% 
    cols_align(
      align = "center", 
      columns = everything()
    ) %>% 
    cols_align(
      align = "right", 
      columns = fancy_alcohol_content
    ) %>% 
    tab_footnote(
      footnote = "Prices are rounded to nearest multiple of 5.", 
      locations =  cells_column_labels(
        column = bottle_price_dollar
      )
    ) %>% 
    tab_source_note(
      source_note = html(icons_des)
    ) %>% 
    tab_style(
      style = cell_borders(sides = "bottom", color = "transparent"), 
      locations = cells_body(columns = bottle_image)
    ) %>% 
    tab_style(
      style = cell_text(font = "Verlag Black", color = "black"),
      locations = cells_title(group = "title")
    ) %>%
    tab_style(
      style = cell_text(font = "Mercury",style = "italic", weight = "bold", color = "black", size = px(25)),
      locations = cells_title(group = "subtitle")
    ) %>% 
    tab_style(
      style = cell_text(weight = 450, transform = "uppercase"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(align = "right"),
      locations = cells_body(columns = c(bottle_price_dollar))
    ) %>% 
    tab_style(
      style = cell_borders(sides = "left", color = "#111111", weight = px(4)), 
      locations = cells_body(allergens)
    ) %>% 
    tab_style(
      style = cell_text(size = px(35),weight = "bold"), 
      locations = cells_body(fancy_alcohol_content)
    ) %>% 
    tab_style(
      style = cell_text(size = px(18)),
      locations = cells_source_notes()
    ) %>% 
    tab_style(
      style = cell_text(v_align = "middle"),
      locations = cells_title(groups = "title")
    ) %>% 
    # opt_row_striping() %>% 
    tab_options(
      container.width = px(2100),
      # table.width = px(2500),
      table.font.names = c("Verlag"),
      table.font.color = "#111111",
      column_labels.border.bottom.width = px(5),
      column_labels.border.bottom.color = "#111111",
      table_body.border.bottom.color = "#111111",
      table_body.border.bottom.width = px(5),
      column_labels.border.top.width = px(0),
      table.border.top.width = px(5),
      table.border.top.color = "#111111",
      table.border.bottom.width  = px(5),
      table.border.bottom.color = "white", # Table background color
      heading.border.bottom.width = px(50),
      heading.border.bottom.color = "white", # Table background color
      data_row.padding = px(4),
      table.margin.left = px(75),
      table.margin.right = px(75),
      # row.striping.background_color = "#F6F6FF"
    )  
}


# Top Wines Table ---------------------------------------------------------

# Selection for the table
wines_selection <- wines %>%  
  slice(c(1:4,7:9,16,20,22,23,28,29,31,32, 38,52,67, 68, 75))

# Main Table Subtitle
subtitle <- "Great wines improve with age. <br>
Let's dive into some of the most extraordinary cuvées in the world.<br>
Those whose grapes possess ethereal aromas and pure minerality that give focus and energy.<br>
Those demonstrating great character, balance with good acidity and plush tannins."

# Table 
(top_wines_tab <- wines_selection %>% 
   format_wines_df() %>%
   gt() %>% 
   tab_header(
     title = html(paste0("<br><br><span style=\"font-size:50px;\">",str_to_upper("Exceptional Wines"),"<span/>")), 
     subtitle = html(str_replace_all(str_wrap(subtitle, width = 80), '\n', '<br>'))
   ) %>% 
   tab_theme()
)

path <- here::here("Graphics","top_wines")
gtsave(top_wines_tab, glue("{path}.png"))
gtsave(top_wines_tab, glue("{path}.html"))

# High Definition Conversion 
webshot::webshot(glue("{path}.html"),glue("{path}.pdf"))

pdftools::pdf_convert(
  pdf = glue("{path}.pdf"),
  filenames = glue("{path}_from_pdf.png"),
  dpi =  640
)

path <- here::here("Graphics","top_wines")
gtsave(top_wines_tab, glue("{path}.png"))
gtsave(top_wines_tab, glue("{path}.html"))

# High Definition Conversion 
webshot::webshot(glue("{path}.html"),glue("{path}.pdf"))

pdftools::pdf_convert(
  pdf = glue("{path}.pdf"),
  filenames = glue("{path}_from_pdf.png"),
  dpi =  320
)

# Burgundy Wines Table ----------------------------------------------------
(burgundy_under_40 <- wines_bg %>% 
   sample_n(25) %>% 
   format_wines_df() %>% 
   gt() %>% 
   tab_header(
     title = html(paste0("<br><br><span style=\"font-size:50px;\">",str_to_upper("Burgundy Wines"),"<span/>")), 
     subtitle = "Under 40 dollars"
   ) %>% 
   tab_theme()
)

gtsave(burgundy_under_40, "Graphics/burgundy_wines_under_40.png")

# USA Wines Table ---------------------------------------------------------
(usa_under_40 <- wines_usa %>% 
   sample_n(25) %>% 
   format_wines_df() %>% 
   gt() %>% 
   tab_header(
     title = html(paste0("<br><br><span style=\"font-size:50px;\">",str_to_upper("USA Wines"),"<span/>")), 
     subtitle = "Under 40 dollars"
   ) %>% 
   tab_theme()
)

gtsave(usa_under_40, "Graphics/usa_wines_under_40.png")


