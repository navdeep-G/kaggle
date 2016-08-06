library(stringr)

preprocess_products <- function(product_data) {
  
  product_names <- as.character(product_data$NombreProducto)
  weight <- unlist(lapply(product_names, extract_weight))
  pieces <- unlist(lapply(product_names, extract_pieces))
  brand <- unlist(lapply(product_names, extract_brand))
  has_choco <- unlist(lapply(product_names, grepl, pattern="Choco"))
  has_vanilla <- unlist(lapply(product_names, grepl, pattern="Va(i)?nilla"))
  has_multigrain <- unlist(lapply(product_names, grepl, pattern="Multigrano"))
  
  data.frame(
    ID=product_data$Producto_ID,
    product_name=product_names,
    brand=brand,
    weight=weight,
    pieces=pieces,
    weight_per_piece=weight/pieces,
    has_choco=has_choco,
    has_vanilla=has_vanilla,
    has_multigrain=has_multigrain
  )
}

extract_token <- function(value, expr) {
  tokens <- strsplit(value, " ")
  index <- grep(expr, tokens)
  ifelse(length(index) == 0, NA, tokens[index[1]])
}

extract_weight <- function(product_name) {
  weight_str <- extract_token(product_name, "\\d+[Kg|g]")
  if (is.na(weight_str)) return(NA)
  groups <- str_match_all(weight_str, "(\\d+)(Kg|g)")
  weight <- strtoi(groups[[1]][2])
  unit <- groups[[1]][3]
  ifelse(unit == "Kg", 1000 * weight, weight)
}

extract_pieces <- function(product_name) {
  pieces_str <- extract_token(product_name, "\\d+p\\b")
  if (is.na(pieces_str)) return(NA)
  groups <- str_match_all(pieces_str, "(\\d+)(p)")
  return(strtoi(groups[[1]][2]))
}

extract_brand <- function(product_name) {
  tokens <- strsplit(product_name, " ")[[1]]
  tokens[length(tokens) - 1]
}

product_data <- read.csv('../../data/producto_tabla.csv')
preprocessed <- preprocess_products(product_data)
write.csv(preprocessed, 'preprocessed_products.csv')