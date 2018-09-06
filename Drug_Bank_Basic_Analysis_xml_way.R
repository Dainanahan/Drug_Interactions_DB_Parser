library(XML)
library(purrr)
library(tibble)
library(magrittr)
library(DBI)

# read and parse the xml database
drugbank_db <- xmlParse("drugbank.xml")
top <- xmlRoot(drugbank_db)

# Extract drug df
drug_df <- function(rec) {
  tibble(
    primary_key = xmlValue(rec["drugbank-id"][[1]]),
    secondary_key = ifelse(length(rec["drugbank-id"]) > 1, xmlValue(rec["drugbank-id"][[2]]), NA),
    third_key = ifelse(length(rec["drugbank-id"]) > 2, xmlValue(rec["drugbank-id"][[3]]), NA),
    type = xmlGetAttr(node = rec, name = "type"),
    created = as.Date(xmlGetAttr(node = rec, name = "created")),
    updated = as.Date(xmlGetAttr(node = rec, name = "updated")),
    name = xmlValue(rec[["name"]]),
    description = xmlValue(rec[["description"]]),
    cas_number = xmlValue(rec[["cas-number"]]),
    unii = xmlValue(rec[["unii"]]),
    state = xmlValue(rec[["state"]]),
    groups_count = xmlSize(rec[["groups"]]),
    articles_count = xmlSize(rec[["general-references"]][["articles"]]),
    books_count = xmlSize(rec[["general-references"]][["textbooks"]]),
    links_count = xmlSize(rec[["general-references"]][["links"]]),
    synthesis_reference = xmlValue(rec[["synthesis-reference"]]),
    indication = xmlValue(rec[["indication"]]),
    pharmacodynamics = xmlValue(rec[["pharmacodynamics"]]),
    mechanism_of_action = xmlValue(rec[["mechanism-of-action"]]),
    metabolism = xmlValue(rec[["metabolism"]]),
    absorption = xmlValue(rec[["absorption"]]),
    half_life = xmlValue(rec[["half-life"]]),
    protein_binding = xmlValue(rec[["protein-binding"]]),
    route_of_elimination = xmlValue(rec[["route-of-elimination"]]),
    volume_of_distribution = xmlValue(rec[["volume-of-distribution"]]),
    clearance = xmlValue(rec[["clearance"]]),
    synonyms_count = xmlSize(rec[["synonyms"]]),
    products_count = xmlSize(rec[["products"]]),
    international_brands = xmlValue(rec[["international-brands"]]),
    mixtures_count = xmlSize(rec[["mixtures"]]),
    manufacturers_count = xmlSize(rec[["manufacturers"]]),
    prices_count = xmlSize(rec[["prices"]]),
    categories_count = xmlSize(rec[["categories"]]),
    affected_organisms_count = xmlSize(rec[["affected-organisms"]]),
    dosages_count = xmlSize(rec[["dosages"]]),
    atc_codes_count = xmlSize(rec[["atc-codes"]]),
    ahfs_codes_count = xmlSize(rec[["ahfs-codes"]]),
    pdb_entries = xmlSize(rec[["pdb-entries"]]),
    fda_label = xmlValue(rec[["fda-label"]]),
    msds = xmlValue(rec[["msds"]]),
    patents_count = xmlSize(rec[["patents"]]),
    food_interactions = xmlSize(rec[["food-interactions"]]),
    drug_interactions_count = xmlSize(rec[["drug-interactions"]]),
    sequences_count = xmlSize(rec[["sequences"]]),
    experimental_properties_count = xmlSize(rec[["experimental-properties"]]),
    external_identifiers_count = xmlSize(rec[["external-identifiers"]]),
    external_links_count = xmlSize(rec[["external-links"]]),
    pathways_count = xmlSize(rec[["pathways"]]),
    reactions_count = xmlSize(rec[["reactions"]]),
    snp_effects_count = xmlSize(rec[["snp-effects"]]),
    snp_adverse_drug_reactions_count = xmlSize(rec[["snp-adverse-drug-reactions"]]),
    targets_count = xmlSize(rec[["targets"]]),
    enzymes_count = xmlSize(rec[["enzymes"]]),
    carriers_count = xmlSize(rec[["carriers"]]),
    transporters_count = xmlSize(rec[["transporters"]]),
     toxicity = xmlValue(rec[["toxicity"]])
    )
}

# Extract drug groups df
drug_groups_df <- function(rec) {
    drug_key <- xmlValue(rec["drugbank-id"][[1]])
    groups <- xmlToDataFrame(rec[["groups"]])
    if (nrow(groups) > 0) {
      groups$drug_key <- drug_key
    } 
    
    return(groups)
}

# Extract drug articles df
drug_articles_df <- function(rec) {
  drug_key <- xmlValue(rec["drugbank-id"][[1]])
  articles <- xmlToDataFrame(rec[["general-references"]][["articles"]])
  if (nrow(articles) > 0) {
    articles$drug_key <- drug_key 
  }
  return(articles)
}

# Extract drug books df
drug_books_df <- function(rec) {
  drug_key <- xmlValue(rec["drugbank-id"][[1]])
  books <- xmlToDataFrame(rec[["general-references"]][["textbooks"]])
  if (nrow(books) > 0) {
    books$drug_key <- drug_key 
  }
  return(books)
}

# Extract drug links df
drug_links_df <- function(rec) {
  drug_key <- xmlValue(rec["drugbank-id"][[1]])
  links <- xmlToDataFrame(rec[["general-references"]][["links"]])
  if (nrow(links) > 0) {
    links$drug_key <- drug_key 
  }
  return(links)
}

#max(nchar(a$absorption))
drug <- map_df(xmlChildren(top), ~drug_df(.x))
drug_groups <- map_df(xmlChildren(top), ~drug_groups_df(.x))
drug_articles <- map_df(xmlChildren(top), ~drug_articles_df(.x))
drug_books <- map_df(xmlChildren(top), ~drug_books_df(.x))
drug_links <- map_df(xmlChildren(top), ~drug_links_df(.x))
#db connection
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "MOHAMMED\\SQL2016", 
                Database = "drugbank", Trusted_Connection = "True")
#set column types
columnTypes <- list(description = "varchar(6349)", mechanism_of_action = "varchar(7189)",
                    pharmacodynamics = "varchar(3179)", indication = "varchar(3165)",
                    absorption = "nvarchar(3579)", route_of_elimination = "varchar(1324)",
                    metabolism = "varchar(2926)", international_brands = "varchar(2904)",
                    protein_binding = "varchar(778)", synthesis_reference="varchar(946)",
                    clearance = "varchar(2128)", half_life = "varchar(1173)",
                    route_of_elimination = "varchar(1324)", absorption = "varchar(3579)",
                    volume_of_distribution = "varchar(1378)",
                    toxicity = "varchar(max)", created = "date", updated = "date")

#store drug in db
dbWriteTable(conn = con, value = drug, name = "drug", field.types = columnTypes)
# add primary key to drug table
dbExecute(conn = con, statement = "Alter table drug
alter column primary_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug add primary key (primary_key);")

#store drug groups in db
dbWriteTable(conn = con, value = drug_groups, name = "drug_groups")
# add foreign key to drug table
dbExecute(conn = con, statement = "Alter table drug_groups
alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_groups ADD CONSTRAINT FK_groups_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug articles in db
dbWriteTable(conn = con, value = drug_articles, name = "drug_articles")
# add foreign key to drug table
dbExecute(conn = con, statement = "Alter table drug_articles
alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_articles ADD CONSTRAINT FK_articles_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug books in db
dbWriteTable(conn = con, value = drug_books, name = "drug_books")
# add foreign key to drug table
dbExecute(conn = con, statement = "Alter table drug_books
alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_books ADD CONSTRAINT FK_books_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug links in db
dbWriteTable(conn = con, value = drug_links, name = "drug_links")
# add foreign key to drug table
dbExecute(conn = con, statement = "Alter table drug_links
alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_links ADD CONSTRAINT FK_links_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

# disconnect db
dbDisconnect(conn = con)