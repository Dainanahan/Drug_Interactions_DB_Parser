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

# Extract drug classfications df
drug_classfications_df <- function(rec) {
  drug_key <- xmlValue(rec["drugbank-id"][[1]])
  classfications <- map_df(rec["classification"], xmlValue)
  if (nrow(classfications) > 0) {
    classfications$drug_key <- drug_key 
  }
  return(classfications)
}

# Extract drug synonyms df
drug_synonyms_df <- function(rec) {
  drug_key <- xmlValue(rec["drugbank-id"][[1]])
  synonyms <- xmlToDataFrame(rec[["synonyms"]])
  if (nrow(synonyms) > 0) {
    synonyms$drug_key <- drug_key 
  }
  return(synonyms)
}

# Extract drug products df
drug_products_df <- function(rec) {
  drug_key <- xmlValue(rec["drugbank-id"][[1]])
  products <- xmlToDataFrame(rec[["products"]])
  if (nrow(products) > 0) {
    products$drug_key <- drug_key 
  }
  return(products)
}

# Extract drug mixtures df
drug_mixtures_df <- function(rec) {
  drug_key <- xmlValue(rec["drugbank-id"][[1]])
  mixtures <- xmlToDataFrame(rec[["mixtures"]])
  if (nrow(mixtures) > 0) {
    mixtures$drug_key <- drug_key 
  }
  return(mixtures)
}

# Extract drug packagers df
drug_sub_df <- function(rec, main_node) {
  drug_key <- xmlValue(rec["drugbank-id"][[1]])
  df <- xmlToDataFrame(rec[[main_node]])
  if (nrow(df) > 0) {
    df$drug_key <- drug_key 
  }
  return(df)
  
}

# Extract drug manufacturers df
get_manufacturer_Rec <- function(r, drug_key) {
  tibble(
    name = xmlValue(r),
    url = xmlGetAttr(r, name="url"),
    generic = xmlGetAttr(r, name="generic"),
    drug_key = drug_key
  )
}
get_manufactures_df <- function(rec) {
  return (map_df(xmlChildren(rec[["manufacturers"]]), ~get_manufacturer_Rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug prices df
get_price_rec <- function(r, drug_key) {
  tibble(
    description = xmlValue(r[["description"]]),
    currency = xmlGetAttr(r[["cost"]], name="currency"),
    cost = xmlValue(r[["cost"]]),
    unit = xmlValue(r[["unit"]]),
    drug_key = drug_key
  )
}
get_pricess_df <- function(rec) {
  return (map_df(xmlChildren(rec[["prices"]]), ~get_price_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug atc-codes df
get_atc_codes_rec <- function(r, drug_key) {
  tibble(
    atc_code = xmlGetAttr(r, name ="code"),
    level_1 = xmlValue(r[[1]]),
    code_1 = xmlGetAttr(r[[1]], name ="code"),
    level_2 = xmlValue(r[[2]]),
    code_2 = xmlGetAttr(r[[2]], name ="code"),
    level_3 = xmlValue(r[[3]]),
    code_3 = xmlGetAttr(r[[3]], name ="code"),
    level_4 = xmlValue(r[[4]]),
    code_4 = xmlGetAttr(r[[4]], name ="code"),
    drug_key = drug_key
  )
}
get_atc_codes_df <- function(rec) {
  return (map_df(xmlChildren(rec[["atc-codes"]]), 
                 ~get_atc_codes_rec(.x,
                                    xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug pathways df
get_pathway_rec <- function(r, drug_key) {
  tibble(smpdb_id = xmlValue(r[["smpdb-id"]]),
         name = xmlValue(r[["name"]]),
         category = xmlValue(r[["category"]]),
         drug_key = drug_key)
}

get_pathways_df <- function(rec) {
  return (map_df(xmlChildren(rec[["pathways"]]), 
                 ~get_pathway_rec(.x,
                                  xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug pathways drugs df
get_pathway_drugs_rec <- function(r) {
  smpdb_id = xmlValue(r[["smpdb-id"]])
  drugs <- xmlToDataFrame(r[["drugs"]])
  drugs$smpdb_id <- smpdb_id
  return(drugs)
}

get_pathways_drugs_df <- function(rec) {
  return (map_df(xmlChildren(rec[["pathways"]]), 
                 ~get_pathway_drugs_rec(.x)))
}

# Extract drug pathways enzymes df
get_pathway_enzymes_rec <- function(r) {
  smpdb_id = xmlValue(r[["smpdb-id"]])
  enzymes <- xmlToDataFrame(r[["enzymes"]])
  if (nrow(enzymes) > 0) {
    enzymes$smpdb_id <- smpdb_id
  }
  
  return(enzymes)
}

get_pathways_enzymes_df <- function(rec) {
  return (map_df(xmlChildren(rec[["pathways"]]), 
                 ~get_pathway_enzymes_rec(.x)))
}

# Extract drug enzymes df
get_enzyme_rec <- function(r, drug_key) {
  tibble(id = xmlValue(r[["id"]]),
         name = xmlValue(r[["name"]]),
         organism = xmlValue(r[["organism"]]),
         known_action = xmlValue(r[["known-action"]]),
         inhibition_strength = xmlValue(r[["inhibition-strength"]]),
         induction_strength = xmlValue(r[["induction-strength"]]),
         position = ifelse(is.null(xmlGetAttr(r, name = "position")), NA, xmlGetAttr(r, name = "position")),
         drug_key = drug_key)
}

get_enzymes_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_enzyme_rec(.x,
                                  xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug enzymes actions df
get_enzymes_actions_rec <- function(r) {
  enzyme_id = xmlValue(r[["id"]])
  actions <- xmlToDataFrame(r[["actions"]])
  if (nrow(actions) > 0) {
    actions$enzyme_id <- enzyme_id
  }
  
  return(actions)
}

get_enzymes_actions_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_enzymes_actions_rec(.x)))
}


# Extract drug enzymes articles df
get_enzymes_articles_rec <- function(r) {
  enzyme_id = xmlValue(r[["id"]])
  articles <- xmlToDataFrame(r[["references"]][["articles"]])
  if (nrow(articles) > 0) {
    articles$enzyme_id <- enzyme_id
  }
  
  return(articles)
}

get_enzymes_articles_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_enzymes_articles_rec(.x)))
}

# Extract drug enzymes textbooks df
get_enzymes_textbooks_rec <- function(r) {
  enzyme_id = xmlValue(r[["id"]])
  textbooks <- xmlToDataFrame(r[["references"]][["textbooks"]])
  if (nrow(textbooks) > 0) {
    textbooks$enzyme_id <- enzyme_id
  }
  
  return(textbooks)
}

get_enzymes_textbooks_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_enzymes_textbooks_rec(.x)))
}

# Extract drug enzymes links df
get_enzymes_links_rec <- function(r) {
  enzyme_id = xmlValue(r[["id"]])
  links <- xmlToDataFrame(r[["references"]][["links"]])
  if (nrow(links) > 0) {
    links$enzyme_id <- enzyme_id
  }
  
  return(links)
}

get_enzymes_links_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_enzymes_links_rec(.x)))
}

# Extract drug enzymes polypeptide df
get_enzyme_polypeptide_rec <- function(r) {
  enzyme_id = xmlValue(r[["id"]])
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    tibble(id = ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id")),
           source  = ifelse(is.null(xmlGetAttr(p, name = "source")), NA, xmlGetAttr(p, name = "source")),
           name = xmlValue(p[["name"]]),
           general_function = xmlValue(p[["general-function"]]),
           specific_function = xmlValue(p[["specific-function"]]),
           gene_name = xmlValue(p[["gene-name"]]),
           locus = xmlValue(p[["locus"]]),
           cellular_location = xmlValue(p[["cellular-location"]]),
           transmembrane_regions = xmlValue(p[["transmembrane-regions"]]),
           signal_regions = xmlValue(p[["signal-regions"]]),
           theoretical_pi = xmlValue(p[["theoretical-pi"]]),
           molecular_weight = xmlValue(p[["molecular-weight"]]),
           chromosome_location = xmlValue(p[["chromosome_location"]]),
           organism = xmlValue(p[["organism"]]),
           organism_ncbi_taxonomy_id = xmlGetAttr(p[["organism"]], name = "ncbi-taxonomy-id"),
           amino_acid_sequence = xmlValue(p[["amino-acid-sequence"]]),
           amindo_acid_format = xmlGetAttr(p[["amino-acid-sequence"]], name = "format"),
           gene_sequence = xmlValue(p[["gene-sequence"]]),
           gene_format = xmlGetAttr(p[["gene-sequence"]], name = "format"),
           enzyme_id = enzyme_id)
  }
}

get_enzymes_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_enzyme_polypeptide_rec(.x)))
}

# Extract drug enzymes polypeptide_external_identifiers df
get_enzymes_polypeptide_external_identifiers <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <- ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id"))
    polypeptide_external_identifiers <- xmlToDataFrame(p[["external-identifiers"]])
    polypeptide_external_identifiers$polypeptide_id <- polypeptide_id
    return(polypeptide_external_identifiers)
  }
}

get_enzymes_polypeptide_external_identifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_enzymes_polypeptide_external_identifiers(.x)))
}


# Extract drug enzymes polypeptid synonyms df
get_enzymes_polypeptide_synonyms <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <- ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id"))
    polypeptide_synonyms <- p[["synonyms"]]
    if (xmlSize(polypeptide_synonyms) > 0) {
      tibble(
        synonyms =  paste(xmlApply(polypeptide_synonyms, xmlValue), collapse = ","),
        polypeptide_id = polypeptide_id 
      )
    }
  }
}

get_enzymes_polypeptide_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_enzymes_polypeptide_synonyms(.x)))
}
#max(nchar(a$absorption))
children <- xmlChildren(top)
drug <- map_df(children, ~drug_df(.x))
drug_groups <- map_df(children, ~drug_groups_df(.x))
drug_articles <- map_df(children, ~drug_articles_df(.x))
drug_books <- map_df(children, ~drug_books_df(.x))
drug_links <- map_df(children, ~drug_links_df(.x))
drug_classfications <- map_df(children, ~drug_classfications_df(.x))
drug_synonyms <- map_df(children, ~drug_synonyms_df(.x))
drug_products <- map_df(children, ~drug_products_df(.x))
drug_mixture <- map_df(children, ~drug_mixtures_df(.x))
drug_packagers <- map_df(children, ~drug_sub_df(.x, "packagers"))
drug_manufacturers <- map_df(children, ~get_manufactures_df(.x))
drug_prices <- map_df(children, ~get_pricess_df(.x))
drug_categories <- map_df(children, ~drug_sub_df(.x, "categories"))
drug_affected_organisms <- map_df(children, ~drug_sub_df(.x, "affected-organisms"))
drug_dosages <- map_df(children, ~drug_sub_df(.x, "dosages"))
drug_atc_codes <- map_df(children, ~get_atc_codes_df(.x))
drug_ahfs_codes <- map_df(children, ~drug_sub_df(.x, "ahfs-codes"))
drug_pdb_entries<- map_df(children, ~drug_sub_df(.x, "pdb-entries"))
drug_patents<- map_df(children, ~drug_sub_df(.x, "patents"))
drug_food_interactions<- map_df(children, ~drug_sub_df(.x, "food-interactions"))
drug_drug_interactions<- map_df(children, ~drug_sub_df(.x, "drug-interactions"))
drug_sequences<- map_df(children, ~drug_sub_df(.x, "sequences"))
drug_experimental_properties<- map_df(children, ~drug_sub_df(.x, "experimental-properties"))
drug_external_identifiers<- map_df(children, ~drug_sub_df(.x, "external-identifiers"))
drug_external_links <- map_df(children, ~drug_sub_df(.x, "external-links"))
drug_pathway <- map_df(children, ~get_pathways_df(.x))
drug_pathway_drugs <- map_df(children, ~get_pathways_drugs_df(.x))
drug_pathway_enzymes <- map_df(children, ~get_pathways_enzymes_df(.x))
#drug_reactions <- map_df(children, ~drug_sub_df(.x, "reactions"))
drug_snp_effects <- map_df(children, ~drug_sub_df(.x, "snp-effects"))
drug_snp_adverse_drug_reactions <- map_df(children, ~drug_sub_df(.x, "snp-adverse-drug-reactions"))
drug_enzymes <- map_df(children, ~get_enzymes_df(.x))
drug_enzymes_actions <- map_df(children, ~get_enzymes_actions_df(.x))
drug_enzymes_articles <- map_df(children, ~get_enzymes_articles_df(.x))
drug_enzymes_textbooks <- map_df(children, ~get_enzymes_textbooks_df(.x))
drug_enzymes_links <- map_df(children, ~get_enzymes_links_df(.x))
drug_enzymes_polypeptides <- map_df(children, ~get_enzymes_polypeptide_df(.x))
drug_enzymes_polypeptide_external_identifiers <- map_df(children, 
                                                        ~get_enzymes_polypeptide_external_identifiers_df(.x))

drug_enzymes_polypeptide_synonyms <- map_df(children, 
                                                        ~get_enzymes_polypeptide_synonyms_df(.x))
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
# add primary key of drug table
dbExecute(conn = con, statement = "Alter table drug
          alter column primary_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug add primary key (primary_key);")

#store drug groups in db
dbWriteTable(conn = con, value = drug_groups, name = "drug_groups")
# add foreign key of drug table
dbExecute(conn = con, statement = "Alter table drug_groups
          alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_groups ADD CONSTRAINT FK_groups_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug articles in db
dbWriteTable(conn = con, value = drug_articles, name = "drug_articles")
# add foreign key of drug table
dbExecute(conn = con, statement = "Alter table drug_articles
          alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_articles ADD CONSTRAINT FK_articles_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug books in db
dbWriteTable(conn = con, value = drug_books, name = "drug_books")
# add foreign key of drug table
dbExecute(conn = con, statement = "Alter table drug_books
          alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_books ADD CONSTRAINT FK_books_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug links in db
dbWriteTable(conn = con, value = drug_links, name = "drug_links")
# add foreign key of drug table
dbExecute(conn = con, statement = "Alter table drug_links
          alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_links ADD CONSTRAINT FK_links_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug classifications in db
dbWriteTable(conn = con, value = drug_classfications, name = "drug_classifications",
             field.types = list(classification = "varchar(2961)"))
# add foreign key of drug table
dbExecute(conn = con, statement = "Alter table drug_classifications
          alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_classifications ADD CONSTRAINT FK_classifications_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug synonyms in db
dbWriteTable(conn = con, value = drug_synonyms, name = "drug_synonyms")
# add foreign key of drug table
dbExecute(conn = con, statement = "Alter table drug_synonyms
          alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_synonyms ADD CONSTRAINT FK_synonyms_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug products in db
dbWriteTable(conn = con, value = drug_products, name = "drug_products")
# add foreign key of drug table
dbExecute(conn = con, statement = "Alter table drug_products
          alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_products ADD CONSTRAINT FK_products_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

#store drug mixtures in db
dbWriteTable(conn = con, value = drug_mixture, name = "drug_mixtures")
# add foreign key of drug table
dbExecute(conn = con, statement = "Alter table drug_mixtures
          alter column drug_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug_mixtures ADD CONSTRAINT FK_mixture_drug 
          FOREIGN KEY (drug_key) REFERENCES drug(primary_key);")

save_drug_sub <- function(df, table_name, save_table_only = FALSE, field.types = NULL,
                          foreign_key = "drug_key", ref_table = "drug(primary_key)") {
  #store drug sub_Table in db
  dbWriteTable(conn = con, value = df, name = table_name, field.types = field.types)
  if (!save_table_only) {
    # add foreign key of drug table
    dbExecute(conn = con, statement = paste("Alter table", table_name,
                                            "alter column", foreign_key, "varchar(255) NOT NULL;"))
    dbExecute(conn = con, statement = paste("Alter table", table_name,"ADD CONSTRAINT",
                                            paste("FK_", table_name,"_drug", sep = ""),
                                            paste("FOREIGN KEY (", foreign_key,") REFERENCES", ref_table,";"))) 
  }
}

save_drug_sub(drug_packagers, "drug_packagers")
save_drug_sub(drug_manufacturers, "drug_manufacturers")
save_drug_sub(drug_prices, "drug_prices")
save_drug_sub(drug_affected_organisms, "drug_affected_organisms")
save_drug_sub(drug_dosages, "drug_dosages")
save_drug_sub(drug_atc_codes, "drug_atc_codes")
save_drug_sub(drug_ahfs_codes, "drug_ahfs_codes")
save_drug_sub(drug_pdb_entries, "drug_pdb_entries")
save_drug_sub(drug_patents, "drug_patents")
save_drug_sub(drug_food_interactions, "drug_food_interactions")
save_drug_sub(drug_drug_interactions, "drug_interactions")
save_drug_sub(drug_sequences, "drug_sequences")
save_drug_sub(drug_experimental_properties, "drug_experimental_properties")
save_drug_sub(drug_external_identifiers, "drug_external_identifiers")
save_drug_sub(drug_external_links, "drug_external_links")
save_drug_sub(drug_pathway, "drug_pathways")
save_drug_sub(drug_pathway_drugs, "drug_pathway_drugs", save_table_only = TRUE)
save_drug_sub(drug_pathway_enzymes, "drug_pathway_enzymes", save_table_only = TRUE)
save_drug_sub(drug_snp_adverse_drug_reactions, "drug_snp_adverse_drug_reactions")
save_drug_sub(drug_snp_effects, "drug_snp_effects")
save_drug_sub(drug_enzymes, "drug_enzymes")
save_drug_sub(drug_enzymes_actions, "drug_enzymes_actions", save_table_only = TRUE)
save_drug_sub(drug_enzymes_articles, "drug_enzymes_articles", save_table_only = TRUE)
save_drug_sub(drug_enzymes_links, "drug_enzymes_links", save_table_only = TRUE)
save_drug_sub(drug_enzymes_textbooks, "drug_enzymes_textbooks", save_table_only = TRUE)
save_drug_sub(drug_enzymes_polypeptides, "drug_enzymes_polypeptides", save_table_only = TRUE,
              field.types = list(general_function =
                                   paste("varchar(",
                                         max(nchar(drug_enzymes_polypeptides$general_function)), ")", sep = ""),
                                 specific_function =
                                   paste("varchar(",
                                         max(nchar(drug_enzymes_polypeptides$specific_function)), ")", sep = ""),
                                 amino_acid_sequence =
                                   paste("varchar(",
                                         max(nchar(drug_enzymes_polypeptides$amino_acid_sequence)), ")", sep = ""),
                                 gene_sequence =
                                   paste("varchar(",
                                         max(nchar(drug_enzymes_polypeptides$gene_sequence)), ")", sep = "")))
save_drug_sub(drug_enzymes_polypeptide_external_identifiers, "drug_enzymes_polypeptides_external_identifiers",
              save_table_only = TRUE)
save_drug_sub(drug_enzymes_polypeptide_synonyms, "drug_enzymes_polypeptides_synonyms", save_table_only = TRUE)
# disconnect db
dbDisconnect(conn = con)