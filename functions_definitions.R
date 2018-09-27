# required librries
library(XML) 
library(purrr)
library(tibble)
library(magrittr)
library(DBI)
library(progress)

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

drug_sub_df <- function(rec, main_node, seconadary_node = NULL,
                        id = "drugbank-id", byValue = FALSE) {
  parent_key <- NULL
  if(!is.null(id)) {
    parent_key <- xmlValue(rec[id][[1]])
  }
  
  if (byValue) {
    df <- map_df(rec[main_node], xmlValue)
  } else {
    if (is.null(seconadary_node)) {
      df <- xmlToDataFrame(rec[[main_node]])
    } else {
      df <- xmlToDataFrame(rec[[main_node]][[seconadary_node]])
    }
    
  }
  
  if (nrow(df) > 0 && !is.null(parent_key)) {
    df$parent_key <- parent_key 
  }
  return(df)
}

# Extract drug manufacturers df
get_manufacturer_rec <- function(r, drug_key) {
  tibble(
    name = xmlValue(r),
    url = xmlGetAttr(r, name="url"),
    generic = xmlGetAttr(r, name="generic"),
    drug_key = drug_key
  )
}
get_manufactures_df <- function(rec) {
  return (map_df(xmlChildren(rec[["manufacturers"]]),
                 ~get_manufacturer_rec(., xmlValue(rec["drugbank-id"][[1]]))))
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
  return (map_df(xmlChildren(rec[["prices"]]),
                 ~get_price_rec(., xmlValue(rec["drugbank-id"][[1]]))))
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
get_pathways_drugs_df <- function(rec) {
  return(map_df(xmlChildren(rec[["pathways"]]),
                ~drug_sub_df(.x, "drugs", id = "smpdb-id")))
}

# Extract drug pathways enzymes df
get_pathways_enzymes_df <- function(rec) {
  return(map_df(xmlChildren(rec[["pathways"]]),
                ~drug_sub_df(.x, "enzymes", id = "smpdb-id")))
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
get_enzymes_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~drug_sub_df(.x, "actions", id = "id")))
}

get_targets_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]),
                ~drug_sub_df(.x, "actions", id = "id")))
}

get_carriers_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]),
                ~drug_sub_df(.x, "actions", id = "id")))
}

get_transporters_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~drug_sub_df(.x, "actions", id = "id")))
}


# Extract drug articles df
get_enzymes_articles_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")))
}

get_transporters_articles_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")))
}

get_carriers_articles_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")))
}

get_targets_articles_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")))
}

# Extract drug textbooks df

get_enzymes_textbooks_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")))
}


get_transporters_textbooks_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")))
}

get_carriers_textbooks_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")))
}

get_targets_textbooks_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")))
}

# Extract drug links df
get_enzymes_links_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "links", id = "id")))
}


get_transporters_links_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "links", id = "id")))
}


get_carriers_links_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "links", id = "id")))
}


get_targets_links_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]),
                ~drug_sub_df(.x, "references", seconadary_node = "links", id = "id")))
}

# Extract drug polypeptide df
get_polypeptide_rec <- function(r) {
  parent_id = xmlValue(r[["id"]])
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
           parent_id = parent_id)
  }
}

get_enzymes_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_polypeptide_rec(.x)))
}

get_transporters_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]), 
                 ~get_polypeptide_rec(.x)))
}

get_carriers_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]), 
                 ~get_polypeptide_rec(.x)))
}

get_targets_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]), 
                 ~get_polypeptide_rec(.x)))
}

# Extract drug polypeptide_external_identifiers df
get_polypeptide_external_identifiers <- function(r) {
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
                 ~get_polypeptide_external_identifiers(.x)))
}

get_transporters_polypeptide_external_identifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]), 
                 ~get_polypeptide_external_identifiers(.x)))
}

get_carriers_polypeptide_external_identifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]), 
                 ~get_polypeptide_external_identifiers(.x)))
}

get_targets_polypeptide_external_identifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]), 
                 ~get_polypeptide_external_identifiers(.x)))
}


# Extract drug polypeptid synonyms df
get_polypeptide_synonyms <- function(r) {
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
                 ~get_polypeptide_synonyms(.x)))
}

get_transporters_polypeptide_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]), 
                 ~get_polypeptide_synonyms(.x)))
}

get_carriers_polypeptide_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]), 
                 ~get_polypeptide_synonyms(.x)))
}

get_targets_polypeptide_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]), 
                 ~get_polypeptide_synonyms(.x)))
}

# Extract drug polypeptide pfams df
get_polypeptide_pfams <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <- ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id"))
    firstCell <- xmlValue(xmlChildren(p[["pfams"]])[[1]])
    if(firstCell != "\n    " ) {
      polypeptide_pfams<- xmlToDataFrame(xmlChildren(p[["pfams"]]))
      polypeptide_pfams$polypeptide_id <- polypeptide_id
      return(polypeptide_pfams)
    }
  }
}

get_enzymes_polypeptide_pfams_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_polypeptide_pfams(.x)))
}

get_targets_polypeptide_pfams_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]), 
                 ~get_polypeptide_pfams(.x)))
}

get_carriers_polypeptide_pfams_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]), 
                 ~get_polypeptide_pfams(.x)))
}

get_transporters_polypeptide_pfams_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]), 
                 ~get_polypeptide_pfams(.x)))
}


# Extract drug polypeptide go-classifiers df
get_polypeptide_go_classifiers <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <- ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id"))
    firstCell <- xmlValue(xmlChildren(p[["pfams"]])[[1]])
    if(firstCell != "\n    " ) {
      polypeptide_go_classifiers <- xmlToDataFrame(xmlChildren(p[["go-classifiers"]]))
      polypeptide_go_classifiers$polypeptide_id <- polypeptide_id
      return(polypeptide_go_classifiers)
    }
  }
}

get_enzymes_polypeptide_go_classifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]), 
                 ~get_polypeptide_go_classifiers(.x)))
}

get_transporters_polypeptide_go_classifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]), 
                 ~get_polypeptide_go_classifiers(.x)))
}

get_carriers_polypeptide_go_classifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]), 
                 ~get_polypeptide_go_classifiers(.x)))
}

get_targets_polypeptide_go_classifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]), 
                 ~get_polypeptide_go_classifiers(.x)))
}

# Extract drug reactions df
get_reactions_rec <- function(r, drug_key) {
  tibble(
    sequence = xmlValue(r[["sequence"]]),
    left_drugbank_id = xmlValue(r[["left-element"]][["drugbank-id"]]),
    left_drugbank_name = xmlValue(r[["left-element"]][["name"]]),
    right_drugbank_id = xmlValue(r[["right-element"]][["drugbank-id"]]),
    right_drugbank_name = xmlValue(r[["right-element"]][["name"]]),
    drug_key = drug_key
  )
}
get_reactions_df <- function(rec) {
  return (map_df(xmlChildren(rec[["reactions"]]), 
                 ~get_reactions_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug reactions enzymes df
get_reactions_enzymes_df <- function(rec) {
  return (map_df(xmlChildren(rec[["reactions"]]),
                 ~drug_sub_df(.x, "enzymes", id = NULL)))
}


# Extract drug carriers df
get_carrier_rec <- function(r, drug_key) {
  tibble(id = xmlValue(r[["id"]]),
         name = xmlValue(r[["name"]]),
         organism = xmlValue(r[["organism"]]),
         known_action = xmlValue(r[["known-action"]]),
         position = ifelse(is.null(xmlGetAttr(r, name = "position")), NA, xmlGetAttr(r, name = "position")),
         drug_key = drug_key)
}

get_carriers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]), 
                 ~get_carrier_rec(.x,
                                  xmlValue(rec["drugbank-id"][[1]]))))
}


get_targets_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]), 
                 ~get_carrier_rec(.x,
                                  xmlValue(rec["drugbank-id"][[1]]))))
}


get_transporters_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]), 
                 ~get_carrier_rec(.x,
                                  xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug classfications df
drug_classfications_df <- function(rec) {
  if (is.null(rec[["classification"]])) return()
  a <- xmlToList(rec[["classification"]])
  return(tibble(
    parent_key = xmlValue(rec["drugbank-id"][[1]]),
    classifications = paste(names(a), a, collapse = ";")
  ))
}

get_synonym_rec <- function(rec, parent_key) {
  return(tibble(
    parent_key = parent_key,
    synonym = xmlValue(rec),
    language = xmlGetAttr(rec, name = "language"),
    coder = xmlGetAttr(rec, name = "coder")
  ))
}

get_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["synonyms"]]), 
                 ~get_synonym_rec(.x,
                                  xmlValue(rec["drugbank-id"][[1]]))))
}

save_drug_sub <- function(con, df, table_name, save_table_only = FALSE, field.types = NULL,
                          primary_key = NULL,
                          foreign_key = "parent_key", ref_table = "drug(primary_key)") {
  #store drug sub_Table in db
  dbWriteTable(conn = con, value = df, name = table_name, field.types = field.types, overwrite = TRUE)
  if (!save_table_only) {
    # add primary key of drug table
    if (!is.null(primary_key)) {
      for (key in primary_key) {
        dbExecute(conn = con, statement = paste("Alter table", table_name,
                                                "alter column", key, "varchar(255) NOT NULL;"))
      }
      dbExecute(conn = con, statement = paste("Alter table", table_name,
                                              "add primary key(", paste(primary_key, collapse = ","), ");"))
      
    }
    # add foreign key of drug table
    if (!is.null(foreign_key)) {
      dbExecute(conn = con, statement = paste("Alter table", table_name,"ADD CONSTRAINT",
                                              paste("FK_", table_name,"_drug", sep = ""),
                                              paste("FOREIGN KEY (", foreign_key,") REFERENCES", ref_table,";")))
    }
 
  }
}

# create progress bar
pb <- progress_bar$new(
  format = ":what [:bar] :percent eta: :elapsed",
  clear = FALSE, total = 75, width = 60)

show_progress <- function(what, step) {
  pb$tick(tokens = list(what = what))
  Sys.sleep(step / 100)
}

# read and parse the xml database
get_xml_db_rows <- function (xml_db_name) {
  drugbank_db <- xmlParse(xml_db_name)
  top <- xmlRoot(drugbank_db)
  children <- xmlChildren(top)
  return(children)
}

extract_data_frames <- function(children) {
#   drug_synonyms <- map_df(children, ~drug_sub_df(.x, "synonyms"))
#   show_progress(what = "successfully parsed drug table   ", 8)
#   drug_products <- map_df(children, ~drug_sub_df(.x, "products"))
#   show_progress(what = "successfully parsed drug table   ", 9)
#   drug_mixtures <- map_df(children, ~drug_sub_df(.x, "mixtures"))
#   show_progress(what = "successfully parsed drug table   ", 10)
#   drug_packagers <- map_df(children, ~drug_sub_df(.x, "packagers"))
#   show_progress(what = "successfully parsed drug table   ", 11)
#   drug_manufacturers <- map_df(children, ~get_manufactures_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 12)
#   drug_prices <- map_df(children, ~get_pricess_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 13)
#   drug_categories <- map_df(children, ~drug_sub_df(.x, "categories"))
#   show_progress(what = "successfully parsed drug table   ", 14)
#   drug_affected_organisms <- map_df(children, ~drug_sub_df(.x, "affected-organisms"))
#   show_progress(what = "successfully parsed drug table   ", 15)
#   drug_dosages <- map_df(children, ~drug_sub_df(.x, "dosages"))
#   show_progress(what = "successfully parsed drug table   ", 16)
#   drug_atc_codes <- map_df(children, ~get_atc_codes_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 17)
#   drug_ahfs_codes <- map_df(children, ~drug_sub_df(.x, "ahfs-codes"))
#   show_progress(what = "successfully parsed drug table   ", 18)
#   drug_pdb_entries<- map_df(children, ~drug_sub_df(.x, "pdb-entries"))
#   show_progress(what = "successfully parsed drug table   ", 19)
#   drug_patents<- map_df(children, ~drug_sub_df(.x, "patents"))
#   show_progress(what = "successfully parsed drug table   ", 20)
#   drug_food_interactions<- map_df(children, ~drug_sub_df(.x, "food-interactions"))
#   show_progress(what = "successfully parsed drug table   ", 21)
#   drug_drug_interactions<- map_df(children, ~drug_sub_df(.x, "drug-interactions"))
#   show_progress(what = "successfully parsed drug table   ", 22)
#   drug_sequences<- map_df(children, ~drug_sub_df(.x, "sequences"))
#   show_progress(what = "successfully parsed drug table   ", 23)
#   drug_experimental_properties<- map_df(children, ~drug_sub_df(.x, "experimental-properties"))
#   show_progress(what = "successfully parsed drug table   ", 24)
#   drug_external_identifiers<- map_df(children, ~drug_sub_df(.x, "external-identifiers"))
#   show_progress(what = "successfully parsed drug table   ", 25)
#   drug_external_links <- map_df(children, ~drug_sub_df(.x, "external-links"))
#   show_progress(what = "successfully parsed drug table   ", 26)
#   drug_pathway <- map_df(children, ~get_pathways_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 27)
#   drug_pathway_drugs <- map_df(children, ~get_pathways_drugs_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 28)
#   drug_pathway_enzymes <- map_df(children, ~get_pathways_enzymes_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 29)
#   drug_snp_effects <- map_df(children, ~drug_sub_df(.x, "snp-effects"))
#   show_progress(what = "successfully parsed drug table   ", 30)
#   drug_snp_adverse_drug_reactions <- map_df(children, ~drug_sub_df(.x, "snp-adverse-drug-reactions"))
#   show_progress(what = "successfully parsed drug table   ", 31)
#   drug_enzymes <- map_df(children, ~get_enzymes_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 32)
#   
#   drug_enzymes_actions <- map_df(children, ~get_enzymes_actions_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 33)
#   drug_targets_actions <- map_df(children, ~get_targets_actions_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 34)
#   drug_carriers_actions <- map_df(children, ~get_carriers_actions_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 35)
#   drug_transporters_actions <- map_df(children, ~get_transporters_actions_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 36)
#   
#   drug_enzymes_articles <- map_df(children, ~get_enzymes_articles_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 37)
#   drug_targets_articles <- map_df(children, ~get_targets_articles_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 38)
#   drug_carriers_articles <- map_df(children, ~get_carriers_articles_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 39)
#   drug_transporters_articles <- map_df(children, ~get_transporters_articles_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 40)
#   
#   drug_enzymes_textbooks <- map_df(children, ~get_enzymes_textbooks_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 41)
#   drug_targets_textbooks <- map_df(children, ~get_targets_textbooks_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 42)
#   drug_carriers_textbooks <- map_df(children, ~get_carriers_textbooks_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 43)
#   drug_transporters_textbooks <- map_df(children, ~get_transporters_textbooks_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 44)
#   
#   drug_enzymes_links <- map_df(children, ~get_enzymes_links_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 45)
#   drug_targets_links <- map_df(children, ~get_targets_links_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 46)
#   drug_carriers_links <- map_df(children, ~get_carriers_links_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 47)
#   drug_transporters_links <- map_df(children, ~get_transporters_links_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 48)
#   
#   drug_enzymes_polypeptides <- map_df(children, ~get_enzymes_polypeptide_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 49)
#   drug_targets_polypeptides <- map_df(children, ~get_targets_polypeptide_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 50)
#   drug_carriers_polypeptides <- map_df(children, ~get_carriers_polypeptide_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 51)
#   drug_transporters_polypeptides <- map_df(children, ~get_transporters_polypeptide_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 52)
#   
#   drug_enzymes_polypeptide_external_identifiers <- map_df(children, 
#                                                           ~get_enzymes_polypeptide_external_identifiers_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 53)
#   drug_targets_polypeptide_external_identifiers <- map_df(children, 
#                                                           ~get_targets_polypeptide_external_identifiers_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 54)
#   drug_carriers_polypeptide_external_identifiers <- map_df(children, 
#                                                            ~get_carriers_polypeptide_external_identifiers_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 55)
#   drug_transporters_polypeptide_external_identifiers <- map_df(children, 
#                                                                ~get_transporters_polypeptide_external_identifiers_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 56)
#   
#   drug_enzymes_polypeptide_synonyms <- map_df(children, 
#                                               ~get_enzymes_polypeptide_synonyms_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 57)
#   drug_targets_polypeptide_synonyms <- map_df(children, 
#                                               ~get_targets_polypeptide_synonyms_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 58)
#   drug_carriers_polypeptide_synonyms <- map_df(children, 
#                                                ~get_carriers_polypeptide_synonyms_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 59)
#   drug_transporters_polypeptide_synonyms <- map_df(children, 
#                                                    ~get_transporters_polypeptide_synonyms_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 60)
#   
#   drug_enzymes_polypeptide_pfams <-  map_df(children, 
#                                             ~get_enzymes_polypeptide_pfams_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 61)
#   drug_targets_polypeptide_pfams <-  map_df(children, 
#                                             ~get_targets_polypeptide_pfams_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 62)
#   drug_carriers_polypeptide_pfams <-  map_df(children, 
#                                              ~get_carriers_polypeptide_pfams_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 63)
#   drug_transporters_polypeptide_pfams <-  map_df(children, 
#                                                  ~get_transporters_polypeptide_pfams_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 64)
#   drug_enzymes_polypeptide_go_classifiers <-  map_df(children, 
#                                                      ~get_enzymes_polypeptide_go_classifiers_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 65)
#   drug_targets_polypeptide_go_classifiers <-  map_df(children, 
#                                                      ~get_targets_polypeptide_go_classifiers_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 66)
#   drug_transporters_polypeptide_go_classifiers <-  map_df(children, 
#                                                           ~get_transporters_polypeptide_go_classifiers_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 67)
#   drug_carriers_polypeptide_go_classifiers <-  map_df(children, 
#                                                       ~get_carriers_polypeptide_go_classifiers_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 68)
#   drug_reactions <-  map_df(children, ~get_reactions_df(.x))
#   drug_reactions_enzymes <-  map_df(children, ~get_reactions_enzymes_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 69)
#   
#   drug_carriers <-  map_df(children, ~get_carriers_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 70)
#   drug_transporters <-  map_df(children, ~get_transporters_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 71)
#   drug_targets <-  map_df(children, ~get_targets_df(.x))
#   show_progress(what = "successfully parsed drug table   ", 72)
}