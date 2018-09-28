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

drug_sub_df <- function(rec,
                        main_node,
                        seconadary_node = NULL,
                        id = "drugbank-id",
                        byValue = FALSE) {
  parent_key <- NULL
  if (!is.null(id)) {
    parent_key <- xmlValue(rec[id][[1]])
  }
  
  if (byValue) {
    df <- map_df(rec[main_node], xmlValue)
  } else {
    if (is.null(seconadary_node) && !is.null(rec[[main_node]])) {
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
    url = xmlGetAttr(r, name = "url"),
    generic = xmlGetAttr(r, name = "generic"),
    parent_key = drug_key
  )
}
get_manufactures_df <- function(rec) {
  return (map_df(xmlChildren(rec[["manufacturers"]]),
                 ~ get_manufacturer_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug prices df
get_price_rec <- function(r, drug_key) {
  tibble(
    description = xmlValue(r[["description"]]),
    currency = xmlGetAttr(r[["cost"]], name = "currency"),
    cost = xmlValue(r[["cost"]]),
    unit = xmlValue(r[["unit"]]),
    parent_key = drug_key
  )
}
get_prices_df <- function(rec) {
  return (map_df(xmlChildren(rec[["prices"]]),
                 ~ get_price_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug atc-codes df
get_atc_codes_rec <- function(r, drug_key) {
  tibble(
    atc_code = xmlGetAttr(r, name = "code"),
    level_1 = xmlValue(r[[1]]),
    code_1 = xmlGetAttr(r[[1]], name = "code"),
    level_2 = xmlValue(r[[2]]),
    code_2 = xmlGetAttr(r[[2]], name = "code"),
    level_3 = xmlValue(r[[3]]),
    code_3 = xmlGetAttr(r[[3]], name = "code"),
    level_4 = xmlValue(r[[4]]),
    code_4 = xmlGetAttr(r[[4]], name = "code"),
    parent_key = drug_key
  )
}
get_atc_codes_df <- function(rec) {
  return (map_df(xmlChildren(rec[["atc-codes"]]),
                 ~ get_atc_codes_rec(.x,
                                     xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug pathways df
get_pathway_rec <- function(r, drug_key) {
  tibble(
    smpdb_id = xmlValue(r[["smpdb-id"]]),
    name = xmlValue(r[["name"]]),
    category = xmlValue(r[["category"]]),
    parent_key = drug_key
  )
}

get_pathways_df <- function(rec) {
  return (map_df(xmlChildren(rec[["pathways"]]),
                 ~ get_pathway_rec(.x,
                                   xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug pathways drugs df
get_pathways_drugs_df <- function(rec) {
  return(map_df(xmlChildren(rec[["pathways"]]),
                ~ drug_sub_df(.x, "drugs", id = "smpdb-id")))
}

# Extract drug pathways enzymes df
get_pathways_enzymes_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["pathways"]]),
    ~ drug_sub_df(.x, "enzymes", id = "smpdb-id")
  ))
}

# Extract drug enzymes df
get_enzyme_rec <- function(r, drug_key) {
  tibble(
    id = xmlValue(r[["id"]]),
    name = xmlValue(r[["name"]]),
    organism = xmlValue(r[["organism"]]),
    known_action = xmlValue(r[["known-action"]]),
    inhibition_strength = xmlValue(r[["inhibition-strength"]]),
    induction_strength = xmlValue(r[["induction-strength"]]),
    position = ifelse(is.null(xmlGetAttr(r, name = "position")), NA, xmlGetAttr(r, name = "position")),
    parent_key = drug_key
  )
}

get_enzymes_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]),
                 ~ get_enzyme_rec(.x,
                                  xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug enzymes actions df
get_enzymes_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~ drug_sub_df(.x, "actions", id = "id")))
}

get_targets_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]),
                ~ drug_sub_df(.x, "actions", id = "id")))
}

get_carriers_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]),
                ~ drug_sub_df(.x, "actions", id = "id")))
}

get_transporters_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~ drug_sub_df(.x, "actions", id = "id")))
}


# Extract drug articles df
get_enzymes_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

get_transporters_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

get_carriers_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

get_targets_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

# Extract drug textbooks df

get_enzymes_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}


get_transporters_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_carriers_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_targets_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

# Extract drug links df
get_enzymes_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}


get_transporters_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}


get_carriers_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}


get_targets_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}

# Extract drug polypeptide df
get_polypeptide_rec <- function(r) {
  parent_id = xmlValue(r[["id"]])
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    tibble(
      id = ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id")),
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
      parent_id = parent_id
    )
  }
}

get_enzymes_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]),
                 ~ get_polypeptide_rec(.x)))
}

get_transporters_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_polypeptide_rec(.x)))
}

get_carriers_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]),
                 ~ get_polypeptide_rec(.x)))
}

get_targets_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]),
                 ~ get_polypeptide_rec(.x)))
}

# Extract drug polypeptide_external_identifiers df
get_polypeptide_external_identifiers <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id"))
    polypeptide_external_identifiers <-
      xmlToDataFrame(p[["external-identifiers"]])
    polypeptide_external_identifiers$polypeptide_id <-
      polypeptide_id
    return(polypeptide_external_identifiers)
  }
}

get_enzymes_polypeptide_external_identifiers_df <- function(rec) {
  return (map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ get_polypeptide_external_identifiers(.x)
  ))
}

get_transporters_polypeptide_external_identifiers_df <-
  function(rec) {
    return (map_df(
      xmlChildren(rec[["transporters"]]),
      ~ get_polypeptide_external_identifiers(.x)
    ))
  }

get_carriers_polypeptide_external_identifiers_df <- function(rec) {
  return (map_df(
    xmlChildren(rec[["carriers"]]),
    ~ get_polypeptide_external_identifiers(.x)
  ))
}

get_targets_polypeptide_external_identifiers_df <- function(rec) {
  return (map_df(
    xmlChildren(rec[["targets"]]),
    ~ get_polypeptide_external_identifiers(.x)
  ))
}


# Extract drug polypeptid synonyms df
get_polypeptide_synonyms <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id"))
    polypeptide_synonyms <- p[["synonyms"]]
    if (xmlSize(polypeptide_synonyms) > 0) {
      tibble(synonyms =  paste(xmlApply(polypeptide_synonyms, xmlValue), collapse = ","),
             polypeptide_id = polypeptide_id)
    }
  }
}

get_enzymes_polypeptide_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]),
                 ~ get_polypeptide_synonyms(.x)))
}

get_transporters_polypeptide_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_polypeptide_synonyms(.x)))
}

get_carriers_polypeptide_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]),
                 ~ get_polypeptide_synonyms(.x)))
}

get_targets_polypeptide_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]),
                 ~ get_polypeptide_synonyms(.x)))
}

# Extract drug polypeptide pfams df
get_polypeptide_pfams <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id"))
    firstCell <- xmlValue(xmlChildren(p[["pfams"]])[[1]])
    if (firstCell != "\n    ") {
      polypeptide_pfams <- xmlToDataFrame(xmlChildren(p[["pfams"]]))
      polypeptide_pfams$polypeptide_id <- polypeptide_id
      return(polypeptide_pfams)
    }
  }
}

get_enzymes_polypeptide_pfams_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]),
                 ~ get_polypeptide_pfams(.x)))
}

get_targets_polypeptide_pfams_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]),
                 ~ get_polypeptide_pfams(.x)))
}

get_carriers_polypeptide_pfams_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]),
                 ~ get_polypeptide_pfams(.x)))
}

get_transporters_polypeptide_pfams_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_polypeptide_pfams(.x)))
}


# Extract drug polypeptide go-classifiers df
get_polypeptide_go_classifiers <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA, xmlGetAttr(p, name = "id"))
    firstCell <- xmlValue(xmlChildren(p[["pfams"]])[[1]])
    if (firstCell != "\n    " && 
        !is.null(p[["go-classifiers"]]) && xmlValue(p[["go-classifiers"]]) != "\n    ") {
      polypeptide_go_classifiers <-
        xmlToDataFrame(xmlChildren(p[["go-classifiers"]]))
      polypeptide_go_classifiers$polypeptide_id <- polypeptide_id
      return(polypeptide_go_classifiers)
    }
  }
}

get_enzymes_polypeptide_go_classifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["enzymes"]]),
                 ~ get_polypeptide_go_classifiers(.x)))
}

get_transporters_polypeptide_go_classifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_polypeptide_go_classifiers(.x)))
}

get_carriers_polypeptide_go_classifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]),
                 ~ get_polypeptide_go_classifiers(.x)))
}

get_targets_polypeptide_go_classifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]),
                 ~ get_polypeptide_go_classifiers(.x)))
}

# Extract drug reactions df
get_reactions_rec <- function(r, drug_key) {
  tibble(
    sequence = xmlValue(r[["sequence"]]),
    left_drugbank_id = xmlValue(r[["left-element"]][["drugbank-id"]]),
    left_drugbank_name = xmlValue(r[["left-element"]][["name"]]),
    right_drugbank_id = xmlValue(r[["right-element"]][["drugbank-id"]]),
    right_drugbank_name = xmlValue(r[["right-element"]][["name"]]),
    parent_key = drug_key
  )
}
get_reactions_df <- function(rec) {
  return (map_df(xmlChildren(rec[["reactions"]]),
                 ~ get_reactions_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug reactions enzymes df
get_reactions_enzymes_df <- function(rec) {
  return (map_df(xmlChildren(rec[["reactions"]]),
                 ~ drug_sub_df(.x, "enzymes", id = NULL)))
}


# Extract drug carriers df
get_carrier_rec <- function(r, drug_key) {
  tibble(
    id = xmlValue(r[["id"]]),
    name = xmlValue(r[["name"]]),
    organism = xmlValue(r[["organism"]]),
    known_action = xmlValue(r[["known-action"]]),
    position = ifelse(is.null(xmlGetAttr(r, name = "position")), NA, xmlGetAttr(r, name = "position")),
    parent_key = drug_key
  )
}

get_carriers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["carriers"]]),
                 ~ get_carrier_rec(.x,
                                   xmlValue(rec["drugbank-id"][[1]]))))
}


get_targets_df <- function(rec) {
  return (map_df(xmlChildren(rec[["targets"]]),
                 ~ get_carrier_rec(.x,
                                   xmlValue(rec["drugbank-id"][[1]]))))
}


get_transporters_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_carrier_rec(.x,
                                   xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug classfications df
drug_classfications_df <- function(rec) {
  if (is.null(rec[["classification"]]))
    return()
  a <- xmlToList(rec[["classification"]])
  return(tibble(
    parent_key = xmlValue(rec["drugbank-id"][[1]]),
    classifications = paste(names(a), a, collapse = ";")
  ))
}

get_synonym_rec <- function(rec, parent_key) {
  return(
    tibble(
      parent_key = parent_key,
      synonym = xmlValue(rec),
      language = xmlGetAttr(rec, name = "language"),
      coder = xmlGetAttr(rec, name = "coder")
    )
  )
}

get_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["synonyms"]]),
                 ~ get_synonym_rec(.x,
                                   xmlValue(rec["drugbank-id"][[1]]))))
}

save_drug_sub <-
  function(con,
           df,
           table_name,
           save_table_only = FALSE,
           field.types = NULL,
           primary_key = NULL,
           foreign_key = "parent_key",
           ref_table = "drug(primary_key)") {
    #store drug sub_Table in db
    dbWriteTable(
      conn = con,
      value = df,
      name = table_name,
      field.types = field.types,
      overwrite = TRUE
    )
    if (!save_table_only) {
      # add primary key of drug table
      if (!is.null(primary_key)) {
        for (key in primary_key) {
          dbExecute(
            conn = con,
            statement = paste(
              "Alter table",
              table_name,
              "alter column",
              key,
              "varchar(255) NOT NULL;"
            )
          )
        }
        dbExecute(
          conn = con,
          statement = paste(
            "Alter table",
            table_name,
            "add primary key(",
            paste(primary_key, collapse = ","),
            ");"
          )
        )
        
      }
      # add foreign key of drug table
      if (!is.null(foreign_key)) {
        dbExecute(
          conn = con,
          statement = paste(
            "Alter table",
            table_name,
            "ADD CONSTRAINT",
            paste("FK_", table_name, "_drug", sep = ""),
            paste(
              "FOREIGN KEY (",
              foreign_key,
              ") REFERENCES",
              ref_table,
              ";"
            )
          )
        )
      }
      
    }
  }

# Extract drug sequences df
get_sequence_rec <- function(r, drug_key) {
  tibble(
    sequence = xmlValue(r),
    format = ifelse(is.null(xmlGetAttr(r, name = "format")), NA, xmlGetAttr(r, name = "format")),
    parent_key = drug_key
  )
}

get_sequences_df <- function(rec) {
  if (is.null(rec[["sequences"]]))
    return()
  return (map_df(xmlChildren(rec[["sequences"]]),
                 ~ get_sequence_rec(.x,
                                    xmlValue(rec["drugbank-id"][[1]]))))
}

# read and parse the xml database
get_xml_db_rows <- function (xml_db_name) {
  drugbank_db <- xmlParse(xml_db_name)
  top <- xmlRoot(drugbank_db)
  children <- xmlChildren(top)
  return(children)
}