library(XML)
library(purrr)
library(tibble)
library(magrittr)
#######Functions#######################

getDrugElement <- function(drug_record, element_index = 2, sub_element = 0, nested = FALSE, onedLists = FALSE) {
  if (sub_element == 0){
    return(drug_record[[element_index]])
  } else if (!nested & !onedLists) {
    return(drug_record[[element_index]][[sub_element]])
  } else if (!onedLists) {
    return(map_df(drug_record[[element_index]][[sub_element]],`[`))
  } else {
    return(as_tibble(drug_record[[element_index]][[sub_element]] %>% map(.x=., ~ifelse(is.null(.x), NA, .x))))
  }
  message ("Could not find: ", names(drug_record[element_index]))
}

getDrugRecord <- function(record) {
  primcary_key <- record %>% extract2("drugbank-id") %>% extract2("text")
  name <- record %>% extract2("name")
}
##########################################

drugbank_db <- xmlParse("drugbank.xml")
drug <- as_tibble()
drug_list <- xmlToList(drugbank_db)
db_size <- length(drug_list) - 1

drug_PK <- map_df(1:db_size, ~drug_list %>% extract2(.x) %>% extract2("drugbank-id") %>% extract2("text"))

drug_SK <- getDrugElement(drug_record, element_index = 2)
drug_TK <- getDrugElement(drug_record, element_index = 3)
drug_name <- getDrugElement(drug_record, element_index = 4)
drug_description <- getDrugElement(drug_record, element_index = 5)
drug_case_number <- getDrugElement(drug_record, element_index = 6)
drug_unii <- getDrugElement(drug_record, element_index = 7)
drug_state <- getDrugElement(drug_record, element_index = 8)
drug_count_of_groups <- length(getDrugElement(drug_record, element_index = 9))

if (drug_count_of_groups > 0) {
  drug_groups <- as_tibble(getDrugElement(drug_record, element_index = 9))
  drug_groups$drug_PK <- drug_PK
}

drug_count_of_articles <- length(getDrugElement(drug_record, element_index = 10, sub_element = 1))
if (drug_count_of_articles > 0) {
  drug_articles <- getDrugElement(drug_record, element_index = 10, sub_element = 1, nested = TRUE)
  drug_articles$drug_PK <- drug_PK
}

drug_count_of_books <- length(getDrugElement(drug_record, element_index = 10, sub_element = 2))
if (drug_count_of_books > 0) {
  drug_books <- getDrugElement(drug_record, element_index = 10, sub_element = 2, nested = TRUE)
  drug_books$drug_PK <- drug_PK
}

drug_count_of_links <- length(getDrugElement(drug_record, element_index = 10, sub_element = 3))
if (drug_count_of_links > 0) {
  drug_links <- getDrugElement(drug_record, element_index = 10, sub_element = 3, nested = TRUE)
  drug_links$drug_PK <- drug_PK
}

drug_synthesis_reference <- getDrugElement(drug_record, element_index = 11)
drug_indication <- getDrugElement(drug_record, element_index = 12)
drug_pharmacodynamics <- getDrugElement(drug_record, element_index = 13)
drug_mechanism_of_action <- getDrugElement(drug_record, element_index = 14)
drug_toxicity <- getDrugElement(drug_record, element_index = 15)
drug_metabolism <- getDrugElement(drug_record, element_index = 16)
drug_absorption <- getDrugElement(drug_record, element_index = 17)
drug_half_life <- getDrugElement(drug_record, element_index = 18)
drug_protein_binding <- getDrugElement(drug_record, element_index = 19)
drug_route_of_elimination <- getDrugElement(drug_record, element_index = 20)
drug_volume_of_distribution <- getDrugElement(drug_record, element_index = 21)
drug_clearance <- getDrugElement(drug_record, element_index = 22)
drug_classification_description <- getDrugElement(drug_record, element_index = 23, sub_element = 1)
drug_direct_parent <- getDrugElement(drug_record, element_index = 23, sub_element = 2)
drug_kingdom <- getDrugElement(drug_record, element_index = 23, sub_element = 3)
drug_superclass <- getDrugElement(drug_record, element_index = 23, sub_element = 4)
drug_class <- getDrugElement(drug_record, element_index = 23, sub_element = 5)
drug_subclass <- getDrugElement(drug_record, element_index = 23, sub_element = 6)
drug_salts <- getDrugElement(drug_record, element_index = 22)

drug_count_of_synonyms <- length(getDrugElement(drug_record, element_index = 25, sub_element = 1))
if (drug_count_of_synonyms > 0) {
  drug_synonyms <- as_tibble(getDrugElement(drug_record, element_index = 25, sub_element = 1))
  drug_synonyms$drug_PK <- drug_PK
}

drug_count_of_products <- length(getDrugElement(drug_record, element_index = 26))
if (drug_count_of_products > 0) {
  drug_product <- map_df(1:drug_count_of_products, ~getDrugElement(drug_record,
                                                                   element_index = 26,
                                                                   sub_element = .x,
                                                                   onedLists = TRUE))
  drug_product$drug_PK <- drug_PK
}

drug_international_brand <- getDrugElement(drug_record, element_index = 27)

drug_count_of_mixtures <- length(getDrugElement(drug_record, element_index = 28))
if (drug_count_of_mixtures > 0) {
  drug_mixture <- map_df(1:drug_count_of_mixtures, ~getDrugElement(drug_record, element_index = 28,
                                                                 sub_element = .x, onedLists = TRUE))
  drug_mixture$drug_PK <- drug_PK
}


drug_count_of_packagers <- length(getDrugElement(drug_record, element_index = 29))
if (drug_count_of_packagers > 0) {
  drug_package <- map_df(1:drug_count_of_packagers, ~getDrugElement(drug_record, element_index = 29,
                                                                    sub_element = .x, onedLists = TRUE))
  drug_package$drug_PK <- drug_PK
}

drug_count_of_manufacturers <- length(getDrugElement(drug_record, element_index = 30))
if (drug_count_of_manufacturers > 0) {
  drug_manufacturer <- map_df(1:drug_count_of_manufacturers, ~flatten(getDrugElement(drug_record, element_index = 30,
                                                                    sub_element = .x)))
  drug_manufacturer$drug_PK <- drug_PK
}

drug_count_of_pricess <- length(getDrugElement(drug_record, element_index = 31))
if (drug_count_of_pricess > 0) {
  drug_price <- map_df(1:drug_count_of_pricess, ~flatten(getDrugElement(drug_record, element_index = 31,
                                                                                     sub_element = .x)))
  drug_price$drug_PK <- drug_PK
}

drug_count_of_categories <- length(getDrugElement(drug_record, element_index = 32))
if (drug_count_of_categories > 0) {
  drug_category <- map_df(1:drug_count_of_categories, ~getDrugElement(drug_record, element_index = 32,
                                                                    sub_element = .x, onedLists = TRUE))
  drug_category$drug_PK <- drug_PK
}

drug_count_of_affected_organisms <- length(getDrugElement(drug_record, element_index = 33))
if (drug_count_of_affected_organisms > 0) {
  drug_affected_organisms <- tibble(name = map_chr(1:drug_count_of_affected_organisms, ~getDrugElement(drug_record,
                                                                                        element_index = 33,
                                                                      sub_element = .x)))
  drug_affected_organisms$drug_PK <- drug_PK
}

drug_count_of_dosages <- length(getDrugElement(drug_record, element_index = 34))
if (drug_count_of_dosages > 0) {
  drug_dosage <- map_df(1:drug_count_of_dosages, ~getDrugElement(drug_record, element_index = 34,
                                                                    sub_element = .x, onedLists = TRUE))
  drug_dosage$drug_PK <- drug_PK
}

drug_count_of_atc_codes <- length(flatten(getDrugElement(drug_record, element_index = 35)))
if (drug_count_of_atc_codes > 0) {
  drug_atc_code <- map_df(1:(drug_count_of_atc_codes-1), ~drug_record[[35]][[1]][[.x]])
  colnames(drug_atc_code) <- c("name", "code")
  drug_atc_code<- add_row(drug_atc_code, code = drug_record[[35]][[1]]$.attrs)
  drug_atc_code$drug_PK <- drug_PK
}

drug_ahfs_codes <- getDrugElement(drug_record, element_index = 36)
drug_pdb_entries <- getDrugElement(drug_record, element_index = 37)
drug_fda_label <- getDrugElement(drug_record, element_index = 38)
drug_msds <- getDrugElement(drug_record, element_index = 39)

drug_count_of_patents <- length(getDrugElement(drug_record, element_index = 40))
if (drug_count_of_patents > 0) {
  drug_patent <- map_df(1:drug_count_of_patents, ~getDrugElement(drug_record, element_index = 40,
                                                                      sub_element = .x))
  drug_patent$drug_PK <- drug_PK
}

drug_food_interactions <- getDrugElement(drug_record, element_index = 41)

drug_count_of_interactions <- length(getDrugElement(drug_record, element_index = 42))
if (drug_count_of_interactions > 0) {
  drug_interactions <- map_df(1:drug_count_of_interactions, ~getDrugElement(drug_record, element_index = 42,
                                                                 sub_element = .x))
  drug_interactions$drug_PK <- drug_PK
}

drug_count_of_sequences <- length(getDrugElement(drug_record, element_index = 43))
if (drug_count_of_sequences > 0) {
  drug_sequence <- map_df(1:drug_count_of_sequences, ~flatten(getDrugElement(drug_record, element_index = 43,
                                                                                     sub_element = .x)))
  drug_sequence$drug_PK <- drug_PK
}

drug_count_of_experimental_properties <- length(getDrugElement(drug_record, element_index = 44))
if (drug_count_of_experimental_properties > 0) {
  drug_experimental_properties <- map_df(1:drug_count_of_experimental_properties,
                                         ~getDrugElement(drug_record,
                                                         element_index = 44,
                                                         sub_element = .x, onedLists = TRUE))
  drug_experimental_properties$drug_PK <- drug_PK
}

drug_count_of_external_identifiers <- length(getDrugElement(drug_record, element_index = 45))
if (drug_count_of_external_identifiers > 0) {
  drug_external_identifiers <- map_df(1:drug_count_of_external_identifiers,
                                         ~getDrugElement(drug_record,
                                                         element_index = 45,
                                                         sub_element = .x, onedLists = TRUE))
  drug_external_identifiers$drug_PK <- drug_PK
}

drug_count_of_external_links <- length(getDrugElement(drug_record, element_index = 46))
if (drug_count_of_external_links > 0) {
  drug_external_links <- map_df(1:drug_count_of_external_links,
                                      ~getDrugElement(drug_record,
                                                      element_index = 46,
                                                      sub_element = .x, onedLists = TRUE))
  drug_external_links$drug_PK <- drug_PK
}

drug_count_of_pathways <- length(drug_record[[47]])
if (drug_count_of_pathways > 0) {
  drug_pathways <- tibble()
  pathway_drugs <- tibble()
  pathway_enzymes <- tibble()
  
  getPathway_drugs<- function(index) {
    pathway_drugs <- map_df(drug_record[[47]][[index]]$drugs, as_tibble)
    pathway_drugs$smpdb_id <- drug_record[[47]][[index]]$`smpdb-id`
    pathway_drugs
  }
  current_drug_pathways_drugs <- map_df(1:drug_count_of_pathways, ~getPathway_drugs(.x))
  pathway_drugs <- rbind(pathway_drugs, 
                         current_drug_pathways_drugs)
  
  getPathway_enzymes<- function(index) {
    pathway_enzymes <- map_df(drug_record[[47]][[index]]$enzymes, as_tibble)
    pathway_enzymes$smpdb_id <- drug_record[[47]][[index]]$`smpdb-id`
    pathway_enzymes
    
  }
  
  current_drug_pathways_enzymes <- map_df(1:drug_count_of_pathways, ~getPathway_enzymes(.x))
  pathway_enzymes <- rbind(pathway_enzymes, 
                         current_drug_pathways_enzymes)
  
  getPathway<- function(index, pathway_drugs_count,  pathway_enzymes_count) {
    current_drug_pathways <- as_tibble(drug_record[[47]][[index]][1:3])
    current_drug_pathways$drugs_count <- pathway_drugs_count
    current_drug_pathways$enzymes_count <- pathway_enzymes_count
    current_drug_pathways$drug_PK <- drug_PK
    current_drug_pathways
  }
  current_drug_pathways <- map_df(1:drug_count_of_pathways,
                                  ~getPathway(.x, pathway_drugs_count = nrow(current_drug_pathways_drugs),
                                              pathway_enzymes_count = nrow(current_drug_pathways_enzymes)))
  drug_pathways <- rbind(drug_pathways, current_drug_pathways)
}

drug_reactions <- getDrugElement(drug_record, element_index = 48)
drug_snp_effects <- getDrugElement(drug_record, element_index = 49)
drug_snp_adverse_drug_reactions <- getDrugElement(drug_record, element_index = 50)

drug_count_of_targets <- length(drug_record[[51]])
if(drug_count_of_targets > 0) {
  target_actions <- tibble()
  target_articles <- tibble()
  target_books <- tibble()
  target_links <- tibble()
  target_polypeptide <- tibble()
  target_polypeptide_organism <- tibble()
  target_polypeptide_external_identifiers <- tibble()
  target_polypeptide_synonyms <- tibble()
  target_polypeptide_amino_acide_sequence <- tibble()
  target_polypeptide_gene_sequence <- tibble()
  target_polypeptide_pfams <- tibble()
  target_polypeptide_go_classifiers <- tibble()
  
  target <- as_tibble(drug_record[[51]][[1]][1:3])
  target_polypeptide <- as_tibble(flatten(drug_record[[51]][[1]]$polypeptide[c(1:11,19)]))
  target_polypeptide_count <- nrow(target_polypeptide)
  target$polypeptide_count <- target_polypeptide_count
  
  getTarget_actions<- function(index) {
    target_actions <- map_df(drug_record[[51]][[index]]$actions, as_tibble)
    target_actions$target_id <- drug_record[[51]][[index]]$id
    target_actions
  }
  current_target_actions <- map_df(1:drug_count_of_targets, ~getTarget_actions(.x))
  target$actions_count <- nrow(current_target_actions)
  target_actions <- rbind(target_actions, 
                         current_target_actions)
  
  getTarget_articles<- function(index) {
    target_articles <- map_df(drug_record[[51]][[index]]$references$articles, as_tibble)
    if (nrow(target_articles) > 0) {
    target_articles$target_id <- drug_record[[51]][[index]]$id
    target_articles
    }
  }
  current_target_articles <- map_df(1:drug_count_of_targets, ~getTarget_articles(.x))
  target$articles_count <- nrow(current_target_articles) 
  if (nrow(current_target_articles) > 0) {
  target_articles <- rbind(target_articles, 
                          current_target_articles)
  }
  
  getTarget_books<- function(index) {
    target_books <- map_df(drug_record[[51]][[index]]$references$textbooks, as_tibble)
    if (nrow(target_books) > 0) {
      target_books$target_id <- drug_record[[51]][[index]]$id
      target_books
    }
    
  }
  current_target_books <- map_df(1:drug_count_of_targets, ~getTarget_books(.x))
  target$books_count <- nrow(current_target_books)
  if (nrow(current_target_books) != 0) {
    target_books <- rbind(target_books, 
                          current_target_books) 
  }
  
  getTarget_links<- function(index) {
    target_links <- map_df(drug_record[[51]][[index]]$references$links, as_tibble)
    if (nrow(target_links) > 0) {
      target_links$target_id <- drug_record[[51]][[index]]$id
      target_links
    }
    
  }
  current_target_links <- map_df(1:drug_count_of_targets, ~getTarget_links(.x))
  target$links_count <- nrow(current_target_links) 
  if (nrow(current_target_links) != 0) {
    target_links <- rbind(target_links, 
                          current_target_links) 
  }
  
  getTarget_polypeptide_organism<- function(index) {
    target_polypeptide_organism <- as_tibble(flatten(drug_record[[51]][[index]]$polypeptide$organism))
    if (nrow(target_polypeptide_organism) > 0) {
      target_polypeptide_organism$polypeptide_id <- target_polypeptide$id
      target_polypeptide_organism
    }
    
  }
  current_target_polypeptide_organism <- map_df(1:drug_count_of_pathways, ~getTarget_polypeptide_organism(.x))
  norganism <- nrow(current_target_polypeptide_organism)
  if (norganism != 0) {
    target_polypeptide$count_of_organism <- norganism
    target_polypeptide_organism <- rbind(target_polypeptide_organism, 
                          current_target_polypeptide_organism) 
  }
  
  getTarget_polypeptide_external_identifiers<- function(index) {
    target_polypeptide_external_identifiers <- map_df(drug_record[[51]][[index]]$
                                                                   polypeptide$`external-identifiers`, as_tibble)
    if (nrow(target_polypeptide_external_identifiers) > 0) {
      target_polypeptide_external_identifiers$polypeptide_id <- target_polypeptide$id
      target_polypeptide_external_identifiers
    }
    
  }
  current_target_polypeptide_external_identifiers <- map_df(1:drug_count_of_pathways,
                                                            ~getTarget_polypeptide_external_identifiers(.x))
  nexternal_identifiers <- nrow(current_target_polypeptide_external_identifiers)
  if (nexternal_identifiers > 0) {
    target_polypeptide$count_of_external_identifiers <- nexternal_identifiers
    target_polypeptide_external_identifiers <- rbind(target_polypeptide_external_identifiers, 
                                         current_target_polypeptide_external_identifiers) 
  }
  
  getTarget_polypeptide_synonyms<- function(index) {
    target_polypeptide_synonyms <- map_df(drug_record[[51]][[index]]$
                                                        polypeptide$synonyms, as_tibble)
    if (nrow(target_polypeptide_synonyms) > 0) {
      target_polypeptide_synonyms$polypeptide_id <- target_polypeptide$id
      target_polypeptide_synonyms
    }
    
  }
  current_target_polypeptide_synonyms <- map_df(1:drug_count_of_pathways,
                                                            ~getTarget_polypeptide_synonyms(.x))
  n_synonyms <- nrow(current_target_polypeptide_synonyms)
  if (n_synonyms > 0) {
    target_polypeptide$count_of_synonyms <- n_synonyms
    target_polypeptide_synonyms <- rbind(target_polypeptide_synonyms, 
                                                     current_target_polypeptide_synonyms) 
  }
  
  getTarget_polypeptide_amino_acid_sequence<- function(index) {
    target_polypeptide_amino_acid_sequence <- as_tibble(flatten(drug_record[[51]][[index]]
                                                                $polypeptide$`amino-acid-sequence`))
    if (nrow(target_polypeptide_amino_acid_sequence) > 0) {
      target_polypeptide_amino_acid_sequence$polypeptide_id <- target_polypeptide$id
      target_polypeptide_amino_acid_sequence
    }
    
  }
  current_target_polypeptide_amino_acid_sequence <- map_df(1:drug_count_of_pathways,
                                                ~getTarget_polypeptide_amino_acid_sequence(.x))
  n_amino_acid_sequence <- nrow(current_target_polypeptide_amino_acid_sequence)
  if (n_amino_acid_sequence > 0) {
    target_polypeptide$count_of_amino_acid_sequence <- n_amino_acid_sequence
    target_polypeptide_amino_acide_sequence <- rbind(target_polypeptide_amino_acide_sequence, 
                                         current_target_polypeptide_amino_acid_sequence) 
  }
  
  
  getTarget_polypeptide_gene_sequence<- function(index) {
    target_polypeptide_gene_sequence <- as_tibble(flatten(drug_record[[51]][[index]]
                                                                $polypeptide$`gene-sequence`))
    if (nrow(target_polypeptide_gene_sequence) > 0) {
      target_polypeptide_gene_sequence$polypeptide_id <- target_polypeptide$id
      target_polypeptide_gene_sequence
    }
    
  }
  current_target_polypeptide_gene_sequence <- map_df(1:drug_count_of_pathways,
                                                           ~getTarget_polypeptide_gene_sequence(.x))
  n_gene_sequence <- nrow(current_target_polypeptide_gene_sequence)
  if (n_gene_sequence > 0) {
    target_polypeptide$count_of_gene_sequence <- n_gene_sequence
    target_polypeptide_gene_sequence <- rbind(target_polypeptide_gene_sequence, 
                                                     current_target_polypeptide_gene_sequence) 
  }
  
  
  getTarget_polypeptide_pfams<- function(index) {
    target_polypeptide_pfams <- map_df(drug_record[[51]][[index]]$polypeptide$pfams, as_tibble)
    if (nrow(target_polypeptide_pfams) > 0) {
      target_polypeptide_pfams$polypeptide_id <- target_polypeptide$id
      target_polypeptide_pfams
    }
    
  }
  current_target_polypeptide_pfams <- map_df(1:drug_count_of_pathways,
                                                     ~getTarget_polypeptide_pfams(.x))
  n_pfams <- nrow(current_target_polypeptide_pfams)
  if (n_pfams > 0) {
    target_polypeptide$count_of_pfams <- n_pfams
    target_polypeptide_pfams <- rbind(target_polypeptide_pfams, 
                                              current_target_polypeptide_pfams) 
  }
  
  getTarget_polypeptide_go_classifiers<- function(index) {
    target_polypeptide_go_classifiers <- map_df(drug_record[[51]][[index]]$polypeptide$`go-classifiers`, as_tibble)
    if (nrow(target_polypeptide_go_classifiers) > 0) {
      target_polypeptide_go_classifiers$polypeptide_id <- target_polypeptide$id
      target_polypeptide_go_classifiers
    }
    
  }
  current_target_polypeptide_go_classifiers <- map_df(1:drug_count_of_pathways,
                                             ~getTarget_polypeptide_go_classifiers(.x))
  n_go_classifiers <- nrow(current_target_polypeptide_go_classifiers)
  if (n_pfams > 0) {
    target_polypeptide$count_of_go_classifiers <- n_go_classifiers
    target_polypeptide_go_classifiers <- rbind(target_polypeptide_go_classifiers, 
                                      current_target_polypeptide_go_classifiers) 
  }
  target_polypeptide$target_id <- target$id
  target$drug_PK <- drug_PK
}

drug_enzymes <- getDrugElement(drug_record, element_index = 52)
drug_carriers <- getDrugElement(drug_record, element_index = 53)
drug_transporters <- getDrugElement(drug_record, element_index = 54)
drug_type <- drug_record[['.attrs']][[1]]
drug_created <- drug_record[['.attrs']][[2]]
drug_updated <- drug_record[['.attrs']][[3]]
