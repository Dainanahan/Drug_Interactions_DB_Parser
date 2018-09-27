source("functions_definitions.R")

init <- function(xml_db_name, driver, server, 
                 output_database, trusted_connection = TRUE) {
  #db connection
  con <<- dbConnect(odbc::odbc(), Driver = driver, Server = server, 
                   Database = output_database, Trusted_Connection = trusted_connection)
  children <<- get_xml_db_rows(xml_db_name)
  
}

parse_drug<- function(){
  #db connection
  drugs <- map_df(children, ~drug_df(.x))
  save_drug_sub(con = con, df = drugs, table_name = "drug", primary_key = "primary_key", foreign_key = NULL,
                field.types = list(description = "varchar(6349)", mechanism_of_action = "varchar(7189)",
                                   pharmacodynamics = "varchar(3179)", indication = "varchar(3165)",
                                   absorption = "nvarchar(3579)", route_of_elimination = "varchar(1324)",
                                   metabolism = "varchar(2926)", international_brands = "varchar(2904)",
                                   protein_binding = "varchar(778)", synthesis_reference="varchar(946)",
                                   clearance = "varchar(2128)", half_life = "varchar(1173)",
                                   route_of_elimination = "varchar(1324)", absorption = "varchar(3579)",
                                   volume_of_distribution = "varchar(1378)",
                                   toxicity = "varchar(max)", created = "date", updated = "date"))
  return(drugs)
}


parse_drug_groups <- function() {
  drug_groups <- map_df(children, ~drug_sub_df(.x, "groups"))
  save_drug_sub(con = con, df = drug_groups, table_name = "drug_groups")
  return(drug_groups)
}

parse_drug_articles <- function() {
  drug_articles <- map_df(children, ~drug_sub_df(.x, "general-references", seconadary_node = "articles"))
  save_drug_sub(con = con, df = drug_articles, table_name = "drug_articles")
  return(drug_articles)
}

parse_drug_books <- function() {
  drug_books <- map_df(children, ~drug_sub_df(.x, "general-references", seconadary_node = "textbooks"))
  save_drug_sub(con = con, df = drug_books, table_name = "drug_books")
  return(drug_books)
}

parse_drug_links <- function() {
  drug_links <- map_df(children, ~drug_sub_df(.x, "general-references", seconadary_node = "links"))
  save_drug_sub(con = con, df = drug_links, table_name = "drug_links")
  return(drug_links)
}


parse_drug_classfications <- function() {
  drug_classfications <- map_df(children, ~drug_classfications_df(.x))
  save_drug_sub(con = con, df = drug_classfications, table_name = "drug_classfications")
  return(drug_classfications)
}

parse_drug_synonyms <- function() {
  drug_synonyms <- map_df(children, ~get_synonyms_df(.x))
  save_drug_sub(con = con, df = drug_synonyms, table_name = "drug_synonyms",
                field.types = list(synonym = "varchar(534)"))
  return(drug_synonyms)
}

parse_drug_products <- function() {
  drug_products <- map_df(children, ~drug_sub_df(.x, "products"))
  save_drug_sub(con = con, df = drug_products, table_name = "drug_products")
  return(drug_products)
}

init(xml_db_name =  "drugbank.xml", driver = "SQL Server",
     server = "MOHAMMED\\SQL2016", output_database = "drugbank2")

parse_drug()
parse_drug_groups()
parse_drug_articles()
parse_drug_books()
parse_drug_links()
parse_drug_classfications()
parse_drug_synonyms()
parse_drug_products()

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
save_drug_sub(drug_targets_polypeptide_external_identifiers, "drug_targets_polypeptides_external_identifiers",
              save_table_only = TRUE)
save_drug_sub(drug_carriers_polypeptide_external_identifiers, "drug_carriers_polypeptides_external_identifiers",
              save_table_only = TRUE)
save_drug_sub(drug_transporters_polypeptide_external_identifiers, "drug_transporters_polypeptides_external_identifiers",
              save_table_only = TRUE)
save_drug_sub(drug_enzymes_polypeptide_synonyms, "drug_enzymes_polypeptides_synonyms", save_table_only = TRUE)
save_drug_sub(drug_targets_polypeptide_synonyms, "drug_targets_polypeptides_synonyms", save_table_only = TRUE)
save_drug_sub(drug_carriers_polypeptide_synonyms, "drug_carriers_polypeptides_synonyms", save_table_only = TRUE)
save_drug_sub(drug_transporters_polypeptide_synonyms, "drug_transporters_polypeptides_synonyms", 
              save_table_only = TRUE)

save_drug_sub(drug_enzymes_polypeptide_pfams, "drug_enzymes_polypeptides_pfams", save_table_only = TRUE)
save_drug_sub(drug_targets_polypeptide_pfams, "drug_targets_polypeptides_pfams", save_table_only = TRUE)
save_drug_sub(drug_carriers_polypeptide_pfams, "drug_carriers_polypeptides_pfams", save_table_only = TRUE)
save_drug_sub(drug_transporters_polypeptide_pfams, "drug_transporters_polypeptides_pfams", save_table_only = TRUE)

save_drug_sub(drug_enzymes_polypeptide_go_classifiers, "drug_enzymes_polypeptides_go_classifiers",
              save_table_only = TRUE)
save_drug_sub(drug_targets_polypeptide_go_classifiers, "drug_enzymes_polypeptides_go_classifiers",
              save_table_only = TRUE)
save_drug_sub(drug_carriers_polypeptide_go_classifiers, "drug_enzymes_polypeptides_go_classifiers",
              save_table_only = TRUE)
save_drug_sub(drug_transporters_polypeptide_go_classifiers, "drug_enzymes_polypeptides_go_classifiers",
              save_table_only = TRUE)
save_drug_sub(drug_reactions, "drug_reactions")
save_drug_sub(drug_reactions_enzymes, "drug_reactions_enzymes")
save_drug_sub(drug_snp_effects, "drug_snp_effects")
save_drug_sub(drug_snp_adverse_drug_reactions, "drug_snp_adverse_drug_reactions")
save_drug_sub(drug_carriers, "drug_carriers", primary_key = c("id", "drug_key"))
save_drug_sub(drug_transporters, "drug_transporters", primary_key = c("id", "drug_key"))
save_drug_sub(drug_targets, "drug_targets", primary_key = c("id", "drug_key"))
save_drug_sub(drug_carriers_actions, "drug_carriers_actions", save_table_only = TRUE)
save_drug_sub(drug_transporters_actions, "drug_transporter_actions", save_table_only = TRUE)
save_drug_sub(drug_targets_actions, "drug_targets_actions", save_table_only = TRUE)
save_drug_sub(drug_targets_articles, "drug_targets_articles", save_table_only = TRUE)
save_drug_sub(drug_carriers_articles, "drug_carriers_articles", save_table_only = TRUE)
save_drug_sub(drug_transporters_articles, "drug_transporters_articles", save_table_only = TRUE)
save_drug_sub(drug_targets_textbooks, "drug_targets_textbooks", save_table_only = TRUE)
save_drug_sub(drug_carriers_textbooks, "drug_carriers_textbooks", save_table_only = TRUE)
save_drug_sub(drug_transporters_textbooks, "drug_transporters_textbooks", save_table_only = TRUE)
save_drug_sub(drug_targets_links, "drug_targets_links", save_table_only = TRUE)
save_drug_sub(drug_carriers_links, "drug_carriers_links", save_table_only = TRUE)
save_drug_sub(drug_transporters_links, "drug_transporters_links", save_table_only = TRUE)
save_drug_sub(drug_targets_polypeptides, "drug_targets_polypeptides", save_table_only = TRUE,
              field.types = list(general_function =
                                   paste("varchar(",
                                         max(nchar(drug_targets_polypeptides$general_function)), ")", sep = ""),
                                 specific_function =
                                   paste("varchar(max)", sep = ""),
                                 amino_acid_sequence =
                                   paste("varchar(max)", sep = ""),
                                 gene_sequence =
                                   paste("varchar(max)", sep = "")))
save_drug_sub(drug_carriers_polypeptides, "drug_carriers_polypeptides", save_table_only = TRUE,
              field.types = list(general_function =
                                   paste("varchar(",
                                         max(nchar(drug_carriers_polypeptides$general_function)), ")", sep = ""),
                                 specific_function =
                                   paste("varchar(",
                                         max(nchar(drug_carriers_polypeptides$specific_function)), ")", sep = ""),
                                 amino_acid_sequence =
                                   paste("varchar(",
                                         max(nchar(drug_carriers_polypeptides$amino_acid_sequence)), ")", sep = ""),
                                 gene_sequence =
                                   paste("varchar(max)", sep = "")))
save_drug_sub(drug_transporters_polypeptides, "drug_transporters_polypeptides", save_table_only = TRUE,
              field.types = list(general_function =
                                   paste("varchar(",
                                         max(nchar(drug_transporters_polypeptides$general_function)), ")", sep = ""),
                                 specific_function =
                                   paste("varchar(",
                                         max(nchar(drug_transporters_polypeptides$specific_function)), ")", sep = ""),
                                 amino_acid_sequence =
                                   paste("varchar(",
                                         max(nchar(drug_transporters_polypeptides$amino_acid_sequence)), ")", sep = ""),
                                 gene_sequence =
                                   paste("varchar(max)", sep = "")))
# disconnect db
dbDisconnect(conn = con)