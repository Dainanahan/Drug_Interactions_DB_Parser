library(XML)
library(purrr)
library(tibble)
library(magrittr)
#library(RODBC)
library(DBI)
# drugbank_db <- xmlParse("drugbank_record.xml")
# xmlToDataFrame(nodes=getNodeSet(drugbank_db,"/drug/drugbank-id[1]"))
drugbank_db <- xmlParse("drugbank.xml")
top <- xmlRoot(drugbank_db)
# xmlName(top)
# names(top)
# names(top[[1]])
# xmlValue(top[[1]]["drugbank-id"][[3]])
# xmlValue(top[[1]][["name"]])
# xmlAttrs(top[[1]])[["type"]]
# drug_record <- top[[1]]
# xpathApply(top, "/drugbank/drug/name", xmlValue)
# xmlToDataFrame(nodes=getNodeSet(drug_record,"/drugbank-id[1]"))
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
    articles_count = xmlSize(top[[1]][["general-references"]][["articles"]]),
    books_count = xmlSize(top[[1]][["general-references"]][["textbooks"]]),
    links_count = xmlSize(top[[1]][["general-references"]][["links"]]),
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
#max(nchar(a$absorption))
a <- map_df(xmlChildren(top), ~drug_df(.x))

con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "MOHAMMED\\SQL2016", 
                Database = "drugbank", Trusted_Connection = "True")
columnTypes <- list(description = "varchar(6349)", mechanism_of_action = "varchar(7189)",
                    pharmacodynamics = "varchar(3179)", indication = "varchar(3165)",
                    absorption = "nvarchar(3579)", route_of_elimination = "varchar(1324)",
                    metabolism = "varchar(2926)", international_brands = "varchar(2904)",
                    protein_binding = "varchar(778)", synthesis_reference="varchar(946)",
                    clearance = "varchar(2128)", half_life = "varchar(1173)",
                    route_of_elimination = "varchar(1324)", absorption = "varchar(3579)",
                    volume_of_distribution = "varchar(1378)",
                    toxicity = "varchar(max)", created = "date", updated = "date")

dbWriteTable(conn = con, value = a, name = "drug", field.types = columnTypes)
dbExecute(conn = con, statement = "Alter table drug
alter column primary_key varchar(255) NOT NULL;")
dbExecute(conn = con, statement = "Alter table drug add primary key (primary_key);")

dbDisconnect(conn = con)