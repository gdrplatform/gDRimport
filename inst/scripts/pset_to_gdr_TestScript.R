## 

debugonce(gDRcore::create_SE)
# collaboration with BHKLab
# co-author: Jermiah Joseph
library(PharmacoGx)
library(gDR)
library(gDRcore)
library(gDRimport)
pset <- .getPSet("CCLE_2015")
pset2 <- .getPSet("FIMM_2016")
pset3 <- .getPSet("NCI_60")
pset4 <- .getPSet("PRISM_2020")

df_ <- gDRimport::convert_pset_to_df(pharmacoset = pset,
                        run_parallel=TRUE,
                        workers = 4L)



# example subset to speed things up. Specific to CCLE Pset
(dt <- data.table::as.data.table(df_))
x <- dt[Clid=="1321N1"]
x
# x$Barcode |> unique() |> length()
# x[,1:7] |> is.na() |> any()

#############################
# RUN DRUG RESPONSE PROCESSING PIPELINE
se <- gDRcore::runDrugResponseProcessingPipeline(as.data.frame(x))
gDRutils::convert_se_assay_to_dt(se[[1]],"RawTreated")
gDRutils::convert_se_assay_to_dt(se[[1]],"Controls")

##########################################################
# RUNNING EACH STEP SEPARATELY IF NEEDED. 
##########################################################

#############################
# CREATE SUMMARIZED EXPERIMENT
se2 <- gDRcore::create_SE(as.data.frame(x),  data_type = "single-agent")
gDRutils::convert_se_assay_to_dt(se2, "RawTreated")
gDRutils::convert_se_assay_to_dt(se2, "Controls")

#############################
# NORMALIZE
se2_norm <- gDRcore::normalize_SE(se = se2, data_type = "single-agent")
gDRutils::convert_se_assay_to_dt(se2_norm, "Normalized")

#############################
# AVERAGE
se2_avg <- gDRcore::average_SE(se2_norm, data_type = "single-agent")
gDRutils::convert_se_assay_to_dt(se2_avg, "Averaged")

#############################
# FIT
se2_fit <- gDRcore::fit_SE(se2_avg, data_type = "single-agent")
gDRutils::convert_se_assay_to_dt(se2_fit, "Metrics")
