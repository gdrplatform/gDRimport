test_that("convert_MAE_to_PSet works as expected", {
  m <- 20
  n <- 10
  rnames <- LETTERS[1:m]
  cnames <- letters[1:n]
  
  # Normal matrix.
  ref_gr_value <-  matrix(runif(m * n), nrow = m, ncol = n, dimnames = list(rnames, cnames))
  se <- SummarizedExperiment::SummarizedExperiment(assays = list(RefGRvalue = ref_gr_value),
                                                   rowData = S4Vectors::DataFrame(rnames),
                                                   colData = S4Vectors::DataFrame(cnames))
  
  mae <- MultiAssayExperiment::MultiAssayExperiment(experiments = list("single-agent" = se))

  pset <- convert_MAE_to_PSet(mae, "my_pset")
  expect_equal(class(pset@treatmentResponse)[1], "TreatmentResponseExperiment")
  expect_equal(sort(rownames(pset@treatmentResponse)), 
               sort(unlist(lapply(MultiAssayExperiment::experiments(mae), rownames), use.names = FALSE)))
  expect_equal(pset@sample$sampleid, cnames)


  se1 <- SummarizedExperiment::SummarizedExperiment(assays = list(RefGRvalue = ref_gr_value[1:10, ]),
                                                   rowData = S4Vectors::DataFrame(rnames)[1:10, , drop = FALSE],
                                                   colData = S4Vectors::DataFrame(cnames))
  
  se2 <- SummarizedExperiment::SummarizedExperiment(assays = list(RefGRvalue = ref_gr_value[11:20, ]),
                                                    rowData = S4Vectors::DataFrame(rnames)[11:20, , drop = FALSE],
                                                    colData = S4Vectors::DataFrame(cnames))
  maeTwoExperiments <- MultiAssayExperiment::MultiAssayExperiment(experiments = list("single-agent" = se1,
                                                                                     "matrix" = se2))
  
  pset_mae2Exp <- convert_MAE_to_PSet(maeTwoExperiments, "my2exp")
  expect_equal(class(pset_mae2Exp@treatmentResponse)[1], "TreatmentResponseExperiment")
  expect_equal(sort(rownames(pset_mae2Exp@treatmentResponse)), 
               sort(unlist(lapply(MultiAssayExperiment::experiments(maeTwoExperiments), rownames), use.names = FALSE)))
  expect_equal(pset_mae2Exp@sample$sampleid, cnames)
  expect_equal(pset_mae2Exp@treatment$treatmentid, 
               c(rownames(assay(maeTwoExperiments, "single-agent")), rownames(assay(maeTwoExperiments, "matrix"))))
})
