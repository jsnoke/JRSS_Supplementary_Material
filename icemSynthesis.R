library(synthpop)

#---------------- data synthesis (or loading) -------------------
load("~/Documents/SyntheticDataChallenge/challenge.Rdata")

subDF = na.omit(challenge[challenge$parish == "EDINBURGH" & is.element(challenge$ctry_bth, c("ENG", "IRL", "DEU",
                                                                                             "ITA", "RUS", "SCT",
                                                                                             "WAL")), c(4:7, 11:13, 15:22)])

sampSize = nrow(subDF) / 1
sampDF = subDF[sample(sampSize), ]

sampDF[, sapply(sampDF, is.factor)] = droplevels(sampDF[, sapply(sampDF, is.factor)])

## create variable from inactive and employ
sampDF$work_status = factor("worker", levels = c("worker", "employer", "neither_independent", "neither_undefined"))
sampDF$work_status[sampDF$employ == "E"] = "employer"
sampDF$work_status[sampDF$inactive == "oth incl scholar" | sampDF$inactive == "own means" | 
                       sampDF$inactive == "pension" | sampDF$inactive == "retired"] = "neither_independent"
sampDF$work_status[sampDF$inactive == "working" & sampDF$employ == " "] = "neither_undefined" 
sampDF$work_status[sampDF$inactive == "formerly occ" | sampDF$inactive == "pauper" | sampDF$inactive == "unemployed"] = 
    "neither_undefined"
sampDF = sampDF[, -c(5, 6)]

sampDF$recodeDisability = factor("disabled", levels = c("none", "disabled"))
sampDF$recodeDisability[sampDF$disability == "None"] = "none"
sampDF = sampDF[, -4]
sampDF$disability = sampDF$recodeDisability
sampDF = sampDF[, -14]

sampDF$n_relations = sampDF$nlodgers + sampDF$nboarders
sampDF$n_lodgers = sampDF$nservants + sampDF$nvisitors + sampDF$nkn
sampDF$n_others = sampDF$nfamgteq15 + sampDF$nfamlt15
sampDF = sampDF[, -c(5:11)]

logitForm = as.formula(paste("Y ~ .^", 2))
nObs = nrow(sampDF) * 2
rm(challenge)
rm(subDF)

timeIn = proc.time()
cartSynDF = syn(sampDF, m = 15, visit.sequence = c(1:2, 5, 8:10, 6:7, 3:4), 
                method = c(rep("", 2), rep("cart", 8)))
parmSynDF = syn(sampDF, m = 15, visit.sequence = c(1:2, 5, 8:10, 6:7, 3:4), 
                method = c(rep("", 2), rep("polyreg", 2), "normrank", "polyreg", "logreg", rep("normrank", 3)))
normSynDF = syn(sampDF, m = 15, visit.sequence = c(1:2, 5, 8:10, 6:7, 3:4), 
                method = c(rep("", 2), rep("polyreg", 2), "norm", "polyreg", "logreg", rep("norm", 3)))
timeOut = proc.time() - timeIn
cat("synth", timeOut, "\n")
synList = list("cartSynDF" = cartSynDF, "parmSynDF" = parmSynDF, "normSynDF" = normSynDF)
save(synList, file = "synList_032017.RData")
rm(synList)

