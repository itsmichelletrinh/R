#Loading data into R
Pinniped1_data <- readDNAStringSet("Pinnipeds_initdata.txt")

#Create MSA
Pinniped1_msa <- msa(Pinniped1_data)
print(Pinniped1_msa, show="complete")

#Format MSA for phangorn
Pinniped1_phyDat <- as.phyDat(Pinniped1_msa)
Pinniped1_phyDat

#Construct distance matrix and NJ tree
dm <- dist.ml(Pinniped1_phyDat, model = "JC69")
treeNJ <- NJ(dm)
plot.phylo(treeNJ, cex = 0.7)

#Create new MSA with new sequences and save MSA
new_data <- readDNAStringSet("new_msa_data.txt")
new_msa <- msa(new_data)
sink(file = "new_msa_output.txt")
print(new_msa, show="complete")
sink()

#Format new MSA
new_msa_phyDat <- as.phyDat(new_msa)

#Maximum likelihood analysis: Model selection
mt <- modelTest(new_msa_phyDat, model=c("JC", "F81", "K80", "HKY", "SYM", "GTR"))
mt
which.min(mt$BIC)

env = attr(mt, "env")
bestmodel <- mt$Model[which.min(mt$BIC)]
fitStart = eval(get(bestmodel, env), env)
fit = optim.pml(fitStart, rearrangement = "stochastic",
                optGamma = TRUE, optInv = TRUE, bestmodel)
fit 

#Maximum likelihood analysis: Bootstrap
bs <- bootstrap.pml(fit, bs=100, optNni=TRUE)
plotBS(midpoint(fit$tree), bs, p = 0.5, type="p", cex = 0.6)
add.scale.bar(y = 10, length = 1, cex = 0.6)
