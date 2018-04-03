InitDataObjects("conc", "stat", FALSE)
mSet<-Read.TextData(mSet, "Replacing_with_your_file_path", "rowu", "disc");
mSet<-SanityCheckData(mSet)
mSet<-ReplaceMin(mSet);
mSet<-Normalization(mSet, "NULL", "LogNorm", "ParetoNorm", "1", ratio=FALSE, ratioNum=20)
mSet<-PlotNormSummary(mSet, "norm_0_", "png", 72, width=NA)
mSet<-PlotSampleNormSummary(mSet, "snorm_0_", "png", 72, width=NA)
mSet<-PCA.Anal(mSet)
mSet<-PlotPCAPairSummary(mSet, "pca_pair_0_", "png", 72, width=NA, 5)
mSet<-PlotPCAScree(mSet, "pca_scree_0_", "png", 72, width=NA, 5)
mSet<-PlotPCA2DScore(mSet, "pca_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)
mSet<-PlotPCALoading(mSet, "pca_loading_0_", "png", 72, width=NA, 1,2,"scatter", 1);
mSet<-PlotPCABiplot(mSet, "pca_biplot_0_", "png", 72, width=NA, 1,2)
mSet<-PlotPCA3DScore(mSet, "pca_score3d_0_", "json", 1,2,3)
mSet<-PlotCmpdView(mSet, "Ce", "png", 72, width=NA)
mSet<-PlotCmpdView(mSet, "Ac", "png", 72, width=NA)
mSet<-PlotCmpdView(mSet, "LPC", "png", 72, width=NA)
mSet<-PlotCmpdView(mSet, "PE", "png", 72, width=NA)
mSet<-PlotCmpdView(mSet, "TG", "png", 72, width=NA)
mSet<-SPLSR.Anal(mSet, 5, 10, "same")
mSet<-PlotSPLSPairSummary(mSet, "spls_pair_0_", "png", 72, width=NA, 5)
mSet<-PlotSPLS2DScore(mSet, "spls_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)
mSet<-PlotSPLS3DScore(mSet, "spls_score3d_0_", "json")
mSet<-PlotSPLSLoading(mSet, "spls_loading_0_", "png", 72, width=NA, 1,"overview");
mSet<-PlotSPLSDA.Classification(mSet, "spls_cv_0_", "Mfold", "png", 72, width=NA)
mSet<-SPLSR.Anal(mSet, 5, 15, "same")
mSet<-PlotSPLSPairSummary(mSet, "spls_pair_1_", "png", 72, width=NA, 5)
mSet<-PlotSPLS2DScore(mSet, "spls_score2d_1_", "png", 72, width=NA, 1,2,0.95,1,0)
mSet<-PlotSPLS3DScore(mSet, "spls_score3d_1_", "json")
mSet<-PlotSPLSLoading(mSet, "spls_loading_1_", "png", 72, width=NA, 1,"overview");
mSet<-PlotSPLSDA.Classification(mSet, "spls_cv_1_", "Mfold", "png", 72, width=NA)
mSet<-PlotSPLSLoading(mSet, "spls_loading_1_", "png", 600, width=NA, 1,"overview");
