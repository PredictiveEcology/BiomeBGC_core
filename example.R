Require::Require(c("reproducible", "SpaDES.core"), 
                 repos = c("https://predictiveecology.r-universe.dev", getOption("repos")))

out <- simInitAndSpades(modules = "BiomeBGC_core",
                        paths = list(modulePath = "~/repos/",
                                     outputPath = "~/outputs",
                                     inputPath = "~/repos/BiomeBGCR/inst/inputs"),
                        objects = list(
                          bbgcSpinup.ini = c("~/repos/BiomeBGCR/inst/inputs/ini/spinup_b.ini",
                                             "~/repos/BiomeBGCR/inst/inputs/ini/spinup_bc.ini",
                                             "~/repos/BiomeBGCR/inst/inputs/ini/spinup_g.ini",
                                             "~/repos/BiomeBGCR/inst/inputs/ini/spinup_m.ini",
                                             "~/repos/BiomeBGCR/inst/inputs/ini/spinup_pr.ini",
                                             "~/repos/BiomeBGCR/inst/inputs/ini/spinup_y.ini"),
                          bbgc.ini = c("~/repos/BiomeBGCR/inst/inputs/ini/cccmat63_b.ini",
                                       "~/repos/BiomeBGCR/inst/inputs/ini/cccmat63_bc.ini",
                                       "~/repos/BiomeBGCR/inst/inputs/ini/cccmat63_g.ini",
                                       "~/repos/BiomeBGCR/inst/inputs/ini/cccmat63_m.ini",
                                       "~/repos/BiomeBGCR/inst/inputs/ini/cccmat63_pr.ini",
                                       "~/repos/BiomeBGCR/inst/inputs/ini/cccmat63_y.ini")
                          )
                        )

out <- simInitAndSpades(modules = "BiomeBGC_core",
                        paths = list(modulePath = "~/repos/",
                                     outputPath = "~/outputs",
                                     inputPath = "~/repos/BiomeBGCR/inst/inputs"),
                        objects = list(
                          bbgc.ini = c("~/repos/BiomeBGCR/inst/inputs/ini/enf_test1.ini")
                        )
)

sites = c("Deer Point", "Boise", "Glacier", "Missoula", "Priest River", "Yellowstone")                        
par(mfcol = c(3,2))
for (i in 1:6){
  maxC = max(out$annualOutput[[i]]$total_C[c((2006-1949):(2089-1949))])
  plot(out$annualOutput[[i]]$total_C[c((2006-1949):(2089-1949))], 
       type = "l", 
       x = c(2006:2089),
       lwd = 2,
       col = "blue",
       xlab = "Year",
       ylab = "Site total C")
  text(x = 2010, y = maxC-0.2, labels = sites[i], font = 2)
}
