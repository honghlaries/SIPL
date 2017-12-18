# Initialization
rm(list = ls())
source("constant.R");source("uniTls_pkgInstall.R");source("uniTls_presetPaths.R");
pkgInitialization(c("dplyr","tidyr","ggplot2"))


# Example
mark <- read.csv("data/meta_treatment.csv") %>%
  dplyr::filter(sterilization == 1) %>%
  dplyr::select(pairTag, littMark, sedmMark, littAdd, littSource)
  
dat.steri <- read.csv("data/result_element.csv") %>%
  dplyr::inner_join(read.csv("data/meta_treatment.csv"), by = c("tubeID" = "tubeID")) %>%
  dplyr::filter(sterilization == 1) %>%
  dplyr::mutate(TIC = C - C.ac, AVS = S - S.ac, orgC_R = C.ac/C, AVS_R=AVS/S, 
         CvN = C.ac/N.ac, CvS = C.ac/S.ac) %>%
  dplyr::select(pairTag, littMark, sedmMark,
         N = N, C = C, S = S, 
         orgN = N.ac, orgC = C.ac, orgS = S.ac,
         TIC, AVS, orgC_R, AVS_R, CvN, CvS) %>%
  tidyr::gather(trait, value.s, N:CvS)
  

dat.usteri <- read.csv("data/result_element.csv") %>%
  dplyr::inner_join(read.csv("data/meta_treatment.csv"), by = c("tubeID" = "tubeID")) %>%
  dplyr::filter(sterilization == 0) %>%
  plyr::mutate(TIC = C - C.ac, AVS = S - S.ac, orgC_R = C.ac/C, AVS_R=AVS/S, 
               CvN = C.ac/N.ac, CvS = C.ac/S.ac) %>%
  dplyr::select(pairTag, littMark, sedmMark,
                N = N, C = C, S = S, 
                orgN = N.ac, orgC = C.ac, orgS = S.ac,
                TIC, AVS, orgC_R, AVS_R, CvN, CvS) %>%
  tidyr::gather(trait, value.us, N:CvS)

dat <- dplyr::inner_join(dat.steri,dat.usteri, by = c("pairTag" = "pairTag", "trait" = "trait")) %>%
  dplyr::mutate(value = value.us - value.s) %>%
  dplyr::select(pairTag, trait, value) %>% 
  tidyr::spread(trait, value) %>%
  dplyr::inner_join(mark, by = c("pairTag" = "pairTag")) %>%
  dplyr::select(-pairTag)

dat.sel <- dplyr::select_(dat, "littMark", "sedmMark", 
                          "littAdd", "littSource", "trait" = "AVS") 
dat.littAdd <- dplyr::group_by(dat.sel, littAdd) %>% 
  summarise(mean = mean(trait,na.rm = T), se = (sd(trait,na.rm = T)/sqrt(3)))
barPlot.littAdd <- ggplot(data = dat.littAdd, 
                          aes(x = littAdd, y = mean, ymax = mean+se, ymin = mean-se,
                              fill = littAdd)) + 
  geom_bar(position = "dodge",stat = "identity") +
  geom_errorbar(stat = "identity", col = "black", 
                width = 0.25, size = 0.3) +
  xlab("") + ylab("AVS(mg/kg)") +
  scale_fill_manual(values = c("grey70","grey30")) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = "none")

