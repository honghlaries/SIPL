# Initialization
rm(list = ls())
source("constant.R");source("uniTls_pkgInstall.R");source("uniTls_presetPaths.R");
pkgInitialization(c("dplyr","tidyr","ggplot2","gridExtra"))
dirPreset("element")

# Functions
elementPatternCompare <- function(dat,tag,unit) {
  dat.sel <- dplyr::select_(dat, "littMark", "sedmMark", 
                            "littAdd", "littSource", "trait" = tag) 
  dat.littAdd <- dplyr::group_by(dat.sel, littAdd) %>% 
    summarise(mean = mean(trait,na.rm = T), se = (sd(trait,na.rm = T)/sqrt(3)))
  barPlot.littAdd <- ggplot(data = dat.littAdd, 
                            aes(x = littAdd, y = mean, ymax = mean+se, ymin = mean-se,
                                fill = littAdd)) + 
    geom_bar(position = "dodge",stat = "identity") +
    geom_errorbar(stat = "identity", col = "black", 
                  width = 0.25, size = 0.3) +
    xlab("") + ylab(paste(tag, unit, sep = "")) +
    scale_fill_manual(values = c("grey70","grey30")) + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 0))
  
  dat.sel <- dplyr::filter(dat.sel, littAdd == "Litter-Added")
  dat.sdem <- dplyr::group_by(dat.sel, sedmMark) %>% 
    summarise(mean = mean(trait,na.rm = T), se = (sd(trait,na.rm = T)/sqrt(3)))
  barPlot.sdem <- ggplot(data = dat.sdem, 
                         aes(x = sedmMark, y = mean, ymax = mean+se, ymin = mean-se,
                             fill = sedmMark)) + 
    geom_bar(position = "dodge",stat = "identity") +
    geom_errorbar(stat = "identity", col = "black", 
                  width = 0.25, size = 0.3) +
    xlab("") + ylab(paste(tag, unit, sep = "")) +
    #scale_fill_manual(values = c("grey70","grey30")) + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 0)) 
  
  dat.litt <- dplyr::group_by(dat.sel, littMark) %>% 
    summarise(mean = mean(trait,na.rm = T), se = (sd(trait,na.rm = T)/sqrt(3)))
  barPlot.litt <- ggplot(data = dat.litt, 
                         aes(x = littMark, y = mean, ymax = mean+se, ymin = mean-se,
                             fill = littMark)) + 
    geom_bar(position = "dodge",stat = "identity") +
    geom_errorbar(stat = "identity", col = "black", 
                  width = 0.25, size = 0.3) +
    xlab("") + ylab(paste(tag, unit, sep = "")) +
    #scale_fill_manual(values = c("grey70","grey30")) + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 0)) 
  
  dat.source <- dplyr::group_by(dat.sel, littSource) %>% 
    summarise(mean = mean(trait,na.rm = T), se = (sd(trait,na.rm = T)/sqrt(3)))
  barPlot.source <- ggplot(data = dat.source,
                           aes(x = littSource, y = mean, ymax = mean+se, ymin = mean-se, 
                               fill = littSource)) + 
    geom_bar(position = "dodge",stat = "identity", alpha = 0.5) +
    geom_errorbar(stat = "identity", col = "black", 
                  width = 0.25, size = 0.3) +
    xlab("") + ylab(paste(tag, unit, sep = "")) +
    scale_fill_manual(values = c("green","red"),breaks = c("Autocht","Allocht")) + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 0)) 
  
  grid.arrange(barPlot.littAdd, barPlot.source,
               barPlot.sdem, barPlot.litt,
               heights = c(3,3), widths = c(3,3), ncol=2)
}

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

tagList <- read.csv("data/tagList.csv")
tagList<- mutate(tagList, tag = as.character(tag),
                 unit = as.character(unit))
nym <- function(tagList) {
  ggsave(paste("element/",tagList$tag,".png", sep=""),
         plot = elementPatternCompare(dat, tagList$tag, tagList$unit),
         height = 4, width = 5)
}
#apply(tagList,1,nym)
for(i in 1:length(tagList$tag)) {
  ggsave(paste("element/",tagList$tag[i],".png", sep=""),
         plot = elementPatternCompare(dat, tagList$tag[i], tagList$unit[i]),
         height = 4, width = 5)
}






