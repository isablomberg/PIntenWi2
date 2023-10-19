
library(tidyverse)
order =c("acacacac", "cacacaca")
tb.fb = c("tftfftft", "ftfttftf")
consis = c("KIIKKIIK", "IKKIIKKI")

a.trials = c("bmpc", "cpmb", "pbcm", "mcbp")
c.trials = c("bmns", "mnsb", "nsbm", "sbmn")

a.c.trials = expand_grid(a.trials, c.trials)

outcome = c("RRLLLRRL", "LLRRRLLR", "LRRRLLRL", "RLLLRRRL", "RRLRRLRL", "LLRLLRLR", "LRRLRRLR", "RLLRLLLR")

cb = expand_grid(order,tb.fb, consis, outcome)

cb <- cb %>%
  mutate(a.trials = rep(a.c.trials$a.trials,4),
         c.trials = rep(a.c.trials$c.trials,4))

cb <- cb %>%
  mutate(a.c.trials = ifelse(order == "acacacac", paste0(substring(cb$a.trials,1,1),
                                                         substring(cb$c.trials,1,1),
                                                         substring(cb$a.trials,2,2),
                                                         substring(cb$c.trials,2,2),
                                                         substring(cb$a.trials,3,3),
                                                         substring(cb$c.trials,3,3),
                                                         substring(cb$a.trials,4,4),
                                                         substring(cb$c.trials,4,4)),
                             paste0(substring(cb$c.trials,1,1),
                                    substring(cb$a.trials,1,1),
                                    substring(cb$c.trials,2,2),
                                    substring(cb$a.trials,2,2),
                                    substring(cb$c.trials,3,3),
                                    substring(cb$a.trials,3,3),
                                    substring(cb$c.trials,4,4),
                                    substring(cb$a.trials,4,4))))

cb$video.5 <- NA
cb$video.6 <- NA
cb$video.7 <- NA
cb$video.8 <- NA
cb$video.9 <- NA
cb$video.10 <- NA
cb$video.11 <- NA
cb$video.12 <- NA

cb$id <- seq(from = 1, to = nrow(cb))

# for video.1
for (j in 8:15) {
  k = j-7
  for(i in 1:nrow(cb)) {
    if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "b" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "TB_ball_rattle_R"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "b" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "TB_ball_rattle_L"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "c" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "TB_compass_bell_R"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "c" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "TB_compass_bell_L"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "p" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "TB_pen_whistle_R"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "p" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "TB_pen_whistle_L"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "m" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "TB_magnifier_tröte_R"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "m" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "TB_magnifier_tröte_L"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "b" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "FB_ball_rattle_R"
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "b" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "FB_ball_rattle_L"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "c" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "FB_compass_bell_R"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "c" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "FB_compass_bell_L"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "p" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "FB_pen_whistle_R"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "p" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "FB_pen_whistle_L"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "m" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "FB_magnifier_tröte_R"
      
    } else if(substring(cb$order[i],k,k) == "a" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "m" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "FB_magnifier_tröte_L"
      
      # non-aspectual trials
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "b" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "TB_bottle_R"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "b" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "TB_bottle_L"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "s" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "TB_sword_R"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "s" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "TB_sword_L"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "n" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "TB_necklace_R"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "n" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "TB_necklace_L"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "m" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "TB_map_R"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "t" & substring(cb$a.c.trials[i],k,k) == "m" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "TB_map_L"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "b" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "FB_bottle_R"
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "b" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "FB_bottle_L"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "s" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "FB_sword_R"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "s" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "FB_sword_L"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "n" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "FB_necklace_R"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "n" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "FB_necklace_L"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "m" & substring(cb$outcome[i],k,k) == "R"){
      cb[i,j] = "FB_map_R"
      
    } else if(substring(cb$order[i],k,k) == "c" & substring(cb$tb.fb[i],k,k) == "f" & substring(cb$a.c.trials[i],k,k) == "m" & substring(cb$outcome[i],k,k) == "L"){
      cb[i,j] = "FB_map_L"
      
    }
    
  }
}


# Familiarisierung
obj.pair = c("cccddtfc","cddtfccc", "dtfccccd", "fccccddt")

consis.fam = c("IKKI", "KIIK")
outcome.fam = c("LRRL", "RRLL", "RLLR", "LLRR")

cb.fam = expand_grid(consis.fam, obj.pair)

cb.fam$outcome.fam = rep(c("LRRL", "RRLL", "RLLR", "LLRR"),2)

cb.fam$video.1 <- NA
cb.fam$video.2 <- NA
cb.fam$video.3 <- NA
cb.fam$video.4 <- NA

# for video 1
d = c(1,3,5,7)
for (j in 1:length(d)) {
  f = d[j]
  k = d[j]+1
  l = j+3
  
  for(i in 1:nrow(cb.fam)) {
    if(substring(cb.fam$obj.pair[i],f,k) == "cc"){
      cb.fam[i,l] = "crown_comb_L"
    } else if(substring(cb.fam$obj.pair[i],f,k) == "dt") {
      cb.fam[i,l] = "die_telescope_R"
      
    } else if(substring(cb.fam$obj.pair[i],f,k) == "cd") {
      cb.fam[i,l] = "cup_drum_R"
    } else if(substring(cb.fam$obj.pair[i],f,k) == "fc") {
      cb.fam[i,l] = "flower_coin_L"
      
    }
  }
}

cb.fam$id <- seq(from = 1, to = nrow(cb.fam))


cb$fam.id <- rep(1:8, 8)

cb$video.1 <- NA
cb$video.2 <- NA
cb$video.3 <- NA
cb$video.4 <- NA
cb$consis.fam <- NA
cb$outcome.fam <- NA
cb$obj.pair <- NA

for(i in 1:nrow(cb)) {
  j = cb$fam.id[i]
  
  cb$consis.fam[i] = as.character(cb.fam[j, "consis.fam"])
  cb$outcome.fam[i] = as.character(cb.fam[j, "outcome.fam"])
  cb$obj.pair[i] = as.character(cb.fam[j, "obj.pair"])

  cb$video.1[i] = as.character(cb.fam[j,"video.1"])
  cb$video.2[i] = as.character(cb.fam[j,"video.2"])
  cb$video.3[i] = as.character(cb.fam[j,"video.3"])
  cb$video.4[i] = as.character(cb.fam[j,"video.4"])
}

cb <- cb %>%
  select(id, consis.fam, outcome.fam, obj.pair, video.1, video.2, video.3, video.4, order, consis, outcome,tb.fb, video.5, video.6, video.7, video.8, video.9, video.10, video.11, video.12)
  
write_csv(cb, "/Users/isagarbisch/ownCloud work/Promotion/Studien/PIntenWi2/prereg:simulation/counterbalancing_PIntenWi2.csv")


cb_stimuli <- cb %>%
  mutate(video.1 = paste0("'videos/",video.1,".mp4'"),
         video.2 = paste0("'videos/",video.2,".mp4'"),
         video.3 = paste0("'videos/",video.3,".mp4'"),
         video.4 = paste0("'videos/",video.4,".mp4'"),
         video.5 = paste0("'videos/",video.5,".mp4'"),
         video.6 = paste0("'videos/",video.6,".mp4'"),
         video.7 = paste0("'videos/",video.7,".mp4'"),
         video.8 = paste0("'videos/",video.8,".mp4'"),
         video.9 = paste0("'videos/",video.9,".mp4'"),
         video.10 = paste0("'videos/",video.10,".mp4'"),
         video.11 = paste0("'videos/",video.11,".mp4'"),
         video.12 = paste0("'videos/",video.12,".mp4'")) %>%
  select(video_1 = video.1, video_2 = video.2, video_3 = video.3, video_4 =video.4, video_5 = video.5, video_6 = video.6, video_7 =video.7, 
         video_8 = video.8,video_9 = video.9, video_10 =video.10, video_11 =video.11, video_12 = video.12, consisfam = consis.fam, outcomefam = outcome.fam,
         order, consis, outcome, tb_fb_order = tb.fb, group = id)

cb_stimuli <- cb_stimuli %>%
  select(video_1, video_2 , video_3, video_4 , video_5 , video_6 , video_7 , 
         video_8,video_9 , video_10 , video_11 , video_12, consisfam, outcomefam, order, consis, outcome, tb_fb_order, group)

write_csv(cb_stimuli, "./cb_PIntenWi2_stimuli.csv")
