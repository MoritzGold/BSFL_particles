# Code for manuscript

#Micro- and nanoplastic size affects uptake and digestive tract region 
#residence time in black soldier fly larvae during food waste bioconversion

#Gold et al. (2025), Waste Management

#--------# 

# load packages

library(openxlsx)
library(tidyverse)
library(svglite)
library(rstatix)

# empty R`s brain

rm(list = ls())


# data for 25DM MP -----

  # load MP and dye ingestion/egestion data and edit for 75% moisture content food waste

  MP_25DM_raw <- openxlsx::read.xlsx(xlsxFile = "data/KUL_ingestion_egestion.xlsx",sheet = "25DM_MP") %>% 
  
  # remove FG, assume FG is early AMG
  
  mutate(section_dye=case_when(section_dye=="FG"~"AMG",TRUE~section_dye)) %>% 
  
  mutate(MP15_section=case_when(MP15_section=="FG"~"AMG",TRUE~MP15_section)) %>% 

  mutate(MP58_section=case_when(MP58_section=="FG"~"AMG",TRUE~MP58_section)) %>% 

  # no dye = no_color, consistent terminology

  mutate(section_dye=case_when(section_dye == "no dye" ~ "no_color",TRUE~section_dye)) %>% 
  
  # make all samples no_color where MP15 count was 0
  
  mutate(MP15_section=case_when(MP15 == 0 ~ "no_color",TRUE~MP15_section)) %>% 

  # make all samples no_color where MP58 count was 0
  
  mutate(MP58_section=case_when(MP58 == 0 ~ "no_color",TRUE~MP58_section)) %>%
  
  mutate(section_dye=factor(section_dye)) %>% 
  
  mutate(MP15_section=factor(MP15_section)) %>% 
  
  mutate(MP58_section=factor(MP58_section))
  
  # write all data
  
  write.xlsx(as.data.frame(MP_25DM_raw), file = "output/D25_MP_edit.xlsx",sheetName = "25DM_MP", append = FALSE)

  # load NP ingestion & egesion data for 75% moisture content food waste 
  
  NP_25DM_raw <- openxlsx::read.xlsx(xlsxFile = "data/240321_NP_final results_raw.xlsx")
  
  # make separate files for residence time modelling (DM25 dye, DM25 MP15, DM25 M58)
  
  # for dye
  
  MP_25DM_dye_section <- 
    
    MP_25DM_raw %>% 
    
    select(sample_name,time,type,replicate_larvae,larval_mass_mg,section_dye) %>% 
    
    # drop NAs
    
    drop_na(section_dye) %>% 
    
    mutate(particle="dye")
  
    # n=231 samples
  
  # for MP15
  
  MP_25DM_MP15_section <- 
    
    MP_25DM_raw %>% 
    
    select(sample_name,time,type,replicate_larvae,larval_mass_mg,MP15_section) %>% 
    
    # drop NAs
    
    drop_na(MP15_section) %>% 
    
    mutate(particle="MP15")
  
  # n=186 samples
  
  
  # for MP58
  
  MP_25DM_MP58_section <- 
    
    MP_25DM_raw %>% 
    
    select(sample_name,time,type,replicate_larvae,larval_mass_mg,MP58_section) %>% 
    
    # drop NAs
    
    drop_na(MP58_section) %>% 
    
    mutate(particle="MP58")
  
  # n=215 samples
  
  write.xlsx(as.data.frame(MP_25DM_dye_section), file = "output/D25_MP_dye_position.xlsx",sheetName = "25DM_MP_dye", append = FALSE)
  write.xlsx(as.data.frame(MP_25DM_MP15_section), file = "output/D25_MP_MP15_position.xlsx",sheetName = "25DM_MP_MP15", append = FALSE)
  write.xlsx(as.data.frame(MP_25DM_MP58_section), file = "output/D25_MP_MP58_position.xlsx",sheetName = "25DM_MP_MP58", append = FALSE)

  
# plot position abundance -----

# combine all data in one data.frame
  
  # DM25 MP (dye, MP15, MP58)
  
  colnames(MP_25DM_dye_section) <- c("sample_name","time","type","replicate_larvae","larval_mass_mg","section","particle")
  colnames(MP_25DM_MP15_section) <- c("sample_name","time","type","replicate_larvae","larval_mass_mg","section","particle")
  colnames(MP_25DM_MP58_section) <- c("sample_name","time","type","replicate_larvae","larval_mass_mg","section","particle")
  
  # all include dye postion of DM25 (only dye) and DM35 (only dye) - influence of moisture content
  
   # 35DM with dye only 
  
    dye_35DM_raw <- openxlsx::read.xlsx(xlsxFile = "data/KUL_ingestion_egestion.xlsx",sheet = "35DM_dye") %>% 
      
      select(sample_name,time,type,replicate_larvae,larval_mass_mg,section_dye) %>% 
      
      mutate(particle="dye") %>% 
      
      mutate(section_dye=factor(section_dye))
    
  # 25DM with dye only 
    
    dye_25DM_raw <- openxlsx::read.xlsx(xlsxFile = "data/KUL_ingestion_egestion.xlsx",sheet = "25DM_dye") %>% 
      
      select(sample_name,time,type,replicate_larvae,larval_mass_mg,section_dye) %>% 
      
      mutate(particle="dye") %>% 
      
      mutate(section_dye=factor(section_dye))
    
  colnames(dye_35DM_raw) <- c("sample_name","time","type","replicate_larvae","larval_mass_mg","section","particle")
  colnames(dye_25DM_raw) <- c("sample_name","time","type","replicate_larvae","larval_mass_mg","section","particle")
  
   # combine all data 
  
  str(dye_25DM_raw)
  str(MP_25DM_dye_section)
  
  ingestion_all <- 
  
  bind_rows(MP_25DM_dye_section,MP_25DM_MP15_section,MP_25DM_MP58_section,dye_35DM_raw,dye_25DM_raw) %>% 
    
    mutate(particle=factor(particle)) %>% 
    
    mutate(sample_name = factor(sample_name)) %>% 
    
    mutate(section=factor(section,levels=c("no_color","AMG","MMG","PMG1","PMG2","HG")))
  
  str(ingestion_all)

  # plot results in bar plots
  
  # influence of moisture content (dye only) -----
  
  ingestion_all %>% 
    
    mutate(time=factor(time)) %>% 
    
    filter(particle=="dye") %>% 
    
    filter(type=="ingestion") %>% 
    
    filter(time == 10 | time == 60 | time == 120 | time == 180) %>% # for DM25 and DM35 dye only recoreded until 180 min
    
    filter(!sample_name=="25DM_MP") %>% 
    
    mutate(sample_name = recode(sample_name,
                                "25DM_dye" = "75% moisture content",
                                "35DM_dye" = "65% moisture content")) %>% 
  
    ggplot(aes(x = time,fill=section)) +
    
    geom_bar(position="fill") +
    
    facet_wrap(~sample_name) +
    
    ylab("% larvae with dye by digestive region") +
    
    xlab("Ingestion time (min)") +
    
    ggtitle("A")+
    
    scale_fill_manual(name = "Digestive region", values = c(
      "no_color" = "#e0e0e0", 
      "AMG" = "#1f78b4",      
      "MMG" = "#ff7f00",     
      "PMG1" = "#33a02c",    
      "PMG2" = "#e31a1c",     
      "HG" = "#6a3d9a"        
    ),
      
      labels=c("no_color"="No ingestion","AMG"="AMG: Anterior midgut",
                               "MMG"="MMG: Middle midgut","PMG1"="PMG1: 1st part posterior midgut",
                               "PMG2"="PMG2: 2nd part posterior midgut","HG"="HG: Hindgut")) +
    
    theme_bw() +
    
    theme(
      plot.title = element_text(hjust = 0, size = 12, face = "bold"),  # Left-aligned and bold title
      plot.title.position = "plot"  # Optional: Use this to position the title with respect to the entire plot area
    )
  
  
    ggsave("output/position_dye.jpeg", dpi = 1200, width = 16,height = 12, units = "cm")
    ggsave("output/position_dye.svg", dpi = 1200, width = 16,height = 12, units = "cm")
    
    # statistical analyses
    
    MC_influence_stat <- ingestion_all %>% 
      
      mutate(time=factor(time)) %>% 
      
      filter(particle=="dye") %>% 
      
      filter(type=="ingestion") %>% 
      
      filter(time == 10 | time == 60 | time == 120 | time == 180) %>% # for DM25 and DM35 dye only recoreded until 180 min
      
      filter(!sample_name=="25DM_MP") %>% 
      
      filter(!section=="no_color") %>% 
      
      select(-particle, -particle,-larval_mass_mg) %>% 
    
      mutate(sample_name = factor(sample_name)) %>% 
      
      mutate(time=factor(time)) %>% 
        
      mutate(section= factor(section))
    

    # Define the order of sections and their corresponding numeric ranks
    section_order <- c("AMG" = 1, "MMG" = 2, "PMG1" = 3, "PMG2" = 4, "HG" = 5)
    
    # Get unique time points
    time_points <- unique(MC_influence_stat$time)
    
    # Loop through each time point
    for (time_point in time_points) {
      # Filter data for the current time point
      data_subset <- MC_influence_stat %>% filter(time == time_point)
      
      # Correct the factor levels in the 'sample_name' column
      data_subset <- data_subset %>%
        mutate(sample_name = factor(sample_name))
      
      # Print levels to verify that 'sample_name' only contains the levels present
      cat("Time Point:", time_point, "\n")
      print(levels(data_subset$sample_name))
      
      # Recode section categories to numeric ranks for the current time point
      data_subset <- data_subset %>%
        mutate(section_rank = recode(section, !!!section_order))
      
      # Perform Mann-Whitney U test (Wilcoxon test) using rstatix
      wilcox_test_result <- data_subset %>%
        wilcox_test(section_rank ~ sample_name) %>%
        adjust_pvalue(method = "BH") %>%
        add_significance()
      
      # Print the results
      cat("Results for Time Point", time_point, ":\n")
      print("Wilcoxon Test Results:")
      print(wilcox_test_result)
      cat("------------------------\n")
    }
   
     # only 25%DM dye + MPs ingestion ----
    
  ingestion_all$section
    
    ingestion_all %>% 
      
      mutate(time=factor(time)) %>% 
      
      mutate(section=fct_relevel(section,c("no_color", "AMG", "MMG", "PMG1", "PMG2","HG"))) %>% 
      
      filter(sample_name=="25DM_MP") %>% 
      
      filter(type=="ingestion") %>% 
      
      ggplot(aes(x = time,fill=section)) +
      
      geom_bar(position="fill") +

      facet_wrap(~particle) +
      
      theme_bw(base_size=14) +
      
      ggtitle("A") +
      
      scale_fill_manual(name = "Digestive region", values = c(
        "no_color" = "#e0e0e0", 
        "AMG" = "#1f78b4",      
        "MMG" = "#ff7f00",     
        "PMG1" = "#33a02c",    
        "PMG2" = "#e31a1c",     
        "HG" = "#6a3d9a"        
      ),
      
      labels=c("no_color"="No ingestion","AMG"="AMG: Anterior midgut",
               "MMG"="MMG: Middle midgut","PMG1"="PMG1: 1st part posterior midgut",
               "PMG2"="PMG2: 2nd part posterior midgut","HG"="HG: Hindgut")) +
      
      
      ylab("% larvae with dye/MPs by digestive region")+
      
      xlab("Ingestion time (min)")+
      
      theme_bw()+
      
      theme(
        plot.title = element_text(hjust = 0, size = 12, face = "bold"),  # Left-aligned and bold title
        plot.title.position = "plot"  # Optional: Use this to position the title with respect to the entire plot area
      )
    
    ggsave("output/position_25DM_MP_ingestion.jpeg", dpi = 1200, width = 28,height = 12, units = "cm")
    ggsave("output/position_25DM_MP_ingestion.svg", dpi = 1200, width = 28,height = 12, units = "cm")
    
    #statistical analyses
    
    MNPs_stat_ingestion <- 
    
    ingestion_all %>% 
      
      mutate(time=factor(time)) %>% 
      
      mutate(section=fct_relevel(section,c("no_color", "HG","PMG2","PMG1","MMG","AMG"))) %>% 
      
      filter(sample_name=="25DM_MP") %>% 
      
      filter(type=="ingestion") %>% 
      
      select(-sample_name,-type,-larval_mass_mg) %>% 
      
      filter(!section=="no_color") %>% 
      
      mutate(particle = factor(particle)) %>% 
      
      mutate(time=factor(time)) %>% 
      
      mutate(section= factor(section))
    

    # get number of samples with particles per time point
    
    MNPs_stat_ingestion_n <- 
    
    MNPs_stat_ingestion %>% 
      
      group_by(time,particle) %>% 
      
      summarise(n=n())
    
    # select timepoints with suffcient replicates (n> 10, for all particle types, MP58 limiting)
    
    MNPs_stat_ingestion <- 
      
    MNPs_stat_ingestion %>% left_join(MNPs_stat_ingestion_n,by = c("time","particle")) %>% 
      
      filter(n>10) %>% 
      
      filter(time==180  | time == 250 | time  == 300) %>% # this is the time points that MP58 has more than 10 samples
    
      mutate(time=factor(time))
      
    # Define the order of sections and their corresponding numeric ranks
    section_order <- c("AMG" = 1, "MMG" = 2, "PMG1" = 3, "PMG2" = 4, "HG" = 5)
    
    # Get unique time points
    time_points <- unique(MNPs_stat_ingestion$time)
    
    # Loop through each time point
    for (time_point in time_points) {
      # Filter data for the current time point
      data_subset <- MNPs_stat_ingestion %>% filter(time == time_point)
      
      # Correct the factor levels in the 'sample_name' column
      data_subset <- data_subset %>%
        mutate(particle = factor(particle))
      
      # Print levels to verify that 'sample_name' only contains the levels present
      cat("Time Point:", time_point, "\n")
      print(levels(data_subset$particle))
      
      # Recode section categories to numeric ranks for the current time point
      data_subset <- data_subset %>%
        mutate(section_rank = recode(section, !!!section_order))
      
      # Perform Mann-Whitney U test (Wilcoxon test) using rstatix
      wilcox_test_result <- data_subset %>%
        wilcox_test(section_rank ~ particle) %>%
        adjust_pvalue(method = "BH") %>%
        add_significance()
      
      # Print the results
      cat("Results for Time Point", time_point, ":\n")
      print("Wilcoxon Test Results:")
      print(wilcox_test_result)
      cat("------------------------\n")
    }
    
    
    # only 25%DM dye + MPs egestion -----
    
    ingestion_all %>% 
      
      filter(sample_name=="25DM_MP") %>% 
      
      filter(type=="egestion" | time == 300 ) %>% 
      
      mutate(time=case_when(time== 300 ~ 0,TRUE~time)) %>% 
      
      mutate(time=factor(time)) %>% 
      
      ggplot(aes(x = time,fill=section)) +
      
      geom_bar(position="fill") +
      
      facet_wrap(~particle) +
      
      theme_bw() +

      ylab("% larvae with dye/MPs by digestive region") +
      
      ggtitle("B") +
      
      xlab("Egestion time (min)") + 
      
      scale_fill_manual(name = "Digestive region", values = c(
        "no_color" = "#e0e0e0", 
        "AMG" = "#1f78b4",      
        "MMG" = "#ff7f00",     
        "PMG1" = "#33a02c",    
        "PMG2" = "#e31a1c",     
        "HG" = "#6a3d9a"        
      ),
      
      labels=c("no_color"="No ingestion","AMG"="AMG: Anterior midgut",
               "MMG"="MMG: Middle midgut","PMG1"="PMG1: 1st part posterior midgut",
               "PMG2"="PMG2: 2nd part posterior midgut","HG"="HG: Hindgut")) +
      
      
      theme(
        plot.title = element_text(hjust = 0, size = 12, face = "bold"),  # Left-aligned and bold title
        plot.title.position = "plot"  # Optional: Use this to position the title with respect to the entire plot area
      )
    
    ggsave("output/position_25DM_MP_egestion.jpeg", dpi = 1200, width = 18,height = 10, units = "cm")
    ggsave("output/position_25DM_MP_egestion.svg", dpi = 1200, width = 18,height = 10, units = "cm")
    
   
    #statistical analyses
    
    MNPs_stat_egestion <- 
      
      ingestion_all %>% 
      
      mutate(time=factor(time)) %>% 
      
      mutate(section=fct_relevel(section,c("no_color", "HG","PMG2","PMG1","MMG","AMG"))) %>% 
      
      filter(sample_name=="25DM_MP") %>% 
      
      filter(type=="egestion") %>% 
      
      select(-sample_name,-type,-larval_mass_mg) %>% 
      
      filter(!section=="no_color") %>% 
      
      mutate(particle = factor(particle)) %>% 
      
      mutate(time=factor(time)) %>% 
      
      mutate(section= factor(section))
    
    
    # get number of samples with particles per time point
    
    MNPs_stat_egestion_n <- 
      
      MNPs_stat_egestion %>% 
      
      group_by(time,particle) %>% 
      
      summarise(n=n())
    
    # select timepoints with suffcient replicates (n> 10, for all particle types, MP58 limiting)
    
    MNPs_stat_egestion <- 
      
      MNPs_stat_egestion %>% left_join(MNPs_stat_egestion_n,by = c("time","particle")) %>% 
      
      filter(n>10) %>% 
      
      mutate(time=factor(time))
    

    # Define the order of sections and their corresponding numeric ranks
    section_order <- c("AMG" = 1, "MMG" = 2, "PMG1" = 3, "PMG2" = 4, "HG" = 5)
    
    # Get unique time points
    time_points <- unique(MNPs_stat_egestion$time)
    
    # Loop through each time point
    for (time_point in time_points) {
      # Filter data for the current time point
      data_subset <- MNPs_stat_egestion %>% filter(time == time_point)
      
      # Correct the factor levels in the 'sample_name' column
      data_subset <- data_subset %>%
        mutate(particle = factor(particle))
      
      # Print levels to verify that 'sample_name' only contains the levels present
      cat("Time Point:", time_point, "\n")
      print(levels(data_subset$particle))
      
      # Recode section categories to numeric ranks for the current time point
      data_subset <- data_subset %>%
        mutate(section_rank = recode(section, !!!section_order))
      
      # Perform Mann-Whitney U test (Wilcoxon test) using rstatix
      wilcox_test_result <- data_subset %>%
        wilcox_test(section_rank ~ particle) %>%
        adjust_pvalue(method = "BH") %>%
        add_significance()
      
      # Print the results
      cat("Results for Time Point", time_point, ":\n")
      print("Wilcoxon Test Results:")
      print(wilcox_test_result)
      cat("------------------------\n")
    }
    
    
# MP numbers (ingestion and eng combined) ----- 

MP_25DM_raw$section_dye

str(MP_25DM_raw)

head(MP_25DM_raw)

# plot MP and NP ingestion

  # NP: detuct blank from samples

    # NP in blank

    NP_25DM_blank <- 

    NP_25DM_raw %>% 
      
      filter(sample_name == "blank") %>% 
      
      group_by(sample_name) %>% 
      
      summarise(blank_mean=mean(NP_108_ug))
    
    # deduct blank
    
    NP_25DM <- 
    
    NP_25DM_raw %>% 
      
      filter(sample_name == "25DM_MP") %>% 
      
      mutate(conc=NP_108_ug-NP_25DM_blank$blank_mean) %>% 
      
      mutate(conc=case_when(conc < 0 ~ 0, TRUE~conc)) %>% 
      
      select(time,type,conc) %>% 
      
      # make egestion continuation of ingestion
      
      mutate(time_dummy=case_when(type=="egestion" ~ "300", TRUE~"0")) %>% # ingestion ended after 300 min
      
      mutate(time_dummy=as.numeric(time_dummy)) %>% 
      
      mutate(time=time+time_dummy) %>% 
      
      filter(time < 1000)  # remove last egestion value form plot
    
    # look at 1080 min egestion point
    
    NP_25DM_raw %>% 
      
      filter(sample_name == "25DM_MP") %>% 
      
      mutate(conc=NP_108_ug-NP_25DM_blank$blank_mean) %>% 
      
      mutate(conc=case_when(conc < 0 ~ 0, TRUE~conc)) %>% 
      
      select(time,type,conc) %>% 
      
      filter(time>1000)
      

   # look at end of ingestion
    
    NP_25DM %>% 
      
      filter(time<1) %>% 
      
      summarise(mean=mean(conc),
                sd=sd(conc))
    
    
    # look at 180 min of egestion
    
    NP_25DM %>% 
      
      filter(time>150) %>% 
      
      summarise(mean=mean(conc),
                sd=sd(conc))
    
    
  # prepare data

  MP_25DM_count_tot <- 
  
  MP_25DM_raw %>% 
    
    select(time,type,MP15,MP58) %>% 
    
    gather(3:4,key="MP_size",value="count") %>% 
    
    filter(!count=="NA") %>% 
    
    filter(!time==1080) %>% 
    
    mutate(type=factor(type)) %>% 
    
    # add egestion as continuation of ingestion
    
    mutate(time_dummy=case_when(type=="egestion" ~ "300", TRUE~"0")) %>% # ingestion ended after 300 min
    
    mutate(time_dummy=as.numeric(time_dummy)) %>% 
    
    mutate(time=time+time_dummy)
  
  # add egestion as continuation of ingestion
  
  MP_25DM_count <- 
  
  MP_25DM_count_tot %>% 
    
    mutate(time_dummy=case_when(type=="egestion" ~ "300", TRUE~"0")) %>% # ingestion ended after 300 min
    
    mutate(time_dummy=as.numeric(time_dummy)) %>% 
    
    mutate(time=time+time_dummy)

  # subset MP15
  
  MP_25DM_count_MP15 <-
    
    MP_25DM_count %>% 
    
    filter(MP_size=="MP15")
  
  
  # subset MP58
  
  MP_25DM_count_MP58 <-
    
    MP_25DM_count %>% 
    
    filter(MP_size=="MP58")

  
  # calculate mean 
  
    # all
  
    MP_25DM_count_mean <- 
    
    MP_25DM_count %>% 
      
      group_by(time,type, MP_size) %>% 
      
      summarise(n=n(),
                mean=mean(count),
                sd=sd(count))
    
    # MP15
    
    MP_25DM_count_mean15 <- 
      
      MP_25DM_count %>% 
      
      filter(MP_size=="MP15") %>% 
      
      group_by(time,type) %>% 
      
      summarise(n=n(),
                mean=mean(count),
                sd=sd(count))
    
    # MP58
    
    MP_25DM_count_mean58 <- 
      
      MP_25DM_count %>% 
      
      filter(MP_size=="MP58") %>% 
      
      group_by(time,type) %>% 
      
      summarise(n=n(),
                mean=mean(count),
                sd=sd(count))
    
    # NP108
    
    NP_25DM_mean <-
      
      NP_25DM %>% 
      
      group_by(time,type) %>% 
      
      summarise(n=n(),
                mean=mean(conc),
                sd=sd(conc))
  
  # plot all
  
  MP_25DM_count_mean %>% 
  
  ggplot(aes(time,mean,color=MP_size)) +
  
  geom_point() +
    
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.3)+
    
  geom_line() +
  
  geom_vline(xintercept = 300,linetype="dotdash") +
      
  geom_point(data=MP_25DM_count,
                aes(time,count),
                size=2,
                position = position_jitter(0.05),
                alpha=.4) +
    
    theme_bw(base_size = 12) +
    
    ggtitle("MP 15 and 58 um, dashed line start of egestion") +
    
    xlab("time (min)") +
    
    ylab("MP count (#)") 
  
  
  ggsave("output/allMP.jpeg", dpi = 1200, width = 16,height = 10, units = "cm")
  
  # plot M15
  
  MP_25DM_count_mean15 %>% 
    
    ggplot(aes(time,mean)) +
    
    geom_point() +
    
    geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.5)+
    
    geom_line() +
    
    geom_vline(xintercept = 300,linetype="dotdash") +
    
    geom_point(data=MP_25DM_count_MP15,
               aes(time,count),
               size=2,
               position = position_jitter(0.05),
               alpha=.4) +
    
    theme_bw(base_size = 12) +
    
    ggtitle("MP 15 um, dashed line start of egestion") +
    
    xlab("time (min)") +
    
    ylab("MP count (#)") 
  
    ggsave("output/15MP.jpeg", dpi = 1200, width = 16,height = 10, units = "cm")
  
    # plot M58
    
    MP_25DM_count_mean58 %>% 
      
      ggplot(aes(time,mean)) +
      
      geom_point() +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.5)+
      
      geom_line() +
      
      geom_vline(xintercept = 300,linetype="dotdash") +
      
      geom_point(data=MP_25DM_count_MP58,
                 aes(time,count),
                 size=2,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      ggtitle("MP 58 um, dashed line start of egestion") +
      
      xlab("time (min)") +
      
      ylab("MP count (#)") 
    
    ggsave("output/58MP.jpeg", dpi = 1200, width = 16,height = 10, units = "cm")

    
    # plot NP108

    NP_25DM_mean %>% 
      
      ggplot(aes(time,mean)) +
      
      geom_point() +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.5)+
      
      geom_line() +
      
      geom_vline(xintercept = 300,linetype="dotdash") +
      
      geom_point(data=NP_25DM,
                 aes(time,conc),
                 size=2,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      ggtitle("NP, dashed line start of egestion") +
      
      xlab("time (min)") +
      
      ylab("NP content (µg)") 
    
    ggsave("output/NP.jpeg", dpi = 1200, width = 16,height = 10, units = "cm")
    
   # statistical analyses
    
      #ingestion
    
    MP_25DM_count_stat_ing <-
    
    MP_25DM_count_tot %>% 
      
      filter(type=="ingestion") %>% 
      
      filter(time > 0)
    
    
    MP_25DM_count_stat_ing <- 
      
      MP_25DM_count_stat_ing %>% 
      
      mutate(time=factor(time)) %>% 
      
      select(-type,-time_dummy) %>% 
      
      mutate(MP_size = factor(MP_size))
      

    # get number of samples with particles per time point
    
    MP_25DM_count_stat_ing_n <- 
      
      MP_25DM_count_stat_ing %>% 
      
      group_by(time,MP_size) %>% 
      
      summarise(n=n())
    
    # select timepoints with suffcient replicates (n> 10, for all particle types, MP58 limiting)
    
    MP_25DM_count_stat_ing <- 
      
      MP_25DM_count_stat_ing %>% left_join(MP_25DM_count_stat_ing_n,by = c("time","MP_size")) %>% 
      
      filter(n>10) %>% 
      
      mutate(time=factor(time))
    
    
    # Get unique time points
    time_points <- unique(MP_25DM_count_stat_ing$time)
    
    # Loop through each time point
    for (time_point in time_points) {
      # Filter data for the current time point
      data_subset <- MP_25DM_count_stat_ing %>% filter(time == time_point)
      
      # Correct the factor levels in the 'sample_name' column
      data_subset <- data_subset %>%
        mutate(MP_size = factor(MP_size))
      
      # Print levels to verify that 'sample_name' only contains the levels present
      cat("Time Point:", time_point, "\n")
      print(levels(data_subset$MP_size))
      
      # Perform Mann-Whitney U test (Wilcoxon test) using rstatix
      wilcox_test_result <- data_subset %>%
        wilcox_test(count ~ MP_size) %>%
        adjust_pvalue(method = "BH") %>%
        add_significance()
      
      # Print the results
      cat("Results for Time Point", time_point, ":\n")
      print("Wilcoxon Test Results:")
      print(wilcox_test_result)
      cat("------------------------\n")
    }
    
    #egestion
    
    MP_25DM_count_stat_eg <-
    
    MP_25DM_raw %>% 
      
      select(time,type,MP15,MP58) %>% 
      
      gather(3:4,key="MP_size",value="count") %>% 
      
      filter(!count=="NA") %>% 
      
      mutate(type=factor(type)) %>% 
      
      # add egestion as continuation of ingestion
      
      mutate(time_dummy=case_when(type=="egestion" ~ "300", TRUE~"0")) %>% # ingestion ended after 300 min
      
      mutate(time_dummy=as.numeric(time_dummy)) %>% 
      
      mutate(time=time+time_dummy) %>% 
      
      filter(type=="egestion") %>% 
      
      filter(time > 0)
    
    
    MP_25DM_count_stat_eg <- 
      
      MP_25DM_count_stat_eg %>% 
      
      mutate(time=factor(time)) %>% 
      
      select(-type,-time_dummy) %>% 
      
      mutate(MP_size = factor(MP_size))
    
    
    # get number of samples with particles per time point
    
    MP_25DM_count_stat_eg_n <- 
      
      MP_25DM_count_stat_eg %>% 
      
      group_by(time,MP_size) %>% 
      
      summarise(n=n())
    
    # select timepoints with suffcient replicates (n> 10, for all particle types, MP58 limiting)
    
    MP_25DM_count_stat_eg <- 
      
      MP_25DM_count_stat_eg %>% left_join(MP_25DM_count_stat_eg_n,by = c("time","MP_size")) %>% 
      
      filter(n>10) %>% 
      
      mutate(time=factor(time))
    
    
    # Get unique time points
    time_points <- unique(MP_25DM_count_stat_eg$time)
    
    # Loop through each time point
    for (time_point in time_points) {
      # Filter data for the current time point
      data_subset <- MP_25DM_count_stat_eg %>% filter(time == time_point)
      
      # Correct the factor levels in the 'sample_name' column
      data_subset <- data_subset %>%
        mutate(MP_size = factor(MP_size))
      
      # Print levels to verify that 'sample_name' only contains the levels present
      cat("Time Point:", time_point, "\n")
      print(levels(data_subset$MP_size))
      
      # Perform Mann-Whitney U test (Wilcoxon test) using rstatix
      wilcox_test_result <- data_subset %>%
        wilcox_test(count ~ MP_size) %>%
        adjust_pvalue(method = "BH") %>%
        add_significance()
      
      # Print the results
      cat("Results for Time Point", time_point, ":\n")
      print("Wilcoxon Test Results:")
      print(wilcox_test_result)
      cat("------------------------\n")
    }
    
     
    # MP numbers (ing only) ----- 
    
    MP_25DM_raw$section_dye
    
    str(MP_25DM_raw)
    
    head(MP_25DM_raw)
    
    # plot MP and NP ingestion and egestion
    
    # NP: deduct blank from samples
    
    # NP in blank
    
    NP_25DM_blank <- 
      
      NP_25DM_raw %>% 
      
      filter(sample_name == "blank") %>% 
      
      group_by(sample_name) %>% 
      
      summarise(blank_mean=mean(NP_108_ug))
    
    # deduct blank
    
    NP_25DM <- 
      
      NP_25DM_raw %>% 
      
      filter(sample_name == "25DM_MP") %>% 
      
      mutate(conc=NP_108_ug-NP_25DM_blank$blank_mean) %>% 
      
      mutate(conc=case_when(conc < 0 ~ 0, TRUE~conc)) %>% 
      
      select(time,type,conc) %>% 
      
      filter(type=="ingestion")
    
    
    # prepare data
    
    MP_25DM_count <- 
      
      MP_25DM_raw %>% 
      
      select(time,type,MP15,MP58) %>% 
      
      gather(3:4,key="MP_size",value="count") %>% 
      
      filter(!count=="NA") %>% 
      
      filter(!time==1080) %>% 
      
      mutate(type=factor(type))
    

    # subset MP15
    
    MP_25DM_count_MP15 <-
      
      MP_25DM_count %>% 
      
      filter(count<600) %>% 
      
      filter(type=="ingestion") %>% 
      
      filter(MP_size=="MP15")
    
    
    # subset MP58
    
    MP_25DM_count_MP58 <-
      
      MP_25DM_count %>% 
      
      filter(count<20) %>% 
      
      filter(type=="ingestion") %>% 
      
      filter(MP_size=="MP58")
    
    

    
    # calculate mean 
    
    # all
    
    MP_25DM_count_mean <- 
      
      MP_25DM_count %>% 
      
      filter(type=="ingestion") %>% 
      
      group_by(time,MP_size) %>% 
      
      summarise(n=n(),
                mean=mean(count),
                sd=sd(count))
    
    # MP15
    
    MP_25DM_count_mean15 <- 
      
      MP_25DM_count %>% 
      
      filter(type=="ingestion") %>% 
      
      filter(MP_size=="MP15") %>% 
      
      group_by(time) %>% 
      
      summarise(n=n(),
                mean=mean(count),
                sd=sd(count))
    
    # MP58
    
    MP_25DM_count_mean58 <- 
      
      MP_25DM_count %>% 
      
      filter(MP_size=="MP58") %>% 
      
      filter(type=="ingestion") %>% 
      
      group_by(time) %>% 
      
      summarise(n=n(),
                mean=mean(count),
                sd=sd(count))
    
    # NP108
    
    NP_25DM_mean <-
      
      NP_25DM %>% 
      
      filter(type=="ingestion") %>% 
      
      group_by(time) %>% 
      
      summarise(n=n(),
                mean=mean(conc),
                sd=sd(conc))

    
    # plot all
    
    MP_25DM_count_mean %>% 
      
      ggplot(aes(time,mean,color=MP_size)) +
      
      geom_point() +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.3)+
      
      geom_line() +
      
      geom_point(data=MP_25DM_count,
                 aes(time,count),
                 size=2,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      ggtitle("MP 15 and 58 um, dashed line start of egestion") +
      
      xlab("time (min)") +
      
      ylab("MP count (#)")
      
 
    ggsave("output/allMP.jpeg", dpi = 1200, width = 16,height = 10, units = "cm")
    
    # plot M15

    MP_25DM_count_mean15 %>% 
      
      filter(mean < 600) %>% 
      
      ggplot(aes(time,mean)) +
      
      geom_point(size=2) +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 5)+
      
      geom_line() +
      
      geom_point(data=MP_25DM_count_MP15,
                 aes(time,count),
                 size=1,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      scale_x_continuous(limits=c(0,305),breaks=c(0, 10, 20, 40, 60, 90, 
                                                  120, 150, 180, 250, 300), expand = c(0.01, 0)) +
      
      ggtitle("C: 15 μm MPs ingestion") +
      
      xlab("Ingestion time (min)") +
      
      ylab("MPs per larva") +
      
      theme(
        plot.title = element_text(hjust = 0, size = 12, face = "bold"),  # Left-aligned and bold title
        plot.title.position = "plot"  # Optional: Use this to position the title with respect to the entire plot area
      )
    
    ggsave("output/15MP_ing.jpeg", dpi = 1200, width = 11,height = 8, units = "cm")
    ggsave("output/15MP_ing.svg", dpi = 1200, width = 11,height = 8, units = "cm")
    
    # plot M58
    
    MP_25DM_count_mean58 %>% 
      
      filter(mean<20) %>% 
      
      ggplot(aes(time,mean)) +
      
      geom_point(size=2) +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 5)+
      
      geom_line() +
      
      geom_point(data=MP_25DM_count_MP58,
                 aes(time,count),
                 size=1,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      scale_x_continuous(limits=c(0,305),breaks=c(0, 10, 20, 40, 60, 90, 
                                                  120, 150, 180, 250, 300), expand = c(0.01, 0)) +
      ggtitle("E: 58 μm MPs ingestion") +
      
      xlab("Ingestion time (min)") +
      
      ylab("MPs per larva") +
      
      theme(
        plot.title = element_text(hjust = 0, size = 12, face = "bold"),  # Left-aligned and bold title
        plot.title.position = "plot"  # Optional: Use this to position the title with respect to the entire plot area
      )
    
    ggsave("output/58MP_ing.jpeg", dpi = 1200, width = 11,height =8, units = "cm")
    ggsave("output/58MP_ing.svg", dpi = 1200, width = 11,height =8, units = "cm")

    # plot NP108
    
    NP_25DM_mean %>% 
      
      ggplot(aes(time,mean)) +
      
      geom_point(size=2) +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 5)+
      
      geom_line() +
      
       geom_point(data=NP_25DM,
                 aes(time,conc),
                 size=1,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      scale_x_continuous(limits=c(0,305),breaks=c(0, 10, 20, 40, 60, 90, 
                                                  120, 150, 180, 250, 300), expand = c(0.01, 0)) +
      ggtitle("A: 200 nm NPs ingestion") +
      
      xlab("Ingestion time (min)") +
      
      theme(
        plot.title = element_text(hjust = 0, size = 12, face = "bold"),  # Left-aligned and bold title
        plot.title.position = "plot"  # Optional: Use this to position the title with respect to the entire plot area
      ) +
      
      ylab("NP content per larva (µg Pd)") 
    
    ggsave("output/NP_ing.jpeg", dpi = 1200, width = 11,height = 8, units = "cm")
    ggsave("output/NP_ing.svg", dpi = 1200, width = 11,height = 8, units = "cm")
    

    # MP numbers (eng only) ----- 
    
    MP_25DM_raw$section_dye
    
    str(MP_25DM_raw)
    
    head(MP_25DM_raw)
    
    # plot MP and NP ingestion
    
    # NP: detuct blank from samples
    
    # NP in blank
    
    NP_25DM_blank <- 
      
      NP_25DM_raw %>% 
      
      filter(sample_name == "blank") %>% 
      
      group_by(sample_name) %>% 
      
      summarise(blank_mean=mean(NP_108_ug))
    
    # deduct blank
    
    NP_25DM <- 
      
      NP_25DM_raw %>% 
      
      filter(sample_name == "25DM_MP") %>% 
      
      filter(time < 1000) %>% 
      
      mutate(conc=NP_108_ug-NP_25DM_blank$blank_mean) %>% 
      
      mutate(conc=case_when(conc < 0 ~ 0, TRUE~conc)) %>% 
      
      select(time,type,conc) %>% 
      
      filter(type=="egestion" | time == 300) %>% 
      
      mutate(time=case_when(time== 300 ~ 0,TRUE~time))
      
    
    # prepare data
    
    MP_25DM_count <- 
      
      MP_25DM_raw %>% 
      
      select(time,type,MP15,MP58) %>% 
      
      gather(3:4,key="MP_size",value="count") %>% 
      
      filter(!count=="NA") %>% 
      
      filter(!time==1080) %>% 
      
      mutate(type=factor(type)) %>% 
      
      filter(type=="egestion" | time == 300) %>% 
      
      mutate(time=case_when(time== 300 ~ 0,TRUE~time))
    
     # subset MP15
    
    MP_25DM_count_MP15 <-
      
      MP_25DM_count %>% 
      
      filter(MP_size=="MP15")
    
    
    # subset MP58
    
    MP_25DM_count_MP58 <-
      
      MP_25DM_count %>% 
      
      filter(MP_size=="MP58") %>% 
      
      filter(count < 20)
    
    
    # calculate mean 
    
    # all
    
    MP_25DM_count_mean <- 
      
      MP_25DM_count %>% 
      
      group_by(time,type, MP_size) %>% 
      
      summarise(n=n(),
                mean=mean(count),
                sd=sd(count))
    
    # MP15
    
    MP_25DM_count_mean15 <- 
      
      MP_25DM_count %>% 
      
      filter(MP_size=="MP15") %>% 
      
      group_by(time,type) %>% 
      
      summarise(n=n(),
                mean=mean(count),
                sd=sd(count))
    
    # MP58
    
    MP_25DM_count_mean58 <- 
      
      MP_25DM_count %>% 
      
      filter(MP_size=="MP58") %>% 
      
      group_by(time,type) %>% 
      
      summarise(n=n(),
                mean=mean(count),
                sd=sd(count)) 
    
    # NP108
    
    NP_25DM_mean <-
      
      NP_25DM %>% 
      
      group_by(time,type) %>% 
      
      summarise(n=n(),
                mean=mean(conc),
                sd=sd(conc))
    
    # plot all
    
    MP_25DM_count_mean %>% 
      
      ggplot(aes(time,mean,color=MP_size)) +
      
      geom_point() +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.3)+
      
      geom_line() +
      
      geom_vline(xintercept = 300,linetype="dotdash") +
      
      geom_point(data=MP_25DM_count,
                 aes(time,count),
                 size=2,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      ggtitle("MP 15 and 58 um, dashed line start of egestion") +
      
      xlab("time (min)") +
      
      ylab("MP count (#)") 
    
    
    ggsave("output/allMP.jpeg", dpi = 1200, width = 16,height = 10, units = "cm")
    
    # plot M15
    
    MP_25DM_count_mean15 %>% 
      
      ggplot(aes(time,mean)) +
      
      geom_point(size=2) +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 5)+
      
      geom_line() +
      
      geom_point(data=MP_25DM_count_MP15,
                 aes(time,count),
                 size=1,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      scale_x_continuous(breaks=c(0, 60, 120, 180)) +
      ggtitle("D: 15 μm MPs egestion") +
      
      xlab("Egestion time (min)") +
      
      theme(
        plot.title = element_text(hjust = 0, size = 12, face = "bold"),  # Left-aligned and bold title
        plot.title.position = "plot"  # Optional: Use this to position the title with respect to the entire plot area
      ) +
      
      ylab("MPs per larva")
    
    ggsave("output/15MP_eng.jpeg", dpi = 1200, width = 8,height = 8, units = "cm")
    ggsave("output/15MP_eng.svg", dpi = 1200, width = 8,height = 8, units = "cm")
    
    # plot M58
    
    MP_25DM_count_mean58 %>% 
      
      filter(mean < 20) %>% 
      
      ggplot(aes(time,mean)) +
      
      geom_point(size=2) +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 5)+
      
      geom_line() +
      
      geom_point(data=MP_25DM_count_MP58,
                 aes(time,count),
                 size=1,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      scale_x_continuous(breaks=c(0, 60, 120, 180)) +
      ggtitle("F: 58 μm MPs egestion") +
      
      xlab("Egestion time (min)") +
      
      theme(
        plot.title = element_text(hjust = 0, size = 12, face = "bold"),  # Left-aligned and bold title
        plot.title.position = "plot"  # Optional: Use this to position the title with respect to the entire plot area
      ) +
      
      ylab("MPs per larva") 
    
    ggsave("output/58MP_eng.jpeg", dpi = 1200, width = 8,height =8, units = "cm")
    ggsave("output/58MP_eng.svg", dpi = 1200, width = 8,height =8, units = "cm")
    
    # plot NP108
    
    NP_25DM_mean %>% 
      
      ggplot(aes(time,mean)) +
      
      geom_point(size=2) +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 5)+
      
      geom_line() +
      
      geom_point(data=NP_25DM,
                 aes(time,conc),
                 size=1,
                 position = position_jitter(0.05),
                 alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      scale_x_continuous(breaks=c(0, 60, 120, 180)) +
      ggtitle("B: 200 nm NPs egestion") +
      
      xlab("Egestion time (min)") +
      
      theme(
        plot.title = element_text(hjust = 0, size = 12, face = "bold"),  # Left-aligned and bold title
        plot.title.position = "plot"  # Optional: Use this to position the title with respect to the entire plot area
      ) +
      
      ylab("NP content per larva (µg Pd)") 
      
    ggsave("output/NP_eng.jpeg", dpi = 1200, width = 8,height = 8, units = "cm")
    ggsave("output/NP_eng.svg", dpi = 1200, width = 8,height = 8, units = "cm")
