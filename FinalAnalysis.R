options(java.parameters = "-Xmx1024m")
setwd("C:/Users/isaac/OneDrive/Desktop/Finan2019")
InstallPackages <- function(){
  install.packages("readxl")
  install.packages("tidyverse")
  install.packages("plyr")
  install.packages("ltm")
  install.packages("bestglm")
  install.packages("mice")
}
InstallLibraries <- function(){
  library(readxl)
  library(tidyverse)
  library(plyr)
  library(ltm)
  library(xlsx)
  library(bestglm)
  library(mice)
}
CronbachsAlphaFunction <- function(data, vectorvalues, factorname){
  
  if(factorname == "Demographics"){
    
    
    selectedcolumns <- data[,vectorvalues]
    selectedcolumns <- c(factorname, NA, rep(NA,6))
    selectedcolumns_data <- data.frame(Name=character(),Alpha=numeric,Variable=character(),N=numeric(),Mean=numeric(),Median=numeric(),Minimum=numeric(),Maximum=numeric())
    for(i in vectorvalues){
      tempvect <- c(rep(NA,2),colnames(data[,i]), length(which(!is.na(data[,i]))), round(sapply(data[,i], mean, na.rm=TRUE),5), sapply(data[,i], median, na.rm=TRUE), min(data[,i], na.rm=TRUE), max(data[,i],na.rm=TRUE))
      selectedcolumns_data <- rbind(selectedcolumns_data,tempvect)
    }
    colnames(selectedcolumns_data) <- c('Factors (face value)','Cronbach\'s Alpha','Variable','N','Mean','Median','Minimum','Maximum')
    newfactor <- rbind(selectedcolumns, selectedcolumns_data)
    colnames(newfactor) <- c('Factors (face value)','Cronbach\'s Alpha','Variable','N','Mean','Median','Minimum','Maximum')
    
    
    return(newfactor)
    
    
  }
  
  
  selectedcolumns <- data[,vectorvalues]
  selectedcolumns <- c(factorname, cronbach.alpha(data=selectedcolumns,na.rm=TRUE)[[1]], rep(NA,6))
  selectedcolumns_data <- data.frame(Name=character(),Alpha=numeric,Variable=character(),N=numeric(),Mean=numeric(),Median=numeric(),Minimum=numeric(),Maximum=numeric())
  for(i in vectorvalues){
    tempvect <- c(rep(NA,2),colnames(data[,i]), length(which(!is.na(data[,i]))), round(sapply(data[,i], mean, na.rm=TRUE),5), sapply(data[,i], median, na.rm=TRUE), min(data[,i], na.rm=TRUE), max(data[,i],na.rm=TRUE))
    selectedcolumns_data <- rbind(selectedcolumns_data,tempvect)
  }
  colnames(selectedcolumns_data) <- c('Factors (face value)','Cronbach\'s Alpha','Variable','N','Mean','Median','Minimum','Maximum')
  newfactor <- rbind(selectedcolumns, selectedcolumns_data)
  colnames(newfactor) <- c('Factors (face value)','Cronbach\'s Alpha','Variable','N','Mean','Median','Minimum','Maximum')
  
  return(newfactor)
  
}
MajorGroup <- function(data){
  
  data$QDoubleMajor <- '0'
  data$QDoubleMajor <- 'Double Major'
  
  for(i in 2:nrow(data)){
    if(grepl('undec', data$Q142[i], ignore.case = TRUE) | grepl('unsure', data$Q142[i], ignore.case = TRUE) | grepl('in flux', data$Q142[i], ignore.case = TRUE) |
       grepl('not sure', data$Q142[i], ignore.case = TRUE) | grepl('N/A', data$Q142[i], ignore.case = TRUE) | grepl('open', data$Q142[i], ignore.case = TRUE) |
       grepl('unknown', data$Q142[i], ignore.case = TRUE) | grepl('un dec', data$Q142[i], ignore.case = TRUE) | grepl('know', data$Q142[i], ignore.case = TRUE) |
       grepl('whoever', data$Q142[i], ignore.case = TRUE) | grepl('none', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'UNDECIDED'
    }else if (grepl('business', data$Q142[i], ignore.case = TRUE) | grepl('pre-bu', data$Q142[i], ignore.case = TRUE) | grepl('pre-management', data$Q142[i], ignore.case = TRUE) | grepl('pre management', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'PRE-BUSINESS'
    }else if (grepl('finan', data$Q142[i], ignore.case = TRUE) | grepl('fiance', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'FINANCE'
    }else if (grepl('IS', data$Q142[i], ignore.case = TRUE) | grepl('info', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'INFORMATION SYSTEMS'
    }else if (grepl('mark', data$Q142[i], ignore.case = TRUE) | grepl('mktg', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'MARKETING'
    }else if (grepl('acc', data$Q142[i], ignore.case = TRUE) | grepl('account', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'ACCOUNTING'
    }else if (grepl('strategic', data$Q142[i], ignore.case = TRUE) | grepl('strategy', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'STRATEGIC MANAGEMENT'
    }else if (grepl('chain', data$Q142[i], ignore.case = TRUE) | grepl('supply', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'GLOBAL SUPPLY CHAIN'
    }else if (grepl('ENT', data$Q142[i], ignore.case = FALSE) | grepl('entrepreneur', data$Q142[i], ignore.case = TRUE) | grepl('entreprenuer', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'ENTREPRENEURIAL MANAGEMENT'
    }else if (grepl('poli', data$Q142[i], ignore.case = TRUE) | grepl('poly', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'POLITICAL SCIENCE'
    }else if (grepl('Asian', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'ASIAN STUDIES'
    }else if (grepl('COMD', data$Q142[i], ignore.case = TRUE) | grepl('disorder', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'COMMUNICATION DISORDERS'
    }else if (grepl('Public', data$Q142[i], ignore.case = TRUE) | grepl('PR', data$Q142[i], ignore.case = FALSE)){data$Q142[i] <- 'PUBLIC RELATIONS'
    }else if (grepl('exdm', data$Q142[i], ignore.case = TRUE) | grepl('experience', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'EXDM'
    }else if (grepl('Theater', data$Q142[i], ignore.case = TRUE) | grepl('Acting', data$Q142[i], ignore.case = TRUE) | grepl('Theatre', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'THEATER'
    }else if (grepl('economics', data$Q142[i], ignore.case = TRUE) | grepl('econ', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'ECONOMICS'
    }else if (grepl('psych', data$Q142[i], ignore.case = TRUE) | grepl('psychology', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'PSYCHOLOGY'
    }else if (grepl('cm', data$Q142[i], ignore.case = TRUE) | grepl('construct', data$Q142[i], ignore.case = TRUE) | grepl('cfm', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'CONSTRUCTION MANAGEMENT'
    }else if (grepl('Italian', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'ITALIAN'
    }else if (grepl('Russian', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'RUSSIAN'
    }else if (grepl('Biology', data$Q142[i], ignore.case = TRUE) | grepl('BIO', data$Q142[i], ignore.case = FALSE)){data$Q142[i] <- 'BIOLOGY'
    }else if (grepl('Physics', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'PHYSICS'
    }else if (grepl('socio', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'SOCIOLOGY'
    }else if (grepl('stat', data$Q142[i], ignore.case = TRUE) | grepl('actuarial', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'STATISTICS'
    }else if (grepl('CS', data$Q142[i], ignore.case = TRUE) | grepl('Computer Sci', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'COMPUTER SCIENCE'
    }else if (grepl('acme', data$Q142[i], ignore.case = TRUE) | grepl('math', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'MATHEMATICS'
    }else if (grepl('communications', data$Q142[i], ignore.case = TRUE) | grepl('comm', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'COMMUNICATIONS'
    }else if (grepl('Civil', data$Q142[i], ignore.case = TRUE) | grepl('CIV', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'CIVIL ENGINEERING'
    }else if (grepl('food', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'FOOD SCIENCE'
    }else if (grepl('portug', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'PORTUGUESE'
    }else if (grepl('philos', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'PHILOSOPHY'
    }else if (grepl('it', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'INFORMATION TECHNOLOGY'
    }else if (grepl('mechanical', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'MECHANICAL ENGINEERING'
    }else if (grepl('chemical', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'CHEMICAL ENGINEERING'
    }else if (grepl('manufac', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'MANUFACTURING ENGINEERING'
    }else if (grepl('Chemistry', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'CHEMISTRY'
    }else if (grepl('biochem', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'BIOCHEMISTRY'
    }else if (grepl('int rel', data$Q142[i], ignore.case = TRUE) | grepl('international', data$Q142[i], ignore.case = TRUE) |
              grepl('relations', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'INTERNATIONAL RELATIONS'
    }else if (grepl('hr', data$Q142[i], ignore.case = TRUE) | grepl('human', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'HUMAN RESOURCES'
    }else if (grepl('urban', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'URBAN PLANNING'
    }else if (grepl('landscape', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'LANDSCAPE MANAGEMENT'
    }else if (grepl('french', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'FRENCH'
    }else if (grepl('middle eastern', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'MIDDLE EASTERN STUDIES'
    }else if (grepl('engineering', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'PRE-ENGINEERING'
    }else if (grepl('media', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'MEDIA ARTS'
    }else if (grepl('chinese', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'CHINESE'
    }else if (grepl('neuro', data$Q142[i], ignore.case = TRUE) | grepl('nuero', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'NEUROSCIENCE'
    }else if (grepl('german', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'GERMAN'
    }else if (grepl('design', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'DESIGN'
    }else if (grepl('Family', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'FAMILY STUDIES'
    }else if (grepl('athletic', data$Q142[i], ignore.case = TRUE) | grepl('exercise', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'EXERCISE SCIENCE'
    }else if (grepl('las', data$Q142[i], ignore.case = TRUE) | grepl('latin', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'EXERCISE SCIENCE'
    }else if (grepl('european', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'EUROPEAN STUDIES'
    }else if (grepl('Music', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'MUSIC'
    }else if (grepl('Elementary', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'ELEMENTARY EDUCATION'
    }else if (grepl('geog', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'GEOGRAPHY'
    }else if (grepl('dance', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'DANCE'
    }else if (grepl('dental', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'DENTAL HYGEINE'
    }else if (grepl('GENERAL', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'GENERAL STUDIES'
    }else if (grepl('ANIMATION', data$Q142[i], ignore.case = TRUE)){data$Q142[i] <- 'ANIMATION'
    }else if (data$Q142[i] == ''){data$Q142[i] <- "UNDECIDED"
    }else if (grepl('nurs',data$Q142[i], ignore.case=TRUE)){data$Q142[i] <- 'NURSING'
    }else{print(data$Q142[i])}
    
    if(data$'Net ID'[i] == 'ethansg' | data$'Net ID'[i] == 'lyoung33' | data$'Net ID'[i] == 'tylerjc5' | data$'Net ID'[i] == 'brigbrau'){
      data$QDoubleMajor[i] <- '1'
    }
    
    
  }
  
  
  
  return(data)
}
TransferSchool <- function(data){
  for(i in 2:nrow(data)){
    
    if(data$Q100[i] == ""){data$Q100[i] <- NA
    }else if (grepl('UVU', data$Q100[i], ignore.case = TRUE) | grepl('Utah Valley', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'UTAH VALLEY UNIVERSITY'
    }else if (grepl('BRIGHAM YOUNG UNIVERSITY I', data$Q100[i], ignore.case = TRUE) | grepl('BRIGHAM YOUNG UNIVERSITY- I', data$Q100[i], ignore.case = TRUE) |grepl('BRIGHAM YOUNG UNIVERSITY-I', data$Q100[i], ignore.case = TRUE) | 
              grepl('BYU- Idaho', data$Q100[i], ignore.case = TRUE) | grepl('BYU-I', data$Q100[i], ignore.case = TRUE) | grepl('Brigham Young University - Idaho', data$Q100[i], ignore.case = TRUE) | grepl('BYUI', data$Q100[i], ignore.case = TRUE) | grepl('BYU I', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'BYU IDAHO'
    }else if (grepl('BYU-H', data$Q100[i], ignore.case = TRUE) | grepl('BRIGHAM YOUNG UNIVERSITY HAWAII', data$Q100[i], ignore.case = TRUE) | grepl('BYU Hawaii', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'BYU HAWAII'
    }else if (grepl('SALT LAKE COMMUNITY', data$Q100[i], ignore.case = TRUE) | grepl('SLCC', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'SALT LAKE COMMUNITY COLLEGE'
    }else if (grepl('UNIVERSITY OF UTAH', data$Q100[i], ignore.case = TRUE) | grepl('U OF U', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'UNIVERSITY OF UTAH'
    }else if (grepl('SOUTHERN UTAH UNIVERSITY', data$Q100[i], ignore.case = TRUE) | grepl('sUU', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'BYU HAWAII'
    }else if (grepl('UTAH STATE', data$Q100[i], ignore.case = TRUE) | grepl('USU', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'UTAH STATE UNIVERSITY'
    }else if (grepl('WEBER', data$Q100[i], ignore.case = TRUE) | grepl('WSU', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'WEBER STATE UNIVERSITY'
    }else if (grepl('BYU SLC', data$Q100[i], ignore.case = TRUE) | grepl('BRIGHAM YOUNG sALT', data$Q100[i], ignore.case = TRUE) | grepl('BYU SALT LAKE', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'BYU SALT LAKE CENTER'
    }else if (grepl('LDS BUSINESS COLLEGE', data$Q100[i], ignore.case = TRUE)| grepl('LDS BC', data$Q100[i], ignore.case = TRUE) | grepl('LDSBC', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'LDS BUSINESS COLLEGE'
    }else if (grepl('SNOW COLLEGE', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'SNOW COLLEGE'
    }else if (grepl('DIXIE', data$Q100[i], ignore.case = TRUE) | grepl('DSU', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'DIXIE STATE COLLEGE'
    }else if (grepl('ASU', data$Q100[i], ignore.case = TRUE) | grepl('ARIZONA STATE', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'ARIZONA STATE UNIVERSITY'
    }else if (grepl('AIR FORCE ACADEMY', data$Q100[i], ignore.case = TRUE) | grepl('NAVAL ACADEMY', data$Q100[i], ignore.case = TRUE)){data$Q100[i] <- 'UNITED STATES MILITARY ACADEMY'
    }else {data$Q100[i] <- 'OTHER'}
    
    
  }
  
  
  
  return(data)
}
SplitColumns <- function(data){
  #Q63 -- 5 categories, column 6
  
  Q63_1 <- grepl('1', data$Q63) %>% as.numeric()
  Q63_1[1] <- 'I could only play a certain amount of video games each day.'
  Q63_2 <- grepl('2', data$Q63) %>% as.numeric()
  Q63_2[1] <- 'I could only play video games after fulfilling my responsibilities'
  Q63_3 <- grepl('3', data$Q63) %>% as.numeric()
  Q63_3[1] <- 'I could only play video games if I had good grades.'
  Q63_4 <- grepl('4', data$Q63) %>% as.numeric()
  Q63_4[1] <- 'I could only play video games with my siblings'
  Q63_5 <- grepl('5', data$Q63) %>% as.numeric()
  Q63_5[1] <- 'Other'
  
  df63 <- data.frame(Q63_1, Q63_2, Q63_3, Q63_4, Q63_5)
  
  data <- add_column(data, df63, .after = 'Q63')
  
  #Q133 -- 7
  
  Q133_1 <- grepl('1', data$Q133) %>% as.numeric()
  Q133_1[1] <- 'Early Morning'
  Q133_2 <- grepl('2', data$Q133) %>% as.numeric()
  Q133_2[1] <- 'Morning'
  Q133_3 <- grepl('3', data$Q133) %>% as.numeric()
  Q133_3[1] <- 'Afternoon'
  Q133_4 <- grepl('4', data$Q133) %>% as.numeric()
  Q133_4[1] <- 'Evening'
  Q133_5 <- grepl('5', data$Q133) %>% as.numeric()
  Q133_5[1] <- 'Night'
  Q133_6 <- grepl('6', data$Q133) %>% as.numeric()
  Q133_6[1] <- 'After Midnight'
  
  df133 <- data.frame(Q133_1, Q133_2, Q133_3, Q133_4, Q133_5, Q133_6)
  
  data <- add_column(data, df133, .after = 'Q133')
  
  
  #Q65 -- 6
  
  Q65_1 <- grepl('1', data$Q65) %>% as.numeric()
  Q65_1[1] <- 'Early Morning'
  Q65_2 <- grepl('2', data$Q65) %>% as.numeric()
  Q65_2[1] <- 'Morning'
  Q65_3 <- grepl('3', data$Q65) %>% as.numeric()
  Q65_3[1] <- 'Afternoon'
  Q65_4 <- grepl('4', data$Q65) %>% as.numeric()
  Q65_4[1] <- 'Evening'
  Q65_5 <- grepl('5', data$Q65) %>% as.numeric()
  Q65_5[1] <- 'Night'
  Q65_6 <- grepl('6', data$Q65) %>% as.numeric()
  Q65_6[1] <- 'After Midnight'
  
  df65 <- data.frame(Q65_1, Q65_2, Q65_3, Q65_4, Q65_5, Q65_6)
  
  data <- add_column(data, df65, .after = 'Q65')
  
  
  #Q128 -- 13
  
  Q128_1 <- grepl('1', data$Q128) %>% as.numeric()
  Q128_1[1] <- 'First Person Shooter'
  Q128_2 <- grepl('2', data$Q128) %>% as.numeric()
  Q128_2[1] <- 'Multiplayer Online Battle Arena'
  Q128_3 <- grepl('3', data$Q128) %>% as.numeric()
  Q128_3[1] <- 'Massive Multiplayer Online Game'
  Q128_4 <- grepl('4', data$Q128) %>% as.numeric()
  Q128_4[1] <- 'Sandbox'
  Q128_5 <- grepl('5', data$Q128) %>% as.numeric()
  Q128_5[1] <- 'Online Role Playing'
  Q128_6 <- grepl('6', data$Q128) %>% as.numeric()
  Q128_6[1] <- 'Sports Simulation'
  Q128_7 <- grepl('7', data$Q128) %>% as.numeric()
  Q128_7[1] <- 'Racing'
  Q128_8 <- grepl('8', data$Q128) %>% as.numeric()
  Q128_8[1] <- 'Platformers'
  Q128_9 <- grepl('9', data$Q128) %>% as.numeric()
  Q128_9[1] <- 'Adventure'
  Q128_10 <- grepl('10', data$Q128) %>% as.numeric()
  Q128_10[1] <- 'Strategy'
  Q128_11 <- grepl('11', data$Q128) %>% as.numeric()
  Q128_11[1] <- 'Mobile'
  Q128_12 <- grepl('12', data$Q128) %>% as.numeric()
  Q128_12[1] <- 'Education'
  Q128_13 <- grepl('13', data$Q128) %>% as.numeric()
  Q128_13[1] <- 'Simulations'
  
  df128 <- data.frame(Q128_1, Q128_2, Q128_3, Q128_4, Q128_5, Q128_6, Q128_7, Q128_8, Q128_9, Q128_10, Q128_11, Q128_12, Q128_13)
  
  data <- add_column(data, df128, .after = 'Q128')
  
  #Q66 -- 18
  
  Q66_1 <- grepl('1', data$Q66) %>% as.numeric()
  Q66_1[1] <- 'Action'
  Q66_2 <- grepl('2', data$Q66) %>% as.numeric()
  Q66_2[1] <- 'Adventure'
  Q66_3 <- grepl('3', data$Q66) %>% as.numeric()
  Q66_3[1] <- 'AR Games'
  Q66_4 <- grepl('4', data$Q66) %>% as.numeric()
  Q66_4[1] <- 'Board'
  Q66_5 <- grepl('5', data$Q66) %>% as.numeric()
  Q66_5[1] <- 'Card'
  Q66_6 <- grepl('6', data$Q66) %>% as.numeric()
  Q66_6[1] <- 'Casino'
  Q66_7 <- grepl('7', data$Q66) %>% as.numeric()
  Q66_7[1] <- 'Casual'
  Q66_8 <- grepl('8', data$Q66) %>% as.numeric()
  Q66_8[1] <- 'Family'
  Q66_9 <- grepl('9', data$Q66) %>% as.numeric()
  Q66_9[1] <- 'Indie'
  Q66_10 <- grepl('10', data$Q66) %>% as.numeric()
  Q66_10[1] <- 'Kids'
  Q66_11 <- grepl('11', data$Q66) %>% as.numeric()
  Q66_11[1] <- 'Music'
  Q66_12 <- grepl('12', data$Q66) %>% as.numeric()
  Q66_12[1] <- 'Puzzle'
  Q66_13 <- grepl('13', data$Q66) %>% as.numeric()
  Q66_13[1] <- 'Role Playing'
  Q66_14 <- grepl('14', data$Q66) %>% as.numeric()
  Q66_14[1] <- 'Simulation'
  Q66_15 <- grepl('15', data$Q66) %>% as.numeric()
  Q66_15[1] <- 'Sports'
  Q66_16 <- grepl('16', data$Q66) %>% as.numeric()
  Q66_16[1] <- 'Strategy'
  Q66_17 <- grepl('17', data$Q66) %>% as.numeric()
  Q66_17[1] <- 'Trivia'
  Q66_18 <- grepl('18', data$Q66) %>% as.numeric()
  Q66_18[1] <- 'Word'
  
  df66 <- data.frame(Q66_1, Q66_2, Q66_3, Q66_4, Q66_5, Q66_6, Q66_7, Q66_8, Q66_9, Q66_10, Q66_11, Q66_12, Q66_13, Q66_14, Q66_15, Q66_16, Q66_17, Q66_18)
  
  data <- add_column(data, df66, .after = 'Q66')
  
  #Q131 -- 9
  
  Q131_1 <- grepl('1', data$Q131) %>% as.numeric()
  Q131_1[1] <- 'Desktop Computer'
  Q131_2 <- grepl('2', data$Q131) %>% as.numeric()
  Q131_2[1] <- 'Laptop Computer'
  Q131_3 <- grepl('3', data$Q131) %>% as.numeric()
  Q131_3[1] <- 'Xbox'
  Q131_4 <- grepl('4', data$Q131) %>% as.numeric()
  Q131_4[1] <- 'Playstation'
  Q131_5 <- grepl('5', data$Q131) %>% as.numeric()
  Q131_5[1] <- 'Nintendo'
  Q131_6 <- grepl('6', data$Q131) %>% as.numeric()
  Q131_6[1] <- 'Ipad/Tablet'
  Q131_7 <- grepl('7', data$Q131) %>% as.numeric()
  Q131_7[1] <- 'Phone'
  Q131_8 <- grepl('8', data$Q131) %>% as.numeric()
  Q131_8[1] <- 'VR'
  Q131_9 <- grepl('9', data$Q131) %>% as.numeric()
  Q131_9[1] <- 'Other'
  
  df131 <- data.frame(Q131_1, Q131_2, Q131_3, Q131_4, Q131_5, Q131_6, Q131_7, Q131_8, Q131_9)
  
  data <- add_column(data, df131, .after = 'Q131')
  
  #Q136 -- 9
  
  Q136_1 <- grepl('1', data$Q136) %>% as.numeric()
  Q136_1[1] <- 'Facebook'
  Q136_2 <- grepl('2', data$Q136) %>% as.numeric()
  Q136_2[1] <- 'Instagram'
  Q136_3 <- grepl('3', data$Q136) %>% as.numeric()
  Q136_3[1] <- 'Twitter'
  Q136_4 <- grepl('4', data$Q136) %>% as.numeric()
  Q136_4[1] <- 'Reddit'
  Q136_5 <- grepl('5', data$Q136) %>% as.numeric()
  Q136_5[1] <- 'Snapchat'
  Q136_6 <- grepl('6', data$Q136) %>% as.numeric()
  Q136_6[1] <- 'Pinterest'
  Q136_7 <- grepl('7', data$Q136) %>% as.numeric()
  Q136_7[1] <- 'Tumblr'
  Q136_8 <- grepl('8', data$Q136) %>% as.numeric()
  Q136_8[1] <- 'YouTube'
  Q136_9 <- grepl('9', data$Q136) %>% as.numeric()
  Q136_9[1] <- 'Other'
  
  df136 <- data.frame(Q136_1, Q136_2, Q136_3, Q136_4, Q136_5, Q136_6, Q136_7, Q136_8, Q136_9)
  
  data <- add_column(data, df136, .after = 'Q136')
  
  #Q54 -- 9
  
  Q54_1 <- grepl('1', data$Q54) %>% as.numeric()
  Q54_1[1] <- 'Khan Academy'
  Q54_2 <- grepl('2', data$Q54) %>% as.numeric()
  Q54_2[1] <- 'YouTube Tutorials'
  Q54_3 <- grepl('3', data$Q54) %>% as.numeric()
  Q54_3[1] <- 'Codecademy'
  Q54_4 <- grepl('4', data$Q54) %>% as.numeric()
  Q54_4[1] <- 'Foreign Language'
  Q54_5 <- grepl('5', data$Q54) %>% as.numeric()
  Q54_5[1] <- 'Pluralsight'
  Q54_6 <- grepl('6', data$Q54) %>% as.numeric()
  Q54_6[1] <- 'Udemy'
  Q54_7 <- grepl('7', data$Q54) %>% as.numeric()
  Q54_7[1] <- 'Udacity'
  Q54_8 <- grepl('8', data$Q54) %>% as.numeric()
  Q54_8[1] <- 'Coursera'
  Q54_9 <- grepl('9', data$Q54) %>% as.numeric()
  Q54_9[1] <- 'Other'
  
  df54 <- data.frame(Q54_1, Q54_2, Q54_3, Q54_4, Q54_5, Q54_6, Q54_7, Q54_8, Q54_9)
  
  data <- add_column(data, df54, .after = 'Q54')
  
  #Q71 -- 5
  
  Q71_1 <- grepl('1', data$Q71) %>% as.numeric()
  Q71_1[1] <- 'Myers-Briggs Type Indicator'
  Q71_2 <- grepl('2', data$Q71) %>% as.numeric()
  Q71_2[1] <- 'Keirsey Test'
  Q71_3 <- grepl('3', data$Q71) %>% as.numeric()
  Q71_3[1] <- 'True Colors'
  Q71_4 <- grepl('4', data$Q71) %>% as.numeric()
  Q71_4[1] <- 'Big Five'
  Q71_5 <- grepl('5', data$Q71) %>% as.numeric()
  Q71_5[1] <- 'Other'
  
  df71 <- data.frame(Q71_1, Q71_2, Q71_3, Q71_4, Q71_5)
  
  data <- add_column(data, df71, .after = 'Q71')
}
SplitColumnsVG <- function(data){
  #Q63 -- 5 categories, column 6
  
  Q63_1 <- grepl('1', data$Q63) %>% as.numeric()
  Q63_2 <- grepl('2', data$Q63) %>% as.numeric()
  Q63_3 <- grepl('3', data$Q63) %>% as.numeric()
  Q63_4 <- grepl('4', data$Q63) %>% as.numeric()
  Q63_5 <- grepl('5', data$Q63) %>% as.numeric()
  
  df63 <- data.frame(Q63_1, Q63_2, Q63_3, Q63_4, Q63_5)
  
  data <- add_column(data, df63, .after = 'Q63')
  
  #Q133 -- 7
  
  Q133_1 <- grepl('1', data$Q133) %>% as.numeric()
  Q133_2 <- grepl('2', data$Q133) %>% as.numeric()
  Q133_3 <- grepl('3', data$Q133) %>% as.numeric()
  Q133_4 <- grepl('4', data$Q133) %>% as.numeric()
  Q133_5 <- grepl('5', data$Q133) %>% as.numeric()
  Q133_6 <- grepl('6', data$Q133) %>% as.numeric()
  
  df133 <- data.frame(Q133_1, Q133_2, Q133_3, Q133_4, Q133_5, Q133_6)
  
  data <- add_column(data, df133, .after = 'Q133')
  
  
  #Q65 -- 6
  
  Q65_1 <- grepl('1', data$Q65) %>% as.numeric()
  Q65_2 <- grepl('2', data$Q65) %>% as.numeric()
  Q65_3 <- grepl('3', data$Q65) %>% as.numeric()
  Q65_4 <- grepl('4', data$Q65) %>% as.numeric()
  Q65_5 <- grepl('5', data$Q65) %>% as.numeric()
  Q65_6 <- grepl('6', data$Q65) %>% as.numeric()
  
  df65 <- data.frame(Q65_1, Q65_2, Q65_3, Q65_4, Q65_5, Q65_6)
  
  data <- add_column(data, df65, .after = 'Q65')
  
  
  #Q128 -- 13
  
  Q128_1 <- grepl('1', data$Q128) %>% as.numeric()
  Q128_2 <- grepl('2', data$Q128) %>% as.numeric()
  Q128_3 <- grepl('3', data$Q128) %>% as.numeric()
  Q128_4 <- grepl('4', data$Q128) %>% as.numeric()
  Q128_5 <- grepl('5', data$Q128) %>% as.numeric()
  Q128_6 <- grepl('6', data$Q128) %>% as.numeric()
  Q128_7 <- grepl('7', data$Q128) %>% as.numeric()
  Q128_8 <- grepl('8', data$Q128) %>% as.numeric()
  Q128_9 <- grepl('9', data$Q128) %>% as.numeric()
  Q128_10 <- grepl('10', data$Q128) %>% as.numeric()
  Q128_11 <- grepl('11', data$Q128) %>% as.numeric()
  Q128_12 <- grepl('12', data$Q128) %>% as.numeric()
  Q128_13 <- grepl('13', data$Q128) %>% as.numeric()
  
  df128 <- data.frame(Q128_1, Q128_2, Q128_3, Q128_4, Q128_5, Q128_6, Q128_7, Q128_8, Q128_9, Q128_10, Q128_11, Q128_12, Q128_13)
  
  data <- add_column(data, df128, .after = 'Q128')
  
  #Q66 -- 18
  
  Q66_1 <- grepl('1', data$Q66) %>% as.numeric()
  Q66_2 <- grepl('2', data$Q66) %>% as.numeric()
  Q66_3 <- grepl('3', data$Q66) %>% as.numeric()
  Q66_4 <- grepl('4', data$Q66) %>% as.numeric()
  Q66_5 <- grepl('5', data$Q66) %>% as.numeric()
  Q66_6 <- grepl('6', data$Q66) %>% as.numeric()
  Q66_7 <- grepl('7', data$Q66) %>% as.numeric()
  Q66_8 <- grepl('8', data$Q66) %>% as.numeric()
  Q66_9 <- grepl('9', data$Q66) %>% as.numeric()
  Q66_10 <- grepl('10', data$Q66) %>% as.numeric()
  Q66_11 <- grepl('11', data$Q66) %>% as.numeric()
  Q66_12 <- grepl('12', data$Q66) %>% as.numeric()
  Q66_13 <- grepl('13', data$Q66) %>% as.numeric()
  Q66_14 <- grepl('14', data$Q66) %>% as.numeric()
  Q66_15 <- grepl('15', data$Q66) %>% as.numeric()
  Q66_16 <- grepl('16', data$Q66) %>% as.numeric()
  Q66_17 <- grepl('17', data$Q66) %>% as.numeric()
  Q66_18 <- grepl('18', data$Q66) %>% as.numeric()
  
  df66 <- data.frame(Q66_1, Q66_2, Q66_3, Q66_4, Q66_5, Q66_6, Q66_7, Q66_8, Q66_9, Q66_10, Q66_11, Q66_12, Q66_13, Q66_14, Q66_15, Q66_16, Q66_17, Q66_18)
  
  data <- add_column(data, df66, .after = 'Q66')
  
  #Q131 -- 9
  
  Q131_1 <- grepl('1', data$Q131) %>% as.numeric()
  Q131_2 <- grepl('2', data$Q131) %>% as.numeric()
  Q131_3 <- grepl('3', data$Q131) %>% as.numeric()
  Q131_4 <- grepl('4', data$Q131) %>% as.numeric()
  Q131_5 <- grepl('5', data$Q131) %>% as.numeric()
  Q131_6 <- grepl('6', data$Q131) %>% as.numeric()
  Q131_7 <- grepl('7', data$Q131) %>% as.numeric()
  Q131_8 <- grepl('8', data$Q131) %>% as.numeric()
  Q131_9 <- grepl('9', data$Q131) %>% as.numeric()
  
  df131 <- data.frame(Q131_1, Q131_2, Q131_3, Q131_4, Q131_5, Q131_6, Q131_7, Q131_8, Q131_9)
  
  data <- add_column(data, df131, .after = 'Q131')
  
  
}
SplitColumnsSM <- function(data){
  #Q136 -- 9
  
  Q136_1 <- grepl('1', data$Q136) %>% as.numeric()
  Q136_2 <- grepl('2', data$Q136) %>% as.numeric()
  Q136_3 <- grepl('3', data$Q136) %>% as.numeric()
  Q136_4 <- grepl('4', data$Q136) %>% as.numeric()
  Q136_5 <- grepl('5', data$Q136) %>% as.numeric()
  Q136_6 <- grepl('6', data$Q136) %>% as.numeric()
  Q136_7 <- grepl('7', data$Q136) %>% as.numeric()
  Q136_8 <- grepl('8', data$Q136) %>% as.numeric()
  Q136_9 <- grepl('9', data$Q136) %>% as.numeric()
  
  df136 <- data.frame(Q136_1, Q136_2, Q136_3, Q136_4, Q136_5, Q136_6, Q136_7, Q136_8, Q136_9)
  
  data <- add_column(data, df136, .after = 'Q136')
  
  
}
SplitColumnsBusinessMajor <- function(data){
  
  data$PreACCT <- as.numeric(nrow(data))
  data$PreBusM <- as.numeric(nrow(data))
  data$PreFIN <- as.numeric(nrow(data))
  data$PreIS <- as.numeric(nrow(data))
  data$PreEXDM <- as.numeric(nrow(data))
  data$PreMarketing <- as.numeric(nrow(data))
  data$PreSupplyChain <- as.numeric(nrow(data))
  data$PreStrategicM <- as.numeric(nrow(data))
  data$PreEntMan <- as.numeric(nrow(data))
  
  for(i in 1:nrow(data)){
    if(data$Major[i] == 'ACCOUNTING'){
      data$PreACCT[i] <- 1
    } else{data$PreACCT[i] <- 0}
    
    if(data$Major[i] == "FINANCE"){
      data$PreFIN[i] <- 1
    } else{data$PreFIN[i] <- 0}
    
    if(data$Major[i] == "INFORMATION SYSTEMS"){
      data$PreIS[i] <- 1
    }else{data$PreIS[i] <- 0}
    
    if(data$Major[i] == "PRE-BUSINESS"){
      data$PreBusM[i] <- 1
    }else{data$PreBusM[i] <- 0}
    
    if(data$Major[i] == "EXDM"){
      data$PreEXDM[i] <- 1
    }else{data$PreEXDM[i] <- 0}
    
    if(data$Major[i] == "MARKETING"){
      data$PreMarketing[i] <- 1
    }else{data$PreMarketing[i] <- 0}
    
    if(data$Major[i] == "GLOBAL SUPPLY CHAIN"){
      data$PreSupplyChain[i] <- 1
    }else{data$PreSupplyChain[i] <- 0}
    
    if(data$Major[i] == "STRATEGIC MANAGEMENT"){
      data$PreStrategicM[i] <- 1
    }else{data$PreStrategicM[i] <- 0}
    
    if(data$Major[i] == "ENTREPRENEURIAL MANAGEMENT"){
      data$PreEntMan[i] <- 1
    }else{data$PreEntMan[i] <- 0}
  }
  
  return(data)
}
GetSocialMediaData <- function(data){
  socialmedia <- survey %>% dplyr::select(c('Q135','Q136_1','Q136_2','Q136_3','Q136_4','Q136_5','Q136_6','Q136_7','Q136_8','Q136_9','Q138','Q90'))
  colnames(socialmedia) <- c('UseSocialMedia','Facebook','Instagram','Twitter','Reddit','Snapchat','Pinterest','Tumblr','YouTube','Other','HoursSocialMedia','SocialMediaDuringClass')
  
  
  return(socialmedia)
}
GetVideoGameData <- function(data){
  videogames <- survey %>% dplyr::select(c('Q127','Q61','Q62','Q63_1','Q63_2','Q63_3','Q63_4','Q63_5','Q129','Q133_1','Q133_2','Q133_3','Q133_4','Q133_5','Q133_6','Q65_1','Q65_2','Q65_3','Q65_4','Q65_5','Q65_6','Q50','Q128_1','Q128_2','Q128_3','Q128_4','Q128_5','Q128_6','Q128_7','Q128_8','Q128_9','Q128_10','Q128_11','Q128_12','Q128_13',
                                           'Q66_1','Q66_2','Q66_3','Q66_4','Q66_5','Q66_6','Q66_7','Q66_8','Q66_9','Q66_10','Q66_11','Q66_12','Q66_13','Q66_14','Q66_15','Q66_16','Q66_17','Q66_18','Q134','Q131_1','Q131_2','Q131_3','Q131_4','Q131_5','Q131_6','Q131_7','Q131_8','Q131_9','Q135_1','Q135_2','Q135_3','Q135_4','Q135_5','Q135_6',
                                           'Q135_7','Q135_8','Q135_9','Q135_10','Q135_11','Q135_12','Q135_13','Q135_14','Q135_15'))
  
  
  colnames(videogames) <- c('VGPlayVideoGames','VGAgeStartedPlayingVG','VGParentsLimit','VGParentsCertainAmount','VGParentsAfterResponsibilites','VGParentsGoodGrades','VGParentsWithSiblings','VGParentsOther','VGAvgAmountPlayWeekly','VGWeekdayEarlyMorning','VGWeekdayMorning','VGWeekdayAfternoon','VGWeekdayEvening','VGWeekdayNight','VGWeekdayAfterMidnight',
                            'VGWeekendEarlyMorning','VGWeekendMorning','VGWeekendAfternoon','VGWeekendEvening','VGWeekendNight','VGWeekendAfterMidnight','VGLengthGamingSession','VGFirstPersonShooter','VGMultiplayerOnlineBattleArena','VGMassiveMultiplayerOnlineGame','VGsandbox','VGOnlineRolePlaying','VGSportsSimulation','VGRacing','VGPlatformers','VGAdventure',
                            'VGStrategy','VGMobile','VGEducation','VGSimulations','VGAction','VGAdventure','VGARGames','VGBoard','VGCard','VGCasino','VGCasual','VGFamily','VGIndie','VGKids','VGMusic','VGPuzzle','VGRolePlaying','VGSimulation','VGSports','VGStrategy','VGTrivia','VGWord','VGPlayedMost','VGConsoleDesktopComputer','VGConsoleLaptopComputer','VGConsoleXbox',
                            'VGConsolePlaystation','VGConsoleNintendo','VGConsoleiPad','VGConsolePhone','VGConsoleVR','VGConsoleOther','VGBecauseBoredom','VGBecauseLonely','VGToRelieveStress','VGHappierAfter','VGPlayAlone','VGPlayWithOthers','VGDistractFromResponsibilities','VGHardToStopPlaying','VGBecauseDepressed','VGRatherThanWatchTV','VGMakeMeBored','VGTVMakeMeBored',
                            'VGInterferesWithHomework','VGTVInterferesWithHomework','VGPreferPlayingWithOthers')
  return(videogames)
}

###########################
#### Finance 201 Regression
###########################

#Uncomment and run the next line if you haven't used R in a while
#InstallPackages()
#Uncomment and run the next line at the beginning of your r session ONCE
InstallLibraries()


################################################################################
##################### Reading in and organizing the data #######################
################################################################################

section2019 <- read.csv("Dr. Holmes 2019 Video Games_June 29, 2020_15.56.csv") %>% dplyr::rename("Net ID" = Q2_1)
section2019$Section <- "1"
section2019 <- section2019[-c(2:3),-c(1:17)]

section2020 <- read.csv("Fin 201 Winter 2020 Master Video Games (COVID Edition)_June 26, 2020_20.29.csv") %>% dplyr::rename("Net ID" = Q2_1, "Q139" = Q96)
section2020$Section <- "0"
section2020 <- section2020[-c(2:3),-c(1:17)]
survey <- rbind.fill(section2019, section2020)

section1.2019grades <- read_xlsx("FIN 201 Fall 2019 Final Grade.xlsx", sheet = "Section 1", skip=16)
section2.2019grades <- read_xlsx("FIN 201 Fall 2019 Final Grade.xlsx", sheet = "Section 2", skip=16)

grades2019 <- rbind(section1.2019grades, section2.2019grades) %>% dplyr::select("Net ID", "Grade 2") %>% dplyr::rename("Grade" = 'Grade 2')
grades2020 <- read_xlsx("Winter 2020 grades.xlsx") %>% dplyr::rename("Grade" = 'Course %')

grades <- rbind(grades2019, grades2020)

survey <- left_join(grades, survey, by="Net ID") %>% as_tibble() %>% drop_na(Q_TotalDuration)


survey <- survey %>% dplyr::select(c('Net ID','Grade','Q127','Q61','Q62','Q63','Q64','Q129','Q133','Q65','Q50','Q128','Q66','Q134','Q131','Q132','Q135_1','Q135_2','Q135_3','Q135_4','Q135_5','Q135_6','Q135_7','Q135_8','Q135_9','Q135_10','Q135_11','Q135_12','Q135_13','Q135_14','Q135_15','Q135','Q136','Q137','Q138','Q90','Q51_1','Q51_2','Q51_3','Q51_4','Q51_5','Q51_6','Q51_7','Q51_8','Q51_9','Q51_10','Q51_11','Q53','Q54','Q55','Q56','Q3','Q8','Q9','Q10','Q11','Q111_1','Q111_2','Q111_3','Q111_4','Q111_5','Q111_6','Q111_7','Q111_8','Q111_9','Q111_10','Q111_11','Q111_12','Q111_13','Q111_14','Q139','Q81','Q83','Q84','Q57','Q58','Q86','Q87','Q88','Q89','Q90.1','Q108','Q91','Q92','Q94','Q109','Q95','Q59','Q99','Q100','Q101','Q110','Q121','Q123','Q134.1','Q142','Q166','Q143','Q91.1','Q172_1','Q173','Q174','Q175','Q176','Q177','Q178','Q179','Q180','Q202','Q203','Q144','Q89.1','Q70','Q71','Q74','Q75','Q72','Q73','Q76','Q140','Q69','Q94.1','Q95.1','Q_TotalDuration','Section','Q144'))


questions <- c('Please enter your NetID','Class Grade','Do you play video games (can be on phone, laptop, console, etc.)?','What age did you start playing video games?','Did your parents regularly limit how much video games you played? (ex: limited amount of daily play time, chores had to be finished first, etc.)','How did your parents limit how much you played video games? (Select all that apply)','If you answered Other, please explain how your parents limited how much you played video games:', 'What is the average amount of time you currently spend playing video games weekly? (in hours)','What time(s) of day do you typically play video games on weekdays?','What time(s) of day do you typically play video games on weekends?','What is the typical length of a gaming session?','What types of video games do you play?','What types of mobile games do you play?','What video game do you play the most?','What console do you play video games on?','If you selected ""Other"" what console do you use?','Rate the following statements on how they apply to you: - I would consider playing video games if I felt bored.','Rate the following statements on how they apply to you: - I would consider playing video games if I felt lonely.','Rate the following statements on how they apply to you: - I would consider playing video games if I felt depressed.','Rate the following statements on how they apply to you: - I would consider playing video games to relieve stress.','Rate the following statements on how they apply to you: - I feel happier after playing video games.','Rate the following statements on how they apply to you: - I play video games when I am alone.',
               'Rate the following statements on how they apply to you: - I play video games with others watching me play or playing with me.','Rate the following statements on how they apply to you: - I feel when I play video games it distracts me from my normal responsibilities.','Rate the following statements on how they apply to you: - I find it difficult to stop playing video games after I start.','Rate the following statements on how they apply to you: - I would rather play video games than watch Hulu, Netflix, TV, etc.','Rate the following statements on how they apply to you: - I get bored playing video games','Rate the following statements on how they apply to you: - I get bored watching Hulu, Netflix, TV, etc.','Rate the following statements on how they apply to you: - I feel when I  play video games it interferes with homework or studying.','Rate the following statements on how they apply to you: - I feel when I watch Hulu, Netflix, TV, etc. it interferes with homework or studying.','Rate the following statements on how they apply to you: - I prefer playing video games with others than playing by myself.','Do you use Social Media (like Facebook, Twitter, Instagram, etc.)?','What social media platforms do you use? (Select all that apply)','If you selected other, please list which social media platforms you use:','How many hours do you use social media daily? (on average)','Do you use social media during class?','Rate the following statements on how they apply to you: - I feel satisfied with my academic efforts.','Rate the following statements on how they apply to you: - I feel satisfied with my academic performance.',
               'Rate the following statements on how they apply to you: - I feel like I understand my course material.','Rate the following statements on how they apply to you: - I learn best when in class.','Rate the following statements on how they apply to you: - I learn best outside of class through personal study.','Rate the following statements on how they apply to you: - I attend all of my classes.','Rate the following statements on how they apply to you: - I feel supported by my peers in my academic efforts.','Rate the following statements on how they apply to you: - I feel my professors want me to succeed.','Rate the following statements on how they apply to you: - I care more about learning material than GPA.','Rate the following statements on how they apply to you: - I feel having my laptop or phone open during class is a distraction.','Rate the following statements on how they apply to you: - I play video games during class.','How often do you use online learning resources outside of regular course instruction (like Khan Academy, YouTube tutorials, etc.) that are not part of assigned material?','What online learning sources do you use?','If you chose other, please list which online learning sources you use:','Regarding online learning sources, how often do you choose to learn about something not required for your coursework?','What is the highest score you received on the ACT?','What is the highest score you received on the SAT? (if you took the SAT before 2005, select ""I took the SAT before 2005"". Otherwise, please select the range in which your highest score falls).','What is your high school unweighted GPA (4.0 maximum)?',
               'How many credit hours are you taking this semester?','Do you have a letter on file with the University Accessibility Center (UAC) that provides academic accommodations to you?','Please select how much you agree or disagree with each of the following statements. - I have confidence in my math ability.','Please select how much you agree or disagree with each of the following statements. - I believe I have good math skills.','Please select how much you agree or disagree with each of the following statements. - I am determined to do well in my business courses because I want to achieve my academic goals.','Please select how much you agree or disagree with each of the following statements. - I am determined to do well in my business courses because I want to pursue a career in business.','Please select how much you agree or disagree with each of the following statements. - I am determined to do well in my business courses because I am interested in learning new subjects.','Please select how much you agree or disagree with each of the following statements. - I get the emotional help and support I need from my family.','Please select how much you agree or disagree with each of the following statements. - My friends really try to help me.','Please select how much you agree or disagree with each of the following statements. - I feel I can handle many things at a time.','Please select how much you agree or disagree with each of the following statements. - When I am in a difficult situation, I can usually find my way out of it.','Please select how much you agree or disagree with each of the following statements. - My belief in myself gets me through hard times.',
               'Please select how much you agree or disagree with each of the following statements. - I perform equally well in large classes and small classes.','Please select how much you agree or disagree with each of the following statements. - Class size affects my overall performance -- the smaller the class the better.','Please select how much you agree or disagree with each of the following statements. - I take good notes in class.','Please select how much you agree or disagree with each of the following statements. - Studying my notes improves my performance in class.','Do you regularly eat car batteries for breakfast?','What is your age?','What is your marital status?','Do you have children?','Has having children changed the amount of time you played video games?','How has your video game play time changed since having children?','Have you served a full-time mission for The Church of Jesus Christ of Latter-Day Saints?','Is English your native/first language?','What time do you typically go to sleep at night?','When do you generally wake up?','How many hours each week have you spent at a paid job this semester?','How many hours each week do you spend serving in your religious or spiritual community?','Are you a Collegiate Athlete?','Did you attend any or all of your sports games during this semester?','Have you received an athletic scholarship?','Approximately how many hours did you spend each week participating in extracurricular activities (for example, ROTC, performing arts groups, membership or leadership in campus clubs, etc.)?','Have you received an academic scholarship?','Are you currently on academic scholarship?',
               'Did you transfer to BYU from another university?','From which school did you transfer?','Which semester did you begin studying at BYU?','What is your BYU GPA (4.00 maximum)?','To which gender identity do you most identify?','How do you identify your race?','Are you a Pre-business Major, Business Minor, or neither?','What is your major?','Do you have a parent, sibling, or close associate who works in this field?','How often did you attend this class this semester?','Through the semester, did this class have assigned readings?','What percentage of the assigned readings do you generally have completed before attending this class? - % Readings Completed Before Class','How many hours per non-exam week do you study outside of class for this class?','How many hours per exam week do you study outside of class for this class?','How often do you use TA Office Hours for this class?','How often do you use tutors (non-TAs) for this class?','Are you in a study group for this class?','How many people (yourself included) regularly participate in your study group?','How did you find one another to form your group?','Do you teach other people principles from the class in order to help you learn the material from class?','What is your personal level of interest in this class topic?','How well do you rate your skills in this class subject matter?','Are you retaking this class?','Did you happen to experience a traumatic life event right before or during this semester (for example, death of a loved one, your own divorce or divorce of your parents, serious illness of self or immediate family member)?','Have you ever taken a personality test?',
               'Please select which all of the following personality tests you have taken and remember your personality result:','What was your primary result for the True Colors test?','What was your primary result for the Big Five Personality test?','What was your result for the MBTI?','What was your primary result for the Keirsey test?','What other personality tests have you taken?','Are you still engaged in taking this survey?','If you have any other feelings about video game usage that you have not been able to express in this survey, please express them here:','How has COVID-19 impacted your video game habits?','How has COVID-19 impacted your study habits?','Q_TotalDuration', 'Which section are you in?','Are you Retaking this class?')


survey <- rbind(questions, survey)

## Here we drop observations that didn't pass our "still paying attention" indicators
survey <- survey[survey$Q140 != 0,]
survey <- survey[survey$Q139 != 1,]

for(i in 2:nrow(survey)){
  if(!is.na(survey$Q110[i])){
    if(survey$Q110[i] == '8.6'){
      survey$Q110[i] <- '3.6'
    } else if(survey$Q110[i] == '8.82'){
      survey$Q110[i] <- '3.82'
    }
  }
}

survey <- SplitColumns(survey)

survey <- MajorGroup(survey)
survey <- TransferSchool(survey)

survey$Q142 <- as.factor(survey$Q142)
survey$Q142 <- relevel(survey$Q142, ref='UNDECIDED')

survey$Q100 <- as.factor(survey$Q100)
survey$Q100 <- relevel(survey$Q100, ref='OTHER')

socialmedia <- GetSocialMediaData(survey)
videogames <- GetVideoGameData(survey)

#Here are the columns we want for the replication
survey <- survey %>% dplyr::select(c('Net ID','Grade','Q142','Q95','Q59','Q3','Q81','Q94','Q143','Q111_10','Q111_3','Q111_4','Q111_5','Q134.1','Q110','Q84','Q91','Q111_1','Q10','Q111_6','Q87','Q111_11','Q109','Q166','Q111_7','Q92','Q111_13','Q121','Q175','Q111_8','Q174','Q90.1','Q108','Q173','Q9','Q202','Q83','Q111_2','Q86','Q111_14','Q111_9','Q178','Q123','Q172_1','Q8','Q203','Q111_12','Q177','Q180','Q99','Q89.1','Q176','Q11','Q88','Q89','Q127','Q135','Q138','Q90','Section', 'Q144'))

colnames(survey) <- c('Net ID','Grade','Major','PrevAcaSchol','AcaSchol','ACT','Age','AthlSchol','AttndClass','BelifHar','BusAcadm','BusCarr','BusLearn','BusMinor','CollegeGPA','Children','CollAthl', 'ConfMath','CredHrs','EmHlpFam','EngFirst','EQLar_Sm','ExtraAct','FamInSub','FrndHelp','GameInSem','GdNotes','Gender','HlpDesk','HndlMany','HrsStudyExamWeek','HrPaidJb','HrRelSer','HrsStudyNonExamWeek','HSGPA','IntSub','Marital','MathSkill','Mission','NotePer','OutDiff','PplGp','Race','ReadingBeforeClass','SAT','ClassSkill','SmBetter','StGrp','TeachPrinciples','Transfer','Trauma','Tutor','UAC','WhnSleep','WhnWake','Gamer','SocialMedia','HrsSocialMedia','SocialMediaClassTime','Section','Retake')

survey <- survey[2:nrow(survey),]

survey$BusinessMajor <- as.numeric(nrow(survey))
survey$BusinessMinor <- as.numeric(nrow(survey))
survey$OtherMajor <- as.numeric(nrow(survey))

for(i in 1:nrow(survey)){
  if(survey$BusMinor[i] == '1'){survey$BusinessMajor[i] <- 1
  } else{survey$BusinessMajor[i] <- 0}
  
  if(survey$BusMinor[i] == '2'){survey$BusinessMinor[i] <- 1
  } else{survey$BusinessMinor[i] <- 0}
  
  if(survey$BusMinor[i] == '0' | survey$BusMinor[i] == '1'){survey$OtherMajor[i] <- 1
  } else{survey$OtherMajor[i] <- 0}
}

survey <- SplitColumnsBusinessMajor(survey) %>% dplyr::select(-c(BusMinor)) %>% relocate('Section', .after='PreEntMan')

survey[,c(4:72)] <- sapply(survey[,c(4:72)], as.numeric)

survey$AcaSchol[is.na(survey$AcaSchol)] <- 0
survey$AthlSchol[is.na(survey$AthlSchol)] <- 0
survey$GameInSem[is.na(survey$GameInSem)] <- 0
survey$PplGp[is.na(survey$PplGp)] <- 0
survey$SAT[survey$SAT==1] <- NA

surveyfinal <- survey

################################################################################
############################## Cronbach's Alphas ###############################
################################################################################

cronbachsalphas <- data.frame(Identifier=character(), CronbachsAlpha=numeric(), Variable=character(), N=numeric(), Mean=numeric(), Median=numeric(), Minimum=numeric(), Maximum=numeric())

#greater than .7
drive <- CronbachsAlphaFunction(survey, 11:13, "Drive")
selfbelief <- CronbachsAlphaFunction(survey, c(10,29,40), "Self Belief")
subjectmatter <- CronbachsAlphaFunction(survey, c(35,45), "Subject Matter")
math <- CronbachsAlphaFunction(survey, c(17,37), "Math")
intelligence <- CronbachsAlphaFunction(survey, c(5,6,44), "Intelligence")
athlete <- CronbachsAlphaFunction(survey, c(8,16,25), "Athlete")
notes <- CronbachsAlphaFunction(survey, c(26,39), "Notes")
studygroup <- CronbachsAlphaFunction(survey, c(41,47), "Study Group")
sleep <- CronbachsAlphaFunction(survey, c(53,54), "Sleep")

#greater than .6
support <- CronbachsAlphaFunction(survey, c(19,24), "Support")


#less than .6
businessmajor <- CronbachsAlphaFunction(survey, c(63:71), "Business Major")
classsize <- CronbachsAlphaFunction(survey, c(21,46), "Class Size")
outsidestress <- CronbachsAlphaFunction(survey, c(50,52), "Outside Stress")
family <- CronbachsAlphaFunction(survey, c(15,36), "Family")
bandwidth <- CronbachsAlphaFunction(survey, c(18,22,31,32), "Bandwidth")
exposuretosubject <- CronbachsAlphaFunction(survey, c(23,60), "Exposure To Subject")
indiveffort <- CronbachsAlphaFunction(survey, c(9,30,33), "Individual Effort")
demographic <- CronbachsAlphaFunction(survey, c(7,20,27,38,42), "Demographics")
gpa <- CronbachsAlphaFunction(survey, c(14,34), "GPA")
univresources <- CronbachsAlphaFunction(survey, c(28,51,59), "University Resources and Opportunities")
other <- CronbachsAlphaFunction(survey, c(4,43,48,49,61,62,72), "Other Variables")

originalcronbachalphas <- rbind(drive,sleep,selfbelief,subjectmatter,math,intelligence,athlete,notes,studygroup,support,businessmajor,classsize,outsidestress,
                                family, bandwidth,exposuretosubject,indiveffort,demographic,gpa,univresources,other)

################
### SM Cron A's
################
socialmediafinal <- socialmedia

socialmedia <- socialmedia[2:nrow(socialmedia),]
socialmedia <- sapply(socialmedia, as.numeric) %>% as_tibble()

socialmediacronbachalphas <- CronbachsAlphaFunction(socialmedia, c(1:12), "Social Media")

################
### VG Cron A's
################

videogames <- videogames[2:nrow(videogames),]
videogames[,1:78] <- sapply(videogames[,1:78], as.numeric) %>% as_tibble()

videogamesfinal <- videogames

vgparentlimit <- CronbachsAlphaFunction(videogames, c(3:8), "Parents Limited Video Games")
vgtype <- CronbachsAlphaFunction(videogames, c(23:53), "Video Game Type")
vgearlymorning <- CronbachsAlphaFunction(videogames, c(10,16), "Early Morning")
vgnighttime <- CronbachsAlphaFunction(videogames, c(14,20), "Nighttime")
vgtimeandtype <- CronbachsAlphaFunction(videogames, c(11,12,13,17,18,19,21,56,57,58,59,61), "Time and Type")
vgemotion <- CronbachsAlphaFunction(videogames, c(64:72), "Emotional Factors in Playing Video Games")
vginterfereswithhomework <- CronbachsAlphaFunction(videogames, c(76:77), "TV and VG Interferes with Homework")
vgother <- CronbachsAlphaFunction(videogames, c(1,2,9,22,55,60,62,63,73,74,75,78), "Other")

vgcronbachalphas <- rbind(vgparentlimit, vgtype, vgearlymorning, vgnighttime, vgtimeandtype, vgemotion,
                          vginterfereswithhomework,vgother)



cronbachsalphas <- rbind(cronbachsalphas,originalcronbachalphas,socialmediacronbachalphas,vgcronbachalphas)


################################################################################
##################### Figures and Tables from the original paper ###############
################################################################################

### Here we calculate the number of each major in the classes
majorcount <- count(survey$Major)
majorcount <- cbind(majorcount[1:(nrow(majorcount)/2),],majorcount[(nrow(majorcount)/2+1):nrow(majorcount),])
colnames(majorcount) <- c("Emphasis","#","Emphasis","#")

### Here we get a list of all the variable names
survey <- cbind(survey,videogames,socialmedia)

alpha_names <- sort(colnames(survey))

variablenames <- cbind(colnames(survey)[1:(length(colnames(survey))/2)],colnames(survey)[((length(colnames(survey))/2)+1):length(colnames(survey))]) %>% as_tibble()
variablenames <- cbind(variablenames[,1],rep(NA,nrow(variablenames)),variablenames[,2],rep(NA,nrow(variablenames)))
first_column <- c('')



colnames(variablenames) <- c('Variable','Description','Variable','Description')

##################################################################################
############################ Spearman Correlations ###############################
##################################################################################

## First subset all the quantitative variables
survey <- survey[,c(2,4:125,127:162)]
survey <- sapply(survey[,1:ncol(survey)], as.numeric)

positivecorrelation <- data.frame('Variable Name'=character(), 'Rho'=numeric(), 'P-Value'=numeric(), 'N'=numeric())
negativecorrelation <- data.frame('Variable Name'=character(), 'Rho'=numeric(), 'P-Value'=numeric(), 'N'=numeric())
neutralcorrelation <- data.frame('Variable Name'=character(), 'Rho'=numeric(), 'P-Value'=numeric(), 'N'=numeric())

for(i in 2:ncol(survey)){
  varname <- colnames(survey)[i]
  rho <- round(cor.test(x=survey[,1], y=survey[,i],method = 'spearman')[[4]][[1]],3)
  p <- round(cor.test(x=survey[,1], y=survey[,i],method = 'spearman')[[3]][[1]],3)
  N <- length(which(!is.na(survey[,i])))
  
  if(is.na(p)){
    neutralcorrelation <- rbind(neutralcorrelation, c(varname, rho, p, N))
  }else if(p < 0.1 & rho > 0){
    positivecorrelation <- rbind(positivecorrelation, c(varname, rho, p, N))
  } else if(p < 0.1 & rho < 0){
    negativecorrelation <- rbind(negativecorrelation, c(varname, rho, p, N))
  } else {
    neutralcorrelation <- rbind(neutralcorrelation, c(varname, rho, p, N))
  }
  
}

##Adjusting number of rows so we can cbind
nrow(positivecorrelation)
nrow(negativecorrelation)
nrow(neutralcorrelation)

positivecorrelation <- rbind(c('Variable Name', 'Rho', 'P-Value', 'N'), positivecorrelation)
negativecorrelation <- rbind(c('Variable Name', 'Rho', 'P-Value', 'N'), negativecorrelation)
neutralcorrelation <- rbind(c('Variable Name', 'Rho', 'P-Value', 'N'), neutralcorrelation)

for(i in 1:(nrow(neutralcorrelation)-nrow(positivecorrelation))){
  positivecorrelation <- rbind(positivecorrelation, rep(NA,4))
}
for(i in 1:(nrow(neutralcorrelation)-nrow(negativecorrelation))){
  negativecorrelation <- rbind(negativecorrelation, rep(NA,4))
}

spearmancorrelations <- cbind(negativecorrelation, positivecorrelation, neutralcorrelation)
colnames(spearmancorrelations) <- c('Negative Correlation', NA, NA, NA, 'Positive Correlation', NA, NA, NA, 'Insignificant Correlation', NA, NA, NA)



################################################################################
################################### T-tests ####################################
################################################################################

#############################
## Split A and non A students
#############################

#####
## A
#####
survey <- survey %>% as_tibble()
AStudents <- subset(survey, Grade>=.94)
NonAStudents <- subset(survey, Grade<.94)

Ttests <- data.frame('Variable'=character(), 'N_Astudents'=numeric(), 'Mean_Astudents'=numeric(), 'N_NonAstudents'=numeric(), 'Mean_NonAstudents'=numeric(), 'p-value'=numeric(), 'Difference'=numeric())

for(i in 2:ncol(survey)){
  varname <- colnames(survey)[i]
  n_astudents <- length(which(!is.na(AStudents[,i])))
  mean_astudents <- round(sapply(AStudents[,i], mean, na.rm=TRUE),3)
  n_nonastudents <- length(which(!is.na(NonAStudents[,i])))
  mean_nonastudents <- round(sapply(NonAStudents[,i], mean, na.rm=TRUE),3)
  pvalue <- round(t.test(AStudents[,i],NonAStudents[,i])$p.value,3)
  difference <- round(mean_astudents - mean_nonastudents,3)
  Ttests <- rbind(Ttests, c(varname,n_astudents,mean_astudents,n_nonastudents,mean_nonastudents,pvalue,difference))
}

colnames(Ttests) <- c('Variable Name', 'N (A Students)', 'Mean (A Students)', 'N (Non A Students)', 'Mean (Non A Students)', 'P-Value', 'Difference')

#######################################################################
############## Here we'll make the dataframe we'll use for regressions
#######################################################################

survey <- surveyfinal
videogames <- videogamesfinal
socialmedia <- socialmediafinal[2:nrow(socialmediafinal),]

#General Variables
Drive <- survey[,11:13] %>% transmute(Drive = BusAcadm + BusCarr + BusLearn) %>% as.data.frame()
SelfBelief <- survey[,c(10,29,40)] %>% transmute(SelfBelief = BelifHar + HndlMany + OutDiff) %>% as.data.frame()
SubjectMatter <- survey[,c(35,45)] %>% transmute(SubjectMatter = IntSub + ClassSkill) %>% as.data.frame() 
Math <- survey[,c(17,37)] %>% transmute(Math = ConfMath + MathSkill) %>% as.data.frame()
Intelligence <- survey[,c(5,6,44)] %>% transmute(Intelligence = rowMeans(survey[,c(6,44)], na.rm=TRUE) + AcaSchol) %>% as.data.frame()
Athlete <- survey[,c(8,16,25)] %>% transmute(Athlete = AthlSchol + CollAthl + GameInSem) %>% as.data.frame()
Notes <- survey[,c(26,39)] %>% transmute(Notes = GdNotes + NotePer) %>% as.data.frame()
StudyGroup <- survey[,c(41,47)] %>% transmute(StudyGroup = PplGp) %>% as.data.frame()
Sleep <- survey[,c(53,54)] %>% transmute(AmountSleep = WhnWake - WhnSleep) %>% as.data.frame()

#Video Game Variable
ParentsLimit <- videogames[,4:8] %>% transmute(ParentLimit = rowSums(.)) %>% as.data.frame()
VideoGameType <- videogames[,23:53] %>% mutate(GameType = rowSums(.)) %>% dplyr::select(32) %>% as.data.frame()
EarlyMorning <- videogames[,c(10,16)] %>% transmute(EarlyMorningPlay = VGWeekdayEarlyMorning + VGWeekendEarlyMorning) %>% as.data.frame()
NightTime <- videogames[,c(14,20)] %>% transmute(NightTimePlay = VGWeekdayNight + VGWeekendNight) %>% as.data.frame()
TimeAndType <- videogames[,c(11,12,13,17,18,19,21,56,57,58,59,61)] %>% transmute(TimeandType = rowSums(.)) %>% as.data.frame()
EmotionsWithGames <- videogames[,64:72] %>% transmute(Emotions = rowSums(.))  %>% as.data.frame()
TVandVGandHomework <- videogames[,76:77] %>% transmute(HomeworkDistraction = rowSums(.)) %>% as.data.frame()

NonFactors <- cbind(survey[,c(4,7,9,14,15,19,20,21,22,27,23,24,28,30,31,32,33,34,36,38,42,43,46,48,49,50,51,52,59,60,61,62,63:71,72)],
                    videogames[c(1,2,9,18,22,55,60,62,63,73,74,75,78)],socialmedia)

factors <- cbind(Drive,SelfBelief,SubjectMatter,Math,Intelligence,Athlete,Notes,StudyGroup,Sleep,ParentsLimit,
                 VideoGameType,EarlyMorning,NightTime,TimeAndType,EmotionsWithGames,TVandVGandHomework)
colnames(factors) <- c('Drive','SelfBelief','SubjectMatter','Math','Intelligence','Athlete','Notes','StudyGroup','Sleep','ParentsLimit',
                       'VideoGameType','EarlyMorning','NightTime','TimeAndType','EmotionsWithGames','TVandVGandHomework')

readytoregress <- cbind(survey[,2],Drive,SelfBelief,SubjectMatter,Math,Intelligence,Athlete,Notes,StudyGroup,Sleep,ParentsLimit,
                        VideoGameType,EarlyMorning,NightTime,TimeAndType,EmotionsWithGames,TVandVGandHomework,NonFactors) %>% dplyr::select(-c(46,50)) %>% as_tibble()



readytoregress <- as.data.frame(readytoregress)


###############################################
###### SumStats
###############################################

sumstats <- data.frame(Variable=character(),N=numeric(),Mean=numeric(),Median=numeric(),Minimum=numeric(),Maximum=numeric())
for(i in 4:71){
  
  tempvect <- c(colnames(survey[,i]), length(which(!is.na(survey[,i]))), sapply(survey[,i], mean, na.rm=TRUE), sapply(survey[,i], median, na.rm=TRUE), min(survey[,i], na.rm=TRUE), max(survey[,i],na.rm=TRUE))
  sumstats <- rbind(sumstats,tempvect)
  
}
colnames(sumstats) <- c('Variable','N','Mean','Median','Minimum','Maximum')



##############################################################################
########################### Regressions ######################################
##############################################################################

readytoregress$HoursSocialMedia <- as.numeric(readytoregress$HoursSocialMedia)
readytoregress$SocialMediaDuringClass <- as.numeric(readytoregress$SocialMediaDuringClass)
readytoregress$Facebook <- as.numeric(readytoregress$Facebook)
readytoregress$Instagram <- as.numeric(readytoregress$Instagram)
readytoregress$Twitter <- as.numeric(readytoregress$Twitter)
readytoregress$Reddit <- as.numeric(readytoregress$Reddit)
readytoregress$Snapchat <- as.numeric(readytoregress$Snapchat)
readytoregress$Pinterest <- as.numeric(readytoregress$Pinterest)
readytoregress$Tumblr <- as.numeric(readytoregress$Tumblr)
readytoregress$YouTube <- as.numeric(readytoregress$YouTube)
readytoregress$Other <- as.numeric(readytoregress$Other)


for(i in 1:nrow(readytoregress)){
  if(readytoregress$Facebook[i]+readytoregress$Instagram[i]+readytoregress$Twitter[i]+readytoregress$Reddit[i]+readytoregress$Snapchat[i]+readytoregress$Pinterest[i]+readytoregress$Tumblr+
     readytoregress$YouTube[i]+readytoregress$Other[i] == 0){
    readytoregress$HoursSocialMedia[i] <- 0
    readytoregress$SocialMediaDuringClass[i] <- 0
  }
}

#First we have to make the categorical variables factors!

readytoregress <- readytoregress %>% dplyr::select(-c(BusinessMajor, OtherMajor, UseSocialMedia)) %>% as_tibble()
readytoregress$Grade <- as.numeric(readytoregress$Grade)
readytoregress$PrevAcaSchol <- as.factor(readytoregress$PrevAcaSchol)
readytoregress$Children <- as.factor(readytoregress$Children)
readytoregress$FamInSub <- as.factor(readytoregress$FamInSub)
readytoregress$Gender <- as.factor(readytoregress$Gender) %>% relevel(ref='0')
readytoregress$EngFirst <- as.factor(readytoregress$EngFirst)
readytoregress$Marital <- as.factor(readytoregress$Marital)
readytoregress$Mission <- as.factor(readytoregress$Mission)
readytoregress$Race <- as.factor(readytoregress$Race) %>% relevel(ref='6')
readytoregress$TeachPrinciples <- as.factor(readytoregress$TeachPrinciples)
readytoregress$Transfer <- as.factor(readytoregress$Transfer)
readytoregress$Trauma <- as.factor(readytoregress$Trauma)
readytoregress$UAC <- as.factor(readytoregress$UAC)
readytoregress$BusinessMinor <- as.factor(readytoregress$BusinessMinor)
readytoregress$PreBusM <- as.factor(readytoregress$PreBusM)
readytoregress$PreFIN <- as.factor(readytoregress$PreFIN)
readytoregress$PreIS <- as.factor(readytoregress$PreIS)
readytoregress$PreEXDM <- as.factor(readytoregress$PreEXDM)
readytoregress$PreMarketing <- as.factor(readytoregress$PreMarketing)
readytoregress$PreSupplyChain <- as.factor(readytoregress$PreSupplyChain)
readytoregress$PreStrategicM <- as.factor(readytoregress$PreStrategicM)
readytoregress$PreEntMan <- as.factor(readytoregress$PreEntMan)
readytoregress$Section <- as.factor(readytoregress$Section)
readytoregress$VGWeekendAfternoon <- as.factor(readytoregress$VGWeekendAfternoon)
readytoregress$VGConsoleDesktopComputer <- as.factor(readytoregress$VGConsoleDesktopComputer)
readytoregress$VGConsoleiPad <- as.factor(readytoregress$VGConsoleiPad)
readytoregress$VGConsoleVR <- as.factor(readytoregress$VGConsoleVR)
readytoregress$VGConsoleOther <- as.factor(readytoregress$VGConsoleOther)
readytoregress$HoursSocialMedia <- as.numeric(readytoregress$HoursSocialMedia)
readytoregress$SocialMediaDuringClass <- as.numeric(readytoregress$SocialMediaDuringClass) %>% as.factor()
readytoregress$Facebook <- as.numeric(readytoregress$Facebook) %>% as.factor()
readytoregress$Instagram <- as.numeric(readytoregress$Instagram) %>% as.factor()
readytoregress$Twitter <- as.numeric(readytoregress$Twitter) %>% as.factor()
readytoregress$Reddit <- as.numeric(readytoregress$Reddit) %>% as.factor()
readytoregress$Snapchat <- as.numeric(readytoregress$Snapchat) %>% as.factor()
readytoregress$Pinterest <- as.numeric(readytoregress$Pinterest) %>% as.factor()
readytoregress$Tumblr <- as.numeric(readytoregress$Tumblr) %>% as.factor()
readytoregress$YouTube <- as.numeric(readytoregress$YouTube) %>% as.factor()
readytoregress$Other <- as.numeric(readytoregress$Other) %>% as.factor()



################################################################
##### This function is to remove observations with too many NAs
################################################################

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
readytoregress <- delete.na(readytoregress, 20)

#imputation of data
readytoregress$Drive[which(is.na(readytoregress$Drive))] <- mean(readytoregress$Drive, na.rm=TRUE)
readytoregress$SelfBelief[which(is.na(readytoregress$SelfBelief))] <- mean(readytoregress$SelfBelief, na.rm=TRUE)
readytoregress$SubjectMatter[which(is.na(readytoregress$SubjectMatter))] <- mean(readytoregress$SubjectMatter, na.rm=TRUE)
readytoregress$Math[which(is.na(readytoregress$Math))] <- mean(readytoregress$Math, na.rm=TRUE)
readytoregress$Intelligence[which(is.na(readytoregress$Intelligence))] <- mean(readytoregress$Intelligence, na.rm=TRUE)
readytoregress$Athlete[which(is.na(readytoregress$Athlete))] <- mean(readytoregress$Athlete, na.rm=TRUE)
readytoregress$Notes[which(is.na(readytoregress$Notes))] <- mean(readytoregress$Notes, na.rm=TRUE)
readytoregress$AmountSleep[which(is.na(readytoregress$AmountSleep))] <- mean(readytoregress$AmountSleep, na.rm=TRUE)
readytoregress$HomeworkDistraction[which(is.na(readytoregress$HomeworkDistraction))] <- mean(readytoregress$HomeworkDistraction, na.rm=TRUE)
readytoregress$Age[which(is.na(readytoregress$Age))] <- mean(readytoregress$Age, na.rm=TRUE)
readytoregress$AttndClass[which(is.na(readytoregress$AttndClass))] <- mean(readytoregress$AttndClass, na.rm=TRUE)
readytoregress$CollegeGPA[which(is.na(readytoregress$CollegeGPA))] <- mean(readytoregress$CollegeGPA, na.rm=TRUE)
readytoregress$EmHlpFam[which(is.na(readytoregress$EmHlpFam))] <- mean(readytoregress$EmHlpFam, na.rm=TRUE)
readytoregress$EQLar_Sm[which(is.na(readytoregress$EQLar_Sm))] <- mean(readytoregress$EQLar_Sm, na.rm=TRUE)
readytoregress$ExtraAct[which(is.na(readytoregress$ExtraAct))] <- mean(readytoregress$ExtraAct, na.rm=TRUE)
readytoregress$FrndHelp[which(is.na(readytoregress$FrndHelp))] <- mean(readytoregress$FrndHelp, na.rm=TRUE)
readytoregress$HlpDesk[which(is.na(readytoregress$HlpDesk))] <- mean(readytoregress$HlpDesk, na.rm=TRUE)
readytoregress$HrsStudyExamWeek[which(is.na(readytoregress$HrsStudyExamWeek))] <- mean(readytoregress$HrsStudyExamWeek, na.rm=TRUE)
readytoregress$HrPaidJb[which(is.na(readytoregress$HrPaidJb))] <- mean(readytoregress$HrPaidJb, na.rm=TRUE)
readytoregress$HrRelSer[which(is.na(readytoregress$HrRelSer))] <- mean(readytoregress$HrRelSer, na.rm=TRUE)
readytoregress$HrsStudyNonExamWeek[which(is.na(readytoregress$HrsStudyNonExamWeek))] <- mean(readytoregress$HrsStudyNonExamWeek, na.rm=TRUE)
readytoregress$HSGPA[which(is.na(readytoregress$HSGPA))] <- mean(readytoregress$HSGPA, na.rm=TRUE)
readytoregress$ReadingBeforeClass[which(is.na(readytoregress$ReadingBeforeClass))] <- mean(readytoregress$ReadingBeforeClass, na.rm=TRUE)
readytoregress$SmBetter[which(is.na(readytoregress$SmBetter))] <- mean(readytoregress$SmBetter, na.rm=TRUE)
readytoregress$Tutor[which(is.na(readytoregress$Tutor))] <- mean(readytoregress$Tutor, na.rm=TRUE)
readytoregress$HoursSocialMedia[which(is.na(readytoregress$HoursSocialMedia))] <- mean(readytoregress$HoursSocialMedia, na.rm=TRUE)

###Impute catagorical data

# Variable to impute EngFirst, FamInFin, Mission, Race, TeachPrinciples, Trauma, UAC, SocialMediaDuringClass, Gender

imputedata <- readytoregress %>% dplyr::select(EngFirst, FamInSub, Mission, Race, TeachPrinciples, Trauma, UAC, SocialMediaDuringClass, Gender)


my_imp <- mice(imputedata, m=5, method=c("logreg","logreg","pmm","pmm","logreg","logreg","logreg","logreg","pmm"), maxit=20)
finalimputeddata <- complete(my_imp, 3)

readytoregress$EngFirst <- finalimputeddata$EngFirst
readytoregress$FamInFin <- finalimputeddata$FamInFin
readytoregress$Mission <- finalimputeddata$Mission
readytoregress$Race <- finalimputeddata$Race
readytoregress$TeachPrinciples <- finalimputeddata$TeachPrinciples
readytoregress$Trauma <- finalimputeddata$Trauma
readytoregress$UAC <- finalimputeddata$UAC
readytoregress$SocialMediaDuringClass <- finalimputeddata$SocialMediaDuringClass
readytoregress$Gender <- finalimputeddata$Gender

##########################################
###### Video Game Dataset
##########################################

readytoregress_vg <- readytoregress %>% filter(VGPlayVideoGames == 1) %>% dplyr::select(-VGPlayVideoGames)
readytoregress$Emotions[which(is.na(readytoregress$Emotions))] <- mean(readytoregress$Emotions, na.rm=TRUE)
readytoregress$VGAgeStartedPlayingVG[which(is.na(readytoregress$VGAgeStartedPlayingVG))] <- mean(readytoregress$VGAgeStartedPlayingVG, na.rm=TRUE)
readytoregress$VGAvgAmountPlayWeekly[which(is.na(readytoregress$VGAvgAmountPlayWeekly))] <- mean(readytoregress$VGAvgAmountPlayWeekly, na.rm=TRUE)
readytoregress$VGLengthGamingSession[which(is.na(readytoregress$VGLengthGamingSession))] <- mean(readytoregress$VGLengthGamingSession, na.rm=TRUE)
readytoregress$VGRatherThanWatchTV[which(is.na(readytoregress$VGRatherThanWatchTV))] <- mean(readytoregress$VGRatherThanWatchTV, na.rm=TRUE)
readytoregress$VGMakeMeBored[which(is.na(readytoregress$VGMakeMeBored))] <- mean(readytoregress$VGMakeMeBored, na.rm=TRUE)
readytoregress$VGTVMakeMeBored[which(is.na(readytoregress$VGTVMakeMeBored))] <- mean(readytoregress$VGTVMakeMeBored, na.rm=TRUE)
readytoregress$VGPreferPlayingWithOthers[which(is.na(readytoregress$VGPreferPlayingWithOthers))] <- mean(readytoregress$VGPreferPlayingWithOthers, na.rm=TRUE)
summary(readytoregress_vg)

##########################################
###### Non Video Game Dataset
##########################################

readytoregress_novg <- readytoregress %>% filter(VGPlayVideoGames == 0) %>% dplyr::select(-c(VGPlayVideoGames, VGAgeStartedPlayingVG,
                                                                                             VGAvgAmountPlayWeekly, VGWeekendAfternoon,
                                                                                             VGLengthGamingSession, VGConsoleDesktopComputer,
                                                                                             VGConsoleiPad, VGConsoleVR, VGConsoleOther, VGRatherThanWatchTV,
                                                                                             VGMakeMeBored, VGTVMakeMeBored, VGPreferPlayingWithOthers, HomeworkDistraction))


### OLS Regression
ols_vg <- lm(Grade ~ ., readytoregress_vg)
ols_novg <- lm(Grade ~ ., readytoregress_novg)

readytoregress_combo <- readytoregress %>% dplyr::select(1:10,18:56,69:79)
ols_combined <- lm(Grade~., readytoregress_combo)

summary(ols_vg)$coef
summary(ols_novg)$coef
summary(ols_combined)


### Logistic Regression

readytoregress_vg_logit <- readytoregress_vg %>% mutate(Grade = ifelse(Grade > 0.94, 1, 0))
readytoregress_novg_logit <- readytoregress_novg %>% mutate(Grade = ifelse(Grade > 0.94, 1, 0))

logit_vg <- glm(Grade~., data = readytoregress_vg_logit, family = binomial)
logit_novg <- glm(Grade~., data = readytoregress_novg_logit, family = binomial)

summary(logit_vg)$coef
summary(logit_novg)$coef

### Combined Logistic Regression

readytoregress_combinedlogit <- readytoregress %>% dplyr::select(1:10,18:56,69:79) %>% mutate(Grade = ifelse(Grade > 0.94, 1, 0))
logit_combined <- glm(Grade~., data=readytoregress_combinedlogit, family=binomial)

summary(logit_combined)$coef




################################################################################
###################### Writing results to Excel ################################
################################################################################

write.xlsx2(sumstats, "VideoGamesAnalysis.xlsx", sheetName = 'SummaryStatistics', row.names=FALSE, append = FALSE)
write.xlsx2(cronbachsalphas, "VideoGamesAnalysis.xlsx", sheetName = 'CronbachAlphas', row.names=FALSE, append = TRUE)
write.xlsx2(variablenames, "VideoGamesAnalysis.xlsx", sheetName = 'Variables', row.names=FALSE, append = TRUE)
write.xlsx2(majorcount, "VideoGamesAnalysis.xlsx", sheetName = 'Majors', row.names=FALSE, append = TRUE)
write.xlsx2(spearmancorrelations, "VideoGamesAnalysis.xlsx", sheetName = 'SpearmanCorrelations', row.names=FALSE, append = TRUE)
write.xlsx2(Ttests, "VideoGamesAnalysis.xlsx", sheetName = 'tTests', row.names=FALSE, append=TRUE)
write.xlsx2(as.data.frame(readytoregress), "VideoGamesAnalysis.xlsx", sheetName = 'Regression Data', row.names=FALSE, append=TRUE)
write.xlsx2(summary(ols_vg)$coef, "VideoGamesAnalysis.xlsx", sheetName='RegressionVideoGames', now.names=FALSE, append=TRUE)
write.xlsx2(summary(ols_novg)$coef, "VideoGamesAnalysis.xlsx", sheetName='RegressionNonVideoGames', now.names=FALSE, append=TRUE)
write.xlsx2(summary(ols_combined)$coef, "VideoGamesAnalysis.xlsx", sheetName='OLSRegressionCombined', now.names=FALSE, append=TRUE)
write.xlsx2(summary(logit_vg)$coef, "VideoGamesAnalysis.xlsx", sheetName='LogitRegressionVideoGames', now.names=FALSE, append=TRUE)
write.xlsx2(summary(logit_novg)$coef, "VideoGamesAnalysis.xlsx", sheetName='LogitRegressionNonVideoGames', now.names=FALSE, append=TRUE)
write.xlsx2(summary(logit_combined)$coef, "VideoGamesAnalysis.xlsx", sheetName='LogitRegressionCombined', now.names=FALSE, append=TRUE)















