# Analysis macro for SF Decompression data
# BM Census 2018
# Gridge

library(ggplot2)
library(lattice)
library(plyr)
library(Rmisc)
library(data.table)
library(ggmap)
library(ggalt)


#Utility function that return a string for pinting value and fraction
printValFrac <- function(numerator, denominator) {
    return(paste0(numerator, ' / ', denominator, ' (', signif(numerator / denominator * 100, digits=2), '%)'))
}

#Make density distribution with 95% poisson CL and weights
#TODO: wrapper
plotDensity <- function(x, y) {
    
}

filterData <- function(inputCSVFile='data/MergedData_DecomNov18.csv', outputCSVFile='',makePlots=TRUE) {

    #read input data and publish as global (changes below won't affect its global value! use return value instead)
    rawMergedData <<- read.csv(inputCSVFile, header=TRUE) #fread(inputCSVFile, header=TRUE, data.table=FALSE)
    nEntries = nrow(rawMergedData)
    print(paste('Total entries: ', nEntries))

    # Check validity of data
    ## Fix Shift (1 invalid shift, all fieds are also empty -> will discard)
    isInvalidShift <- unlist(lapply(rawMergedData$Shift, is.na))
    print(paste('Number of invalid-shift cards:', printValFrac(nrow(rawMergedData[isInvalidShift,]), nEntries)))

    ## Fix Lane
    rawMergedData[rawMergedData$Lane == 'G','Lane'] <- 'g'
    rawMergedData$Lane <- as.character(rawMergedData$Lane)
    rawMergedData[rawMergedData$Lane == '','Lane'] <- 'NA'
    rawMergedData$Lane <- as.factor(rawMergedData$Lane)

    isValidClicker <- unlist(lapply(rawMergedData$Clicker, is.numeric))
    nInvalidClicker <- nrow(rawMergedData[!isValidClicker,])
    print(paste('Invalid clickers (not numeric): ', nInvalidClicker))

    VetoSkipValues <- factor(rawMergedData$Veto)
    isSkip <- (rawMergedData[,'Veto'] %in% c('s', 'S'))
    nSkips <- nrow(rawMergedData[isSkip,])
    print(paste('Number of skips: ', printValFrac(nSkips, nEntries)))

    isVeto <- (rawMergedData[,'Veto'] %in% c('a', 'A', 'c', 'C'))
    isChildVeto <- (rawMergedData[,'Veto'] %in% c('c', 'C'))
    nVetos = nrow(rawMergedData[isVeto,])
    nChildVetos = nrow(rawMergedData[isChildVeto,])
    print(paste('Vetos: ', printValFrac(nVetos, nEntries), ', out of which child-veto: ', printValFrac(nChildVetos, nVetos)))
    if (makePlots) {
        x11()
        with(rawMergedData[isVeto,], hist(Shift,main='Number of Vetos vs Shift')) #plot vetos per shift
    }
    #calculate correction factors for un-even distribution of vetos, will be stored as weightVetos
    vetoFractionNumerator <- as.vector(with(rawMergedData[isVeto,], summary(as.factor(Shift))))
    vetoFractionDenominator <- as.vector(table(rawMergedData$Shift))
    vetoFractions <- vetoFractionNumerator / vetoFractionDenominator
    if (makePlots) {
        x11()
        plot(vetoFractions, xlab='Shifts',ylab='Density',main='Fraction of Vetos per Shift',sub='1=2-5PM, 2=5-8PM, 3=8-11PM')
    }
    rawMergedData[rawMergedData$Veto %in% c("",'.'), 'Veto'] <- 0 #Assign 0 to empty cells
    rawMergedData[is.na(rawMergedData$Veto), 'Veto'] <- 0 #Assign 0 to empty cells

    #Correct X1
    rawMergedData$X1 <- suppressWarnings(as.numeric(as.character(rawMergedData$X1)))
    rawMergedData[(!is.na(rawMergedData$X1)) & (rawMergedData$X1 == 1996), 'X1'] = 96
    
    #Correct X2a
    rawMergedData$X2a <- as.character(rawMergedData$X2a)
    rawMergedData[rawMergedData$X2a == '3,4', 'X2a'] = 3
    rawMergedData[rawMergedData$X2a == '12', 'X2a'] = 2
    rawMergedData[rawMergedData$X2a == '23', 'X2a'] = 2
    rawMergedData[(rawMergedData$X2a %in% c('', '.')), 'X2a'] = '<NA>'
    #rawMergedData$X2a <- suppressWarnings(as.numeric(as.character(rawMergedData$X2a)))
    rawMergedData$X2a <- factor(rawMergedData$X2a,exclude=NULL)
    addNA(rawMergedData$X2a)

    #Correct X2b, fixing different spellings as well
    rawMergedData$X2b <- as.character(rawMergedData$X2b)
    rawMergedData[!is.na(rawMergedData$X2a) & (rawMergedData$X2a == 1), 'X2b'] = 'San Francisco'
    rawMergedData[rawMergedData$X2b == '2', 'X2b'] = ''
    rawMergedData[rawMergedData$X2b == '1', 'X2b'] = ''
    rawMergedData[rawMergedData$X2b == '0', 'X2b'] = ''
    rawMergedData[rawMergedData$X2b == '.', 'X2b'] = ''
    rawMergedData[is.na(rawMergedData$X2b), 'X2b'] = ''
    rawMergedData[rawMergedData$X2b == 'La Grange, CA', 'X2b'] = 'La Grange'
    rawMergedData[rawMergedData$X2b == 'Stockton, CA', 'X2b'] = 'Stockton'
    rawMergedData[rawMergedData$X2b == 'San_Marcos', 'X2b'] = 'San Marcos'
    rawMergedData[rawMergedData$X2b == 'South_Bay/Sunnyvale', 'X2b'] = 'Sunnyvale'
    rawMergedData[rawMergedData$X2b == 'Santa_Cruz', 'X2b'] = 'Santa Cruz'
    rawMergedData[rawMergedData$X2b == 'San_Mateo', 'X2b'] = 'San Mateo'
    rawMergedData[rawMergedData$X2b == 'Oakland, CA', 'X2b'] = 'Oakland'
    rawMergedData[rawMergedData$X2b == 'LA', 'X2b'] = 'Los Angeles'
    rawMergedData[rawMergedData$X2b == 'Mountain  View', 'X2b'] = 'Mountain View'
    rawMergedData[rawMergedData$X2b == 'Oak', 'X2b'] = 'Oakland'
    rawMergedData[rawMergedData$X2b == 'El_Cerrito', 'X2b'] = 'El Cerrito'
    rawMergedData[rawMergedData$X2b == 'Palo_Alto', 'X2b'] = 'Palo Alto'
    rawMergedData[rawMergedData$X2b == 'San_Lorenzo', 'X2b'] = 'San Lorenzo'
    rawMergedData[rawMergedData$X2b == 'Pleasant Hill, CA', 'X2b'] = 'Pleasant Hill'
    rawMergedData[rawMergedData$X2b == 'San Jose/Sacramento', 'X2b'] = 'San Jose'
    rawMergedData[rawMergedData$X2b == 'San leandro', 'X2b'] = 'San Leandro'
    rawMergedData[rawMergedData$X2b == 'San_Jose', 'X2b'] = 'San Jose'
    rawMergedData[rawMergedData$X2b == 'San_Carlos', 'X2b'] = 'San Carlos'
    rawMergedData[rawMergedData$X2b == 'San_Rafael', 'X2b'] = 'San Rafael'
    rawMergedData[rawMergedData$X2b == "on the road between Miami + SF; Standing Rock Reservation, SD", 'X2b'] = 'N/A'
    rawMergedData[rawMergedData$X2b == 'n/c', 'X2b'] = 'N/A'
    rawMergedData[rawMergedData$X2b == 'all_over', 'X2b'] = 'N/A'
    rawMergedData[rawMergedData$X2b == 'NYC', 'X2b'] = 'New York'
    rawMergedData[rawMergedData$X2b == 'Vae_Dubai', 'X2b'] = 'Dubai'
    rawMergedData[rawMergedData$X2b == 'WashingtonNW', 'X2b'] = 'Washington, NW'
    rawMergedData[rawMergedData$X2b == 'Philly', 'X2b'] = 'Philadelphia, PA'
    rawMergedData[!is.na(rawMergedData$X2a) & (rawMergedData$X2a == 2), 'X2b'] = 
      paste0(rawMergedData[!is.na(rawMergedData$X2a) & (rawMergedData$X2a == 2), 'X2b'], ', CA')
    rawMergedData[rawMergedData$X2b == ', CA', 'X2b'] = 'N/A, CA'
    rawMergedData$X2b <- as.factor(rawMergedData$X2b)        

    #Correct X3,4
    rawMergedData$X3 <- as.character(rawMergedData$X3)
    rawMergedData[(rawMergedData$X3 %in% c('', '.')), 'X3'] = '<NA>'
    rawMergedData$X3 <- factor(rawMergedData$X3, exclude = NULL)
    rawMergedData$X4 <- as.character(rawMergedData$X4)
    rawMergedData[(rawMergedData$X4 %in% c('', '.')), 'X4'] = '<NA>'
    rawMergedData$X4 <- factor(rawMergedData$X4, exclude = NULL)

    #Correct X5a
    rawMergedData$X5a <- as.character(rawMergedData$X5a)
    spuriousX5a12 <- (rawMergedData[,'X5a'] == '12' & !is.na(rawMergedData[,'X5a']))
    rawMergedData[spuriousX5a12, 'X5a'] = '1'
    spuriousX5a23 <- (rawMergedData[,'X5a'] == '23' & !is.na(rawMergedData[,'X5a']))
    rawMergedData[spuriousX5a23, 'X5a'] = '3'
    rawMergedData[rawMergedData$X5a == '5', 'X5a'] = '1'
    rawMergedData[(rawMergedData$X5a %in% c('', '.')), 'X5a'] = '<NA>'
    rawMergedData$X5a <- factor(rawMergedData$X5a, exclude = NULL)

    #Correct X5b, based on answers to X5a
    rawMergedData[rawMergedData$X5b %in% c('No'),'X5b'] = 0
    rawMergedData[rawMergedData$X5b %in% c('Friends', 'N/A', ''),'X5b'] = NA
    rawMergedData$X5b <- suppressWarnings(as.numeric(as.character(rawMergedData$X5b)))

    #Finally filter out any of the above we don't want,
    mergedData <- rawMergedData[!isSkip & !isVeto & !isInvalidShift,]
    #add the necessary variables,
    ## weight for each entry to compensate vetos
    ##  N/(N-V) = 1./(1-V/N) = 1./(1-vetoFraction)    
    weightVecVetos = 1.0 / (1.0 - vetoFractions)
    ## Year, translated in age
    currentYear <- 17
    mergedData = within(mergedData, {
        weightVetos = ifelse(Shift == 2, weightVecVetos[1], ifelse(Shift == 5, weightVecVetos[2], ifelse(Shift == 8, weightVecVetos[3], 1.0)))
        Age <- ifelse(is.na(X1),-1,ifelse(X1>currentYear,currentYear+100-X1,currentYear-X1))  
    })

    nFinalEntries = nrow(mergedData)
    print(paste('Total number of output entries:', printValFrac(nFinalEntries, nEntries)))
    print(paste(' # non-fatal Invalid entries for X1: ', printValFrac(nrow(mergedData[is.na(mergedData$X1),]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X2a: ', printValFrac(nrow(mergedData[mergedData$X2a == '<NA>',]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X2b: ', printValFrac(nrow(mergedData[mergedData$X2b == '',]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X3: ', printValFrac(nrow(mergedData[mergedData$X3 == '<NA>',]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X4: ', printValFrac(nrow(mergedData[mergedData$X4 == '<NA>',]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X5a: ', printValFrac(nrow(mergedData[mergedData$X5a == '<NA>',]), nFinalEntries)))
    print(paste(' # non-fatal Invalid entries for X5b: ', printValFrac(nrow(mergedData[is.na(mergedData$X5b),]), nFinalEntries)))
    print(paste(' #non-fatal Invalid entries for X5b (for X5a==1,2): ', printValFrac(nrow(mergedData[(mergedData$X5b == 0) & (mergedData$X5a %in% c(1,2)),]), nrow(mergedData[(mergedData$X5a %in% c(1,2)),]))))
    print("Weights stored into 'weightVeto' to compensate vetos per-shift (1=2-5PM, 2=5-8PM, 3=8-11PM):")
    print(weightVecVetos)

    #if not empty, write to CSV file
    if (outputCSVFile != '') {
        #fwrite(mergedData, outputCSVFile)
        write.csv(mergedData, outputCSVFile)
    }

    return(mergedData)
}

analyze <- function(inputCSVFile='data/MergedData_DecomNov18.csv',savePlots = FALSE) {
    #Filter data first
    mergedData <<- filterData(inputCSVFile, '', FALSE)
    nEntries <<- nrow(mergedData)

    ##################
    #Common plot style
    ##################
    colorScheme = c("#EA008B","#CC308D","#AE608E","#909090")
    themeSetting <- theme(panel.grid.major = element_blank(),
                          panel.grid.minor.y = element_line(color="#666464", size = .1),
                          panel.background  = element_blank(), axis.line.x = element_line(color="Black", size=.75),
                          title = element_text (size = 15), legend.key = element_blank(),
                          axis.line.y = element_line(color="Black", size=.75),
                          axis.title.y = element_text(face = "bold", margin = margin(0,20,0,0), size = 14),
                          axis.title.x = element_text(face = "bold", margin = margin(20,0,0,0), size = 14),
                          axis.text = element_text(size = 13), plot.title = element_text(face = "bold", hjust = 0.5))
    colorSetting <- scale_color_manual(values=colorscheme)
    fillSetting <- scale_fill_manual(values=colorscheme)
    
    ##################
    # Example plot to show effect of weightVetos
    ##################
    #x11()
    plotYearWeight <- ggplot() + themeSetting + 
      geom_bar(fill='red', alpha=0.5, data=mergedData, aes(x=Age)) +
      geom_bar(fill='blue', alpha=0.5, data=mergedData, aes(x=Age,weight=weightVetos)) +  
      #scale_fill_manual(name='Method', 
      #                  labels=c('Weighted','Unweighted'), 
      #                  values=c("h1"="blue","h2"="red")) +
      theme(legend.position="none") +
      labs(title='Age (Weighted vs Unweighted)',x="Year", y="Count") +
      geom_text(aes(label='Unweighted',x=50, y=50,colour='red'),show.legend = TRUE) +
      geom_text(aes(label='Weighted',x=50, y=43,colour='blue'))
    #geom_errorbar(aes(x=Age, weight=weightVetos, y=..count.., ymin=..count.. - 5, ymax=..count.. + 5))
    #geom_histogram(binwidth=10, fill='red', alpha=0.5, data=mergedData, aes(x=Age,weight=1./nEntries)) +
    #geom_histogram(binwidth=10, alpha=0.5,fill='blue', data=mergedData, aes(x=Age,weight=weightVetos/sum(weightVetos)))
    if (savePlots) ggsave("plots/YearWeight.png")
    
    ##################
    # Simple frequency/density distributions with weighted entries
    ##################
    #x11()
    summAge <- summary(mergedData[mergedData$Age >= 0,'Age'])
    plotYear <- ggplot() + themeSetting + 
        geom_bar(fill=colorScheme[1], data=mergedData, aes(x=Age,weight=100*weightVetos/sum(weightVetos))) +  
        labs(title='Age of population',x="Age (years)", y="Percentage (%)") +
        annotate("text",x=45,y=6,label=paste('Median:',summAge[3]),size=7) +
        annotate('text',x=45,y=5,label='(only valid entries)',size=3)
      #geom_errorbar(aes(x=Age, weight=weightVetos, y=..count.., ymin=..count.. - 5, ymax=..count.. + 5))
      #geom_histogram(binwidth=10, alpha=0.5,fill='blue', data=mergedData, aes(x=Age,weight=weightVetos/sum(weightVetos)))
    if (savePlots) ggsave("plots/Year.png")
    
    ## Location
    plotResidenceSumm <- ggplot(data=mergedData,mapping = aes(x=X2a,weight=weightVetos/sum(weightVetos))) + themeSetting + 
      geom_bar(fill=colorScheme[1]) +
      labs(title='Residence',x='',y='Precentage') +
      scale_x_discrete(breaks = c(1:4,"<NA>"), labels=c("San Francisco","CA (not SF)","Other US","Outside US", "N/A")) +
      scale_y_continuous(labels=scales::percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      coord_flip() +
      geom_text(stat='count', aes(label=scales::percent(..count..)), hjust=1.0, vjust=0.2)
    if (savePlots) ggsave("plots/ResidenceSumm.png")
    
    summResidence <- count(df=mergedData,vars=c('X2b'),wt_var = c('weightVetos/sum(weightVetos)'))
    summResidence <- summResidence[order(summResidence$freq,decreasing = TRUE),]
    summResidence$X2b <- factor(summResidence$X2b, levels=summResidence$X2b,exclude=NULL)
    levels(summResidence$X2b) <- sub("^$","N/A",summResidence$X2b)
    plotResidence <- ggplot(data=summResidence[summResidence$freq > 0.01,], aes(x=X2b,y=freq)) + 
      themeSetting + 
      geom_bar(stat="identity", fill=colorScheme[1]) +
      labs(title='Residence',x='',y='Precentage') +
      scale_y_continuous(labels=scales::percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    #TODO: add "other" bin to sum up the rest
    if (savePlots) ggsave("plots/Residence.png")
  
    ## Gender
    plotGender <- ggplot(data=mergedData,mapping = aes(x=X3,weight=weightVetos/sum(weightVetos))) + themeSetting + 
      geom_bar(fill=colorScheme[1]) +
      labs(title='Gender',x='',y='Precentage') +
      scale_x_discrete(breaks = c(1:3,"<NA>"), labels=c("Female", "Male", "Both/Neither/Fluid", "N/A")) +
      scale_y_continuous(labels=scales::percent,limits=c(NA,0.50)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      geom_text(stat='count', aes(label=scales::percent(..count..)), hjust=0.5, vjust=-0.5)
    if (savePlots) ggsave("plots/Gender.png")
    
    ## Attendance Decom
    plotAttendanceDecom <- ggplot(data=mergedData,mapping = aes(x=X4,weight=weightVetos/sum(weightVetos))) + themeSetting + 
      geom_bar(fill=colorScheme[1]) +
      labs(title='Attendance previous Decompression SF',x='',y='Precentage') +
      scale_x_discrete(breaks = c(1:2,"<NA>"), labels=c("Yes", "No", "N/A")) +
      #scale_y_continuous(labels=scales::percent,limits=c(NA,0.55)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      geom_text(stat='count', aes(label=scales::percent(..count..)), hjust=0.5, vjust=-0.5)
    if (savePlots) ggsave("plots/AttendanceDecom.png")
    
    ## Attendance BM
    plotAttendanceBM <- ggplot(data=mergedData,mapping = aes(x=X5a,weight=weightVetos/sum(weightVetos))) + themeSetting + 
      geom_bar(fill=colorScheme[1]) +
      labs(title='Attendance Burning Man main event',x='',y='Precentage') +
      scale_x_discrete(breaks = c(1:3,"<NA>"), labels=c("Yes\n(incl. 2017)", "Yes\n(but not 2017)", "No","N/A")) +
      #scale_y_continuous(labels=scales::percent,limits=c(NA,0.55)) +
      #theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      geom_text(stat='count', aes(label=scales::percent(..count..)), hjust=0.5, vjust=-0.5)
    if (savePlots) ggsave("plots/AttendanceBM.png")
    
    plotBMYears <- ggplot() + themeSetting + 
      geom_bar(fill=colorScheme[1], data=mergedData, aes(x=X5b,weight=100*weightVetos/sum(weightVetos))) +  
      labs(title='Number of previous burns',x="Count (0 = never)", y="Percentage (%)") +
    if (savePlots) ggsave("plots/BMYears.png")
    
    ##################    
    # Correlations
    ##################
    ## Attendance previous Decom and BM
    plotCorrAttendance <- ggplot(data=mergedData,mapping = aes(x=X5a,y=X4,weight=weightVetos/sum(weightVetos))) + themeSetting + 
      geom_bin2d() +
      stat_bin2d(geom="text",aes(label=scales::percent(..count..))) +
      labs(title='Attendance Correlation',x='Burning Man',y='Prev. Decompression') +
      scale_x_discrete(breaks = c(1:3,"<NA>"), labels=c("Yes\n(incl. 2017)", "Yes\n(but not 2017)", "No","N/A")) +
      scale_y_discrete(breaks = c(1:2,"<NA>"), labels=c("Yes", "No", "N/A")) +
      #scale_fill_continuous(name = "Percentage (%)") +
      scale_fill_gradient2(name = "Percentage (%)", low = "white", high = "red") +
      theme(legend.position = "top")
    if (savePlots) ggsave("plots/Corr-Attendance.png")    
  
    ##################
    #Heat map of location
    ##################
    listAllLocations <- levels(summResidence$X2b)
    #Use cached file, if need to re-generate, see instructions below (but needs manual editing too!)
    locAlllocations <- read.csv('locations.csv')
    #locAlllocations <- geocode(listAllLocations)
    #locAlllocations$place <- listAllLocations
    summResidence$lon <- locAlllocations[match(summResidence$X2b,locAlllocations$place), 'lon']
    summResidence$lat <- locAlllocations[match(summResidence$X2b,locAlllocations$place), 'lat']
    mergedData$lon <- locAlllocations[match(mergedData$X2b,locAlllocations$place), 'lon']
    mergedData$lat <- locAlllocations[match(mergedData$X2b,locAlllocations$place), 'lat']
    ca_map <- get_map(location = "San Francisco, CA, USA", maptype = "roadmap", zoom = 6, source = "google")
    heatMapCA <- ggmap(ca_map,extent = 'device') +
      #geom_point(aes(x=lon,y=lat),color="red",alpha=0.5,size=2,data=summResidence)
      geom_density2d(data=summResidence, aes(x=lon,y=lat),size=0.3) +
      stat_density2d(data=summResidence, aes(x=lon,y=lat,fill=..level..,alpha=..level..),size=0.01,
          bins=16, geom="polygon")+
      scale_fill_gradient(low = "green", high = "red") + 
      scale_alpha(range = c(0, 0.3), guide = FALSE)
    if (savePlots) ggsave("plots/Corr-HeatMapCA.png")    
    ca_map <- get_map(location = "San Francisco, CA, USA", maptype = "roadmap", zoom = 10, source = "google")
    heatMapSF <- ggmap(ca_map,extent = 'device') +
      geom_point(aes(x=lon,y=lat),color="red",alpha=0.5,size=2,data=mergedData)
      #geom_density2d(data=mergedData, aes(x=lon,y=lat),size=0.3) +
      #stat_density2d(data=mergedData, aes(x=lon,y=lat,fill=..level..,alpha=..level..),size=0.01,
      #               bins=16, geom="polygon")+
      #scale_fill_gradient(low = "green", high = "red") + 
      #scale_alpha(range = c(0, 0.3), guide = FALSE)
    if (savePlots) ggsave("plots/Corr-HeatMapSF.png")    
    # Draw requested plot
    #plotYearWeight
    #plotYear
    #plotResidenceSumm
    #plotResidence
    #plotGender
    #plotAttendanceDecom
    #plotAttendanceBM
    #plotBMYears
    #plotCorrAttendance
    #heatMapCA
    heatMapSF
    
    
}
