#create complete report without filling in the AAs summer 2020

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(forcats)
library(plyr)
library(readxl)
library(data.table)
library(magick)
library(openxlsx)
library(cowplot)
library(extrafont)
library(gridExtra)
library(rlang)
library(reader)
library(rmarkdown)
library(plotrix)
library(chron)

#location of static pages within report
setwd("/Volumes/My Passport/PDP Report Fall 2020")
#setwd("/Volumes/My Passport/PDP Sample Folder/Static Pages")

#static PDF pages for report
HMdoc <- image_read_pdf("RapsodoHittingMetricDefinitions.pdf")
RPMdoc <- image_read_pdf("RapsodoPitchingMetricDefinitions.pdf")
SprayChartTemp <- image_scale(image_read("RapsodoFieldSprayChart.png"), "x900")
pitchSilhouette <- image_read("pitcherSilhouette.png")
noTest <- image_read("DidNotTest.png")


#location of data files for event (home folder)
setwd("/Volumes/My Passport/NCCU 10.2.20/Run 2")
#setwd("/Volumes/My Passport/PDP Sample Folder")

# ADD EMPTY FOLDERS : COVER PAGES and PDF FULL REPORTS

#collected event data - all files in this section muct be Excel files
internal <- read_excel(path = "NCCU Run 2Durham, NCInternal.xlsx", col_names = TRUE) #internal
CMJ <- read_excel(path="NCCUcmjext2.xlsx") #CMJ extended
drift <- read_excel("NCCU2dextended2.xlsx",col_names = TRUE) #2D extended
Gaittest <- read_excel(path="NCCUgait2.xlsx", sheet="OJ Data",col_names = TRUE) #gait
DriftProtocol <- read_excel(path="NCCU2dsimple2.xlsx",sheet="Protocol data",skip = 1,col_names = FALSE) #2D simple
RapHit <- read_excel(path = "States Play 2019Miami, FLRapsodoHittingMaster.xlsx", col_names = TRUE) #rapsodo hitting
DK <- read_excel(path = "States Play DK Master.xlsx", col_names = TRUE) # DK
RapPitch <- read_excel(path = "States Play 2019Miami, FLRapsodoPitchingMaster.xlsx", col_names = TRUE) #rapsodo pitching

#reformatting/clean up of the internal sheet
internal$`Reaction to Go (sec)` <- na_if(internal$`Reaction to Go (sec)`, "-")
internal$`10 yd split (sec)` <- na_if(internal$`10 yd split (sec)`, "-")
internal$`30 Total` <- na_if(internal$`30 Total`, "-")
# internal$`ISO 10` <- na_if(internal$`ISO 10`, "-")
# internal$`ISO 30` <- na_if(internal$`ISO 30`, "-")
internal$Hawkeye <- na_if(internal$Hawkeye, "-")
internal$Hawkeye <- ifelse(startsWith(internal$Hawkeye, "32"), "32", internal$Hawkeye)
internal$`Green Box` <- na_if(internal$`Green Box`, "-")
internal$`Green 3` <- na_if(internal$`Green 3`, "-")
internal$`Agility Diff` <- na_if(internal$`Agility Diff`, "-")
internal$`% Change` <- na_if(internal$`% Change`, "-")

internal$`Green Box` <- as.numeric(internal$`Green Box`)
internal$`Green 3` <- as.numeric(internal$`Green 3`)
internal$`Agility Diff` <- as.numeric(internal$`Agility Diff`)
internal$`% Change` <- as.numeric(internal$`% Change`)
internal$Hawkeye <- as.numeric(internal$Hawkeye)

internal$`CMJ GCT[sec]` <- na_if(internal$`CMJ GCT[sec]`, "-")
internal$`CMJ Height[in]` <- na_if(internal$`CMJ Height[in]`, "-")
internal$`CMJ Power [W/Kg]` <- na_if(internal$`CMJ Power [W/Kg]`, "-")

internal$`CMJ GCT[sec]` <- as.numeric(internal$`CMJ GCT[sec]`)
internal$`CMJ Height[in]` <- as.numeric(internal$`CMJ Height[in]`)
internal$`CMJ Power [W/Kg]` <- as.numeric(internal$`CMJ Power [W/Kg]`)

internal$`BJ GCT[sec]` <- na_if(internal$`BJ GCT[sec]`, "-")
internal$`BJ Power[W/Kg]` <- na_if(internal$`BJ Power[W/Kg]`, "-")
internal$`BJ Distance (ft)` <- na_if(internal$`BJ Distance (ft)`, "-")

internal$`BJ GCT[sec]` <- as.numeric(internal$`BJ GCT[sec]`)
internal$`BJ Power[W/Kg]` <- as.numeric(internal$`BJ Power[W/Kg]`)
internal$`BJ Distance (ft)` <- as.numeric(internal$`BJ Distance (ft)`)

#format player names to match Rapsodo data files
internal$RapNames <- paste(internal$`First Name`, internal$`Last Name`, sep="")
DK$firstAndLast <- paste(DK$`First Name`, DK$`Last Name`, sep = "")

#calculate event averages from PDP Assessment
EventAverages <- data.frame("Last Name" = "Event Averages", "Green Box" = mean(as.numeric(internal$`Green Box`), na.rm = TRUE), "Green 3" = mean(as.numeric(internal$`Green 3`), na.rm = TRUE), "Agility Diff" = mean(as.numeric(internal$`Agility Diff`), na.rm = TRUE), "% Change" = mean(as.numeric(internal$`% Change`), na.rm = TRUE), "Reaction" = mean(as.numeric(internal$`Reaction to Go (sec)`), na.rm = TRUE), "10 yd" = mean(as.numeric(internal$`10 yd split (sec)`), na.rm = TRUE), "30 yd" = mean(as.numeric(internal$`30 Total`), na.rm = TRUE), "Hawkeye" = mean(as.numeric(internal$Hawkeye), na.rm = TRUE), "CMJ.GCT.sec." = mean(as.numeric(internal$`CMJ GCT[sec]`), na.rm = TRUE), "CMJ.Height.in." = mean(as.numeric(internal$`CMJ Height[in]`), na.rm = TRUE), "CMJ.Power..W.Kg." = mean(as.numeric(internal$`CMJ Power [W/Kg]`), na.rm = TRUE), "BJ.GCT.sec." = mean(as.numeric(internal$`BJ GCT[sec]`), na.rm = TRUE), "BJ.Power.W.Kg." = mean(as.numeric(internal$`BJ Power[W/Kg]`), na.rm = TRUE), "BJ.Distance..ft." = mean(as.numeric(internal$`BJ Distance (ft)`), na.rm = TRUE))
EventAverages[,2:15] <- as.numeric(EventAverages[,2:15])

# EventAverages <- data.frame("Last Name" = "Event Averages", "Green Box" = mean(as.numeric(internal$`Green Box`), na.rm = TRUE), "Green 3" = mean(as.numeric(internal$`Green 3`), na.rm = TRUE), "Agility Diff" = mean(as.numeric(internal$`Agility Diff`), na.rm = TRUE), "% Change" = mean(as.numeric(internal$`% Change`), na.rm = TRUE), "10 yd" = mean(as.numeric(internal$`ISO 10`), na.rm = TRUE), "30 yd" = mean(as.numeric(internal$`ISO 30`), na.rm = TRUE), "Hawkeye" = mean(as.numeric(internal$Hawkeye), na.rm = TRUE), "CMJ.GCT.sec." = mean(as.numeric(internal$`CMJ GCT[sec]`), na.rm = TRUE), "CMJ.Height.in." = mean(as.numeric(internal$`CMJ Height[in]`), na.rm = TRUE), "CMJ.Power..W.Kg." = mean(as.numeric(internal$`CMJ Power [W/Kg]`), na.rm = TRUE), "BJ.GCT.sec." = mean(as.numeric(internal$`BJ GCT[sec]`), na.rm = TRUE), "BJ.Power.W.Kg." = mean(as.numeric(internal$`BJ Power[W/Kg]`), na.rm = TRUE), "BJ.Distance..ft." = mean(as.numeric(internal$`BJ Distance (ft)`), na.rm = TRUE))
# EventAverages[,2:14] <- as.numeric(EventAverages[,2:14])

#calculate Rapsodo hitting event averages
RapHitAverages <- data.frame("Player Name" = "Event Averages", "Ball Exit Speed" = mean(as.numeric(RapHit$exitSpeed), na.rm = TRUE), "Launch Angle" = mean(as.numeric(RapHit$launchAngle), na.rm = TRUE), "Launch Direction" = mean(as.numeric(RapHit$launchDirection), na.rm = TRUE), "Spin Rate" = mean(as.numeric(RapHit$spin), na.rm = TRUE))
RapHitAverages[,2:5] <- as.numeric(RapHitAverages[,2:5])

#calculate DK event averages
DKAverages <- data.frame("Last Name" = "Event Averages", "Max Barrel Speed" = mean(as.numeric(DK$`Maximum Barrel Speed`), na.rm = TRUE), "Hand Speed" = mean(as.numeric(DK$`Maximum Hand Speed`), na.rm = TRUE), "Impact Momentum" = mean(as.numeric(DK$`Impact Momentum`), na.rm = TRUE), "Trigger To Impact Time" = mean(as.numeric(DK$`Trigger To Impact`), na.rm = TRUE), "Max Acceleration" = mean(as.numeric(DK$`Maximum Acceleration`), na.rm = TRUE), "Approach Angle" = mean(as.numeric(DK$`Attack Angle`), na.rm = TRUE), "Hand Cast" = mean(as.numeric(DK$`Hand Cast`), na.rm = TRUE))
DKAverages[,2:8] <- as.numeric(DKAverages[,2:8])

###### start of loop for entire report creation ######
for (i in 1:3) { #for all- "i in 1:nrow(internal)"; for some- "i in 1:5" or "i in 1:27"...etc
  indPlayerData <- subset(internal[i,])
  
  #pull Green Box Green 3 testing information & reformat for output
  data1 <- as.data.frame(c(indPlayerData[,c("Last Name", "First Name", "Green Box","Green 3","Agility Diff","% Change")]))
  data1$Last.Name <- paste(data1$First.Name, data1$Last.Name)
  data1 <- data1[,c("Last.Name", "Green.Box", "Green.3", "Agility.Diff", "X..Change")]
  data1 <- rbindlist(list(data1, EventAverages[,1:5])) #compare to event averages
  names(data1) <- c("Last.Name","Green Box", "Green 3", "Agility Diff", "% Change")
  data1 <- data1[,c("% Change", "Agility Diff","Green 3","Green Box", "Last.Name")]

  #create agility plot if player did test Green Box/Green 3
  if (!is.na(data1$`Agility Diff`[1]) & !is.na(data1$`Green 3`[1]) & !is.na(data1$`Green Box`[1])) {
    d <- melt(data1, id.vars="Last.Name")
    d$value <- as.numeric(d$value)
    d[1,3] <- d[1,3]*100
    d[2,3] <- d[2,3]*100
    AgilityPlot <- ggplot(d, aes(fill=Last.Name, y=value, x=variable)) + theme_classic() +
      geom_bar(position="dodge", stat="identity") + geom_hline(yintercept = 0,size = 0.15) + ylab("Time (seconds)") + coord_flip() + geom_text(aes(x = variable, y = value, label =round(value, digits = 2), group = Last.Name, hjust = ifelse(value >= 0, -0.3, 1.3), family = "DINCondensed-Bold"),
                                                                                                                                                                                             size = 5,
                                                                                                                                                                                             position = position_dodge(width = 1),
                                                                                                                                                                                             inherit.aes = TRUE) + scale_fill_manual(values = alpha(c("mediumblue", "red"))) + theme(axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "bottom", text = element_text(family = "DINCondensed-Bold", size = 17)) + ylim(c(as.numeric(ifelse(min(d$value, na.rm = TRUE) >= 0, min(d$value, na.rm = TRUE) - 3, min(d$value, na.rm = TRUE) - 7)), as.numeric(max(d$value, na.rm = TRUE) + 6)))
    ggsave(AgilityPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"AgilityGraph",".png"),width=4.8,height=3.6,units="in", dpi = 280)
  }

  #create gait analysis graph if player has sprint data
  if (!is.na(internal$`Reaction to Go (sec)`[i]) & !is.na(internal$`10 yd split (sec)`[i]) & !is.na(internal$`30 Total`[i])) {
    #pull sprint data from internal sheet & reformat
    data2 <- as.data.frame(c(indPlayerData[,c("Last Name", "First Name", "Reaction to Go (sec)" ,"10 yd split (sec)","30 Total")]))
    data2$Last.Name <- paste(data2$First.Name, data2$Last.Name)
    data2 <- data2[,c("Last.Name", "Reaction.to.Go..sec.", "X10.yd.split..sec.", "X30.Total")]
    data2 <- rbindlist(list(data2, EventAverages[, c("Last.Name", "Reaction", "X10.yd", "X30.yd")]), use.names = FALSE) #compare to event averages
    names(data2) <- c("Last.Name", "Reaction", "10 Yard", "30 Yard")
    d2 <- melt(data2, id.vars="Last.Name")
    d2$value <- as.numeric(d2$value)
    SprintPlot <- ggplot(d2, aes(x = Last.Name, y = value, fill = variable)) + theme_classic() +
      geom_bar(position = "dodge", stat = "identity") + geom_hline(yintercept = 0,size = 0.15) + ylab("Time (seconds)") + geom_text(aes(x = Last.Name, y = value, label = round(value, digits = 2), group = variable, family = "DINCondensed-Bold"), hjust = -0.3, size = 5,
                                                                                                                                                                position = position_dodge(width = 1),
                                                                                                                                                                inherit.aes = TRUE) + coord_flip() + scale_fill_manual(values = alpha(c("gray", "mediumblue", "red"))) + theme(axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "bottom", text = element_text(family = "DINCondensed-Bold", size = 17)) + ylim(c(0, as.numeric(max(d2$value, na.rm = TRUE)) + .5))
    ggsave(SprintPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"SprintGraph",".png"),width=4.5,height=2.8,units="in", dpi = 260) #4.5x2.8
  }

  #ISO 10 & 30
  # if (!is.na(internal$`ISO 10`[i]) & !is.na(internal$`ISO 30`[i])) {
  #   #pull sprint data from internal sheet & reformat
  #   data2 <- as.data.frame(c(indPlayerData[,c("Last Name", "First Name", "ISO 10", "ISO 30")]))
  #   data2$Last.Name <- paste(data2$First.Name, data2$Last.Name)
  #   data2 <- data2[,c("Last.Name", "ISO.10", "ISO.30")]
  #   data2 <- rbindlist(list(data2, EventAverages[, c("Last.Name", "X10.yd", "X30.yd")]), use.names = FALSE) #compare to event averages
  #   names(data2) <- c("Last.Name", "10 Yard Split", "30 Yard Split")
  #   d2 <- melt(data2, id.vars="Last.Name")
  #   d2$value <- as.numeric(d2$value)
  #   SprintPlot <- ggplot(d2, aes(x = Last.Name, y = value, fill = variable)) + theme_classic() +
  #     geom_bar(position = "dodge", stat = "identity") + geom_hline(yintercept = 0,size = 0.15) + ylab("Time (seconds)") + geom_text(aes(x = Last.Name, y = value, label = round(value, digits = 2), group = variable, family = "DINCondensed-Bold"), hjust = -0.3, size = 5,
  #                                                                                                                                   position = position_dodge(width = 1),
  #                                                                                                                                   inherit.aes = TRUE) + coord_flip() + scale_fill_manual(values = alpha(c("mediumblue", "red"))) + theme(axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "bottom", text = element_text(family = "DINCondensed-Bold", size = 17)) + ylim(c(0, as.numeric(max(d2$value, na.rm = TRUE)) + .5))
  #   ggsave(SprintPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"SprintGraph",".png"),width=7,height=6,units="in", dpi = 260)
  # }

  #create Hawkeye graph if player did Hawkeye test
  if(!is.na(internal$Hawkeye[i])) {
    data3 <- as.data.frame(c(indPlayerData[,c("Last Name", "First Name", "Hawkeye")]))
    data3$Last.Name <- paste(data3$First.Name, data3$Last.Name)
    data3 <- data3[,c("Last.Name", "Hawkeye")]
    data3 <- rbindlist(list(data3, EventAverages[, c("Last.Name", "Hawkeye")])) #compare to event average
    d3 <- melt(data3, id.vars="Last.Name")
    d3$value <- as.numeric(d3$value)
    HawkeyePlot <- ggplot(d3, aes(x = variable)) + theme_classic() +
      geom_col(aes(y = value, fill = Last.Name),
               position = position_dodge(width = 1),
               width = .5) + geom_hline(yintercept = 0,size = 0.15) + ylab("Time (milliseconds)") + geom_text(aes(x = variable, y = value, label = round(value, digits = 0), group =Last.Name, family = "DINCondensed-Bold"), size = 5, hjust = -0.3,
                                                                                                                                               position = position_dodge(width = 1),
                                                                                                                                               inherit.aes = TRUE) + coord_flip() + scale_fill_manual(values = alpha(c("mediumblue", "red"))) + theme(axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "bottom", text = element_text(family = "DINCondensed-Bold", size = 17)) + ylim(c(0, as.numeric(pmax(d3[1,3], d3[2,3], na.rm = TRUE) + 200)))
    ggsave(HawkeyePlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i], "HawkeyeGraph",".png"),width=5.3,height=3.10,units="in", dpi = 265)
  }

  #create CMJ avg height/power graph if player tested CMJ
  if (!is.na(internal$`CMJ GCT[sec]`[i]) & !is.na(internal$`CMJ Height[in]`[i]) & !is.na(internal$`CMJ Power [W/Kg]`[i])) {
    data4 <- as.data.frame(c(indPlayerData[,c("Last Name", "First Name", "CMJ GCT[sec]", "CMJ Height[in]", "CMJ Power [W/Kg]")]))
    data4$Last.Name <- paste(data4$First.Name, data4$Last.Name)
    data4 <- data4[,c("Last.Name", "CMJ.GCT.sec.", "CMJ.Height.in.", "CMJ.Power..W.Kg.")]
    data4 <- rbindlist(list(data4, EventAverages[,c("Last.Name", "CMJ.GCT.sec.", "CMJ.Height.in.", "CMJ.Power..W.Kg.")])) #compare to event averages
    names(data4) <- c("Last.Name","Ground Contact Time", "Height (in.)", "Power (W/kg)")
    d <- melt(data4, id.vars="Last.Name")
    CMJPlot <- ggplot(d, aes(x = variable, y = value, fill = Last.Name)) + theme_classic() +
      geom_bar(position = "dodge", stat = "identity") + geom_hline(yintercept = 0,size = 0.15) + geom_text(aes(x = variable, y = value, label = round(value, digits = 2), group = Last.Name, family = "DINCondensed-Bold"), hjust = -0.3, size = 5,
                                                                                                           position = position_dodge(width = 1),
                                                                                                           inherit.aes = TRUE) + coord_flip() + scale_fill_manual(values = alpha(c("mediumblue", "red"))) + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "bottom", text = element_text(family = "DINCondensed-Bold", size = 17)) + ylim(c(0, as.numeric(max(d$value, na.rm = TRUE)) + 7))
    ggsave(CMJPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"CMJGraph",".png"), width=4.75, height=3, units="in", dpi = 260)
  }

  #create individual CMJ jump graph if player tested
  if (data1$Last.Name[1] %in% CMJ$`Last & first names`) {
    CMJPlayer <- subset(CMJ,CMJ$`Last & first names`==data4$Last.Name[1]&grepl("\\d",CMJ$`#`)&is.na(CMJ$Elevation)!=TRUE)
    colnames(CMJPlayer)[24] <- "jumpNumber"
    CMJHeight <- ggplot(CMJPlayer,aes(x=jumpNumber,y=Elevation))+geom_bar(stat="identity",fill="mediumblue")+ theme_classic() +
      coord_cartesian(ylim=c(min(CMJPlayer$Elevation)-2,max(CMJPlayer$Elevation)+1)) + labs(y="Height (in.)",x="Jump Number")+
      theme(text = element_text(family = "DINCondensed-Bold", size = 17))+ geom_text(size = 5, aes(x = jumpNumber, y = Elevation, label = round(Elevation, digits = 3), family = "DINCondensed-Bold"), vjust = -1, position = position_dodge(width = 1), inherit.aes = TRUE)


    ggsave(CMJHeight,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"CMJHeightGraph",".png"),width=4,height=3,units="in", dpi = 260)
  }

  #create 2D Drift data table if player tested 2D Drift
  if (data1$Last.Name[1] %in% DriftProtocol$...4) {
    dataBlockStart <- which(DriftProtocol$...4==data1$Last.Name[1])
    dataBlockStart <- dataBlockStart[length(dataBlockStart)]
    driftHeightDelta <- DriftProtocol[dataBlockStart+2,24:27]
    driftPowerDelta <- DriftProtocol[dataBlockStart+3,24:27]
    driftGCTDelta <- DriftProtocol[dataBlockStart+4,24:27]
    driftFlighSelta <- DriftProtocol[dataBlockStart+5,24:27]
    driftDevLR <- DriftProtocol[dataBlockStart+6,25:26]
    driftDevFB <- DriftProtocol[dataBlockStart+7,25:26]
    driftDriftLR <- DriftProtocol[dataBlockStart+8,25:26]
    driftDriftFB <- DriftProtocol[dataBlockStart+9,25:26]
    driftAreaDelta <- DriftProtocol[dataBlockStart+10,24:27]
    driftTable <- rbind(driftHeightDelta,driftPowerDelta,driftGCTDelta,driftFlighSelta,driftAreaDelta)
    colnames(driftTable) <- c("","L","R","delta%")
  }

  #create 2D Drift landing plot for both legs if player tested
  if (data1$Last.Name[1] %in% drift$`Last & first names`) {
    driftBoxData <- rbind(driftDevLR,driftDevFB,driftDriftLR,driftDriftFB)
    rightRect <- data.frame(xmin=as.numeric(driftDevLR[1,2])-abs(as.numeric(driftDriftLR[1,2])),xmax=as.numeric(driftDevLR[1,2])+abs(as.numeric(driftDriftLR[1,2])),ymin=as.numeric(driftDevFB[1,2])-abs(as.numeric(driftDriftFB[1,2])),ymax=as.numeric(driftDevFB[1,2])+abs(as.numeric(driftDriftFB[1,2])))
    playerRLeg <- subset(drift,Test=="5 JUMPS SINGLE LEG RIGHT 2D DRIFT"&drift$`Last & first names`==data1$Last.Name[1])
    playerRLeg[1,64] <- 0
    playerRLeg[1,70] <- 0
    RLeg <- ggplot(playerRLeg,aes(x=(playerRLeg$WalkingPointY*12),y=(playerRLeg$WalkingPointX*12))) + geom_point(color="mediumblue",size=2.5,shape=16) + geom_text(label=playerRLeg$`#`, vjust=2, size=5, family = "DINCondensed-Bold") + coord_cartesian(xlim=c(-20,20), ylim=c(-20,20)) +
      labs(x="Horizontal Jump Location (in)", y="Vertical Jump Location (in)") + theme(text = element_text(family = "DINCondensed-Bold", size = 17), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE")) +
      geom_rect(data=rightRect, inherit.aes=FALSE, aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill="blue", alpha=0.1)
    ggsave(RLeg, file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i], "RightLeg2dGraph", ".png"), width=4.5, height=4.5, units="in", dpi = 280)

    leftRect <- data.frame(xmin = as.numeric(driftDevLR[1,1]) - abs(as.numeric(driftDriftLR[1,1])), xmax = as.numeric(driftDevLR[1,1]) + abs(as.numeric(driftDriftLR[1,1])), ymin = as.numeric(driftDevFB[1,1]) - abs(as.numeric(driftDriftFB[1,1])), ymax = as.numeric(driftDevFB[1,1]) + abs(as.numeric(driftDriftFB[1,1])))
    playerLLeg <- subset(drift, Test == "5 JUMPS SINGLE LEG LEFT 2D DRIFT" & drift$`Last & first names` == data1$Last.Name[1])
    playerLLeg[1,64] <- 0
    playerLLeg[1,70] <- 0
    LLeg <- ggplot(playerLLeg, aes(x=(WalkingPointY*12), y=(WalkingPointX*12))) + geom_point(color="mediumblue", size=2.5, shape=16) + geom_text(label=playerLLeg$`#`, vjust=2, size=5, family = "DINCondensed-Bold") + coord_cartesian(xlim=c(-20,20), ylim=c(-20,20)) +
      labs(x="Horizontal Jump Location (in)", y="Vertical Jump Location (in)") + theme(text = element_text(family = "DINCondensed-Bold", size = 16), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE")) +
      geom_rect(data = leftRect, inherit.aes=FALSE, aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill="blue", alpha=0.1)

    ggsave(LLeg,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"LeftLeg2dGraph",".png"),width=4.5,height=4.5,units="in")
    FinalLeg <- plot_grid(LLeg, RLeg, labels = "Note: Point 1 is initial takeoff; Point 6 is final landing", label_size = 15, label_fontfamily = "DINCondensed-Bold", hjust = -0.25, vjust = 2)
    ggsave(FinalLeg,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"2dGraph",".png"),width=9,height=3.75,units="in",dpi=265)
  }

  #pull broad jump data for player
  data5 <- as.data.frame(c(indPlayerData[,c("Last Name", "First Name", "BJ GCT[sec]", "BJ Power[W/Kg]", "BJ Distance (ft)")]))
  data5$Last.Name <- paste(data5$First.Name, data5$Last.Name)
  data5 <- data5[,c("Last.Name", "BJ.GCT.sec.", "BJ.Power.W.Kg.", "BJ.Distance..ft.")]
  data5 <- rbindlist(list(data5, EventAverages[,c("Last.Name","BJ.GCT.sec.", "BJ.Power.W.Kg.", "BJ.Distance..ft.")]))
  names(data5) <- c("Last.Name","Ground Contact Time", "Power (W/kg)", "Distance (ft)")

  #create broad jump power/distance graph if player tested broad jump
  if (!is.na(internal$`BJ Distance (ft)`[i]) & !is.na(internal$`BJ Power[W/Kg]`[i]) & !is.na(internal$`BJ GCT[sec]`[i])) {
    d <- melt(data5, id.vars="Last.Name")
    BJPlot <- ggplot(d, aes(x = variable, y = value, fill = Last.Name)) +
      geom_bar(position = "dodge", stat = "identity") + theme_classic() + geom_hline(yintercept = 0,size = 0.15) + geom_text(aes(x = variable, y = value, label = round(value, digits = 2), group = Last.Name, family = "DINCondensed-Bold"), hjust = -0.3, size = 5,
                                                                                                           position = position_dodge(width = 1),
                                                                                                           inherit.aes = TRUE) + coord_flip() + scale_fill_manual(values = alpha(c("mediumblue", "red"))) + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "bottom", text = element_text(family = "DINCondensed-Bold", size = 17)) + ylim(c(0, as.numeric(max(d$value, na.rm = TRUE)) + 3))
    ggsave(BJPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"BJGraph",".png"),width=7.25,height=4.5,units="in", dpi = 280)
  }

  #create gait analysis speed/accel graph if player tested gait
  if (data1$Last.Name[1] %in% Gaittest$`Last & first names`) {
    test <- Gaittest %>% filter(Gaittest$`Last & first names` == data1$Last.Name[1] & (!is.na(Gaittest$`L/R`)))
    test$`#` <- as.numeric(test$`#`)
    velocity <- ggplot(test, aes(x = test$`#`, y = Speed)) + theme_classic() + geom_line(color = "red") + geom_point(color = "mediumblue", size = 2.5, shape = 15) + geom_text(size = 4, aes(x = test$`#`, y = Speed, label = round(Speed, digits = 2), family = "DINCondensed-Bold"), vjust = -1, position = position_dodge(width = 1), inherit.aes = TRUE) + geom_text(size = 4, aes(x = test$`#`, y = Speed, label = test$`L/R`, family = "DINCondensed-Bold"), vjust = -2.5, position = position_dodge(width = 1), inherit.aes = TRUE) + ylim(c(as.numeric(min(test$Speed, na.rm = TRUE)) - 3, as.numeric(max(test$Speed, na.rm = TRUE)) + 5)) + theme_classic() + theme(text = element_text(family = "DINCondensed-Bold", size = 17), plot.title = element_text(family = "DINCondensed-Bold", size = 20)) + xlab("Step Number") + ylab("Feet/Second") + ggtitle("SPEED") + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
    acceleration <- ggplot(test, aes(x = test$`#`, y = Acc)) + theme_classic() + geom_bar(position = "dodge", stat = "identity", fill = "mediumblue") + geom_hline(yintercept = 0,size = 0.2) + geom_text(size = 4, aes(x = test$`#`, y = Acc, label = round(Acc, digits = 2), vjust = ifelse(Acc >= 0, -1, 1.8), family = "DINCondensed-Bold"), position = position_dodge(width = 1), inherit.aes = TRUE) + ylim(c(as.numeric(min(test$Acc, na.rm = TRUE)) - 5, as.numeric(max(test$Acc, na.rm = TRUE)) + 5)) + geom_text(size = 4, aes(x = test$`#`, y = Acc, label = test$`L/R`, vjust = ifelse(Acc >= 0, -2.5, 3.3), family = "DINCondensed-Bold"), position = position_dodge(width = 1), inherit.aes = TRUE) + xlab("Step Number") + ylab(expression(paste("Feet/Second²"))) + ggtitle("ACCELERATION") + theme(text = element_text(family = "DINCondensed-Bold", size = 17), plot.title = element_text(family = "DINCondensed-Bold", size = 20)) + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
    ggsave(velocity,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"VelocityGraph",".png"),width=4.25,height=3.2,units="in",dpi=265)
    ggsave(acceleration,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"AccelGraph",".png"),width=4.25,height=3.2,units="in",dpi=265)
  }




  #### EDIT EVENT NAME (update 8.13.20 to only make cover pages with no HWG)
  # if (is.na(indPlayerData$`Height (in)`) & is.na(indPlayerData$Weight) & is.na(indPlayerData$`LEFT DOWN`)) {
  #   COVdoc <- image_read_pdf("E:/PR Walkthrough 8.28.20/Static Pages/CoverPageBlankNoHWG.pdf")
  #   #COVdoc <- image_read_pdf("/Volumes/My Passport/PDP Sample Folder/Static Pages/CoverPageBlankNoHWG.pdf")
  #   COVdoc <- image_annotate(COVdoc, toupper(indPlayerData$`Last Name`), font = "DIN", size = 75, color = "white", location = "+500+2640")
  #   COVdoc <- image_annotate(COVdoc, toupper(indPlayerData$`First Name`), font = "DIN", size = 175, color = "white", location = "+500+2480")
  #   # CHANGE LOCATION (ALIGNMENT)
  #   COVdoc <- image_annotate(COVdoc, "PDP SAMPLE REPORT", font = "DIN", size = 100, color = "white", location = "+1430+100")
  #   COVdoc <- image_annotate(COVdoc, format(indPlayerData$Date, "%m.%d.%y"), font = "DIN Condensed", size = 100, color = "white", location = "+2110+100")
  #   image_write(COVdoc,path=paste0("Cover Pages/", internal$`Last Name`[i],internal$`First Name`[i],"Cover.pdf"), format="pdf", quality = 100, density = 300)
  # }
  
  COVdoc <- image_read_pdf("/Volumes/My Passport/PDP Report Fall 2020/CoverPageBlankNoHWG.pdf")
  COVdoc <- image_annotate(COVdoc, toupper(indPlayerData$`Last Name`), font = "DIN Condensed", size = 275, color = "white", location = "+500+2640")
  COVdoc <- image_annotate(COVdoc, toupper(indPlayerData$`First Name`), font = "DIN Condensed", size = 175, color = "white", location = "+500+2480")
  # CHANGE LOCATION (ALIGNMENT) 
  COVdoc <- image_annotate(COVdoc, "NORTH CAROLINA CENTRAL - RUN 2", font = "DIN Condensed", size = 100, color = "white", location = "+985+100")
  #COVdoc <- image_annotate(COVdoc, format(indPlayerData$Date, "%m.%d.%y"), font = "DIN Condensed", size = 100, color = "white", location = "+2110+100")
  COVdoc <- image_annotate(COVdoc, '10.02.20', font = "DIN Condensed", size = 100, color = "white", location = "+2110+100")
  image_write(COVdoc,path=paste0("Cover Pages/", internal$`Last Name`[i],internal$`First Name`[i],"Cover.pdf"), format="pdf", quality = 100, density = 300)
  

  # if (!is.na(indPlayerData$`Height (in)`) | !is.na(indPlayerData$Weight) | !is.na(indPlayerData$`LEFT DOWN`)) {
  #   #COVdoc <- image_read_pdf("/Volumes/My Passport/PR Walkthrough 8.28.20/Static Pages/CoverPageBlank.pdf")
  #   COVdoc <- image_read_pdf("/Volumes/My Passport/PDP Sample Folder/Static Pages/CoverPageBlank.pdf")
  #   COVdoc <- image_annotate(COVdoc, toupper(indPlayerData$`Last Name`), font = "DIN Condensed", size = 400, color = "white", location = "+400+665")
  #   COVdoc <- image_annotate(COVdoc, toupper(indPlayerData$`First Name`), font = "DIN Condensed", size = 250, color = "white", location = "+400+445")
  #   # CHANGE LOCATION (ALIGNMENT)
  #   COVdoc <- image_annotate(COVdoc, "PDP SAMPLE REPORT", font = "DIN Condensed", size = 100, color = "white", location = "+1430+100")
  #   COVdoc <- image_annotate(COVdoc, format(indPlayerData$Date, "%m.%d.%y"), font = "DIN Condensed", size = 100, color = "white", location = "+2110+100")
  #   COVdoc <- image_annotate(COVdoc, paste0(round(indPlayerData$`Height (in)`, 2), " IN"), font = "DIN Condensed", size = 115, color = "white", location = "+674+2685")
  #   COVdoc <- image_annotate(COVdoc, paste0(indPlayerData$Weight, " LBS"), font = "DIN Condensed", size = 115, color = "white", location = "+998+2685")
  #   COVdoc <- image_annotate(COVdoc, indPlayerData$`RIGHT DOWN`, font = "DIN Condensed", size = 90, color = "white", location = "+1760+2580")
  #   COVdoc <- image_annotate(COVdoc, indPlayerData$`RIGHT 90`, font = "DIN Condensed", size = 90, color = "white", location = "+1760+2678")
  #   COVdoc <- image_annotate(COVdoc, indPlayerData$`RIGHT UP`, font = "DIN Condensed", size = 90, color = "white", location = "+1760+2776")
  #   COVdoc <- image_annotate(COVdoc, indPlayerData$`LEFT DOWN`, font = "DIN Condensed", size = 90, color = "white", location = "+2173+2580")
  #   COVdoc <- image_annotate(COVdoc, indPlayerData$`LEFT 90`, font = "DIN Condensed", size = 90, color = "white", location = "+2173+2678")
  #   COVdoc <- image_annotate(COVdoc, indPlayerData$`LEFT UP`, font = "DIN Condensed", size = 90, color = "white", location = "+2173+2776")
  #   image_write(COVdoc,path=paste0("Cover Pages/", internal$`Last Name`[i],internal$`First Name`[i],"Cover.pdf"), format="pdf", quality = 100, density = 300)
  # }


  #read in blank AA and apply images to the pages
  doc <- image_read_pdf("/Volumes/My Passport/PDP Report Fall 2020/AAReportBlank.pdf") # change to AAReportBlankNoGait.pdf if players have no gait
  page_num <- 0

  if ((!is.na(data1$`Agility Diff`[1]) & !is.na(data1$`Green 3`[1]) & !is.na(data1$`Green Box`[1])) | !is.na(indPlayerData$Hawkeye)) {
    if(!is.na(data1$`Agility Diff`[1]) & !is.na(data1$`Green 3`[1]) & !is.na(data1$`Green Box`[1])) {
      docup1 <- image_annotate(doc[page_num + 1], paste0(indPlayerData$`Green Box`, " SEC"), font = "DIN Condensed", size = 70, location = "+310+1268")
      docup1 <- image_annotate(docup1, paste0(indPlayerData$`Green 3`, " SEC"), font = "DIN Condensed", size = 70, location = "+760+1268")
      docup1 <- image_annotate(docup1, paste0(round(indPlayerData$`Agility Diff`, 2), " SEC"), font = "DIN Condensed", size = 70, location = "+330+1418")
      docup1 <- image_annotate(docup1, paste0(round(indPlayerData$`% Change` * 100, 2), "%"), font = "DIN Condensed", size = 70, location = "+795+1418")
      pic1 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"AgilityGraph.png"))
      docup1 <- image_composite(docup1, pic1, offset = "+1150+640")
    } else {
      docup1 <- image_composite(doc[page_num + 1], noTest, offset = "+1150+640")
    }

    if(!is.na(indPlayerData$Hawkeye)) {
      pic2 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"HawkeyeGraph.png"))
      docup1 <- image_composite(docup1, pic2, offset = "+150+1825")
      docup1 <- image_annotate(docup1, paste0(indPlayerData$Hawkeye, " MSEC"), font = "DIN Condensed", size = 70, location = "+1867+2528")
    } else {
      docup1 <- image_composite(docup1, noTest, offset = "+150+1825")
    }
    doc[page_num + 1] <- docup1
    page_num <- page_num + 1
  } else {
    doc <- doc[-(page_num + 1)]
  }

  if (!is.na(internal$`CMJ GCT[sec]`[i]) & !is.na(internal$`CMJ Height[in]`[i])) {
    docup2 <- image_annotate(doc[page_num + 1], paste0(round(indPlayerData$`CMJ Height[in]`, 1), " IN"), font = "DIN Condensed", size = 70, location = "+1189+2354")
    docup2 <- image_annotate(docup2, paste0(round(indPlayerData$`CMJ Power [W/Kg]`, 2), " W/KG"), font = "DIN Condensed", size = 70, location = "+1154+2520")
    docup2 <- image_annotate(docup2, paste0(round(indPlayerData$`CMJ GCT[sec]`, 3), " SEC"), font = "DIN Condensed", size = 70, location = "+1191+2680")
    pic3 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"CMJGraph.png"))
    docup2 <- image_composite(docup2, pic3, offset = "+65+550")
    pic4 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"CMJHeightGraph.png"))
    docup2 <- image_composite(docup2, pic4, offset = "+277+1350")
    doc[page_num + 1] <- docup2
    page_num <- page_num + 1

  } else {
    doc <- doc[-(page_num + 1)]
  }

  if (data1$Last.Name[1] %in% drift$`Last & first names`) {
    pic5 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"2dGraph.png"))
    docup3 <- image_composite(doc[page_num + 1], pic5, offset = "+50+650")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[1,2]), " IN"), font = "DIN Condensed", size = 60, location = "+522+1817")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[3,2]), " SEC"), font = "DIN Condensed", size = 60, location = "+880+1817")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[2,2]), " W/KG"), font = "DIN Condensed", size = 60, location = "+1352+1817")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[4,2]), " SEC"), font = "DIN Condensed", size = 60, location = "+1790+1817")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[5,2]), " IN²"), font = "DIN Condensed", size = 60, location = "+2161+1817")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[1,3]), " IN"), font = "DIN Condensed", size = 60, location = "+522+1973")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[3,3]), " SEC"), font = "DIN Condensed", size = 60, location = "+880+1973")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[2,3]), " W/KG"), font = "DIN Condensed", size = 60, location = "+1352+1973")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[4,3]), " SEC"), font = "DIN Condensed", size = 60, location = "+1790+1973")
    docup3 <- image_annotate(docup3, paste0(as.numeric(driftTable[5,3]), " IN²"), font = "DIN Condensed", size = 60, location = "+2161+1973")
    docup3 <- image_annotate(docup3, as.character(driftTable[1,4]), font = "DIN Condensed", size = 60, location = "+522+2128")
    docup3 <- image_annotate(docup3, as.character(driftTable[3,4]), font = "DIN Condensed", size = 60, location = "+880+2128")
    docup3 <- image_annotate(docup3, as.character(driftTable[2,4]), font = "DIN Condensed", size = 60, location = "+1352+2128")
    docup3 <- image_annotate(docup3, as.character(driftTable[4,4]), font = "DIN Condensed", size = 60, location = "+1790+2128")
    docup3 <- image_annotate(docup3, as.character(driftTable[5,4]), font = "DIN Condensed", size = 60, location = "+2161+2128")
    doc[page_num + 1] <- docup3
    page_num <- page_num + 1
  } else {
    doc <- doc[-(page_num + 1)]
  }

  if (!is.na(internal$`BJ Distance (ft)`[i]) & !is.na(internal$`BJ Power[W/Kg]`[i]) & !is.na(internal$`BJ GCT[sec]`[i])) {
    docup4 <- image_annotate(doc[page_num + 1], paste0(round(indPlayerData$`BJ Distance (ft)`, 2), " FT"), font = "DIN Condensed", size = 70, location = "+1768+2020")
    docup4 <- image_annotate(docup4, paste0(round(indPlayerData$`BJ Power[W/Kg]`, 2), " W/KG"), font = "DIN Condensed", size = 70, location = "+1737+2195")
    docup4 <- image_annotate(docup4, paste0(indPlayerData$`BJ GCT[sec]`, " SEC"), font = "DIN Condensed", size = 70, location = "+1755+2365")
    pic6 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"BJGraph.png"))
    docup4 <- image_composite(docup4, pic6, offset = "+295+520")
    doc[page_num + 1] <- docup4
    page_num <- page_num + 1
  } else {
    doc <- doc[-(page_num + 1)]
  }

  # PAGE FOR GAIT ANALYSIS
  if (!is.na(internal$`Reaction to Go (sec)`[i]) & !is.na(internal$`10 yd split (sec)`[i]) & !is.na(internal$`30 Total`[i])) {
    pic7 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"SprintGraph.png"))
    docup5 <- image_composite(doc[page_num + 1], pic7, offset = "+50+545")
    if (data1$Last.Name[1] %in% Gaittest$`Last & first names`) {
      pic8 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"VelocityGraph.png"))
      pic9 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"AccelGraph.png"))
    } else {
      pic8 <- noTest
      pic9 <- noTest
    }
    docup5 <- image_composite(docup5, pic8, offset = "+100+1300")
    docup5 <- image_composite(docup5, pic9, offset = "+1300+1300")
    docup5 <- image_annotate(docup5, paste0(indPlayerData$`Reaction to Go (sec)`, " SEC"), font = "DIN Condensed", size = 70, location = "+1155+2382")
    docup5 <- image_annotate(docup5, paste0(indPlayerData$`10 yd split (sec)`, " SEC"), font = "DIN Condensed", size = 70, location = "+1154+2562")
    docup5 <- image_annotate(docup5, paste0(indPlayerData$`30 Total`, " SEC"), font = "DIN Condensed", size = 70, location = "+1154+2740")
    doc[page_num + 1] <- docup5
    page_num <- page_num + 1
  } else {
    doc <- doc[-(page_num + 1)]
  }

  # FOR REVISED PAGE W/ NO GAIT
  # if (!is.na(internal$`Reaction to Go (sec)`[i]) & !is.na(internal$`10 yd split (sec)`[i]) & !is.na(internal$`30 Total`[i])) {
  #   pic7 <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"SprintGraph.png"))
  #   docup5 <- image_composite(doc[page_num + 1], pic7, offset = "+370+1400")
  #   docup5 <- image_annotate(docup5, paste0(indPlayerData$`Reaction to Go (sec)`, " SEC"), font = "DIN Condensed", size = 70, location = "+575+702")
  #   docup5 <- image_annotate(docup5, paste0(indPlayerData$`10 yd split (sec)`, " SEC"), font = "DIN Condensed", size = 70, location = "+575+895")
  #   docup5 <- image_annotate(docup5, paste0(indPlayerData$`30 Total`, " SEC"), font = "DIN Condensed", size = 70, location = "+575+1084")
  #   doc[page_num + 1] <- docup5
  #   page_num <- page_num + 1
  # } else {
  #   doc <- doc[-(page_num + 1)]
  # }


#create Rapsodo hitting statistics if player has Rapsodo data
  if (indPlayerData$RapNames[1] %in% RapHit$`Player Name`) {
    rapHitPlayer <- RapHit %>% filter(RapHit$`Player Name` == indPlayerData$RapNames[1])
    rapHitPlayer <- rapHitPlayer %>% filter(exitSpeed != 0 & spin != 0 & distance != 0)
    rapHitPlayer$classification <- ifelse(rapHitPlayer$launchAngle < 0, "Dribbler", ifelse(rapHitPlayer$launchAngle >= 0 & rapHitPlayer$launchAngle < 6, "Ground Ball", ifelse(rapHitPlayer$launchAngle >= 6 & rapHitPlayer$launchAngle < 15, "Low Line Drive", ifelse(rapHitPlayer$launchAngle >= 15 & rapHitPlayer$launchAngle < 24, "High Line Drive", ifelse(rapHitPlayer$launchAngle >= 24 & rapHitPlayer$launchAngle < 50, "Fly Ball", "Pop Up")))))
    rapHitPlayer$avgLA <- mean(rapHitPlayer$launchAngle)
    rapHitPlayer$avgLD <- mean(rapHitPlayer$launchDirection)
    rapHitPlayer$avgEV <- mean(rapHitPlayer$exitSpeed)
    rapHitPlayer$maxEV <- max(rapHitPlayer$exitSpeed)
    rapHitPlayer$hardHitThreshold <- rapHitPlayer$maxEV - (rapHitPlayer$maxEV * 0.1)
    rapHitPlayer$isHardHit <- ifelse(rapHitPlayer$exitSpeed >= rapHitPlayer$hardHitThreshold, 1, 0)
    rapHitPlayer$hardHitPct <- sum(rapHitPlayer$isHardHit)/nrow(rapHitPlayer) * 100
    rapHitPlayer$isRope <- ifelse(rapHitPlayer$isHardHit == 1 & rapHitPlayer$launchAngle >= 10 & rapHitPlayer$launchAngle < 20, 1, 0)
    rapHitPlayer$isBomb <- ifelse(rapHitPlayer$isHardHit == 1 & rapHitPlayer$launchAngle >= 20, 1, 0)
    rapHitPlayer$isBIP <- ifelse(rapHitPlayer$launchDirection >= -45 & rapHitPlayer$launchDirection <= 45, 1, 0)
    rapHitPlayer$ropesBIP <- sum(rapHitPlayer$isRope)/sum(rapHitPlayer$isBIP) * 100
    rapHitPlayer$bombsBIP <- sum(rapHitPlayer$isBomb)/sum(rapHitPlayer$isBIP) * 100
    rapHitPlayer$avgRPM <- mean(rapHitPlayer$spin)
    rapHitPlayer$HHBRPM <- mean(rapHitPlayer$spin[rapHitPlayer$isHardHit == 1])
    rapHitPlayer$avgDist <- mean(rapHitPlayer$distance)
    rapHitPlayer$maxDist <- max(rapHitPlayer$distance)
    rapHitPlayer$dribblerPct <- count(rapHitPlayer$classification == "Dribbler")[2,2]/nrow(rapHitPlayer) * 100
    rapHitPlayer$gbPct <- count(rapHitPlayer$classification == "Ground Ball")[2,2]/nrow(rapHitPlayer) * 100
    rapHitPlayer$LLDPct <- count(rapHitPlayer$classification == "Low Line Drive")[2,2]/nrow(rapHitPlayer) * 100
    rapHitPlayer$HLDPct <- count(rapHitPlayer$classification == "High Line Drive")[2,2]/nrow(rapHitPlayer) * 100
    rapHitPlayer$flyBallPct <- count(rapHitPlayer$classification == "Fly Ball")[2,2]/nrow(rapHitPlayer) * 100
    rapHitPlayer$popUpPct <- count(rapHitPlayer$classification == "Pop Up")[2,2]/nrow(rapHitPlayer) * 100
    rapHitPlayer$dribblerPct <- ifelse(is.na(rapHitPlayer$dribblerPct[1]), 0, rapHitPlayer$dribblerPct)
    rapHitPlayer$gbPct <- ifelse(is.na(rapHitPlayer$gbPct[1]), 0, rapHitPlayer$gbPct)
    rapHitPlayer$LLDPct <- ifelse(is.na(rapHitPlayer$LLDPct[1]), 0, rapHitPlayer$LLDPct)
    rapHitPlayer$HLDPct <- ifelse(is.na(rapHitPlayer$HLDPct[1]), 0, rapHitPlayer$HLDPct)
    rapHitPlayer$flyBallPct <- ifelse(is.na(rapHitPlayer$flyBallPct[1]), 0, rapHitPlayer$flyBallPct)
    rapHitPlayer$popUpPct <- ifelse(is.na(rapHitPlayer$popUpPct[1]), 0, rapHitPlayer$popUpPct)


    #create Rapsodo spray chart
    colors_plot <- rep(NA, nrow(rapHitPlayer))
    colors_plot[rapHitPlayer$classification == "Dribbler"] <- "black"
    colors_plot[rapHitPlayer$classification == "Ground Ball"] <- "blue"
    colors_plot[rapHitPlayer$classification == "Low Line Drive"] <- "darkorange1"
    colors_plot[rapHitPlayer$classification == "High Line Drive"] <- "green3"
    colors_plot[rapHitPlayer$classification == "Fly Ball"] <- "purple"
    colors_plot[rapHitPlayer$classification == "Pop Up"] <- "red"

    png(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i], "SprayChart", ".png"), width = 1700, height = 1700)
    old.par <- par(bg=NA)
    radial.plot(rapHitPlayer$distance, radial.pos = (rapHitPlayer$launchDirection*0.0174533), start = (pi*1/2), radial.labels = NULL, show.radial.grid = FALSE, show.grid.labels = FALSE, rp.type = 's', show.grid = FALSE, point.symbols = 16, point.col = colors_plot, cex = 3, radial.lim = c(0, 450))
    dev.off()
    par(old.par)

    #create LA / Distance / Spin graphs
    cols <- c("Dribbler" = "black", "Ground Ball" = "blue", "Low Line Drive" = "darkorange1", "High Line Drive" = "green3", "Fly Ball" = "purple", "Pop Up" = "red")
    LADistPlot <- ggplot(rapHitPlayer, aes(x = rapHitPlayer$launchAngle, y = rapHitPlayer$distance)) + geom_point(aes(color = rapHitPlayer$classification), show.legend = FALSE) + scale_color_manual(values = cols) + xlab("Launch Angle (degrees)") + ylab("Distance (feet)") + geom_hline(yintercept = 330, linetype = "dashed", color = "black") + geom_text(aes(min(rapHitPlayer$launchAngle) + 18, 330, label = "Typical Wall Distance", vjust = -0.5, hjust = 1, family = "DINCondensed-Bold")) + theme(text = element_text(family = "DINCondensed-Bold", size = 17), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE"), legend.position = "none") + ylim(0, 400)
    LASpinPlot <- ggplot(rapHitPlayer, aes(x = rapHitPlayer$launchAngle, y = rapHitPlayer$spin)) + geom_point(aes(color = rapHitPlayer$classification), show.legend = FALSE) + scale_color_manual(values = cols) + xlab("Launch Angle (degrees)") + ylab("Spin Rate (RPM)") +  theme(text = element_text(family = "DINCondensed-Bold", size = 17), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE"), legend.position = "none")
    ggsave(LADistPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"LADist",".png"),width=4.5,height=3.5,units="in",dpi=265)
    ggsave(LASpinPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"LASpin",".png"),width=4.5,height=3.5,units="in",dpi=265)

    #read in blank Rapsodo hitting and apply graphs (needs full file path if not located in home folder)
    RHdoc <- image_read_pdf("/Volumes/My Passport/Misc/RapsodoHittingBlankComplete.pdf")
    LADist_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"LADist",".png"))
    LASpin_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"LASpin",".png"))
    spray_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i], "SprayChart", ".png"))
    RHdoc <- image_composite(RHdoc, LADist_pic, offset = "+100+2200")
    RHdoc <- image_composite(RHdoc, LASpin_pic, offset = "+1250+2200")
    RHdoc <- image_composite(RHdoc, SprayChartTemp, offset = "+317+1290")
    RHdoc <- image_composite(RHdoc, spray_pic, offset = "+0+1202")
    RHdoc <- image_annotate(RHdoc, paste0(round(RapHitAverages$Ball.Exit.Speed[1], 1), " MPH"), font = "DIN Condensed", size = 55, location = "+1100+700")
    RHdoc <- image_annotate(RHdoc, paste0(round(RapHitAverages$Launch.Angle[1], 1), "°"), font = "DIN Condensed", size = 55, location = "+1140+833")
    RHdoc <- image_annotate(RHdoc, paste0(round(RapHitAverages$Launch.Direction[1], 1), "°"), font = "DIN Condensed", size = 55, location = "+1150+965")
    RHdoc <- image_annotate(RHdoc, paste0(round(RapHitAverages$Spin.Rate[1], 0), " RPM"), font = "DIN Condensed", size = 55, location = "+1095+1097")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$avgEV, 1), " MPH"), font = "DIN Condensed", size = 55, location = "+715+700")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$avgLA, 1), "°"), font = "DIN Condensed", size = 55, location = "+755+833")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$avgLD, 1), "°"), font = "DIN Condensed", size = 55, location = "+750+965")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$avgRPM, 0), " RPM"), font = "DIN Condensed", size = 55, location = "+705+1097")

    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$maxEV, 1), " MPH"), font = "DIN Condensed", size = 55, location = "+1973+438")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$ropesBIP, 1), "%"), font = "DIN Condensed", size = 55, location = "+1995+565")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$bombsBIP, 1), "%"), font = "DIN Condensed", size = 55, location = "+2007+693")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$avgRPM, 0), " RPM"), font = "DIN Condensed", size = 55, location = "+1955+816")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$HHBRPM, 0), " RPM"), font = "DIN Condensed", size = 55, location = "+1955+960")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$avgDist, 0), " FT"), font = "DIN Condensed", size = 55, location = "+1985+1102")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$maxDist, 0), " FT"), font = "DIN Condensed", size = 55, location = "+1985+1228")

    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$dribblerPct, 1), "%"), font = "DIN Condensed", size = 55, location = "+1985+1523")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$gbPct, 1), "%"), font = "DIN Condensed", size = 55, location = "+1985+1624")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$LLDPct, 1), "%"), font = "DIN Condensed", size = 55, location = "+1985+1726")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$HLDPct, 1), "%"), font = "DIN Condensed", size = 55, location = "+1985+1827")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$flyBallPct, 1), "%"), font = "DIN Condensed", size = 55, location = "+1985+1928")
    RHdoc <- image_annotate(RHdoc, paste0(round(rapHitPlayer$popUpPct, 1), "%"), font = "DIN Condensed", size = 55, location = "+1985+2030")

    doc <- image_join(doc, RHdoc)
    doc <- image_join(doc, HMdoc)
  }

  #create DK swing statistics if player has DK data
  if (indPlayerData$RapNames[1] %in% DK$firstAndLast) {
    DKPlayer <- DK %>% filter(DK$firstAndLast == indPlayerData$RapNames[1])
    DKPlayer$avgMaxBarrel <- mean(DKPlayer$`Maximum Barrel Speed`)
    DKPlayer$avgHandSpeed <- mean(DKPlayer$`Maximum Hand Speed`)
    DKPlayer$avgImpactMom <- mean(DKPlayer$`Impact Momentum`)
    DKPlayer$avgTriggerImpact <- mean(DKPlayer$`Trigger To Impact`)
    DKPlayer$maxAccel <- mean(DKPlayer$`Maximum Acceleration`)
    DKPlayer$avgApproachAngle <- mean(DKPlayer$`Attack Angle`)
    DKPlayer$avgHandCast <- mean(DKPlayer$`Hand Cast`)

    #create LA / Distance / Spin graphs
    dktable <- as.data.frame(c(DKPlayer[,c("Last Name", "First Name", "avgMaxBarrel","avgHandSpeed","avgImpactMom","avgTriggerImpact", "maxAccel", "avgApproachAngle", "avgHandCast")][1,]))
    dktable$Last.Name <- paste(dktable$First.Name, dktable$Last.Name)
    dktable <- dktable[,c("Last.Name", "avgMaxBarrel","avgHandSpeed","avgImpactMom","avgTriggerImpact", "maxAccel", "avgApproachAngle", "avgHandCast")]
    dktable <- rbindlist(list(dktable, DKAverages[,1:8]), use.names = FALSE) #compare to event averages
    names(dktable) <- c("Last.Name","Max Barrel Speed (MPH)","Max Hand Speed (MPH)","Impact Momentum (KG/M)","Trigger to Impact Speed (SEC)", "Max Acceleration (G)", "Attack Angle (Degrees)", "Hand Cast (IN)")
    dktable <- dktable[,c("Trigger to Impact Speed (SEC)", "Attack Angle (Degrees)", "Hand Cast (IN)", "Impact Momentum (KG/M)", "Max Hand Speed (MPH)", "Max Acceleration (G)", "Max Barrel Speed (MPH)","Last.Name")]
    dktable$`Trigger to Impact Speed (SEC)` <- dktable$`Trigger to Impact Speed (SEC)`/1000

    dktable1 <- melt(dktable, id.vars="Last.Name")
    dktable1$value <- as.numeric(dktable1$value)
    DKMetricsPlot <- ggplot(dktable1, aes(fill=Last.Name, y=value, x=variable)) + theme_classic() +
      geom_bar(position="dodge", stat="identity") + geom_hline(yintercept = 0,size = 0.15) + coord_flip() + geom_text(aes(x = variable, y = value, label =round(value, digits = 2), group = Last.Name, hjust = ifelse(value >= 0, -0.3, 1.3), family = "DINCondensed-Bold"),
                                                                                                                                                 size = 5,
                                                                                                                                                 position = position_dodge(width = 1),
                                                                                                                                                 inherit.aes = TRUE) + scale_fill_manual(values = alpha(c("mediumblue", "red"))) + theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank(), legend.position = "bottom", text = element_text(family = "DINCondensed-Bold", size = 17)) + ylim(0, as.numeric(max(dktable1$value, na.rm = TRUE) + 10))
    ggsave(DKMetricsPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"DKMetricsPlot",".png"),width=7,height=4.5,units="in",dpi=265)

    #read in blank Rapsodo hitting and apply graphs (needs full file path if not located in home folder)
    DKdoc <- image_read_pdf("/Volumes/My Passport/Misc/DKBlankComplete.pdf")
    DKMetricsPlot_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"DKMetricsPlot",".png"))
    DKdoc <- image_composite(DKdoc, DKMetricsPlot_pic, offset = "+300+570")

    doc <- image_join(doc, DKdoc)
  }


  #create Rapsodo pitching stats and plots of player has Rapsodo pitching data
  if (indPlayerData$RapNames[1] %in% RapPitch$`Player Name`) {
    rapPitchPlayer <- RapPitch %>% filter(RapPitch$`Player Name` == indPlayerData$RapNames[1])
    rapPitchPlayer <- rapPitchPlayer %>% filter(Spin != '-' & rapPitchPlayer$`Spin Efficiency (release)` != '-')
    rapPitchPlayer$Spin <- as.numeric(as.character(rapPitchPlayer$Spin))
    rapPitchPlayer$`Spin Efficiency (release)` <- as.numeric(as.character(rapPitchPlayer$`Spin Efficiency (release)`))
    rapPitchPlayer$`Spin Axis` <- format(as.POSIXct(strptime(rapPitchPlayer$`Spin Axis`, "%H:%M")), "%H:%M:%S")
    rapPitchPlayer$`VB (spin)` <- as.numeric(as.character(rapPitchPlayer$`VB (spin)`))
    rapPitchPlayer$`HB (spin)` <- as.numeric(as.character(rapPitchPlayer$`HB (spin)`))
    rapPitchPlayer$`Release Angle` <- as.numeric(as.character(rapPitchPlayer$`Release Angle`))
    rapPitchPlayer$`Release Height (ft)` <- as.numeric(as.character(rapPitchPlayer$`Release Height (ft)`))
    rapPitchPlayer$`Release Side (ft)` <- as.numeric(as.character(rapPitchPlayer$`Release Side (ft)`))
    rapPitchPlayer$`Gyro Degree (deg)` <- as.numeric(as.character(rapPitchPlayer$`Gyro Degree (deg)`))
    rapPitchPlayer$`Strike Zone Side` <- as.numeric(as.character(rapPitchPlayer$`Strike Zone Side`))
    rapPitchPlayer$`Strike Zone Height` <- as.numeric(as.character(rapPitchPlayer$`Strike Zone Height`))

    #create HB/VB and release location plots
    cols <- c("Fastball" = "blue", "TwoSeamFastball" = "cyan", "CurveBall" = "green3", "ChangeUp" = "darkorange", "Cutter" = "red", "Slider" = "purple", "Splitter" = "deeppink", "Sinker" = "darkgoldenrod1")
    HBVBPlot <- ggplot(rapPitchPlayer, aes(x = rapPitchPlayer$`HB (spin)`, y = rapPitchPlayer$`VB (spin)`)) + geom_point(aes(color = rapPitchPlayer$`Pitch Type`), show.legend = FALSE) + scale_color_manual(values = cols) + xlab("Horizontal Break (in)") + ylab("Vertical Break (in)") +  theme(text = element_text(family = "DINCondensed-Bold", size = 15), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE"), legend.position = "none") + xlim(-max(abs(rapPitchPlayer$`HB (spin)`)), max(abs(rapPitchPlayer$`HB (spin)`))) + ylim(-max(abs(rapPitchPlayer$`VB (spin)`)), max(abs(rapPitchPlayer$`VB (spin)`)))
    releasePlot <- ggplot(rapPitchPlayer, aes(x = rapPitchPlayer$`Release Side (ft)`, y = rapPitchPlayer$`Release Height (ft)`)) + geom_point(aes(color = rapPitchPlayer$`Pitch Type`), show.legend = FALSE) + scale_color_manual(values = cols) + xlab("Release Side (ft)") + ylab("Release Height (ft)") +  theme(text = element_text(family = "DINCondensed-Bold", size = 15), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE"), legend.position = "none") + xlim(-max(abs(rapPitchPlayer$`Release Side (ft)`)), max(abs(rapPitchPlayer$`Release Side (ft)`))) + ylim(4.5,max(rapPitchPlayer$`Release Height (ft)`))
    if (mean(rapPitchPlayer$`Release Side (ft)`) < 0) {
      releasePlot <- ggdraw() + draw_image(image_flop(pitchSilhouette), scale = 0.47, x = 0.05, y = -0.05) + draw_plot(releasePlot)
    }
    if (mean(rapPitchPlayer$`Release Side (ft)`) > 0) {
      releasePlot <- ggdraw() + draw_image(pitchSilhouette, scale = 0.47, x = 0.1, y = -0.05) + draw_plot(releasePlot)
    }
    SZPlot <- ggplot(rapPitchPlayer, aes(x = rapPitchPlayer$`Strike Zone Side`, y = rapPitchPlayer$`Strike Zone Height`)) + geom_rect(xmin = -8.5, xmax = 8.5, ymin = 18, ymax = 42, color = "black", fill = "#EEEEEE") + geom_point(aes(color = rapPitchPlayer$`Pitch Type`), show.legend = FALSE) + scale_color_manual(values = cols) + xlab("Strike Zone Side (in)") + ylab("Strike Zone Height (in)") + theme(text = element_text(family = "DINCondensed-Bold", size = 15), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE"), legend.position = "none") + xlim(-35,35) + ylim(0,60)
    ggsave(HBVBPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"HBVB",".png"),width=2.6,height=2.6,units="in",dpi=265)
    ggsave(releasePlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"releaseLoc",".png"),width=2.6,height=2.6,units="in",dpi=265)
    ggsave(SZPlot,file=paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"strikezone",".png"),width=2.6,height=2.6,units="in",dpi=265)

    rapPitchAvgs <- data.frame(matrix(NA, nrow = length(unique(rapPitchPlayer$`Pitch Type`)), ncol = 16))
    colnames(rapPitchAvgs) <- c("Number of Pitches", "AVG Spin", "MAX Spin", "AVG HB", "MAX HB", "AVG VB", "MAX VB", "Spin Eff", "Gyro Deg", "Spin Dir", "AVG Velo", "MAX Velo", "MIN Velo", "Release Height", "Release Side", "Release Angle")
    row.names(rapPitchAvgs) <- unique(rapPitchPlayer$`Pitch Type`)

    absmax <- function(x) { x[which.max( abs(x) )]}

    #calculate pitch statistics for each pitch type for player
    for (j in 1:nrow(rapPitchAvgs)) {
      pitch_type <- row.names(rapPitchAvgs)[j]
      rapPitchAvgs[j,"Number of Pitches"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% nrow()
      rapPitchAvgs[j,"AVG Spin"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(Spin)))
      rapPitchAvgs[j,"MAX Spin"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(max(Spin)))
      rapPitchAvgs[j,"AVG HB"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(mean(`HB (spin)`))
      rapPitchAvgs[j,"MAX HB"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(absmax(`HB (spin)`))
      rapPitchAvgs[j,"AVG VB"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(mean(`VB (spin)`))
      rapPitchAvgs[j,"MAX VB"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(absmax(`VB (spin)`))
      rapPitchAvgs[j,"Spin Eff"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(mean(`Spin Efficiency (release)`))
      rapPitchAvgs[j,"Gyro Deg"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(mean(`Gyro Degree (deg)`))
      rapPitchAvgs[j,"Spin Dir"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(substring(mean(times(`Spin Axis`)), 0, 5))
      rapPitchAvgs[j,"AVG Velo"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(Speed), 1))
      rapPitchAvgs[j,"MAX Velo"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(max(Speed), 1))
      rapPitchAvgs[j,"MIN Velo"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(min(Speed), 1))
      rapPitchAvgs[j,"Release Height"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(`Release Height (ft)`), 1))
      rapPitchAvgs[j,"Release Side"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(`Release Side (ft)`), 1))
      rapPitchAvgs[j,"Release Angle"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(`Release Angle`), 1))

    }

    RPdoc <- image_read_pdf("/Volumes/My Passport/Misc/RapsodoPitchingBlankComplete.pdf")
    HBVB_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"HBVB",".png"))
    release_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"releaseLoc",".png"))
    SZ_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"strikezone",".png"))
    RPdoc <- image_composite(RPdoc, HBVB_pic, offset = "+860+2150")
    RPdoc <- image_composite(RPdoc, release_pic, offset = "+1600+2150")
    RPdoc <- image_composite(RPdoc, SZ_pic, offset = "+120+2150")

    if (!is.na(rapPitchAvgs["Fastball", 1])) {
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Fastball",1], font = "DIN Condensed", size = 45, location = "+550+630")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",13], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+750+630")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",11], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1015+630")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",12], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1278+630")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",8], 1), "%"), font = "DIN Condensed", size = 45, location = "+1578+630")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",9], 1), "°"), font = "DIN Condensed", size = 45, location = "+1855+630")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Fastball",10], font = "DIN Condensed", size = 45, location = "+2142+630")

      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",2], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+464+1451")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",3], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+672+1451")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",4], 1), "\""), font = "DIN Condensed", size = 45, location = "+908+1451")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",5], 1), "\""), font = "DIN Condensed", size = 45, location = "+1097+1451")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",6], 1), "\""), font = "DIN Condensed", size = 45, location = "+1279+1451")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",7], 1), "\""), font = "DIN Condensed", size = 45, location = "+1464+1451")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",14], 1), "\""), font = "DIN Condensed", size = 45, location = "+1700+1451")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",15], 1), "\""), font = "DIN Condensed", size = 45, location = "+1932+1451")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",16], 1), "\""), font = "DIN Condensed", size = 45, location = "+2157+1451")

    }

    if (!is.na(rapPitchAvgs["TwoSeamFastball", 1])) {
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["TwoSeamFastball",1], font = "DIN Condensed", size = 45, location = "+550+715")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",13], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+750+715")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",11], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1015+715")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",12], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1278+715")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",8], 1), "%"), font = "DIN Condensed", size = 45, location = "+1578+715")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",9], 1), "°"), font = "DIN Condensed", size = 45, location = "+1855+715")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["TwoSeamFastball",10], font = "DIN Condensed", size = 45, location = "+2142+715")

      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",2], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+464+1534")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",3], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+672+1534")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",4], 1), "\""), font = "DIN Condensed", size = 45, location = "+908+1534")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",5], 1), "\""), font = "DIN Condensed", size = 45, location = "+1097+1534")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",6], 1), "\""), font = "DIN Condensed", size = 45, location = "+1279+1534")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",7], 1), "\""), font = "DIN Condensed", size = 45, location = "+1464+1534")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",14], 1), "\""), font = "DIN Condensed", size = 45, location = "+1700+1534")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",15], 1), "\""), font = "DIN Condensed", size = 45, location = "+1932+1534")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",16], 1), "\""), font = "DIN Condensed", size = 45, location = "+2157+1534")

    }

    if (!is.na(rapPitchAvgs["ChangeUp", 1])) {
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["ChangeUp",1], font = "DIN Condensed", size = 45, location = "+550+798")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",13], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+750+798")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",11], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1015+798")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",12], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1278+798")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",8], 1), "%"), font = "DIN Condensed", size = 45, location = "+1578+798")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",9], 1), "°"), font = "DIN Condensed", size = 45, location = "+1855+798")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["ChangeUp",10], font = "DIN Condensed", size = 45, location = "+2142+798")

      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",2], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+464+1619")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",3], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+672+1619")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",4], 1), "\""), font = "DIN Condensed", size = 45, location = "+908+1619")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",5], 1), "\""), font = "DIN Condensed", size = 45, location = "+1097+1619")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",6], 1), "\""), font = "DIN Condensed", size = 45, location = "+1279+1619")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",7], 1), "\""), font = "DIN Condensed", size = 45, location = "+1464+1619")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",14], 1), "\""), font = "DIN Condensed", size = 45, location = "+1700+1619")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",15], 1), "\""), font = "DIN Condensed", size = 45, location = "+1932+1619")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",16], 1), "\""), font = "DIN Condensed", size = 45, location = "+2157+1619")

    }

    if (!is.na(rapPitchAvgs["CurveBall", 1])) {
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["CurveBall",1], font = "DIN Condensed", size = 45, location = "+550+881")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",13], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+750+881")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",11], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1015+881")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",12], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1278+881")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",8], 1), "%"), font = "DIN Condensed", size = 45, location = "+1578+881")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",9], 1), "°"), font = "DIN Condensed", size = 45, location = "+1855+881")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["CurveBall",10], font = "DIN Condensed", size = 45, location = "+2142+881")

      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",2], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+464+1700")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",3], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+672+1700")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",4], 1), "\""), font = "DIN Condensed", size = 45, location = "+908+1700")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",5], 1), "\""), font = "DIN Condensed", size = 45, location = "+1097+1700")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",6], 1), "\""), font = "DIN Condensed", size = 45, location = "+1279+1700")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",7], 1), "\""), font = "DIN Condensed", size = 45, location = "+1464+1700")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",14], 1), "\""), font = "DIN Condensed", size = 45, location = "+1700+1700")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",15], 1), "\""), font = "DIN Condensed", size = 45, location = "+1932+1700")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",16], 1), "\""), font = "DIN Condensed", size = 45, location = "+2157+1700")

    }

    if (!is.na(rapPitchAvgs["Slider", 1])) {
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Slider",1], font = "DIN Condensed", size = 45, location = "+550+961")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",13], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+750+961")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",11], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1015+961")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",12], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1278+961")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",8], 1), "%"), font = "DIN Condensed", size = 45, location = "+1578+961")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",9], 1), "°"), font = "DIN Condensed", size = 45, location = "+1855+961")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Slider",10], font = "DIN Condensed", size = 45, location = "+2142+961")

      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",2], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+464+1781")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",3], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+672+1781")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",4], 1), "\""), font = "DIN Condensed", size = 45, location = "+908+1781")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",5], 1), "\""), font = "DIN Condensed", size = 45, location = "+1097+1781")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",6], 1), "\""), font = "DIN Condensed", size = 45, location = "+1279+1781")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",7], 1), "\""), font = "DIN Condensed", size = 45, location = "+1464+1781")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",14], 1), "\""), font = "DIN Condensed", size = 45, location = "+1700+1781")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",15], 1), "\""), font = "DIN Condensed", size = 45, location = "+1932+1781")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",16], 1), "\""), font = "DIN Condensed", size = 45, location = "+2157+1781")
    }

    if (!is.na(rapPitchAvgs["Cutter", 1])) {
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Cutter",1], font = "DIN Condensed", size = 45, location = "+550+1040")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",13], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+750+1040")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",11], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1015+1040")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",12], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1278+1040")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",8], 1), "%"), font = "DIN Condensed", size = 45, location = "+1578+1040")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",9], 1), "°"), font = "DIN Condensed", size = 45, location = "+1855+1040")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Cutter",10], font = "DIN Condensed", size = 45, location = "+2142+1040")

      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",2], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+464+1862")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",3], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+672+1862")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",4], 1), "\""), font = "DIN Condensed", size = 45, location = "+908+1862")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",5], 1), "\""), font = "DIN Condensed", size = 45, location = "+1097+1862")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",6], 1), "\""), font = "DIN Condensed", size = 45, location = "+1279+1862")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",7], 1), "\""), font = "DIN Condensed", size = 45, location = "+1464+1862")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",14], 1), "\""), font = "DIN Condensed", size = 45, location = "+1700+1862")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",15], 1), "\""), font = "DIN Condensed", size = 45, location = "+1932+1862")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",16], 1), "\""), font = "DIN Condensed", size = 45, location = "+2157+1862")
    }

    if (!is.na(rapPitchAvgs["Splitter", 1])) {
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Splitter",1], font = "DIN Condensed", size = 45, location = "+550+1123")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",13], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+750+1123")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",11], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1015+1123")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",12], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1278+1123")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",8], 1), "%"), font = "DIN Condensed", size = 45, location = "+1578+1123")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",9], 1), "°"), font = "DIN Condensed", size = 45, location = "+1855+1123")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Splitter",10], font = "DIN Condensed", size = 45, location = "+2142+1123")

      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",2], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+464+1944")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",3], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+672+1944")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",4], 1), "\""), font = "DIN Condensed", size = 45, location = "+908+1944")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",5], 1), "\""), font = "DIN Condensed", size = 45, location = "+1097+1944")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",6], 1), "\""), font = "DIN Condensed", size = 45, location = "+1279+1944")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",7], 1), "\""), font = "DIN Condensed", size = 45, location = "+1464+1944")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",14], 1), "\""), font = "DIN Condensed", size = 45, location = "+1700+1944")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",15], 1), "\""), font = "DIN Condensed", size = 45, location = "+1932+1944")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",16], 1), "\""), font = "DIN Condensed", size = 45, location = "+2157+1944")
    }

    if (!is.na(rapPitchAvgs["Sinker", 1])) {
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Sinker",1], font = "DIN Condensed", size = 45, location = "+550+1203")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",13], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+750+1203")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",11], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1015+1203")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",12], 1), " MPH"), font = "DIN Condensed", size = 45, location = "+1278+1203")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",8], 1), "%"), font = "DIN Condensed", size = 45, location = "+1578+1203")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",9], 1), "°"), font = "DIN Condensed", size = 45, location = "+1855+1203")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Sinker",10], font = "DIN Condensed", size = 45, location = "+2142+1203")

      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",2], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+464+2024")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",3], 0), " RPM"), font = "DIN Condensed", size = 45, location = "+672+2024")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",4], 1), "\""), font = "DIN Condensed", size = 45, location = "+908+2024")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",5], 1), "\""), font = "DIN Condensed", size = 45, location = "+1097+2024")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",6], 1), "\""), font = "DIN Condensed", size = 45, location = "+1279+2024")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",7], 1), "\""), font = "DIN Condensed", size = 45, location = "+1464+2024")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",14], 1), "\""), font = "DIN Condensed", size = 45, location = "+1700+2024")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",15], 1), "\""), font = "DIN Condensed", size = 45, location = "+1932+2024")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",16], 1), "\""), font = "DIN Condensed", size = 45, location = "+2157+2024")
    }

    doc <- image_join(doc, RPdoc)
    doc <- image_join(doc, RPMdoc)


  
}

  #write final document to wd
  image_write(doc,path=paste0("PDF Full Reports/", internal$`Last Name`[i],internal$`First Name`[i],"FullReportFinal.pdf"),format="pdf", quality = 100,density = 300)

}







