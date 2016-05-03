### functions.R

###########################################################
## printmap.R
## print colored worldmap

printmap <- function(trade,io,nafta){
  world <- map_data("world")
  nafta <- c("Canada","Mexico","USA")
  
  mapPaper <- filter(world, is.element(region,c(levels(io$Country),"South Africa", "South Korea")))
  mapNafta <- filter(world, is.element(region,nafta))
  mapROW <- filter(world, !is.element(region,c(levels(io$Country),"South Africa", "South Korea")))
  
  map_world <- ggplot() +
    geom_polygon(data=mapPaper, aes(x=long, y=lat, group=group), fill="orange") +
    geom_polygon(data=mapNafta, aes(x=long, y=lat, group=group), fill="red") +
    geom_polygon(data=mapROW, aes(x=long, y=lat, group=group), fill="grey77") +
    geom_path(data=world, aes(x=long, y=lat, group=group)) +
    scale_y_continuous(breaks=30*(-2:2)) +
    scale_x_continuous(breaks=45*(-4:4)) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(colour="grey"),
          panel.background = element_rect(fill = "white"))
  map_world 
}

############################################################
## printplot3.R
## print pplot3

printplot3 <- function(trade,nafta){
 
 nafta <- c("Canada","Mexico","USA")

 pp3 <- trade %>%
    mutate(NAFTA_Source=ifelse(is.element(Source_C,nafta),"NAFTA","World_beyond")) %>%
    group_by(NAFTA_Dest,NAFTA_Source) %>%
    summarize(Tariffs=sum(Trade_Vol*Tariffs1993)/sum(Trade_Vol), 
              Tariffs05=sum(Trade_Vol*Tariffs2005)/sum(Trade_Vol)) 
  print(pp3)
  
  pplot3 <- ggplot(pp3) + 
    geom_bar(mapping=aes(x=NAFTA_Source, y=Tariffs),stat="identity", 
             position=position_dodge(0.8),  fill="lightblue", colour="lightblue4") +
    geom_bar(mapping=aes(x=NAFTA_Source, y=Tariffs05),stat="identity", 
             fill="darkorange", color="orangered") +
    facet_grid(.~NAFTA_Dest)
  pplot3
  
}

###########################################################
## print_frechet

print_frechet <-function(a=0,b=2.4){
  t <- c(0.028,0.582,0.65)
  o <- 8.28
  y <- matrix(0,ncol=4,nrow=1001)
  y[,1] <- (0:1000)/1000*(b-a) +a
  y[,2] <- exp( -t[1]*y[,1]^-o)
  y[,3] <- exp( -t[2]*y[,1]^-o)
  y[,4] <- exp( -t[3]*y[,1]^-o)
  y <- data.frame(y)
  colnames(y) <- c("xx","Portugal","Japan","USA")
  
  pp <- ggplot(y) +
    geom_line(mapping=aes(x=xx, y=Portugal, color="Portugal")) +
    geom_line(mapping=aes(x=xx, y=Japan,        color="Japan")) +
    geom_line(mapping=aes(x=xx, y=USA,       color="USA")) +
    scale_colour_manual("", 
                        breaks = c("Portugal", "Japan", "USA"),
                        values = c("red", "green", "blue")) +
    labs(title="Frechet cumulative density distributions", x="Production efficiency", y="Probability") #+
  pp
  
}


print_frechet2 <-function(a=0,b=2.4){
  t <- c(0.18,0.32,0.65)
  o <- 8.28
  y <- matrix(0,ncol=4,nrow=1001)
  y[,1] <- (0:1000)/1000*(b-a) +a
  y[,2] <- exp( -t[1]*y[,1]^-o)
  y[,3] <- exp( -t[2]*y[,1]^-o)
  y[,4] <- exp( -t[3]*y[,1]^-o)
  y <- data.frame(y)
  colnames(y) <- c("xx","Australia","UK","USA")
  
  pp <- ggplot(y) +
    geom_line(mapping=aes(x=xx, y=Australia, color="Australia")) +
    geom_line(mapping=aes(x=xx, y=UK,        color="UK")) +
    geom_line(mapping=aes(x=xx, y=USA,       color="USA")) +
    scale_colour_manual("", 
                        breaks = c("Australia", "UK", "USA"),
                        values = c("red", "green", "blue")) +
    labs(title="Frechet cumulative density distributions", x="Production efficiency", y="Probability") #+
  pp
  
}

#################################################

#### get_thetas


get_thetas <- function(Data, Sectors=NULL){
  
  if(!is.null(Sectors)){  
    Data <- filter(Data, is.element(sector,Sectors))
  }
  Data <- Data[complete.cases(Data),]
  
  nb_sect <- length(Sectors) 
  
  agg <- lm(data=Data, formula = y ~0+x)
  agg_s <- summary(agg) 
  
  agg_se <- agg_s$sigma
  agg_c <- agg_s$coefficients[1,1]
  
  
  sec_temp <- ifelse(nb_sect==1,
                     Sectors,
                     "multiple sectors")
  
  list(DOF=df.residual(agg), Theta= -agg_c, Sectors= sec_temp)
}


#################################################

#### print_thetas


print_thetas <- function(Data, Sectors=NULL){
  
  if(!is.null(Sectors)){  
    Data <- filter(Data, is.element(sector,Sectors))
  }
  Data <- Data[complete.cases(Data),]
  
  nb_sect <- length(Sectors) 
  
  agg <- lm(data=Data, formula = y ~0+x)
  agg_s <- summary(agg) 
  
  agg_se <- agg_s$sigma
  agg_c <- agg_s$coefficients[1,1]
  
  
  sec_temp <- ifelse(nb_sect==1,
                     Sectors,
                     "multiple sectors")
  ttitle = cat("Estimation of the negative trade elasticity in", sec_temp, ":")
  #print("Estimation of the negative trade elasticity in", sec_temp, ":")
  
  p <- ggplot(Data) +
    geom_jitter(mapping=aes(x=x,y=y)) +
    geom_abline(intercept=0, slope=agg_c) +
    labs(title="Estimation of the negative trade elasticity", x="x-value in the regression = coefficient of -(Theta) in equation 13", y="y-value in the regression, equation 13")
  p
 
}

###########################################################

### trade_elasticities
### some calculations that explain the thetas

trade_elasticities <- function(dest,sect,sour,percentage_change=1,trade){
  
  # data for sensitivity analysis
  data_sensi <- filter(trade, Sector==sect, Dest_C==dest)
  theta <- unique(data_sensi$Trade_Elast)
  
  go_s <- unique(filter(trade, Source_C==dest, Sector==sect)$GO_Source) 
  expo <- summarize(filter(trade,Source_C==dest, Sector==sect), exp=sum(Trade_Vol))$exp
  # calculate domestic sales
  domsales <- go_s - expo

  foreign_expend <- trade %>%
    filter(Dest_C==dest, Sector==sect) %>%
    transmute(Source_C=Source_C, Expend=(1+Tariffs1993/100)*Trade_Vol) 
  
  sum_foreign_expend <- summarize(foreign_expend,total=sum(Expend))
  
  total_expend <- sum_foreign_expend + domsales

  expend <- bind_rows(foreign_expend,data.frame(Source_C=as.character(dest), Expend=domsales ) )
  rm(foreign_expend)

  expend_share <- mutate(expend, Expend_shares =100*Expend/as.numeric(total_expend))
  names(expend_share) <- c("countries", "expend", "expend_shares")
  expend_share <- arrange(expend_share,countries)

  countries <- data.frame(countries=unique(trade$Source_C),row.names = NULL)
  sectors <- unique(trade$Sector)

  tariffs <- transmute(filter(trade,Sector==sect,Dest_C==dest), countries=Source_C, tariffs=Tariffs1993)
  tariffs <- bind_rows(tariffs,data.frame(countries=as.character(dest),tariffs=as.numeric(0)) )
  tariffs$tariffs <- tariffs$tariffs/100 +1
  tariffs <- arrange(tariffs,countries)
  
  tariffs <- mutate(tariffs,tariffs_t=tariffs^(-theta))
  
  
  data_all <- left_join(expend_share,tariffs)

  data_all <- mutate(data_all, S=expend/tariffs_t)
  data_all <- mutate(data_all, tariffs_new= ifelse(countries==sour, 1+(tariffs-1)*(1+percentage_change/100),tariffs))
  data_all <- mutate(data_all, tariffs_tn = tariffs_new^(-theta))
  data_all <- mutate(data_all, expend_shares_new = 100*(S*tariffs_tn) / sum(S*tariffs_tn))
  


	cat("Sector: ", sect, "\n")
 
    	cat("Theta: ", theta, "\n")

	cat("Difference: ", transmute(filter(data_all,countries==sour),out=expend_shares_new-expend_shares)$out, "\n")

	cat("Changes in percent: ", transmute(filter(data_all,countries==sour),out=100*(expend_shares_new-expend_shares)/expend_shares)$out, "\n")

}

####################################

