municipios2 <- municipios
colnames(municipios2)[3] <- c("CVE_MUN")
rural<- left_join(rural,municipios2[3:4])
View(rural)

ageb <- c()
mun <- subset(addresses2_hits,(grepl("municipality matches!",addresses2_hits$HITS)))
mun<- mun$MUNICIPIO
for (i in mun) {#randomly creating various AGEB's to add to df
  tmp <- rural$CVE_AGEB[rural$Nombre.del.Municipio == i]#whichever ageb's equal that iteration name
  tmp1 <- sample(tmp, size = 1)#sample 1 from those ageb's
  ageb <- c(ageb, tmp1)#adding to the vector every time
}
ageb


placeholder <- data.frame(Nombre.del.Municipio=mun,CVE_AGEB=ageb)%>%
  left_join(rural)
View(placeholder)

##Example 
# mun <- c(1, 1, 1, 6, 6, 6)
# ageb <- c()
# 
# for (i in mun) {#randomly creating various AGEB's to add to df
#   tmp <- rural$CVE_AGEB[rural$CVE_MUN == i]#whichever ageb's equal that iteration name
#   tmp1 <- sample(tmp, size = 1)#sample 1 from those ageb's
#   ageb <- c(ageb, tmp1)#adding to the vector every time
# }
# ageb
# df <- data.frame(CVE_MUN = mun, CVE_AGEB = ageb) %>%
#   left_join(rural)
# df
#so here add ageb to addresses2

##cODE For getting right amount of sampling
#For loop, nesting a while loop with target depending on the numb of schools needed
target=1
n <- 0
# install.packages("lwgeom")
library(lwgeom)

# for (i in seq()){
  while(n < target){
    rand.coords<- st_sample(urbana2$geometry[1],target,"random")#+2*target
    n <- length(rand.coords)
    print(n)
    if (n >= target) k <- rand.coords[1]
  }
    
    if (n >= target){
      portion <- sample(x = 1:n, target)
      rand.coords <- rand.coords[portion]#sampling from random positions (13,10,12) in rand.coords
      #from above to get exactly the target of 10
    }
  }
}
rand.coords


###NOTES
> nrow(unique(select(rural, CVE_MUN, CVE_AGEB)))
[1] 346
> length(unique(select(rural, CVE_MUN, CVE_AGEB)))
[1] 3
> unique(select(rural, CVE_MUN, CVE_AGEB)) %>% nrow()
[1] 346
rural %>% select(CVE_MUN,CVE_AGEB) %>% unique
