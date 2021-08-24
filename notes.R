selectCases(randomCsf(full.ct(list(A = 0:2, B = 0:2, C = 0:2, D = 0:2, E = 0:2)), n.asf = 2), 
                      full.ct(list(A = 0:2, B = 0:2, C = 0:2, D = 0:2, E = 0:2)))


fct <- full.ct(list(A = 1:2, B = 1:2, E = 1:2))




ta <- randomAsf(mvct(fct))
               
selectCases("A = 1 * B = 1 <-> E = 1", mvct(fct))               


selectCases("A <-> E", full.ct(5))

ct <- full.ct(list(A = 0:2, B = 0:2, C = 0:2, D = 0:2, E = 0:2))
  
df <- selectCases("A=1*B=2*C=2*D=2<->E=0", ct)  
#df
nrow(df)
nrow(df)/3


ct2 <- full.ct(list(A = 0:2, B = 0:2, C = 0:2, D=0:2))
df2 <- selectCases("A=1*B=1<->C=1", ct2)  

nrow(df2)

ct2 <- ct2df(ct2)

selectCases("(A=1*B=1<->C=1)*(D=1+B=2<->C=2)", ct2)


dplyr::setdiff(ct2df(ct2), ct2df(df2))
 
ct3 <- full.ct(list(A = 0:2, B = 0:2))
df3 <- selectCases("A=1<->B=1", ct3)  
dplyr::setdiff(ct2df(ct3), ct2df(df3))


csd <- full.ct(3)
selectCases("A=1*B=1<->C=1", csd) 
ct2df(csd) |> filter(A==1&B==1)
