library(tidyr); library(dplyr); library(arules)
load("~/Cross Sell/NBP Recommend/2017-09/Anonymized/assocs.RData")
btidy = gather(baskets, OLP, Owns, -ID) %>% filter(Owns>0) %>% arrange(ID)
user_item_matrix = as(split(btidy[,"OLP"], btidy[,"ID"]), "transactions")

b2 = baskets[2, ] #The 2nd basket. Want to mine rules so as to recommend items for this basket.

rhsv <- setdiff(unique(btidy$OLP), exclude.rhs) #Items eligible to appear in RHS.
lhsv <- names(b2[,-1])[b2[,-1] > 0]  #Vector of names of items in the customer's basket.
lhsv <- setdiff(lhsv, exclude.lhs)  #Items eligible to appear in LHS.

appearList = list(A=list(default="rhs", lhs=lhsv), #Constrain appearance of items in LHS.
                  B=list(default="lhs", rhs=rhsv), # .............................in RHS.
                  C=list(default="none", lhs=lhsv, rhs=rhsv)) #........... in both sides.

#Mine rules for use by the 2nd basket.
rules = apriori(data=user_item_matrix, 
                parameter=list(supp=0.03, conf = 0.05, maxlen=11), 
                appearance = appearList$A,
                control = list(verbose=F))
