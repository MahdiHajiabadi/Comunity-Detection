setwd('/home/khsh/MetaData/Codes/Inference')
library('igraph')
gr = read.graph('/home/khsh/MetaData/Codes/Inference/Elizeveth-2-4.gml',format = 'gml')
gr = as.undirected(gr)
Nodes_No = length(V(gr))
Sparsity = 2 * length(E(gr))/(Nodes_No * (Nodes_No - 1))
alpha = 0.01
# Sparsity = 0.09
community = 2
Membership = matrix(0,community,length(V(gr)))
##########Just for the Lawyer Friendship Network
# Membership[1,c(17,neighbors(gr,17))] = 1
# Membership[2,c(65,neighbors(gr,64))] = 1
########################## Conductance for Initialization and Building the Membership functions
# Con = Conductance(gr = gr,community)
# index = which(Con[1,]==0)
# for (i in 1:community) {
#   index = which(Con[i,]>0)
#   Membership[i,Con[i,index]] = 1
# }
####+================================================Initializing With Spectral Clustering
A = get.adjacency(gr)
D.inv = diag(1./(sqrt(apply(A, 1, sum))+1e-7));
Laplacian = D.inv %*% A %*% D.inv;
L.svd = svd(Laplacian);
U.K = L.svd$u[, 1:community];
spec.cluster = kmeans(U.K, community, nstart=10)$cluster;
G.fit = array(0, c(Nodes_No, community));
for(k in 1:community){
  G.fit[spec.cluster==k, k] = 1;
}
Membership =  t(G.fit)
############################ Extract the Attributes of each nodes (Inherited and Generated)
#  Temp = as.double(V(gr)$Inherit)
#=================================For WeddellSea Network
 Temp = as.double(V(gr)$Inherit)
S = matrix(0,length(V(gr)),community)
S[,1] = Temp
S[,2] = 1 - Temp
 F = matrix(0,length(V(gr)),community)
F[,1] = as.double(V(gr)$Generated1)
F[,2] = as.double(V(gr)$Generated2)
####################### Parameters Definition
I = matrix(data = 0,nrow = length(S[1,]),ncol = community)
I[1,2] = 3
I[2,1] = 3
W = matrix(data = 0,nrow = length(F[1,]),ncol = community)
# W[1,1] = 3
# W[2,2] = 3
# I = matrix(rexp(length(F[1,])),nrow = length(S[1,]),ncol = community)
Beta =  0.3 * diag(community) + 0.1
################Updating the Hidden Variables
Struct = matrix(0,community,1)
SecElement = Struct
SumNeigh = Struct
DeltaLatent = matrix(0,community,length(V(gr)))
 for (iter in 1:5){
  for (i in 1:length(V(gr))){
    Neigh = neighbors(graph = gr,v = i)
    Temp = Membership[,Neigh]
    Struct1 = Membership[,i]
    First =t(Struct1)%*% Beta%*%Temp
    # print(First)
    Struct = sum(First)
    for (j in 1:community){
      if (length(Temp)==community){
        # print(i)
        SumNeigh[j,1] = sum(Temp[j])
        Sum = sum(Membership[j,])
        SecElement[j,1] = Sum - SumNeigh[j,1] - Membership[j,i]
      }
      else{
        SumNeigh[j,1] = sum(Temp[j,])
        Sum = sum(Membership[j,]) * Sparsity
        SecElement[j,1] = Sum - SumNeigh[j,1] - Membership[j,i]
      }
    }
    # SecFinal = Sparsity * Beta%*%SecElement
    SecFinal = Beta%*%SecElement
    Exponent = exp(-Struct)
    Up = Exponent
    Down = 1 - Exponent
    if (Down==0){Down = 1}
    Struct = Up/Down 
    Struct = Struct * Beta%*%SumNeigh
    Down = (1/(1+exp(-1 * S[i,]%*%I)))^2
    Up = exp(-1 * S[i,]%*%I)
    Inh = Up/Down
    Difference = S[i,]%*%t(Inh)
#     StructCoef = as.vector(Difference) * (Struct )
    StructCoef = as.vector(Difference) * (Struct - SecFinal)
#     MaxCol = which.max(StructCoef)
# print(MaxCol)
#     StructCoef[MaxCol] = abs(StructCoef[MaxCol])
    
    Q = (F[i,] - 1/(1 + exp(-1 * Membership[,i] %*% t(W))))%*% W
    # DiffAtt = as.vector(Difference)%*%Q
    DiffAtt = Q
    for (j in 1:community){
      DeltaLatent[j,i]  = DeltaLatent[j,i] +  StructCoef[j] + DiffAtt[j] 
    }
  MaxCol = which.max(DeltaLatent[,i])
  DeltaLatent[MaxCol,i] = abs(DeltaLatent[MaxCol,i])
  }
print('Updating The Latent Variable is Done')
   Membership = Membership + alpha * DeltaLatent
  ###################### Updating Parameters
  ################### Updating the I parameter Section
  DeltaI = matrix(data = 0,nrow = length(S[1,]),ncol = community)
  DeltaBeta = matrix(0,community,community)
  DeltaW = matrix(data = 0,nrow = length(F[1,]),ncol = community)
  for (i in 1:length(V(gr))){
    Current = S[i,]
    Neigh = neighbors(gr,v = i)
    First = S[i,]
    First = 1 + exp(-First%*%I)
    Temp = S[Neigh,]
    Vec = matrix(0,community,1)
    NonNeigh = Vec
    for (j in 1:community){
      if (length(Temp)==community){
        Vec[j] = sum(Temp[j])
        Non  = sum(S[,j])
        NonNeigh[j] = Non - Vec[j] - Current[j]
      }
      else{
        Vec[j] = sum(Temp[,j])
        Non  = sum(S[,j])
        NonNeigh[j] = Non - Vec[j] - Current[j]
      }
    }
    # print(i)
    Second = 1 + exp(-t(Vec)%*%I)
    Up  = (Current)%*%(exp(-S[i,]%*%I))
    Down = 1/(First^2)%*%Beta%*%(1/t(Second))
    Down = as.vector(Down)
    if (Down==0){Down = 1}
    FirstElement = Up/Down
    Up  = (Vec)%*%(exp(-t(Vec)%*%I))
    Down = 1/(First)%*%Beta%*%(1/t(Second^2))
    Down = as.vector(Down)
    if (Down==0){Down = 1}
    FirstElement = FirstElement + Up/Down
    ########################################Non Neighbors for I Parameters
    Second = 1 + exp(-t(NonNeigh)%*%I)
    Up  = (Current)%*%(exp(-S[i,]%*%I))  
    Down = 1/(First^2)%*%Beta%*%(1/t(Second))
    Down = as.vector(Down)
    FirstElement = FirstElement + Up/Down
    Up  = (NonNeigh)%*%(exp(-t(NonNeigh)%*%I))
    Down = 1/(First)%*%Beta%*%(1/t(Second^2))
    Down = as.vector(Down)
    if (Down==0){Down = 1}
    FirstElement = FirstElement + Up/Down
    DeltaI = DeltaI + FirstElement
    ############## Updaing For the Causing Features  The First And The Third
    Current = S[i,]
    CausingCurrent = F[i,]
    First = 1 + exp(-Current%*%I)
    First = 1/First
    Up = exp(-First%*%t(W))%*%W
    Coef = Up
    Up = Coef%*%t(exp(-Current%*%I))
    Up = as.vector(Coef) * Current%*%t(CausingCurrent)
    Up2 = as.vector(Coef) * Current%*%t(1 - CausingCurrent)
    First = (1 + exp(-Current%*%I))^2
    Temp = (1 + exp(-Current%*%I))
    Temp = ((1/Temp)%*%t(W)) + 1
    Down = t(First)%*%Temp
    FirstDelta = Up %*%t(1/Down)
    ThirdDelta = Up2%*%t(1/Down)
    ################# Updaing The Second Causing Features
    CausingCurrent = 1 - CausingCurrent
    First = CausingCurrent%*%W
    First = First%*%t((exp(-Current%*%I)))
    Up = as.vector(First) * Current
    Down = (1 + exp(-Current%*%I))^2
    SecondDelta = -Up%*%(1/Down)
    ################### Updaing The Final I
    DeltaI = DeltaI + FirstDelta + SecondDelta + ThirdDelta
    ################### Updating The W Parameter
    CausingCurrent = F[i,]
    Temp = 1 + exp(-Membership[,i]%*%t(W))
    Temp = 1/Temp
    First = CausingCurrent - Temp
    DeltaW = DeltaW + t(First)%*%Membership[,i]  
    ################### Updaing The Beta Parameter
    MCurrent = Membership[,i]
    Neigh = neighbors(graph = gr,v = i)
#     if (length(Neigh)==1){
      MNeighbors = Membership[,Neigh]
    }
    NonNeigh = MCurrent
    SumCurrent = MCurrent
  # print(Neigh)
    for (j in 1:community){
      if (length(MNeighbors)==2){
        SumCurrent[j] = sum(MNeighbors[j])
        Non  = sum(Membership[j,])
        NonNeigh[j] = Non - MCurrent[j] - SumCurrent[j]
      }
      else{
        SumCurrent[j] = sum(MNeighbors[j,])
        Non  = sum(Membership[j,])
        NonNeigh[j] = Non - MCurrent[j] - SumCurrent[j]
      }
#       SumCurrent[j] = sum(MNeighbors[j,])
#       Non  = sum(Membership[j,])
#       NonNeigh[j] = Non - MCurrent[j] - SumCurrent[j]
    }
    First = exp(-MCurrent%*%Beta%*%SumCurrent)
    Up = as.vector(First) * MCurrent%*%t(SumCurrent)
    Down = 1 - exp(-MCurrent%*%Beta%*%SumCurrent)
    if (Down==0){Down = 1}
    FirstCoef = Up/(as.vector(Down))
    Second = MCurrent%*%t(NonNeigh)
    DeltaBeta = DeltaBeta + FirstCoef - Second    
  }
  I = I + alpha * DeltaI
  W = W + alpha * DeltaW
# }

###############################Evaluation Section
# 
# 
Threshold = -log2(1 - 1/Nodes_No)
Threshold = Threshold^(1/4)
SSS2 = matrix(0,community,length(V(graph = gr)))
for (j in 1:community){
  SSS2[j,Membership[j,]>Threshold] =  1
}

IDX = dim(Membership)
FinalMembership = matrix(0,IDX[1],IDX[2])
for (j in 1:length(V(gr))){
  index = which.max(Membership[,j])
  FinalMembership[index,j] = 1
}
SSS = matrix(0,community,length(V(gr)))
# index1 = which(Membership[1,]>0)
# mean1 = mean(Membership[1,index1])
# SSS[1,Membership[1,]>mean1] = 1
# index2 = which(Membership[2,]>0)
# mean2 = mean(Membership[2,index2])
# SSS[2,Membership[2,]>mean2] = 1
Result = final(final_class = SSS2,truth = t(S))
 print(compare(Result,t(S),'nmi'))
FMeasure = f1_score(gr,Result,t(S),community)
# compare(SSS,t(S),'nmi')
# f1_score(gr,SSS,t(S),community)
# compare(SSS2,t(S),'nmi')
# f1_score(gr,SSS2,t(S),community)
# compare(final12,t(S),'nmi')
# f1_score(gr,final12,t(S),community)