
# prep

library(mlbench)
data(PimaIndiansDiabetes2)
pima <- PimaIndiansDiabetes2
pima <- na.exclude(pima)

library(qeML)
data(pef)
pef1 <- pef
occ <- ifelse(pef1$occ == '100','yes100','no100')
occ <- as.factor(occ)
pef1$occ <- occ

print("don't just check with Pima; all numeric, no factors")

z <- qeFairRidgeLog(pima,'diabetes',list(pregnant=0.2),'age','pos',holdout=NULL)
newx <- pima[5,-9]
predict(z,newx)  # 0.5913352
set.seed(9999)
z <- qeFairRidgeLog(pima,'diabetes',list(pregnant=0.2),'age','pos')
z$testAcc  # 0.2307692
z$baseAcc  # 0.3316327
z$corrs  # 0.3059256
# must test on data in which some X are factors
set.seed(9999)
z <- qeFairRidgeLog(pef1,'occ',list(age=0.2),'sex','yes100')  
z$testAcc # 0.247
z$baseAcc # 0.2285714
z$corrs # 0.6103466 

z <- qeFairKNN(pima,'diabetes',list(pregnant=0.2),'age','pos',holdout=NULL)
predict(z,newx)  # 0.48
set.seed(9999)
z <- qeFairKNN(pima,'diabetes',list(pregnant=0.2),'age','pos')
holdout set has  39 rows
z$testAcc  # 0.1794872
z$baseAcc  # 0.3316327
z$corrs  # 0.4355221

# qeFairRidgeLin; NOTE: will replace current by the one in New*R
z <- qeFairRidgeLin(pef,'wageinc',list(occ=0.2),'sex',holdout=NULL)
predict(z,pef[1,-5])  # 77260.69
predict(z,pef[1,-(4:5)])  # 77260.69
set.seed(9999)
z <- qeFairRidgeLin(pef,'wageinc',list(occ=0.2),'sex')
z$testAcc  # 26070.07
z$baseAcc  # 32686.57
z$corrs  # 0.2456668

z <- qeSU(pef,'wageinc',list(unfairness=0.2),'sex',holdout=NULL)
predict(z,pef[1,-(4:5)],pef[1,4])  # 70691.8, MUST use 4:5, not 5
set.seed(9999)
z <- qeSU(pef,'wageinc',list(unfairness=0.2),'sex')
z$testAcc  # 25946.28
z$baseAcc  # NULL, must fix
z$corrs  # 0.243306
z <- qeSU(pima,'diabetes',list(unfairness=0.2),'age','pos',holdout=NULL)
newx <- pima[5,-(8:9)]
newsens <- pima[5,8]
predict(z,newx,newsens)  # 0.3656508
z <- qeSU(pima,'diabetes',list(unfairness=0.2),'age','pos')
z$testAcc  # 0.3333333
> z$baseAcc  # NULL, must fix
z$corrs  0.2863631




