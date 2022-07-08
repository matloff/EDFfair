
# prep

library(mlbench)
data(PimaIndiansDiabetes2)
pima <- PimaIndiansDiabetes2
pima <- na.exclude(pima)

library(qeML)

print("don't just check with Pima; all numeric, no factors")

z <- qeFairRidgeLog(pima,'diabetes',list(pregnant=0.2),'age','pos',holdout=NULL)
newx <- pima[5,-9]
predict(z,newx)  # 0.5913352
set.seed(9999)
z <- qeFairRidgeLog(pima,'diabetes',list(pregnant=0.2),'age','pos')
z$testAcc  # 0.2307692
z$baseAcc  # 0.3316327
z$corrs  # 0.3059256

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
predict(z,pef[1,-5])  # 76741.08
predict(z,pef[1,-(4:5)])  # 76741.08
set.seed(9999)
z <- qeFairRidgeLin(pef,'wageinc',list(occ=0.2),'sex')
z$testAcc  # 26085.05
z$baseAcc  # 32686.57
z$corrs  # 0.2453135 

z <- qeSU(pef,'wageinc',list(unfairness=0.2),'sex',holdout=NULL)



