library(mlr3verse)
library(mlr3oml)
library(mlr3mbo)
library(DiceKriging)
library(ranger)
library(skimr)
library(data.table)
#import the datasets
source("configuration.R")
tasks = lapply(ids, function(i) tsk("oml", data_id = i))
# investigate the structure of datasets
skim(tasks[[1]]$data())
skim(tasks[[2]]$data())
skim(tasks[[3]]$data())
skim(tasks[[4]]$data())
skim(tasks[[5]]$data())
skim(tasks[[6]]$data())
skim(tasks[[7]]$data())
skim(tasks[[8]]$data())
skim(tasks[[9]]$data())
skim(tasks[[10]]$data())
#guarantee reprodicibility
set.seed(123456)
#do the imputation separately for numerical and factorial variable
pom=po("missind")
nop=po("nop",id="nop1")
pon=po("imputehist",id="imputer_num")
pou=po("featureunion",id="union1")
pof=po("imputelearner",lrn("classif.rpart",id="rpart1"),id="imputer_fct",affect_columns=
         selector_type("factor"))
impgraph = list(
  pom,
  nop
) %>>% pou %>>% pof %>>% pon

#build the stacking graph
lrn = lrn("classif.rpart",id="rpart2")
lrn_0 = po("learner_cv", lrn$clone(),id="rpart_cv")

level_0 = gunion(list(lrn_0, po("nop")))%>>%po("featureunion",id="union2")


# Built the graph of operators to tune over
imbalanced_graph = po("removeconstants")%>>%impgraph%>>%po("classbalancing",id="oversample",
                   adjust="minor",reference="minor",shuffle=F)%>>%
                   level_0%>>%po("learner",learner=
                   lrn("classif.ranger",num.trees=50))
# show the built graph
imbalanced_graph$plot(html = FALSE)
# Convert Graph into a mlr3 learner
imbalanced_supermodel = GraphLearner$new(imbalanced_graph)

# set the search space for hyperparameters to tune
search_space_over=ps(oversample.ratio=p_dbl(1,10),classif.ranger.mtry.ratio=p_dbl(0,1),classif.ranger.sample.fraction=p_dbl(0.1,1)
                     )
# Make the learner self-tuning with the AutoTuner
imbalanced_automl = auto_tuner(imbalanced_supermodel,method = tnr("mbo"),resampling = rsmp("cv",folds=3),
                               measure = msr("classif.bacc"),terminator=trm("evals",n_evals=2),search_space = search_space_over)
#outer resampling for nested resampling
resampling_outer = rsmp("cv",folds=4)
#build an empty list for results
list<-vector(mode="list",length = length(tasks))
#graph for random forest learner with default hyerparameter settings
ranger2<-po("removeconstants")%>>%impgraph%>>%po("learner",learner=lrn("classif.ranger",id="ranger2"))
#reproducibility guarantee
  set.seed(654321)
#do the benchmark tests between designed and untuned learner
  design=benchmark_grid(tasks[[1]],list(imbalanced_automl,ranger2),resampling_outer)
  bmr=benchmark(design,store_models=T)
  res<-bmr$aggregate(measure)
  list[[1]]<-res
  
  design=benchmark_grid(tasks[[2]],list(imbalanced_automl,ranger2),resampling_outer)
  bmr=benchmark(design,store_models=T)
  res<-bmr$aggregate(measure)
  list[[2]]<-res

  design=benchmark_grid(tasks[[3]],list(imbalanced_automl,ranger2),resampling_outer)
  bmr=benchmark(design,store_models=T)
  res<-bmr$aggregate(measure)
  list[[3]]<-res

  design=benchmark_grid(tasks[[4]],list(imbalanced_automl,ranger2),resampling_outer)
  bmr=benchmark(design,store_models=T)
  res<-bmr$aggregate(measure)
  list[[4]]<-res

  design=benchmark_grid(tasks[[5]],list(imbalanced_automl,ranger2),resampling_outer)
  bmr=benchmark(design,store_models=T)
  res<-bmr$aggregate(measure)
  list[[5]]<-res
  
  design=benchmark_grid(tasks[[6]],list(imbalanced_automl,ranger2),resampling_outer)
  bmr=benchmark(design,store_models=T)
  res<-bmr$aggregate(measure)
  list[[6]]<-res
  
  design=benchmark_grid(tasks[[7]],list(imbalanced_automl,ranger2),resampling_outer)
  bmr=benchmark(design,store_models=T)
  res<-bmr$aggregate(measure)
  list[[7]]<-res
  
  design=benchmark_grid(tasks[[8]],list(imbalanced_automl,ranger2),resampling_outer)
  bmr=benchmark(design,store_models=T)
  res<-bmr$aggregate(measure)
  list[[8]]<-res
  
  design=benchmark_grid(tasks[[9]],list(imbalanced_automl,ranger2),resampling_outer)
  bmr=benchmark(design,store_models=T)
  res<-bmr$aggregate(measure)
  list[[9]]<-res

  #bind results together
  final_list<-rbindlist(list[c(1:9)])
  final_list[,c(3,7)]
  