****************************************************************

The latest version of this library is stored at

https://github.com/YitaoLiang/Scala-LearnPsdd, 

where many known bugs have been fixed. 

****************************************************************

This repository contains the LearnPSDD code and all the experiments we run for the paper "Learning the Structure of Probabilistic Sentential Decision Diagrams" published in UAI, 2017.

Code can be compiled by running "sbt assembly". The code would create a psdd.jar in the directory target/scala-2.11
Experiments can be reproduced by running "java -jar target/scala-2.11/psdd.jar <name of learner> <name of dataset> <number of component learners>". For example, if you want to benchmark LearnPSDD on dataset nltcs, please use the command "java -jar target/scala-2.11/psdd.jar SoftEM nltcs 5".
  
All these 20 datasets can be found on https://github.com/UCLA-StarAI/Density-Estimation-Datasets

If you have any problem regarding this project, feel free to shoot me an email at "yliang@cs.ucla.edu".

