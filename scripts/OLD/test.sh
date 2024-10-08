#!/bin/bash
#$ -cwd #uses current working directory
# error = Merged with joblog
#$ -o /u/home/y/yingyanw/edu_cog_CHARLS/joblogs/joblog.$JOB_ID.$TASK_ID #creates a file called joblog.jobidnumber to write to. 
#$ -j y 
#$ -l h_rt=10:00:00,h_data=16G #requests 10 hours, 16GB of data (per core) 
#$ -pe shared 1 #requests 1 core
# Email address to notify
#$ -M $USER@mail #don't change this line, finds your email in the system 
# Notify when
#$ -m bea #sends you an email (b) when the job begins (e) when job ends (a) when job is aborted (error)
# submit array job:
# run an array of n jobs; this corresponds to $SGE_TASK_ID, nrow of scenario_tib
#$ -t 1-1:1
## 

# load the job environment:
. /u/local/Modules/default/init/modules.sh
# To load R 4.3.0 (survey package and WeMix package)
module load gcc/10.2.0
module load R/4.3.0 #loads R/4.3.0 for use
module load nlopt
export OMP_NUM_THREADS=1 #uses max 1 thread (needs to match -pe shared)


echo "======"
echo SGE_TASK_ID=$SGE_TASK_ID      
R CMD BATCH --no-save --no-restore "--args scenario_num=$SGE_TASK_ID"  /u/home/y/yingyanw/edu_cog_CHARLS/scripts/test_imp_1_boot_8.R /u/home/y/yingyanw/edu_cog_CHARLS/console_output/output.$JOB_ID.$SGE_TASK_ID
echo R CMD BATCH --no-save --no-restore "--args scenario_num=$SGE_TASK_ID"  /u/home/y/yingyanw/edu_cog_CHARLS/scripts/test_imp_1_boot_8.R /u/home/y/yingyanw/edu_cog_CHARLS/console_output/output.$JOB_ID.$SGE_TASK_ID