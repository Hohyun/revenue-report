###################################################################################
#  Written by Hohyun Kim
#  Last updated: 2016-06-08
#  
#  You need to change execution policy of powershell before using this script
#  > Set-ExecutionPolicy RemoteSigned
####################################################################################

# folder name: should be setup properly.
Set-Location D:\Works\adf\Project\revenue-report

# update revenue.RData
Rscript .\prepareDataForConsole.R

# generate html report
Rscript -e "rmarkdown::render('weekly-revenue-report.Rmd', encoding = 'utf-8')"

# send email
Invoke-Expression -Command ".\mail-send.ps1"
