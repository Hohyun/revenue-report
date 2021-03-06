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

# show dir
Get-ChildItem
Write-Host ""

$x = Read-Host -prompt "Which job do you want to do? [1] open shiny-app [2] open html-report  [3] quit ..."

#$x = $host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")

If ($x -eq 1) {
    # open shiny app
    Rscript -e "shiny::runApp(launch.browser=TRUE)"
} ElseIf ($x -eq 2) {
    # open html report
    Invoke-Item .\weekly-revenue-report.html
}