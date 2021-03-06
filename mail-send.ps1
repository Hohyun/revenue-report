##############################################################################################
#  This is powershell script to send e-mail using outlook
# 
#  Written by Hohyun Kim
#  Last updated: 2016-06-08
# 
#  Next variable should be setup properly before executing this script
#   - $recipients
#   - $reportFile
#   - $asOfDate 
###############################################################################################

$to = "skykim63@icloud.com; skykim63@gmail.com"
$cc = "hohkim@koreanair.com"
$attach = "D:\Works\ADF\Project\revenue-report\weekly-revenue-report.html"
$asOfDate = (get-date).AddDays(-(get-date).DayOfWeek-1).ToShortDateString()

# create object
$OL = New-Object -ComObject outlook.application
#Create Item
$mail = $OL.CreateItem("olMailItem")
$mail.To = $to
#$Mail.Recipients.Add("User1@domain.com") 
$mail.CC = $cc
$mail.Subject = ("Weekly Revenue Report (As Of: {0})" -f $asOfDate)
$mail.Body = ("{0} 현재 주간, 월간, 년간 수익 현황을 첨부와 같이 보고 드립니다." -f $asOfDate)
$mail.Attachments.Add($attach)
$mail.Send()