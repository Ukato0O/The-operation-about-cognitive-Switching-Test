#Setting
install.packages(dplyr)
install.packages(forcats)
install.packages(tidyr)
install.packages(openxlsx)

library(dplyr)
library(forcats)
library(tidyr)
library(openxlsx)
#Main
    data = read.table(file.choose(), header = T, sep = ",")
    dta = data |> dplyr::select(ExperimentName, Subject, Age, Sex, Picture, Key, Key2, Procedure, Running, TaskSti.ACC, TaskSti.RT)
    dta$Sex = factor(dta$Sex, levels = c("男性", "女性"))
    dta$Key = factor(dta$Key, levels = c("NSBlock", "Mixed"))

#tmp為資料操作檔案
    tmp = dta |> dplyr::mutate(Key2 = ifelse(grepl("Mono", Key2), "NS", Key2))
    #改S1(Aging_Switch)
    tmp = dta |> dplyr::mutate(Key2 = ifelse(grepl("S1", Key2), "NS", Key2))
    tmp$Key2 = factor(tmp$Key2, levels = c("NS", "SW"))
    tmp = tmp |> dplyr::mutate(Picture = ifelse(grepl("red", Picture), "red", Picture))
    tmp = tmp |> dplyr::mutate(Picture = ifelse(grepl("green", Picture), "green", Picture))
    tmp$Picture = factor(tmp$Picture)
#消除Prac資料
    tmp = tmp |> dplyr::filter(Procedure == "Switching")
#消除monoblock資料
    tmp = tmp |> dplyr::filter(Key == "Mixed")
#先處理計算ACC，RT會再洗資料(改看Key2)
    account = tmp |> dplyr::group_by(ExperimentName, Subject, Age, Sex, Key2) |>
                     summarise(ACC = mean(TaskSti.ACC), .groups = "drop") |>
                     tidyr::pivot_wider(names_from = Key2, values_from = ACC) |>
                     rename("AC_NS" = "NS", "AC_S" = "SW")
#ACC資料合併
    ACC_Mix = bind_rows(get0("ACC_Mix"), account)


#以下為RT計算的必要資料處理，ACC已經可直接計算
#以1紀錄各類狀態的第一題，附帶簡查MonoBlock資料是否有消除
    tmp = tmp |> dplyr::mutate(Key3 = ifelse(Running != lag(Running, default = "MonoSize"), 1, as.character(Running)))
    tmp1 = tmp |> dplyr::filter(TaskSti.RT > 200 & TaskSti.RT <= 2000) |>
                  dplyr::filter(TaskSti.ACC == 1) |>
                  dplyr::filter(Key3 != 1)
    
    rtcount = tmp1 |> dplyr::group_by(ExperimentName, Subject, Age, Sex, Key2) |>
                      summarise(RT = mean(TaskSti.RT), .groups = "drop") |>
                      tidyr::pivot_wider(names_from = Key2, values_from = RT) |>
                      rename("RT_NS" = "NS", "RT_S" = "SW")
#RT資料合併
    RT_Mix = bind_rows(get0("RT_Mix"), rtcount)
#總資料彙整
    dataset_Mix = rbind(get0("dataset_Mix"), tmp)
###到此重複對所有csv檔執行###
    
    write.xlsx(ACC_Mix, file.choose(), colnames = T)
    write.xlsx(RT_Mix, file.choose(), colnames = T)
    