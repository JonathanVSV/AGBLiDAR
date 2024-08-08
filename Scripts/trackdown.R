library(trackdown)

# upload_file("Manuscript/Manuscript_LiDARAGBCometa.Rmd",
#             hide_code = TRUE,
#             path_output = "D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLiDARrevML/Manuscript/Manuscript_LiDARAGBCometa.docx")

# Para subir el archivo a Google Drive
update_file("Manuscript/Manuscript_LiDARAGBCometa.Rmd",
            hide_code = TRUE,
            path_output = "D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLiDARrevML/Manuscript/Manuscript_LiDARAGBCometa.docx")
            #rich_text = TRUE)

# PAra descargarlo
download_file(file = "Manuscript/Manuscript_LiDARAGBCometa.Rmd",
              gfile = "trackdown/Manuscript_LiDARAGBCometa.Rmd",
              # shared_drive = "trackdown",
              gpath = "trackdown")
