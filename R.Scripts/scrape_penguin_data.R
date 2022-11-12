penguin_data =read.table("https://storage.googleapis.com/kagglesdsdata/datasets/703056/1228604/penguins_lter.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20221112%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20221112T183702Z&X-Goog-Expires=259200&X-Goog-SignedHeaders=host&X-Goog-Signature=ae671c2bbb45fdc7cf6e7322d51b2c0490c2a038c7484afad326b70cdc15b16acfed4cc95f5c4ab03efca4d6b23e609b84cafbabd8968e3d9b157080e0f3cfefbf0cee308f2ce8bdfcf4e7910d3e057aebae822286fd68bbce181f113194be974fa0e531ae2fc39ae1c1cb9d6155025aeb48c5c6b04d1b39865461964afce5e3b3631ab466db6c172eb51e884fa3d91536ce9c1d47eb81eea274a2b7de8dc2d43db7fc217d15d37ad341db0d8b481e7895ea6f464c1cd16b3cd03919d5ba7dcf0176ec5e74f28c5df0d087e54b44a53d6ad659fbf8f8e0ae128e4318da4e78e5892818a4d19cc3e005c278efdc01405d58174db62acecef6a16a6bff4ed00457",
           sep = ",", header = TRUE)
View(penguin_data)

write.csv(penguin_data,"/Volumes/Time Capsule 2TB/week12/Penguin_data.csv"
          ,row.names = FALSE)

