# Required library
install.packages("openssl")
library(openssl)

# Base64 encoded video string
mydata_15_ <- read_csv("~/ownCloud work/Promotion/Studien/PIntenWi2/Vyond Videos/mydata (18).csv")

base64_string <- mydata_15_$record_video_data[5]

# Decode the Base64 string to get raw binary data
video_data <- base64_decode(base64_string)

# Write the binary data to a video file
writeBin(video_data, "output_video.mp4")
