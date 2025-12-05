# install.packages("magick")
library(magick)

# 1. Read your source image
img <- image_read("result/plot1/figS2.png")

# 2. (Optional) Resize to desired pixel dimensions, e.g. 1200×1800 px
#    Remove or adjust this step if you don’t need to change the pixel size.
img <- image_resize(img, "2400x1600!")

# 3. Write out as a 300 dpi TIFF
image_write(
  img,
  path    = "result/plot1/FigureS1(B).tif",
  format  = "tiff",
  density = 600
)


