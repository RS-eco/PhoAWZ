#remotes::install_github("jhelvy/xaringanBuilder")
library(xaringanBuilder)
#build_gif("slides.html", output_file="slides.pdf")
build_pdf("slides.html", output_file="slides.pdf")

# ?xaringan::decktape

pagedown::chrome_print("slides.Rmd",output="slides.pdf"
                       #, options = list(transferMode = "ReturnAsStream"))
)
