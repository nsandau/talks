library(argparser)
library(here)
library(stringr)


### ARGS
p <- arg_parser("rmd to pptx")
p <- add_argument(p, "input", help = "rmd file to render")
args <- parse_args(p)

fname <- str_remove(args$input, ".rmd")

fname_html <- str_c(fname, ".html")
fname_pdf <- str_c(fname, ".pdf")


rmarkdown::render(
    input = args$input,
    output_file = fname_html,
)

xaringan::decktape(
    fname_html,
    output = fname_pdf,
    docker = T
)

# convert to pptx
cmd <- str_c("pdf2pptx", fname_pdf, sep = " ")
system(cmd)