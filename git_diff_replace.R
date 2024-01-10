# Replace string

fp <- commandArgs(TRUE)[1]

old_text <- readLines(fp)

# new_text <- gsub(
#     "(\\[-(?!@))(.+?)(-\\])", 
#     "\\\\textcolor{BrickRed}{\\2}", 
#     old_text, 
#     perl = TRUE
# )

# new_text <- gsub(
#     "({\\+)(.+?)(\\+})", 
#     "\\\\textcolor{OliveGreen}{\\2}", 
#     new_text, 
#     perl = TRUE
# )

new_text <- gsub(
    "(\\[-(?!@))(.+?)(-\\])", 
    "[\\2]{color=\"BrickRed\"}", 
    old_text, 
    perl = TRUE
)

new_text <- gsub(
    "({\\+)(.+?)(\\+})", 
    "[\\2]{color=\"OliveGreen\"}",
    new_text, 
    perl = TRUE
)

writeLines(new_text, fp)