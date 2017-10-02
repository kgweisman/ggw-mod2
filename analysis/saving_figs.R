# making source files for images

# SVG ------

# figure 1
svg(filename = "//Users/kweisman/Documents/Research (Stanford)/Writing/Factor Analysis write-up/2017-02-xx PNAS/publication/Figures/fig1.svg", width = 12, height = 12, pointsize = 20)
s4_subset
graphics.off()

# # figure S1
# svg(filename = "//Users/kweisman/Documents/Research (Stanford)/Writing/Factor Analysis write-up/2017-02-xx PNAS/publication/Figures/figS1.svg", width = 20, height = 12, pointsize = 20)
# figS1
# graphics.off()

# figure S2
svg(filename = "//Users/kweisman/Documents/Research (Stanford)/Writing/Factor Analysis write-up/2017-02-xx PNAS/publication/Figures/figS2.svg", width = 12, height = 20, pointsize = 20)
s123
graphics.off()

# figure S3
svg(filename = "//Users/kweisman/Documents/Research (Stanford)/Writing/Factor Analysis write-up/2017-02-xx PNAS/publication/Figures/figS3.svg", width = 36, height = 12, pointsize = 20)
s4
graphics.off()


# PDF ------

# figure 1
pdf(file = "//Users/kweisman/Documents/Research (Stanford)/Writing/Factor Analysis write-up/2017-02-xx PNAS/publication/Figures/fig1.pdf", width = 12, height = 12, pointsize = 20)
s4_subset
graphics.off()

# # figure S1
# pdf(file = "//Users/kweisman/Documents/Research (Stanford)/Writing/Factor Analysis write-up/2017-02-xx PNAS/publication/Figures/figS1.pdf", width = 20, height = 12, pointsize = 20)
# figS1
# graphics.off()

# figure S2
pdf(file = "//Users/kweisman/Documents/Research (Stanford)/Writing/Factor Analysis write-up/2017-02-xx PNAS/publication/Figures/figS2.pdf", width = 12, height = 20, pointsize = 20)
s123
graphics.off()

# figure S3
pdf(file = "//Users/kweisman/Documents/Research (Stanford)/Writing/Factor Analysis write-up/2017-02-xx PNAS/publication/Figures/figS3.pdf", width = 36, height = 12, pointsize = 20)
s4
graphics.off()
