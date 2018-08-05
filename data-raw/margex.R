# build script for 'margex' dataset

# load packages
requireNamespace("margex")
requireNamespace("rio")

# load data
webuse::webuse("margex")

# factorize
margex$sex <- rio::factorize(margex$sex)
margex$group <- factor(margex$group)
margex$agegroup <- rio::factorize(margex$agegroup)
margex$treatment <- factor(margex$treatment)
margex$arm <- factor(margex$arm)

# drop attributes
attr(margex$y, "format.stata") <- NULL
attr(margex$outcome, "format.stata") <- NULL
attr(margex$age, "format.stata") <- NULL
attr(margex$distance, "format.stata") <- NULL
attr(margex$ycn, "format.stata") <- NULL
attr(margex$yc, "format.stata") <- NULL
attr(margex$arm, "format.stata") <- NULL

# overwrite
devtools::use_data(margex, overwrite = TRUE)
