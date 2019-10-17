source("figuReLayouts.R")
library(png)



## The most basic layout structure just involves slots of equal sizes in rectangle structures
## That is just the plots.layouts.ABC() function.

## EXAMPLE 1: Making a 2x2 figure with 4 boring scatter plots
plotfxns <- list(p1, p2, p3, p4)
plots.layouts.ABC(plotfxns, nr = 2, nc = 2)
plots.layouts.ABC(plotfxns, nr = 2, nc = 2, alphabet = lowercase)
plots.layouts.ABC(plotfxns, nr = 2, nc = 2, fill.in.byrow = F)

## EXAMPLE 2: Arrange these plots in the first 4 slots of a matrix with more empty space 
##    The empty space can be used for other things outside...
plots.layouts.ABC(plotfxns, nr = 2, nc = 3)
plots.layouts.ABC(plotfxns, nr = 3, nc = 2)
plots.layouts.ABC(plotfxns, nr = 3, nc = 3)


## EXAMPLE 3: Similar to 2, but add letters to empty slots by providing emptyplot() in plotfxns list
## If you want to add other figures elsewhere (illustrator, keynote, pptx)
plotfxns <- list(p1, emptyplot, p2, emptyplot, p3, emptyplot, p4, emptyplot)
plots.layouts.ABC(plotfxns, nr = 4, nc = 2)
## lowercase letters. 
plots.layouts.ABC(plotfxns, nr = 4, nc = 2, alphabet = lowercase)


## EXAMPLE 4: Mix plots with PNG images
plotfxns <- list(p1, plotpng, p2, plotpng.preserveAspectRatio, p3, plotpng, p4, plotpng.preserveAspectRatio)
plots.layouts.ABC(plotfxns, nr = 4, nc = 2, alphabet = lowercase)


## Example 5: Special layout 1
plotfxns <- list(p1, plotpng, plotpng.preserveAspectRatio, p1, plotpng, plotpng.preserveAspectRatio, p1, plotpng, plotpng.preserveAspectRatio, p1, plotpng, plotpng.preserveAspectRatio)
plots.layouts.special1(plotfxns = plotfxns)
plots.layouts.special1(plotfxns = plotfxns, addletters = TRUE)
plots.layouts.special1(plotfxns = plotfxns, h=6, addletters = TRUE)



