
# sp2rd

A simple R package to generate animated transitions between spatial
locations and reduced dimensional embeddings.

**This package is currently under development.**

## Installation

To install the development version:

``` r
library(remotes)
install_github("BodenmillerGroup/sp2rd")
```

## Usage

The `sp2rd` function takes a `SingleCellExperiment` or
`SpatialExperiment` object as input. You will need to specify under
which name the spatial coordinates are stored and which low dimensional
embeding to project them to:

``` r
options(timeout = 100000)
library(sp2rd)

spe <- readRDS(url("https://zenodo.org/record/7432486/files/spe.rds", "rb"))

# Select one image
cur_spe <- spe[,spe$sample_id == "Patient1_001"]

sp2rd(cur_spe, 
      coords = c("Pos_X", "Pos_Y"), 
      color_by = "celltype", 
      img_id = "sample_id", 
      dimred = "UMAP")
```

![](man/figures/README-unnamed-chunk-3-1.gif)<!-- -->

The animation can be saved using `anim_save("path/to/gif_name.gif")`.

## Maintainer

[Nils Eling](https://github.com/nilseling)
