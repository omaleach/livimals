# livimals
An R package for adding animal silhouettes to ggplot2 plots. 
You can preview available silhouettes, customize size, color, and specify exact placement on your plots.

---

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes") if needed
remotes::install_github("omaleach/livimals")
```

## Available Silhouettes

To see all available silhouettes:

```r
list_livimals()
```

To preview a single silhouette:
```r
preview_livimal()
#Ex. preview_livimal("hare")
```

## Examples

You can place a silhouette using numeric x and y coordinates
Example: 

```r
library(ggplot2)
library(ggimage)
library(magick)
library(livimals)

df <- data.frame(x = 1:5,y = c(3, 2, 5, 4, 1) )

ggplot(df, aes(x, y)) +
       theme_classic() +
       add_livimal("hare", x= 3, y=2, size = 0.15)
```

OR

You can place a silhouette using characters in aes()

```r
df <- data.frame(
  method = c("Trapping", "Chemical", "Physical"),
  animal_group = c("Mammals", "Birds", "Reptiles"))
full_grid <- expand.grid(
  method = unique(df$method),
  animal_group = unique(df$animal_group))

#Ex. simple aes mapping

ggplot(full_grid, aes(x = method, y = animal_group)) +
  geom_point(size = 3) +
  theme_classic() +
  add_livimal("hare")

#Ex. specifying columns for placement

ggplot(full_grid, aes(x = method, y = animal_group)) +
  geom_point(size = 3) +
  theme_classic() +
  add_livimal("hare", x = "Chemical", y = "Mammals", size = 0.2) +
  add_livimal("bird", x = "Trapping", y = "Birds", size = 0.2) 

```

  
