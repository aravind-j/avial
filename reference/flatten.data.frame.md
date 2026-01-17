# Flatten Duplicate Rows in a Data Frame

Flatten Duplicate Rows in a Data Frame

## Usage

``` r
flatten.data.frame(df, cols, collapse = ":")
```

## Arguments

- df:

  The data frame to be flattened.

- cols:

  The columns(s) according to which the duplicate rows are to be
  identified for flattening.

- collapse:

  The character string to separate the results of flattening in a cell.
  Default is "`:`".

## Value

A data frame with the duplicate rows flattened.

## Examples

``` r
data <- data.frame(team = c(rep("Justice League", 17), 
                            rep("Avengers", 13)),
                   name = c(rep("Superman", 5),
                            rep("Batman", 7), 
                            rep("Green Lantern", 5), 
                            rep("Iron Man", 4),
                            rep("Captain America", 5), 
                            rep("Thor", 4)),
                   identity = c("Clark Kent", "Christopher Kent", 
                                "Kon-El", "John Henry Irons", 
                                "Val-Zod", "Dick Grayson", "Bruce Wayne",
                                "Tim Drake", "Jean-Paul Valley", 
                                "Terry McGinnis", "Jace Fox", 
                                "Damian Wayne", 
                                "Hal Jordan", " John Stewart", 
                                " Guy Gardner", " Kyle Rayner", 
                                "Jessica Cruz", "Tony Stark", 
                                "Riri Williams", "James \"Rhodey\" Rhodes",
                                "Pepper Potts", 
                                "Steve Rogers", 
                                "Sam Wilson", "Bucky Barnes", 
                                "Isaiah Bradley", "John Walker", 
                                "Beta Ray Bill", "Jane Foster", 
                                "Odinson", "Eric Masterson"))

data_flat1 <- flatten.data.frame(df = data, cols = "team",
                               collapse = ", ")
data_flat2 <- flatten.data.frame(df = data, cols = c("team", "name"),
                                 collapse = ", ")

```
