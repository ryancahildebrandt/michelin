# michelin

---

---
[*Open*](https://gitpod.io/#https://github.com/ryancahildebrandt/michelin) *in gitpod*

## *Purpose*

A project to explore Michelin star restaurants, applying k-modes clustering and 3D vizualization. Makes use of [**this**](https://www.kaggle.com/jackywang529/michelin-restaurants) dataset, which details restaurants with 1, 2, or 3 Michelin stars, their locations, cuisines, and price points.

---

## *Code & Analyses*

[**Here**](http://htmlpreview.github.io/?https://github.com/ryancahildebrandt/michelin/blob/master/michelin.html) is the knit output of [**this**](/michelin.Rmd) R code, containing most data processing, analyses, and graphics for the study.

---

## *Interactive Outputs*

- [Map](https://htmlpreview.github.io/?https://github.com/ryancahildebrandt/michelin/blob/master/michelin_map.html)

- [3D Scatter](https://htmlpreview.github.io/?https://github.com/ryancahildebrandt/michelin/blob/master/michelin_3d.html)

NOTE: I'm working on getting a way to display these interactively via github, so if they're not pulling up just download the html files or the full knit output and view them all together.

---

## *Conclusion*

The boundary clusters aren't as clear as I'd hoped they'd be, and they were most clearly delineated along the price point and number of stars.

| Cluster | Cuisine  | Price  | Stars |
| ------- | -------- | ------ | ----- |
| 1       | Modern   | $$$$   | 1     |
| 2       | Chinese  | $$$$   | 1     |
| 3       | Modern   | $$$$   | 1     |
| 4       | Japanese | $$$$$$ | 2     |
