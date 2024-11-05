This is still in a building phase. Right now I am just trying out what the best
workflow for me is, how to best get images and equations in, and learning a bit
about Jekyll while I am at it.

This is supposed to become a minimalist blog where I document questions that
come up in statistics consultations regularly.

TODOs:

* How to fix the path for images? They are there:
  https://nwickel.github.io/blog/figures/2024-11-05_plot-1.png but not shown

  --> Quick and dirty fix: I used `knitr::include_graphics()` and set the path
  to`https://raw.githubusercontent.com/nwickel/blog/refs/heads/main/figures`

* Why are equations not rendered correctly?

  --> Jekyll was not rendering the latex math equations coorectly, instead
  interpreting them as markdown. I fixed this by adding the `default.html` in
  `_layouts` and adding a line for MathJax in there and by switching the
  markdown engine to kramdown. All math (inline and display) now needs to be
  surrounded be `$$`.

