---
title: 'kim: A Toolkit for Behavioral Scientists'
tags:
  - R
  - behavioral science
  - experimental data
  - anova
  - regression
  - interaction
  - floodlight analysis
authors:
  - name: Jin Kim
    orcid: 0000-0002-5013-3958
affiliations:
 - name: Jin Kim, Yale School of Management, USA
   index: 1
date: 9 October 2022
bibliography: paper.bib

---

# Summary

The package {kim} is a collection of functions that can expedite data analyses
by behavioral scientists. 
will find useful in conducting analyses of data, typically those collected 
in experiments online or in behavioral labs or consisting of 
various functions that expedite analyses of experimental data for the R programming language (R Core Team, 2021) provides 
a toolbox to assist in key steps involved in any data analysis workflow: (1) wrangling
the raw data to get it in the needed form, (2) applying preprocessing steps and statistical
transformations, and (3) compute statistical summaries of data properties and distributions.
Therefore, it can be a valuable tool for R users and developers looking for a lightweight option
for data preparation

# Statement of need

`kim` is an R package for conducting analyses typically performed by 
behavioral scientists. It contains various functions that can expedite
analyses of data from experiments conducted online (e.g., MTurk or Prolific),
or in behavioral labs.

`kim` was designed to be used by both behavioral scientists or by students 
taking courses on basic statistics or behavioral science. It has already 
been used scientific publications (preprints), such as [@kim:2021] and 
[@kim:2022].

`kim` will provide an easy way to 

tAstropy-affiliated Python package for galactic dynamics. Python
enables wrapping low-level languages (e.g., C) for speed without losing
flexibility or ease-of-use in the user-interface. The API for `Gala` was
designed to provide a class-based and user-friendly interface to fast (C or
Cython-optimized) implementations of common operations such as gravitational
potential and force evaluation, orbit integration, dynamical transformations,
and chaos indicators for nonlinear dynamics. `Gala` also relies heavily on and
interfaces well with the implementations of physical units and astronomical
coordinate systems in the `Astropy` package [@astropy] (`astropy.units` and
`astropy.coordinates`).

`Gala` was designed to be used by both astronomical researchers and by
students in courses on gravitational dynamics or astronomy. It has already been
used in a number of scientific publications [@Pearson:2017] and has also been
used in graduate courses on Galactic dynamics to, e.g., provide interactive
visualizations of textbook material [@Binney:2008]. The combination of speed,
design, and support for Astropy functionality in `Gala` will enable exciting
scientific explorations of forthcoming data releases from the *Gaia* mission
[@gaia] by students and experts alike.

# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$

You can also use plain \LaTeX for equations
\begin{equation}\label{eq:fourier}
\hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
\end{equation}
and refer to \autoref{eq:fourier} from text.

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }

# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References
