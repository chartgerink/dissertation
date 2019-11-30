# Dissertation

This repository tracks the official dissertation. The goal is to
provide a fully reproducible and digitally-born dissertation, which I
can easily export to paper format to fullfill my university's
requirements. The [`bookdown`](https://bookdown.org/home/)-borne
dissertation is a *work in progress* but already available at the
static webpage for [this
repository](https://chartgerink.github.io/dissertation/).

You can rerun the entire compilation process by running the following
from `R`, with the repository as the working directory (assuming you
have all dependencies!)

```R
bookdown::render_book('index.Rmd', "bookdown::pdf_book")
```

The R dependencies are AFAIK

```
bookdown
car
devtools
ggplot2
kableExtra
knitr
latex2exp
magrittr
pROC
plyr
viridis
xtable
```

Remember that there might be other dependencies like `pandoc`, `pandoc-citeproc`, and some LaTeX dependencies I haven't included and depend on your installation.

For the PDF, I did some manual adjustments to get to the final version because LaTeX...

## Outline

__Contributions towards understanding and building sustainable science__

0. Introduction chapter short
  - [x] Ready

1. [Research practices and assessment of research misconduct](http://doi.org/10.14293/S2199-1006.1.SOR-SOCSCI.ARYSBI.v1)
  - [x] Preprint: NA
  - [x] Status: published in ScienceOpen (license: CC-BY 4.0)
  - [x] Data: NA
  - [x] Reproducibility packet: [![DOI](https://zenodo.org/badge/DOI/10.14293/S2199-1006.1.SOR-SOCSCI.ARYSBI.v1.svg)](https://zenodo.org/record/276035)
  - [x] License: [CC BY 4.0](http://creativecommons.org/licenses/by/4.0) 
  - [x] Bookdown-ready

2. [Reanalyzing Head et al. (2015): Investigating the robustness of widespread p-hacking](https://doi.org/10.7717/peerj.3068)
  - [x] Preprint: [PeerJ preprints](https://doi.org/10.7287/peerj.preprints.2439v1)
  - [x] Status: [published in PeerJ](https://doi.org/10.7717/peerj.3068)
  - [x] Data: data from original Head et al. paper, available on [Dryad](http://dx.doi.org/10.5061/dryad.79d43/1)
  - [x] Reproducibility packet: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.269668.svg)](https://doi.org/10.5281/zenodo.269668)
  - [x] License: [CC 0 rights waiver](http://creativecommons.org/publicdomain/zero/1.0/)
  - [x] Bookdown-ready

3. [Distributions of p-values between .01-.05 in psychology: what is really going on?](https://doi.org/10.7717/peerj.1935)
  - [x] Preprint: [PeerJ preprints](https://peerj.com/preprints/1642/)
  - [x] Status: [published in PeerJ](https://doi.org/10.7717/peerj.1935)
  - [x] Data: [https://osf.io/gdr4q/](https://osf.io/gdr4q/)
  - [x] Reproducibility packet: [https://osf.io/4d2g9/](https://osf.io/4d2g9/)
  - [x] License: [CC BY 4.0](http://creativecommons.org/licenses/by/4.0)
  - [x] Bookdown-ready

4. [Too good to be false: Nonsignificant results revisited](http://doi.org/10.1525/collabra.71)
  - [x] Preprint: [PsyArxiv](https://osf.io/preprints/rkumy)
  - [x] Status: [published in Collabra](http://doi.org/10.1525/collabra.71)
  - [x] Data: direct downloads linked in [Sweave file](https://github.com/chartgerink/2014tgtbf/blob/master/submission%2Fmanuscript.Rnw)
  - [x] Reproducibility packet: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.250492.svg)](https://doi.org/10.5281/zenodo.250492)
  - [x] License: [CC BY 4.0](http://creativecommons.org/licenses/by/4.0)
  - [x] Bookdown-ready

5. [688,112 Statistical Results: Content Mining Psychology Articles for Statistical Test Results](http://doi.org/10.3390/data1030014)
  - [x] Preprint: [MDPI preprints](https://doi.org/10.20944/preprints201608.0191.v1)
  - [x] Status: published in Data (license: CC-BY 4.0)
  - [x] Data: [EASY-DANS](http://dx.doi.org/10.17026/dans-2cm-v9j9)
  - [x] Reproducibility packet: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.59818.svg)](https://doi.org/10.5281/zenodo.59818)
  - [x] License: [CC BY 4.0](http://creativecommons.org/licenses/by/4.0)
  - [x] Bookdown-ready

6. [Detection of data fabrication using statistical tools](https://github.com/chartgerink/2015ori-1)
  - [x] Preprint: [PsyArxiv](https://psyarxiv.com/jkws4)
  - [x] Status: Ready to submit
  - [x] Data: [GitHub](https://github.com/chartgerink/2015ori-1/tree/master/data) and archived in the reproducibility packet
  - [x] Reproducibility packet: [![DOI](https://www.zenodo.org/badge/44164786.svg)](https://www.zenodo.org/badge/latestdoi/44164786)
  - [x] Bookdown-ready
  
7. Extracting data from vector figures in scholarly articles
  - [x] Preprint: [https://arxiv.org/abs/1709.02261](https://arxiv.org/abs/1709.02261)
  - [x] Status: manuscript that underwent public feedback
  - [x] Data: see reproducibility packet
  - [x] Reproducibility packet: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1010360.svg)](https://doi.org/10.5281/zenodo.1010360)
  - [x] License: [CC 0 rights waiver](http://creativecommons.org/publicdomain/zero/1.0/)
  - [x] Bookdown-ready
  
8. "As-you-go" instead of "after-the-fact": A network approach to scholarly communication and evaluation
  - [x] Preprint: [https://doi.org/10.7287/peerj.preprints.26462v1](https://doi.org/10.7287/peerj.preprints.26462v1)
  - [x] Status: [published in Publications](https://doi.org/10.3390/publications6020021)
  - [x] Data: NO DATA
  - [x] Reproducibility packet: [![DOI](https://zenodo.org/badge/97157193.svg)](https://zenodo.org/badge/latestdoi/97157193)
  - [x] License: [CC 0 rights waiver](http://creativecommons.org/publicdomain/zero/1.0/)
  - [x] Bookdown-ready
  
9. Verified, shared, modular research communication with the Dat protocol
  - [x] Preprint: [https://dat-com-chris.hashbase.io/](https://dat-com-chris.hashbase.io/)
  - [x] Status: [Publications](https://doi.org/10.3390/publications7020040)
  - [x] Data: NO DATA
  - [x] Reproducibility packet: [![DOI](https://www.zenodo.org/badge/143130982.svg)](https://www.zenodo.org/badge/latestdoi/143130982)
  - [x] License: [CC 0 rights waiver](http://creativecommons.org/publicdomain/zero/1.0/)
  - [x] Bookdown-ready

10. Epilogue
  - [x] Done
