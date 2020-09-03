panelvar
=========

Experimental tools on pVAR models for timely estimates.
---

The material provided herein will enable you to reproduce the experiments presented in _Eurostat_ statistical working paper on [**New methods for timely estimates**](https://ec.europa.eu/eurostat/web/products-statistical-working-papers/-/KS-TC-20-005) (_cite this source code or the reference's doi: [10.2785/600130](http://dx.doi.org/10.2785/600130)_). Further details are also available in the other associated working papers (see Kapetanios _et al._'s publications [below](#References)).

**Description**

The source code is provided *as is* in the [**_model/_**](model) folder so as to explore out-of-sample forecasting performance of mixed-frequency panel vector autoregression (pVAR) models for four key macroeconomic variables, with the goal of providing evidence on the usefulness and reliability of these models for use by official statistical agencies. 

Data from four European economies, as used in the paper, are made available under the [**_data/input_**](data) folder. Output data will be stored into an **data/output/ folder that needs to be created beforehand**.  

Additionally, the source code in [**_tables/_**](tables) enable to reproduce some figures of the working paper that are stored in the [**_data/tables_**](tables) folder

**<a name="Quick-launch"></a>Quick launch**

* **Run a pVar model from within a [`Jupyter`](https://jupyter.org/) notebook in [`binder`](https://mybinder.org/)** (current build with commit [d89d9bd](https://github.com/eurostat/statistics-coded/commit/d89d9bd6f5c0637a3d2c698eb210a5920cc40668)). We provide the interactive environments with already installed packages to run the experiments, for instance: <!-- generate new badges: https://mybinder.readthedocs.io/en/latest/howto/badges.html -->
[![badge](https://img.shields.io/badge/launch%20Jupyter-binder-579ACA.svg?logo=data:image/png;base64,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)](http://mybinder.org/v2/gh/eurostat/panelvar/d89d9bd6f5c0637a3d2c698eb210a5920cc40668?filepath=notebooks/DE-single-m1.ipynb) <!--(http://mybinder.org/v2/gh/eurostat/panelvar/master?filepath=notebooks/DE-single-m1.ipynb) -->

  <!-- You can also run this notebook in [`Google colab`](https://colab.research.google.com/)** (although you will need a _Google_ login): [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/eurostat/panelvar/blob/master/notebooks/DE-single-m1.ipynb) -->

* **Run [`RStudio`](https://rstudio.com/) in `binder`**, and [source](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/source) any of the pVar models from the platform:  [![badge](https://img.shields.io/badge/launch%20RStudio-binder-579ACA.svg?logo=data:image/png;base64,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)](http://mybinder.org/v2/gh/eurostat/panelvar/d89d9bd6f5c0637a3d2c698eb210a5920cc40668?urlpath=rstudio) <!--(http://mybinder.org/v2/gh/eurostat/panelvar/master?urlpath=rstudio) -->

**Usage**

To run the experiments (*e.g.*, in your local), you will need to prior install the following package dependencies: [`forecast`](https://cran.r-project.org/web/packages/forecast/index.html), [`imputeTS`](https://cran.r-project.org/web/packages/imputeTS/index.html), [`panelvar`](https://cran.r-project.org/web/packages/panelvar/panelvar.pdf), [`lubridate`](https://cran.r-project.org/web/packages/lubridate/index.html), [`vars`](https://cran.r-project.org/web/packages/vars/index.html), [`moments`](https://cran.r-project.org/web/packages/moments/index.html) and [`zoo`](https://cran.r-project.org/web/packages/zoo/index.html).

Once the packages installed, you can run the `bash` scripts in the [**_bin/_**](bin) folder.

**<a name="About"></a>About**

<table align="center">
    <tr>     <td  rowspan="4" align="center" width="140px"> <a href="https://ec.europa.eu/eurostat/documents/3888793/10879237/KS-TC-20-005-EN-N.pdf"><img src="docs/working_paper_front_cover.png"></img></a></td>
<td align="left"><i>authors</i></td> <td align="left"> <a href="mailto:fotis.papailias@quantf.com">Papailias F.</a>, 
	<a href="mailto:kapetaniosgeorge@gmail.com">Kapetanios G.</a>, <a href="mailto:massimiliano.marcellino@unibocconi.it">Marcellino M.</a>, 
	and <a href="mailto:glmazzi@pt.lu">Mazzi G.L.</a></td> </tr> 
    <tr> <td align="left"><i>version</i></td> <td align="left">1.0</td> </tr> 
    <tr> <td align="left"><i>status</i></td> <td align="left">2020 &ndash; <b>closed</b></td> </tr> 
    <tr> <td align="left"><i>license</i></td> <td align="left"><a href="https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdfEUPL">EUPL</a> <i>(cite the source code or the reference above!)</i></td> </tr> 
</table>

**<a name="References"></a>References** 

* Kapetanios G., Marcellino M., Papailias F. and Mazzi G.L. (2020): [**New methods for timely estimates**](https://ec.europa.eu/eurostat/web/products-statistical-working-papers/-/KS-TC-20-005), _Eurostat_ Statistical Working Paper KS-TC-20-005-EN, doi: [10.2785/600130](http://dx.doi.org/10.2785/600130).

* Mazzi G.L. and Mitchell J. (2020): [**New methods for timely estimates: nowcasting euro area GDP growth using quantile regression**](https://ec.europa.eu/eurostat/documents/3888793/10879121/KS-TC-20-004-EN-N.pdf/9916fb06-56e5-bf81-56c7-8b7c8d1f7c2e), _Eurostat_ Statistical Working Paper S-TC-20-004-EN, doi: [10.2785/26603](http://dx.doi.org/10.2785/26603).

* Sigmund M. and Ferstl R. (2019): **Panel Vector Autoregression in R with the package Panelvar**, _Quarterly Review of Economics and Finance_, doi: [10.2139/ssrn.2896087](http://dx.doi.org/10.2139/ssrn.2896087).

* Mazzi G.L. and Ladiray D., eds. (2017): 
*[**Handbook on Rapid Estimates**](http://ec.europa.eu/eurostat/documents/3859598/8555708/KS-GQ-17-008-EN-N.pdf), 
_Publications Office of the European Union_, doi:[10.2785/4887400](http://dx.doi.org/10.2785/4887400).

* Dees S. and Gunter J. (2014): [**Analysing and forecasting price dynamics across Euro area countries and sectors - A panel var approach**](https://www.ecb.europa.eu/pub/pdf/scpwps/ecbwp1724.pdf), _European Central Bank_ Working Paper QB-AR-14-098-EN, no. 1724.

* Croissant Y. and Millo G. (2008): [**Panel data econometrics in R: The plm package**](https://www.jstatsoft.org/v27/i02/paper), _Journal of Statistical Software_, 27(2):1-43, doi: [10.18637/jss.v027.i02](http://dx.doi.org/10.18637/jss.v027.i02).
