
## fastText
<br>

The **fastText** R package is an interface to the [fastText](https://github.com/facebookresearch/fastText) library for efficient learning of word representations and sentence classification. More details on the functionality of fastText can be found in the [first](http://mlampros.github.io/2019/04/11/fastText_updated_version/) and [second](http://mlampros.github.io/2021/05/14/fasttext_language_identification/) blog posts and in the package [documentation](https://mlampros.github.io/fastText/reference/index.html).

<br>

You can either install the package from CRAN using,

```R

install.packages("fastText")
 

```

<br>

or from Github using the *install_github* function of the *remotes* package,

```R

remotes::install_github('mlampros/fastText')


```
<br>

**or** directly download the fastText-zip file using the **Clone or download** button in the [repository page](https://github.com/mlampros/fastText), extract it locally (rename it to *fastText* if necessary and check that files such as DESCRIPTION, NAMESPACE etc. are present when you open the fastText folder) and then run,


```R

#-------------
# on a Unix OS
#-------------

setwd('/your_folder/fastText/')
Rcpp::compileAttributes(verbose = TRUE)
setwd('/your_folder/')
system("R CMD build fastText")
system("R CMD INSTALL fastText_1.0.1.tar.gz")


#------------------
# on the Windows OS  
#------------------

setwd('C:/your_folder/fastText/')
Rcpp::compileAttributes(verbose = TRUE)
setwd('C:/your_folder/')
system("R CMD build fastText")
system("R CMD INSTALL fastText_1.0.1.tar.gz")

```
<br>

Use the following link to report bugs/issues (for the R package port),
<br><br>

[https://github.com/mlampros/fastText/issues](https://github.com/mlampros/fastText/issues)

<br>

### **Citation:**

If you use the **fastText** R package in your paper or research please cite both **fastText** and the **original articles / software** `https://CRAN.R-project.org/package=fastText`:

<br>

```R
@Manual{,
  title = {{fastText}: Efficient Learning of Word Representations and
    Sentence Classification using R},
  author = {Lampros Mouselimis},
  year = {2021},
  note = {R package version 1.0.2},
  url = {https://CRAN.R-project.org/package=fastText},
}
```

<br>
