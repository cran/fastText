
## fastText 1.0.4

* I added a figure to the README.md file showing the differences between *static* and *contextualised* word embeddings
* A missing include directive was added to the *'/inst/include/utils.h'* file (see the Github pull request: https://github.com/mlampros/fastText/pull/5)
* I removed the "CXX_STD = CXX11" from the "Makevars" files, and the "[[Rcpp::plugins(cpp11)]]" from the ".cc" files due to the following NOTE from CRAN, "NOTE Specified C++11: please drop specification unless essential" (see also: https://www.tidyverse.org/blog/2023/03/cran-checks-compiled-code/#note-regarding-systemrequirements-c11)


## fastText 1.0.3

* I added a test case for the *'language_identification()'* function (see Github issue: https://github.com/mlampros/fastText/issues/3)
* I added the '*verbose*' parameter to the C++ functions of the '*src/main.cc*' file that did not take any variables as input to avoid the CRAN WARNING *'function declaration isn't a prototype [-Wstrict-prototypes]'*


## fastText 1.0.2

* I added the *URL* and *BugReports* fields in the *DESCRIPTION* file
* I updated the documentation of the *print_parameters()* function 
* I updated the details section of the *fasttext_interface()* function regarding the *output* parameter which exists in the named list that is passed to the *list_params* parameter of the *fasttext_interface()* function. Although this *output* parameter is a file path (and not a directory name) it will be saved in both *.vec* and *.bin* file name extensions.


## fastText 1.0.1

* I fixed the **LTO** (Link Optimization Error) - Additional Issues - by replacing the **qnorm** variable with **qnorm_param** in the Rcpp files
* I modified the **quantize** function in the Rcpp file to return the **.ftz** file by specifying the exact file path
* I adjusted the **Examples** section of the **fasttext_interface** function and the **testthat tests** to account for the changes in the **quantize** function
* I fixed a bug of the **fasttext_interface** function related to parameters that do not take a value


## fastText 1.0.0

* I've added the *CITATION* file in the 'inst' directory
* I've added the **language_identification()** function
* **20-04-2021** : I've added the pre-trained language identification model **lid.176.ftz** which can be downloaded from https://fasttext.cc/docs/en/language-identification.html In the same website exists also the **lid.176.bin** model which is bigger in size, faster and slightly more accurate.
* **14-07-2019** : I fixed typos in vignette and modified the *plot_progress_logs()* function because it threw an error of the form : *line 1 did not have 11 elements* ( I added the *fill = TRUE* parameter to the *utils::read.table()* function to account for NA's as described in a [stackoverflow issue](https://stackoverflow.com/a/18161099/8302386) )
