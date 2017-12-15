## README

For user guide please go to: <a href=//gaitlaboratory.pages.mcri.edu.au/gaitfeature/index.html target=_blank>http://gaitlaboratory.pages.mcri.edu.au/gaitfeature/index.html</a>

## Installation
Assuming you have access right to this repository, to install `gaitFeature`, simply execute the following:

`devtools::install_git("http://git.mcri.edu.au/GaitLaboratory/gaitfeature.git",credentials = git2r::cred_user_pass("replace by your mcri username","replace by your password"))`

If you are missing the required R pakcages `devtools` and `git2r` you can install them from CRAN. 

`install.packages(c("devtools","git2r"))`

`git2r::cred_user_pass()` can be replaced by other `git2r` authentation method such as `git2r::cred_ssh_key()` or `git2r::cred_ssh_token()`.
