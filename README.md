InteractiveTradeR
=================

Connect R to the world's markets.

Installation
------------

#### 1. Download and install the latest version of R from the [official CRAN
website](https://www.r-project.org/).
- **Windows Users** should also install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/installer.html).
- If you're a **Mac User**, you'll probably want
[Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12) at some point
during your work with InteractiveTradeR or R in general, so you might as well
goahead and install it now from the Apple Store.

#### 2. Download and install the latest version of [Trader Workstation](https://www1.interactivebrokers.com/en/index.php?f=16040) (TWS).

You may also choose to install
[IBGateway](https://www1.interactivebrokers.com/en/index.php?f=16457) (IBG). IB
Gateway will not work with the free demo, provided by Interactive Brokers, but
for paper and trading accounts it provides all of the functionality of Trader
Workstation without the charting and graphical user interface.

##### (OPTIONAL) Create an account with Interactive Brokers.
This can be done for free, and you do **not** have to fund the account with real
money. The advantage of creating an account is that doing so allows you to
create a [paper trading
account](https://www.interactivebrokers.com/en/software/omnibrokers/topics/papertrader.htm)
that you may use to develop and test any trading system you develop.

While you are free to use `InteractiveTradeR` on your paper account, note that
**using `InteractiveTradeR` to place trades on an actual trading account is a
violation of the license**, which does not allow for commercial use. To obtain a
commercial license, please contact the author.

##### 3. Download and install the [latest version of
RStudio](https://rstudio.com/products/rstudio/download/#download).

##### 4. Open RStudio and install the package `devtools` with:
``` r
install.packages("devtools")
```

##### 5. In RStudio, install the `InteractiveTradeR` package with:
``` r
install.packages("JakeVestal/InteractiveTradeR")
```

##### 6. Load `InteractiveTradeR` with:
``` r
library("InteractiveTradeR")
```
If this is the first time you've loaded `InteractiveTradeR` in the current
RStudio project, you'll be asked if you want to establish some default settings.
Do so. This action will store default connection parameters (port number, host,
master client ID) as R variables so that you don't have to re-enter them. You
may change these parameters at any later time.

##### 6. Allow API connection to TWS / IBG
1. Open whichever program you installed and wish to use -- TWS or IBG -- and log
in to your account, paper account, or the demo (TWS only).

2. From the File menu, find File > Global Configuration > API > Settings.

3. Check the "Enable ActiveX and Socket Clients" box.

4. Uncheck the "Read-Only API" box if you wish to allow `InteractiveTradeR` to
write to the API, for example, to place orders.

5. In RStudio, retreive your randomly generated Master Client ID by entering
command `active_connection_parameters()`. Copy-paste this numeric ID into the
"MASTER CLIENT ID" field in the API Settings window.

6. Click "Apply and OK" to close the API Settings window.

#### Test your connection
At this point, you should have an instance of TWS or IBG running and accepting
connections from the API. Within RStudio, with `InteractiveTradeR` having been
loaded, you can test your connection with command `req_current_time()`. If you
get a timestamp response, all's well and you're set up!

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!
