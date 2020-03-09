# InteractiveTradeR

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/JakeVestal/InteractiveTradeR.svg?branch=master)](https://travis-ci.org/JakeVestal/InteractiveTradeR)
<!-- badges: end -->

Connect R to the world's markets.

## Installation

#### 1. Download and install the latest version of R from the [official CRAN
website](https://www.r-project.org/).
- **Windows Users** should also install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/installer.html).
- If you're a **Mac User**, also install
[Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12). Go get a coffee,
it'll take a few minutes.

#### 2. Install Git
Follow the instructions on [Git's
webpage](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) for your
particular OS.

#### 3. Download and install the latest version of [Trader Workstation](https://www1.interactivebrokers.com/en/index.php?f=16040) (TWS).

You may also choose to install
[IBGateway](https://www1.interactivebrokers.com/en/index.php?f=16457) (IBG). IB
Gateway will not work with the free demo provided by Interactive Brokers, but
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

#### 4. Download and install the [latest version of
RStudio](https://rstudio.com/products/rstudio/download/#download).

#### 5. Open RStudio and install the package `devtools` with:
``` r
install.packages("devtools")
```

#### 6. In RStudio, install the `InteractiveTradeR` package with:
``` r
devtools::install_github("JakeVestal/InteractiveTradeR")
```

#### 7. In RStudio, create a new project (or open an existing one).
To create new, use File > New Project > New Directory > New Project. Name the
new project anything you want.

#### 8. Open your project and load `InteractiveTradeR` with:
``` r
library("InteractiveTradeR")
```
If this is the first time you've loaded `InteractiveTradeR` in the current
RStudio project, you'll be asked if you want to establish some default settings.
Do so. This action will store default connection parameters (port number, host,
master client ID) as R variables so that you don't have to re-enter them. You
may change these parameters at any later time.

#### 9. Allow API connection to TWS / IBG
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

#### 10. Test your connection
At this point, you should have an instance of TWS or IBG running and accepting
connections from the API. Within RStudio, with `InteractiveTradeR` having been
loaded, you can test your connection with command `req_current_time()`. If you
get a timestamp response, all's well and you're set up!
