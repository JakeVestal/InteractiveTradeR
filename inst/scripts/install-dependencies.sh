#! /bin/sh

set -ev

sudo wget -O scripts/ibgateway-latest-standalone-linux-x64.sh https://download2.interactivebrokers.com/installers/ibgateway/latest-standalone/ibgateway-latest-standalone-linux-x64.sh
sudo chmod ugo+x scripts/ibgateway-latest-standalone-linux-x64.sh
yes n | sudo ./scripts/ibgateway-latest-standalone-linux-x64.sh
sudo rm scripts/ibgateway-latest-standalone-linux-x64.sh