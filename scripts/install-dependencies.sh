#! /bin/sh

set -ev

wget -O /scripts https://download2.interactivebrokers.com/installers/ibgateway/latest-standalone/ibgateway-latest-standalone-linux-x64.sh
sudo chmod ugo+x scripts/ibgateway-latest-standalone-linux-x64.sh
yes n | ./scripts/ibgateway-latest-standalone-linux-x64.sh
