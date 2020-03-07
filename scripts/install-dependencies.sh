#! /bin/sh

set -ev

wget https://download2.interactivebrokers.com/installers/ibgateway/latest-standalone/ibgateway-latest-standalone-linux-x64.sh

ls
sudo chmod ugo+x ibgateway-latest-standalone-linux-x64.sh
./ibgateway-latest-standalone-linux-x64.sh
