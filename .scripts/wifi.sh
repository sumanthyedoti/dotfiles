#!/bin/bash

iwctl device wlan0 set-property Powered off
systemctl restart dhcpcd@wlan1.service
iwctl device wlan1 set-property Powered on
iwctl station wlan1 scan
iwctl station wlan1 connect YFI_5G --passphrase 1234567890

