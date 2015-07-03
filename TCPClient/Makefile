BINARIES=HelloClient
include standard.mk

run: HelloClient
	-sudo xl destroy HelloClient
	sudo xl create HelloClient.config
	sleep 5
	sudo xl dmesg
