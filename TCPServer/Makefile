BINARIES=HelloServer
include standard.mk

run: HelloServer
	-sudo xl destroy HelloServer
	sudo xl create HelloServer.config -c
