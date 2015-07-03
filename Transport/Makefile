BINARIES=Node1 Node2
include standard.mk

run: Node1 Node2
	sudo xl create Node1.config
	sudo xl create Node2.config
	sleep 3
	sudo xl dmesg -c
	-sudo xl destroy Node1
	-sudo xl destroy Node2
