#This program is a compilation of basic IT network utility commands

#IMPORTANT: This program is made by an indie developer, and therefore, it is not sponsored by, nor endorsed by
#major tech firms like Google and Microsoft.
#IMPORTANT: This program is made in BASH (not Git Bash), and therefore, may not be compatible with all shells.
#IMPORTANT: This program is not intended for use in high-stakes scenarios involving lives and/or money.

#WARNING: Extra installations may have been installed on the machine the program has been coded on.

#NOTE: This program is meant for educational purposes.
#NOTE: This program is does not use SUDO, and therefore, it may not give you the necessary power to XYZ.

ip addr | grep "inet"
ip link | grep "link"
ping -c 3 $HOSTNAME
traceroute 8.8.8.8
dig $HOSTNAME
host $HOSTNAME
ss -tulnp
