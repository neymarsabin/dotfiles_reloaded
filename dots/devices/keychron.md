### setup keychron k2
- the module `hid-apple` was not loaded and udev rules were not triggered

### solution
- get some info on `hid-apple` kernel module with the help of `modinfo`

        modinfo hid-apple

- load modules with modprobe

        sudo modprobe hid-apple

- `/sys/module/hid_apple/parameters/fnmode` check if the file exists?
- add `udev` rules to `/etc/udev/rules.d/70-keychron.rules`

        ACTION=="add", SUBSYSTEM=="usb", ATTRS{idVendor}=="05ac", ATTRS{idProduct}=="0220", RUN+="/bin/sh -c 'echo 2 > /sys/module/hid_apple/parameters/fnmode'"
