---
title: Install Mathematica on Mobile (Android)
date: 2020-08-21
category: [ mathematica, userland ]
---

If you have an Android mobile running on the armhf architecture, lucky! You can use the Wolfram Mathematica build for the Raspberry pi. Most of recent devices, unfortunately, are running on arm64 (aarch64). However, we can use `dpkg`'s multi-arch support!

Install UserLAnd from the Play Store, and install your favourite (Debian-based) Linux distribution to proceed. FYI, my machine is running on Debian Buster, with UserLAnd!

First, run the following command to add the support for the `armhf` architecture.

```sh
sudo dpkg --add-architecture armhf
```

Download `wolfram-engine_12.0.1*_armhf.deb` from [the offician repo](http://archive.raspberrypi.org/debian/pool/main/w/wolfram-engine/).

Run

```sh
sudo dpkg -i wolfram-engine_12.0.1*_armhf.deb
```

to check the dependencies. The message something as follows will be shown:

```text
The following packages have unmet dependencies:
wolfram-engine:armhf :
Depends: libportaudio2:armhf but it is not going to be installed
Depends: libssl1.0.2:armhf but it is not installable
Depends: liboauth0:armhf but it is not going to be installed
Depends: libmp3lame0:armhf but it is not going to be installed
Depends: libmad0:armhf but it is not going to be installed
Depends: libgmime-2.6-0:armhf but it is not going to be installed
Depends: libsndfile1:armhf but it is not going to be installed
Depends: libxmu6:armhf but it is not going to be installed
Depends: libglu1-mesa:armhf but it is not going to be installed
Depends: libexiv2-14:armhf but it is not going to be installed
Depends: libgfortran3:armhf but it is not installable
Depends: libncurses5:armhf but it is not going to be installed
Depends: wolframscript:armhf but it is not installable
```

For any dependencies which appear in messages ending with `but it is not going to be installed`, you can get it by `apt-get`ting. For other packages, download the `.deb` files from either the repo for Debian Stretch (oldstable) or the repo for Raspbian (to install `wolframscript:armhf`.) After some efforts, you may succeed to install `wolfram-engine:armhf`.

Now, go to the installation directory. You can check it from

```sh
$ whereis wolframscript
wolframscript: /usr/bin/wolframscript [...]

$ ls -l /usr/bin/wolframscript
[...] /usr/bin/wolframscript -> /opt/Wolfram/WolframScript/bin/wolframscript
```

then `/opt/Wolfram/WolframScript/bin` is your installation directory for binary files. In that directory, run

```sh
./WolframKernel
```

If the web authentication failed, try to [request a password](https://user.wolfram.com/portal/passwordrequest.html).

After logging in, run wolfram binaries with the environment variable `SystemIDList=Linux-ARM`: for example,

```sh
SystemIDList=Linux-ARM wolframscript -code 2+2
```

You may set some aliases to omit such a tedious process:

```sh
alias wls='SystemIDList=Linux-ARM wolframscript'
```

in `.bashrc`, for instance.
