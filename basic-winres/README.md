
#### Using MinGW / GCC (Easiest & Completely Free)

If you use MinGW-w64, you can build the DLL using windres (the GNU Windows resource compiler) and ld (the GNU linker).

Create your resource script: Save a file named `myresource.rc` containing your `assets.rc`
```rc
// Example: Embedding an icon and a string table
101 ICON "app_icon.ico"

STRINGTABLE
BEGIN
    1 "Welcome to the application!"
    2 "An error has occurred."
END
```

Compile the resource script into a COFF object
```
windres -i myresource.rc -o myresource.o
```

Link it into a resource-only DLL
```
ld --dll -o myresource.dll myresource.o
```
> NOTE: the unfortunate `ICON` statement in a Windows Resource (.rc) file is *native*, and the value source  file *must* be in the standard `.ico` binary format.
> Failure to take this into account e.g. using  another format (like .png, .bmp, or .jpg) in right hand side in the ICON statement,  
> leads to the Windows Resource Compiler (`rc.exe`) compilation error, such as:
> ``` 
> RC2175: icon file is not in 3.00 format. 
> ```
> This happens because the compiler explicitly expects an `icon directory` header followed by individual `icon image`
> components to split them into `RT_ICON` structures.
> In other words *it happens*.

The alternarive solution is to convert subject image to Windows Bitmap format (.bmp), 
and use the BITMAP statement:
```rc
IDB_MY_BITMAP BITMAP "assets/image.bmp"
```


### Usage

```sh
docker build -f Dockerfile  -t test .
```

```text
Sending build context to Docker daemon  6.144kB
Step 1/4 : FROM ubuntu:18.04
18.04: Pulling from library/ubuntu
7c457f213c76: Pull complete
Digest: sha256:152dc042452c496007f07ca9127571cb9c29697f42acbfad72324b2bb2e43c98
Status: Downloaded newer image for ubuntu:18.04
 ---> f9a80a55f492
Step 2/4 : RUN apt-get update &&     apt-get install -y mingw-w64 make &&     rm -rf /var/lib/apt/lists/*
 ---> Running in 6ff29fb809e2
Get:1 http://security.ubuntu.com/ubuntu bionic-security InRelease [102 kB]
Get:2 http://archive.ubuntu.com/ubuntu bionic InRelease [242 kB]
Get:3 http://security.ubuntu.com/ubuntu bionic-security/restricted amd64 Packages [1658 kB]
Get:4 http://archive.ubuntu.com/ubuntu bionic-updates InRelease [102 kB]
Get:5 http://security.ubuntu.com/ubuntu bionic-security/multiverse amd64 Packages [23.5 kB]
Get:6 http://security.ubuntu.com/ubuntu bionic-security/universe amd64 Packages [1622 kB]
Get:7 http://archive.ubuntu.com/ubuntu bionic-backports InRelease [102 kB]
Get:8 http://security.ubuntu.com/ubuntu bionic-security/main amd64 Packages [3328 kB]
Get:9 http://archive.ubuntu.com/ubuntu bionic/main amd64 Packages [1344 kB]
Get:10 http://archive.ubuntu.com/ubuntu bionic/multiverse amd64 Packages [186 kB]
Get:11 http://archive.ubuntu.com/ubuntu bionic/restricted amd64 Packages [13.5 kB]
Get:12 http://archive.ubuntu.com/ubuntu bionic/universe amd64 Packages [11.3 MB]
Get:13 http://archive.ubuntu.com/ubuntu bionic-updates/universe amd64 Packages [2389 kB]
Get:14 http://archive.ubuntu.com/ubuntu bionic-updates/main amd64 Packages [3735 kB]
Get:15 http://archive.ubuntu.com/ubuntu bionic-updates/restricted amd64 Packages [1697 kB]
Get:16 http://archive.ubuntu.com/ubuntu bionic-updates/multiverse amd64 Packages [30.5 kB]
Get:17 http://archive.ubuntu.com/ubuntu bionic-backports/universe amd64 Packages [20.5 kB]
Get:18 http://archive.ubuntu.com/ubuntu bionic-backports/main amd64 Packages [63.4 kB]
Fetched 28.0 MB in 9s (3170 kB/s)
Reading package lists...
Reading package lists...
Building dependency tree...
Reading state information...
The following additional packages will be installed:
  binutils-common binutils-mingw-w64-i686 binutils-mingw-w64-x86-64
  g++-mingw-w64 g++-mingw-w64-i686 g++-mingw-w64-x86-64 gcc-mingw-w64
  gcc-mingw-w64-base gcc-mingw-w64-i686 gcc-mingw-w64-x86-64 libisl19 libmpc3
  libmpfr6 mingw-w64-common mingw-w64-i686-dev mingw-w64-x86-64-dev
Suggested packages:
  gcc-7-locales make-doc wine wine64
The following NEW packages will be installed:
  binutils-common binutils-mingw-w64-i686 binutils-mingw-w64-x86-64
  g++-mingw-w64 g++-mingw-w64-i686 g++-mingw-w64-x86-64 gcc-mingw-w64
  gcc-mingw-w64-base gcc-mingw-w64-i686 gcc-mingw-w64-x86-64 libisl19 libmpc3
  libmpfr6 make mingw-w64 mingw-w64-common mingw-w64-i686-dev
  mingw-w64-x86-64-dev
0 upgraded, 18 newly installed, 0 to remove and 1 not upgraded.
Need to get 129 MB of archives.
After this operation, 747 MB of additional disk space will be used.
Get:1 http://archive.ubuntu.com/ubuntu bionic-updates/main amd64 binutils-common amd64 2.30-21ubuntu1~18.04.9 [197 kB]
Get:2 http://archive.ubuntu.com/ubuntu bionic/main amd64 libisl19 amd64 0.19-1 [551 kB]
Get:3 http://archive.ubuntu.com/ubuntu bionic/main amd64 libmpfr6 amd64 4.0.1-1 [243 kB]
Get:4 http://archive.ubuntu.com/ubuntu bionic/main amd64 make amd64 4.1-9.1ubuntu1 [154 kB]
Get:5 http://archive.ubuntu.com/ubuntu bionic/universe amd64 binutils-mingw-w64-i686 amd64 2.30-7ubuntu1+8ubuntu1 [2324 kB]
Get:6 http://archive.ubuntu.com/ubuntu bionic/universe amd64 binutils-mingw-w64-x86-64 amd64 2.30-7ubuntu1+8ubuntu1 [2677 kB]
Get:7 http://archive.ubuntu.com/ubuntu bionic/universe amd64 mingw-w64-common all 5.0.3-1 [4756 kB]
Get:8 http://archive.ubuntu.com/ubuntu bionic/universe amd64 mingw-w64-i686-dev all 5.0.3-1 [2212 kB]
Get:9 http://archive.ubuntu.com/ubuntu bionic/universe amd64 gcc-mingw-w64-base amd64 7.3.0-11ubuntu1+20.2build1 [172 kB]
Get:10 http://archive.ubuntu.com/ubuntu bionic/main amd64 libmpc3 amd64 1.1.0-1 [40.8 kB]
Get:11 http://archive.ubuntu.com/ubuntu bionic/universe amd64 gcc-mingw-w64-i686 amd64 7.3.0-11ubuntu1+20.2build1 [33.2 MB]
Get:12 http://archive.ubuntu.com/ubuntu bionic/universe amd64 g++-mingw-w64-i686 amd64 7.3.0-11ubuntu1+20.2build1 [22.1 MB]
Get:13 http://archive.ubuntu.com/ubuntu bionic/universe amd64 mingw-w64-x86-64-dev all 5.0.3-1 [3333 kB]
Get:14 http://archive.ubuntu.com/ubuntu bionic/universe amd64 gcc-mingw-w64-x86-64 amd64 7.3.0-11ubuntu1+20.2build1 [33.3 MB]
Get:15 http://archive.ubuntu.com/ubuntu bionic/universe amd64 g++-mingw-w64-x86-64 amd64 7.3.0-11ubuntu1+20.2build1 [23.0 MB]
Get:16 http://archive.ubuntu.com/ubuntu bionic/universe amd64 g++-mingw-w64 all 7.3.0-11ubuntu1+20.2build1 [172 kB]
Get:17 http://archive.ubuntu.com/ubuntu bionic/universe amd64 gcc-mingw-w64 all 7.3.0-11ubuntu1+20.2build1 [172 kB]
Get:18 http://archive.ubuntu.com/ubuntu bionic/universe amd64 mingw-w64 all 5.0.3-1 [9076 B]
debconf: delaying package configuration, since apt-utils is not installed
Fetched 129 MB in 7s (18.8 MB/s)
Selecting previously unselected package binutils-common:amd64.
(Reading database ... 4050 files and directories currently installed.)
Preparing to unpack .../00-binutils-common_2.30-21ubuntu1~18.04.9_amd64.deb ...
Unpacking binutils-common:amd64 (2.30-21ubuntu1~18.04.9) ...
Selecting previously unselected package libisl19:amd64.
Preparing to unpack .../01-libisl19_0.19-1_amd64.deb ...
Unpacking libisl19:amd64 (0.19-1) ...
Selecting previously unselected package libmpfr6:amd64.
Preparing to unpack .../02-libmpfr6_4.0.1-1_amd64.deb ...
Unpacking libmpfr6:amd64 (4.0.1-1) ...
Selecting previously unselected package make.
Preparing to unpack .../03-make_4.1-9.1ubuntu1_amd64.deb ...
Unpacking make (4.1-9.1ubuntu1) ...
Selecting previously unselected package binutils-mingw-w64-i686.
Preparing to unpack .../04-binutils-mingw-w64-i686_2.30-7ubuntu1+8ubuntu1_amd64.deb ...
Unpacking binutils-mingw-w64-i686 (2.30-7ubuntu1+8ubuntu1) ...
Selecting previously unselected package binutils-mingw-w64-x86-64.
Preparing to unpack .../05-binutils-mingw-w64-x86-64_2.30-7ubuntu1+8ubuntu1_amd64.deb ...
Unpacking binutils-mingw-w64-x86-64 (2.30-7ubuntu1+8ubuntu1) ...
Selecting previously unselected package mingw-w64-common.
Preparing to unpack .../06-mingw-w64-common_5.0.3-1_all.deb ...
Unpacking mingw-w64-common (5.0.3-1) ...
Selecting previously unselected package mingw-w64-i686-dev.
Preparing to unpack .../07-mingw-w64-i686-dev_5.0.3-1_all.deb ...
Unpacking mingw-w64-i686-dev (5.0.3-1) ...
Selecting previously unselected package gcc-mingw-w64-base.
Preparing to unpack .../08-gcc-mingw-w64-base_7.3.0-11ubuntu1+20.2build1_amd64.deb ...
Unpacking gcc-mingw-w64-base (7.3.0-11ubuntu1+20.2build1) ...
Selecting previously unselected package libmpc3:amd64.
Preparing to unpack .../09-libmpc3_1.1.0-1_amd64.deb ...
Unpacking libmpc3:amd64 (1.1.0-1) ...
Selecting previously unselected package gcc-mingw-w64-i686.
Preparing to unpack .../10-gcc-mingw-w64-i686_7.3.0-11ubuntu1+20.2build1_amd64.deb ...
Unpacking gcc-mingw-w64-i686 (7.3.0-11ubuntu1+20.2build1) ...
Selecting previously unselected package g++-mingw-w64-i686.
Preparing to unpack .../11-g++-mingw-w64-i686_7.3.0-11ubuntu1+20.2build1_amd64.deb ...
Unpacking g++-mingw-w64-i686 (7.3.0-11ubuntu1+20.2build1) ...
Selecting previously unselected package mingw-w64-x86-64-dev.
Preparing to unpack .../12-mingw-w64-x86-64-dev_5.0.3-1_all.deb ...
Unpacking mingw-w64-x86-64-dev (5.0.3-1) ...
Selecting previously unselected package gcc-mingw-w64-x86-64.
Preparing to unpack .../13-gcc-mingw-w64-x86-64_7.3.0-11ubuntu1+20.2build1_amd64.deb ...
Unpacking gcc-mingw-w64-x86-64 (7.3.0-11ubuntu1+20.2build1) ...
Selecting previously unselected package g++-mingw-w64-x86-64.
Preparing to unpack .../14-g++-mingw-w64-x86-64_7.3.0-11ubuntu1+20.2build1_amd64.deb ...
Unpacking g++-mingw-w64-x86-64 (7.3.0-11ubuntu1+20.2build1) ...
Selecting previously unselected package g++-mingw-w64.
Preparing to unpack .../15-g++-mingw-w64_7.3.0-11ubuntu1+20.2build1_all.deb ...
Unpacking g++-mingw-w64 (7.3.0-11ubuntu1+20.2build1) ...
Selecting previously unselected package gcc-mingw-w64.
Preparing to unpack .../16-gcc-mingw-w64_7.3.0-11ubuntu1+20.2build1_all.deb ...
Unpacking gcc-mingw-w64 (7.3.0-11ubuntu1+20.2build1) ...
Selecting previously unselected package mingw-w64.
Preparing to unpack .../17-mingw-w64_5.0.3-1_all.deb ...
Unpacking mingw-w64 (5.0.3-1) ...
Setting up make (4.1-9.1ubuntu1) ...
Setting up libmpfr6:amd64 (4.0.1-1) ...
Setting up gcc-mingw-w64-base (7.3.0-11ubuntu1+20.2build1) ...
Setting up binutils-common:amd64 (2.30-21ubuntu1~18.04.9) ...
Setting up libmpc3:amd64 (1.1.0-1) ...
Setting up mingw-w64-common (5.0.3-1) ...
Setting up mingw-w64-i686-dev (5.0.3-1) ...
Setting up libisl19:amd64 (0.19-1) ...
Setting up binutils-mingw-w64-x86-64 (2.30-7ubuntu1+8ubuntu1) ...
Setting up binutils-mingw-w64-i686 (2.30-7ubuntu1+8ubuntu1) ...
Setting up gcc-mingw-w64-i686 (7.3.0-11ubuntu1+20.2build1) ...
update-alternatives: using /usr/bin/i686-w64-mingw32-gcc-posix to provide /usr/bin/i686-w64-mingw32-gcc (i686-w64-mingw32-gcc) in auto mode
update-alternatives: warning: skip creation of /usr/bin/i686-w64-mingw32-gcc-7 because associated file /usr/bin/i686-w64-mingw32-gcc-7.2-posix (of link group i686-w64-mingw32-gcc) doesn't exist
update-alternatives: using /usr/bin/i686-w64-mingw32-gcc-win32 to provide /usr/bin/i686-w64-mingw32-gcc (i686-w64-mingw32-gcc) in auto mode
update-alternatives: warning: skip creation of /usr/bin/i686-w64-mingw32-gcc-7 because associated file /usr/bin/i686-w64-mingw32-gcc-7.2-win32 (of link group i686-w64-mingw32-gcc) doesn't exist
Setting up mingw-w64-x86-64-dev (5.0.3-1) ...
Setting up g++-mingw-w64-i686 (7.3.0-11ubuntu1+20.2build1) ...
update-alternatives: using /usr/bin/i686-w64-mingw32-g++-posix to provide /usr/bin/i686-w64-mingw32-g++ (i686-w64-mingw32-g++) in auto mode
update-alternatives: using /usr/bin/i686-w64-mingw32-g++-win32 to provide /usr/bin/i686-w64-mingw32-g++ (i686-w64-mingw32-g++) in auto mode
Setting up gcc-mingw-w64-x86-64 (7.3.0-11ubuntu1+20.2build1) ...
update-alternatives: using /usr/bin/x86_64-w64-mingw32-gcc-posix to provide /usr/bin/x86_64-w64-mingw32-gcc (x86_64-w64-mingw32-gcc) in auto mode
update-alternatives: warning: skip creation of /usr/bin/x86_64-w64-mingw32-gcc-7 because associated file /usr/bin/x86_64-w64-mingw32-gcc-7.2-posix (of link group x86_64-w64-mingw32-gcc) doesn't exist
update-alternatives: using /usr/bin/x86_64-w64-mingw32-gcc-win32 to provide /usr/bin/x86_64-w64-mingw32-gcc (x86_64-w64-mingw32-gcc) in auto mode
update-alternatives: warning: skip creation of /usr/bin/x86_64-w64-mingw32-gcc-7 because associated file /usr/bin/x86_64-w64-mingw32-gcc-7.2-win32 (of link group x86_64-w64-mingw32-gcc) doesn't exist
Setting up gcc-mingw-w64 (7.3.0-11ubuntu1+20.2build1) ...
Setting up g++-mingw-w64-x86-64 (7.3.0-11ubuntu1+20.2build1) ...
update-alternatives: using /usr/bin/x86_64-w64-mingw32-g++-posix to provide /usr/bin/x86_64-w64-mingw32-g++ (x86_64-w64-mingw32-g++) in auto mode
update-alternatives: using /usr/bin/x86_64-w64-mingw32-g++-win32 to provide /usr/bin/x86_64-w64-mingw32-g++ (x86_64-w64-mingw32-g++) in auto mode
Setting up g++-mingw-w64 (7.3.0-11ubuntu1+20.2build1) ...
Setting up mingw-w64 (5.0.3-1) ...
Processing triggers for libc-bin (2.27-3ubuntu1.6) ...
Removing intermediate container 6ff29fb809e2
 ---> 4bacf6661781
Step 3/4 : RUN x86_64-w64-mingw32-windres --version
 ---> Running in 6553ffd4f881
GNU windres (GNU Binutils) 2.30
Copyright (C) 2018 Free Software Foundation, Inc.
This program is free software; you may redistribute it under the terms of
the GNU General Public License version 3 or (at your option) any later version.
This program has absolutely no warranty.
Removing intermediate container 6553ffd4f881
 ---> 29987975ab55
Step 4/4 : WORKDIR /app
 ---> Running in c318644b6ab1
Removing intermediate container c318644b6ab1
 ---> af985ff45d94
Successfully built af985ff45d94
Successfully tagged test:latest
SECURITY WARNING: You are building a Docker image from Windows against a non-Windows Docker host. All files and directories added to build context will have '-rwxr-xr-x' permissions. It is recommended to double check and reset permissions for sensitive files and directories.
```

```sh
docker run --rm -it -w /tmp -v ~/test:/tmp test sh
```
```sh
/usr/bin/x86_64-w64-mingw32-windres myresource.rc  -o myresource.o
/usr/x86_64-w64-mingw32/bin/ld --dll -o myresource.dll myresource.o
exit
```
```sh
ls ~/test
```
```text
icon1.bmp  icon1.ico  myresource.dll*  myresource.o  myresource.rc
```
#### Exotic Option

Using MSVC Command Line Tools (Build Tools)

If you don't want the Visual Studio IDE but still want to use Microsoft's compiler, you can download the standalone Visual Studio Build Tools.
1. Open the Developer Command Prompt for VS.
2. Compile the resource script using rc.exe to generate a .res file:cmd
rc.exe myresource.rc
Use code with caution.
3. Link the resource into a DLL using link.exe:cmd
link.exe /DLL /NOENTRY /MACHINE:X64 myresource.res /OUT:myresource.dll
Use code with caution.

	• Note: The /NOENTRY flag is mandatory. It prevents the linker from looking for a DllMain or _main function, creating a pure data/resource-only binary.

Method 3: For .NET / C# Assemblies

If you are trying to create a resource-only assembly for a .NET environment rather than native Win32, you can use the built-in SDK tools resgen.exe and al.exe:
1. Convert an XML resource file (.resx) to a binary .resources file:cmd
resgen.exe Strings.resx Strings.resources
Use code with caution.
2. Embed it into a DLL assembly using the Assembly Linker (al.exe):cmd
al.exe /embed:Strings.resources /out:Strings.resources.dll
Use code with caution.
Let me know:
• What type of resources you are trying to embed (images, strings, icons, or binary data)?
• Which programming language or environment will be loading this DLL?
I can give you the exact .rc script syntax or the code snippets to load them!


### See Also

  * [Image](https://hub.docker.com/r/mwaeckerlin/mingw) and [source](https://github.com/mwaeckerlin/mingw) of `mwaeckerlin/mingw` -  `Dockerfile` with advanced set of bash build scripts for cross compiling Windows targets in a Ubuntu Docker Container. The image is likely capable of all sorts of much more interesing stuff than `winres` execution is a possible far overkill
 