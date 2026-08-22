### UiPath

Note the __Download UPiPath for Desktop__ link is actually pointing the location `https://download.uipath.com/UiPathStudioCloud.msi`. File name `UiPathStudioCloud.msi` is confusing.

![msi1](screenshots/msi1.jpg)

In addition the installer refuses to continue when the __UiPath Connected Platform Installation__ product is present on the machine.
Another freely available installer `UiPathPlatform.msi` representing the latter does not appear to really contain or install the __UiPath Studio__ . It advertises it but not really

![download1](screenshots/download1.jpg)

One can download adequate installer from the obscurely versed link "install locally" which is pointing to the page `https://cloud.uipath.com/nonamjnbyjbd/portal_/resource-center`

![components1](screenshots/components1.jpg)

it turns out one can select exactly one but not both products from

  * Studio (think of it as a vendor skinned __Microsoft Workflow Foundtion__ __Visual Studo__ revival
  * Robot (i am guessing what on earth *that* may be)

- one can run installer multiple times. Note: it is bootstrap (a.k.a. web) installer, not a standalone

```cmd
pushd %userprofile%\downloads
msiexec.exe  /l*vx "full.log" /i UiPathPlatform.msi  
```
![UI Path Custom Drawn Installer Challenging](screenshots/capture-custom-installer-options.png)

One will need to explicitly install the `UiPath.Terminal.Activities` nuget package from within __UiPath Studio__

Get ready for a really long install

There isn’t a well-supported "thin‑install" or truly portable Thinstall‑style version of __UiPath Studio__ / __Robot__ for __Community Edition__. UiPath does use Windows registry keys, and their installation is not fully "portable" in the traditional sense.

The is a `HKEY_LOCAL_MACHINE\SOFTWARE\UiPath`, `HKEY_CURRENT_USER\SOFTWARE\UiPath` keys and application files will be placed in all around the place: in `%PROGRAMDATA%\UiPath`, `%LOCALAPPDATA%\UiPath` application directories

### Tweaks

__Studio__ install can technically be in a profile mode that hides developer features. However try this first
* __Settings__ __Manage Sources__ : Add user managed source

![sources1](screenshots/sources1.png)

The url is :`https://www.myget.org/F/workflow/api/v3/index.json`

* __New Project__ __Process__ __Manage Packages__

![package1](screenshots/package1.png)

![package2](screenshots/package2.png)

* in the search enter `terminal`

![msi1](screenshots/activit1y.png)

* drag an drop __Terminal Session__ into __Main Sequence__
* configure connection

![configure1](screenshots/configure1.png)

 **Add Terminal Session**

   - Drag **Terminal Session** into the workflow (Main Sequence / Do container).
   - Configure connection:
     - Protocol: `TELNET`
     - Host: `192.168.99.100` (the ip of the __Docker Toolbox__ VM or a Linux node)
     - Port: `3270`
     - Encoding: `IBM-037` (or appropriate European __IBM__ [EBCDIC](https://en.wikipedia.org/wiki/EBCDIC) code page)

4. **Add Terminal Steps (Do container)**

   - **Set Field** for username:
     - Text: `"MYUSERNAME"` (literal string)
     - FollowedBy: `Enter`
   - **Set Field** for password:
     - Text: `"MYPASSWORD"` (literal string)
     - FollowedBy: `Enter`
   - **Wait Field Text** (optional):
     - Text: `"MAIN MENU"`
     - FollowedBy: `None`
   - **Get Field** (optional): capture a value from screen into a variable
   - **Send Control Key**: e.g., `F2` to exit session

5. **Save Workflow**

   - Press **Ctrl+S** to save all changes.

6. **Run Workflow**

F5

### Sut

One can emulate a mainframe terminal using open-source software like:

  * TN3270 emulator: emulates IBM 3270 terminals (green-screen)
  * z/OS or Hercules emulator: Hercules can emulate a full mainframe CPU and OS, but for beginners, it’s overkill.

There are no direct __TN3270__ emulator applications available as standalone images on Docker Hub, as emulators like x3270 are typically desktop applications.

Users typically run a mainframe system emulator in a Docker container, and then connect to it using a standard TN3270 emulator installed on their local machine.

```cmd
msiexec.exe /uninstall UiPathStudio.msi KEEP_USER_DATA=1
```

* pull the [tk4-helcules](https://hub.docker.com/r/skunklabz/tk4-hercules) image

```sh
IMAGE=skunklabz/tk4-hercules
docker image pull $IMAGE:latest
```
```text
latest: Pulling from skunklabz/tk4-hercules
Digest: sha256:bac92c3d232423a101c90f272dcc0be565b98f4b6ef7b61a1dcb5adceeb5e8aa
Status: Image is up to date for skunklabz/tk4-hercules:latest
docker.io/skunklabz/tk4-hercules:latest
```
> NOTE: the `latest` tag has been last pushed almost 6 years ago by developer

```sh
IMAGE=skunklabz/tk4-hercules
docker images $IMAGE
```
```txt
REPOSITORY               TAG                 IMAGE ID            CREATED             SIZE
skunklabz/tk4-hercules   latest              9e5995edb029        5 years ago         310MB
```

* pin the image if possible

```sh
ID=$(docker image ls | grep tk4-hercules | awk '{print $2 }')
echo $ID
```
ignore the warning
```text
WARNING: This output is designed for human readability. For machine-readable output, please use --format.
```
>NOTE: on some older versions one may need to print `$3` instead of `$2`.
```txt
9e5995edb029
```

```sh
docker image inspect --format='{{index .RepoDigests 0}}' $ID
```
```txt
skunklabz/tk4-hercules@sha256:bac92c3d232423a101c90f272dcc0be565b98f4b6ef7b61a1dcb5adceeb5e8aa
```

```sh
docker image inspect --format '{{.RepoTags}}' $ID
```

```text
[skunklabz/tk4-hercules:latest]
```

### Test

```sh
ID=$(docker ps --filter 'ancestor=skunklabz/tk4-hercules' --format '{{.ID}}')
```
the ID will likely be blank or else
```sh
test  -z $ID || docker rm -f $ID
```
```sh
IMAGE=skunklabz/tk4-hercules
docker run -d -p 23:23 -p 8038:8038 -p3270:3270 --name tn3270 $IMAGE
```
### Remark

![Setup Complete](screenshots/capture-custom-installer-complete.png)

On first launch __UiPath prompts for the login, one can have one's credentials ready.
`7oIpqZe82Ni@`
 
#### About UiPath Workflows (XAML)

__UiPath__ workflows are stored as **XAML files**, which are essentially **Windows Workflow Foundation (WF) declarative XML** under the hood.

A few observations from a 25-year WF retrospective:

- **Pros:**
  - Fully declarative and machine-readable
  - Supports complex sequences, variables, arguments, and custom activities
  - Enables drag-and-drop tooling in UiPath Studio

- **Cons / quirks:**
  - Extremely verbose — in a typical `Main.xaml`, **90–99% of the file content is boilerplate, tags, and metadata**, not actual logic
  - Red borders or “invalid” flags can appear if a Terminal field or activity isn’t resolved correctly
  - Hard to visually parse at a glance; diffs in source control are noisy

- **Octane meter remark:**
  XAML is **very low-octane fuel** — most of its “weight” is decoration characters, not executable logic. But this overhead is what allows Studio and UiPath Robot to **serialize, deserialize, and run workflows reliably**, even across machines and versions.

> In short: __WF__ may be a quarter-century old, but with __UiPath__ it finds a practical second life — especially for terminal automation, TN3270 emulation, and green-screen workflows.

### Note

Yes, there are few __CICS__ emulators, including `cicsterm` and `cicsprnt` provided with IBM's __CICS Transaction Gateway__, and commercial options like the Raincode QIX CICS emulator for modern platforms like .NET and Azure.

Rocket Software (formerly Micro Focus) Enterprise Server: A leading solution in the field that provides a comprehensive environment for compiling and running mainframe COBOL and CICS applications on distributed systems.
Raincode CICS emulator (Raincode QIX): An emulator specifically designed for the Microsoft .NET and Azure platforms, enabling CICS applications to run in a scalable cloud environment and integrate with C# or VB.NET code.
IBM z Development and Test Environment (zD&T): A proper, licensed emulator of the entire IBM z/OS mainframe architecture, used for development and testing purposes. A free trial version is sometimes available via the IBM Z trial program.### See Also

Hercules: An open-source, community-driven mainframe emulator that can run older MVS systems (like the TK4- distribution), allowing enthusiasts to learn and explore the mainframe environment for free

The choice of "most popular" often depends on the user's specific goals:
For enterprise development and migration, commercial products like Rocket Enterprise Server and Raincode QIX are dominant.
For accessing a running mainframe, IBM PCOMM is a common terminal emulator.
For learning or personal exploration, the open-source Hercules emulator is a popular free option

### See Also

 * [automating Terminals and Mainframes](https://www.uipath.com/kb-articles/automating-terminals-and-mainframes)
 * [guide To Seamless Mainframe Automation](https://www.uipath.com/blog/automation/guide-to-seamless-mainframe-automation)

