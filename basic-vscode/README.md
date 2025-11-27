### Info

this directory contains replica of the 
[VS Code in Docker](https://github.com/pubkey/vscode-in-docker)
repository, modified to reduce the size of the image, migrate from base image Ubuntu [22.04](https://releases.ubuntu.com/jammy) (__Jammy__:) to Ubuntu [24.04](https://canonical.com/blog/canonical-releases-ubuntu-24-04-noble-numbat) (__Noble__) LTS and fix the UID collision damaging volume sharing on Linux on Linux Docker development
The project *does not* appear to have a published image on __Docker Hub__ (or any registry) under the same name: the GitHub repo mentions "No packages" in its [README](https://github.com/pubkey/vscode-in-docker/blob/master/README.md)

### Note

The project directory layout is unusual, it reflects the original project directory layout. Also there is a number of files with `.bash` extension which are operational and install bash scripts. Eventually it will be normalized

### Testing

```sh
sudo bash run.bash
```

```
sudo usermod -a -G docker $(whoami)
```
log out of your user session, then log back in (not tested)

```txt
vscode           latest              60b0e8b9f2f5   2 hours ago      2.81GB
```

### See Also

### Building

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


