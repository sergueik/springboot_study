FROM ruanbekker/vscode-server:slim

ARG VERSION=4.4.0

ARG VSCODEUSER=coder
# NOTE: do not use
# ARG VSCODEUSER=$(whoami)
# it will be read literally
# NOTE: mkdir: cannot create directory ‘/home/$(whoami)’: Permission denied
ENV HOME=/home/${VSCODEUSER}

USER root

RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp
ADD bruno-api-client.bruno-${VERSION}.vsix .
RUN mkdir -p ${HOME}/Files \
    && mv bruno-api-client.bruno-${VERSION}.vsix ${HOME}/Files/bruno-api-client.bruno.vsix

# Create VS Code config directory properly
RUN mkdir -p "${HOME}/.local/share/code-server/User"

# Copy settings + keybindings to code server default user setting location

COPY Files/settings.json "${HOME}/.local/share/code-server/User/settings.json"
COPY Files/keybindings.json "${HOME}/.local/share/code-server/User/keybindings.json"
# NOTE: Different directory layout with VS Code "Server" than with VS Code "Desktop" 
# COPY Files/settings.json $HOME/.config/Code/User/settings.json
# COPY Files/keybindings.json $HOME/.config/Code/User/keybindings.json

RUN chown -R ${VSCODEUSER}:${VSCODEUSER} ${HOME}

USER ${VSCODEUSER}

RUN code-server --install-extension ${HOME}/Files/bruno-api-client.bruno.vsix --force

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s \
CMD curl -f http://localhost:8080/ || exit 1

