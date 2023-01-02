# FROM node:16.12.0-alpine3.11
# Use alpine-based NodeJS base image
FROM node:latest

#USER root

# Install latest stable Chrome
# https://gerg.dev/2021/06/making-chromedriver-and-chrome-versions-match-in-a-docker-image/
RUN echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" | \
    tee -a /etc/apt/sources.list.d/google.list && \
    wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | \
    apt-key add - && \
    apt-get update && \
    apt-get install -y google-chrome-stable libxss1

# Determine major browser version
ARG BROWSER_MAJOR=$(google-chrome --version | sed 's/Google Chrome \([0-9]*\).*/\1/g')

# Install the Chromedriver version that corresponds to the installed major Chrome version
# https://blogs.sap.com/2020/12/01/ui5-testing-how-to-handle-chromedriver-update-in-docker-image/
RUN google-chrome --version | grep -oE "[0-9]{1,10}.[0-9]{1,10}.[0-9]{1,10}" > /tmp/chromebrowser-main-version.txt
RUN wget --no-verbose -O /tmp/latest_chromedriver_version.txt https://chromedriver.storage.googleapis.com/LATEST_RELEASE_$(cat /tmp/chromebrowser-main-version.txt)
RUN wget --no-verbose -O /tmp/chromedriver_linux64.zip https://chromedriver.storage.googleapis.com/$(cat /tmp/latest_chromedriver_version.txt)/chromedriver_linux64.zip && rm -rf /opt/selenium/chromedriver && unzip /tmp/chromedriver_linux64.zip -d /opt/selenium && rm /tmp/chromedriver_linux64.zip && mv /opt/selenium/chromedriver /opt/selenium/chromedriver-$(cat /tmp/latest_chromedriver_version.txt) && chmod 755 /opt/selenium/chromedriver-$(cat /tmp/latest_chromedriver_version.txt) && ln -fs /opt/selenium/chromedriver-$(cat /tmp/latest_chromedriver_version.txt) /usr/bin/chromedriver

# Set the variable for the container working directory
ARG WORK_DIRECTORY=/program

# Create app directory
RUN mkdir -p $WORK_DIRECTORY
WORKDIR $WORK_DIRECTORY

# Install npm packages (do this AFTER setting the working directory)

# NOTE: npm ERR! `unsafe-perm` is not a valid npm option
# RUN npm config set unsafe-perm true

RUN npm install selenium-webdriver
ENV NODE_ENV=development NODE_PATH=$WORK_DIRECTORY

# Copy script to execute to working directory
COPY runtest.js .

EXPOSE 8080

# Execute the script in NodeJS
CMD ["node", "runtest.js"]
