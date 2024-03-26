### info
https://github.com/piomin/jenkins-slave-dind-jnlp
#### Usage

* slave configuration
FYI

`Dockerfile`
```sh
COPY --chmod=777 entrypoint.sh /usr/local/bin/entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint"]
```
`entrypoint.sh`
```sh
echo "starting jnlp slave... " $JENKINS_SLAVE_NAME
exec java -jar /usr/share/jenkins/slave.jar \
	-jnlpUrl $JENKINS_URL/computer/$JENKINS_SLAVE_NAME/slave-agent.jnlp \
	-secret $JENKINS_SLAVE_SECRET
```

### See Also






[continuous Integration with Docker and Jenkins](https://medium.com/@leihetito/continuous-integration-with-docker-and-jenkins-b2d2e916e8ea)
[jenkins in Docker: Running Docker in a Jenkins Container](https://hackmamba.io/blog/2022/04/running-docker-in-a-jenkins-container/)
[quickstart CI with Jenkins and Docker-in-Docker](https://medium.com/swlh/quickstart-ci-with-jenkins-and-docker-in-docker-c3f7174ee9ff)
[repo](https://github.com/Kikiodazie/Backend-RESTful-API)
https://blog.docker.com/2013/09/docker-can-now-run-within-docker/
https://forums.docker.com/t/using-docker-in-a-dockerized-jenkins-container/322
https://gist.github.com/adelmofilho/5a30a87eaf1cd4a03052f37b516d6714
https://gist.github.com/adelmofilho/5a30a87eaf1cd4a03052f37b516d6714
https://github.com/ahunnargikar/jenkins-dind
https://github.com/alekslitvinenk/dind
https://github.com/dkapanidis/dind-jenkins-slave
https://github.com/jpetazzo/dind
https://github.com/jpetazzo/dind
https://github.com/killercentury/docker-jenkins-dind
https://github.com/killercentury/docker-jenkins-dind
https://github.com/killercentury/docker-jenkins-dind
https://github.com/killercentury/docker-jenkins-slave-dind
https://github.com/piomin/jenkins-slave-dind-jnlp
https://github.com/tehranian/dind-jenkins-slave
https://github.com/tehranian/dind-jenkins-slave
https://hackmamba.io/blog/2022/04/running-docker-in-a-jenkins-container/
https://hackmamba.io/blog/2022/04/running-docker-in-a-jenkins-container/
https://stackoverflow.com/questions/1894967/how-to-request-administrator-access-inside-a-batch-file
https://stackoverflow.com/questions/1894967/how-to-request-administrator-access-inside-a-batch-file
https://stackoverflow.com/questions/7985755/how-to-detect-if-cmd-is-running-as-administrator-has-elevated-privileges
https://stackoverflow.com/questions/7985755/how-to-detect-if-cmd-is-running-as-administrator-has-elevated-privileges
https://wiki.jenkins-ci.org/display/JENKINS/Docker+Plugin
https://www.jenkins.io/doc/book/installing/docker/
