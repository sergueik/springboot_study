### Info

Docker project with some repository hosted in the Docker image based on the 
[Dockerising a Perl application](https://despairlabs.com/posts/2014-07-15-dockerising-a-perl-application/)
article exploring the latest committed hash in the target repository to trigger git pull
into the docker container. The odirinal `Dockerfile` converted to
run on an Alpine openjdk jre base image, with the conditional git apk update

### Usage

* Build the container 2 times
```sh
docker build -f Dockerfile -t basic-workspace .
```
the second iteration will skip every step with `Using cache` verdict
and creating a potentially stale state of the repository workspace burned into the image.

Adding the following argument and passing the latest hash via build arg command line option does not force Docker to re-run the step in question

```sh
PROJECT=springboot_study
APPS_DIR=/$HOME/workspace
LATEST_HASH=$(git --git-dir=${APPS_DIR}/${PROJECT}/.git rev-parse --short HEAD)
```
this aparenrently ignores the hash change:
```sh
docker build -f Dockerfile --build-arg "LATEST_HASH=$(git --git-dir=${APPS_DIR}/${PROJECT}/.git rev-parse --short HEAD)" -t basic-workspace .
```

the only way seems to update the `Dockerfile` directly:
```sh
sed -i "s|LATEST_HASH=\".*\"|LATEST_HASH=\"${LATEST_HASH}\"|" Dockerfile
docker build -f Dockerfile -t basic-workspace .
```
This will lead to the line

```sh

ARG LATEST_HASH=""
RUN echo "latest hash is ${LATEST_HASH}"
RUN echo "Pulling hash ${LATEST_HASH}" && cd ${APPS_DIR} && git clone https://github.com/sergueik/${PROJECT}
```
be reappled
```sh
Step 10/11 : RUN echo "Pulling hash ${LATEST_HASH}" &&     cd ${APPS_DIR} &&     git clone https://github.com/sergueik/${PROJECT}
 ---> Running in 5dc6d4df9f40
Pulling hash abcd
Cloning into 'springboot_study'...
Removing intermediate container 5dc6d4df9f40
 ---> 89623d0015f0
```
Note: separating the lines
```sh
ARG LATEST_HASH=""
RUN echo "latest hash is ${LATEST_HASH}"
RUN cd ${APPS_DIR} && git clone https://github.com/sergueik/${PROJECT}
```

is also possible:
```sh
LATEST_HASH='efgh'
sed -i "s|LATEST_HASH=\".*\"|LATEST_HASH=\"${LATEST_HASH}\"|" Dockerfile
docker build -f Dockerfile -t basic-workspace .
```
leads
```sh
Step 8/11 : ARG LATEST_HASH="efgh"
 ---> Running in 8fd9e2a64364
Removing intermediate container 8fd9e2a64364
 ---> 26d0ae873524
Step 9/11 : RUN echo "latest hash is ${LATEST_HASH}"
 ---> Running in 585e6e09dbef
latest hash is efgh
Removing intermediate container 585e6e09dbef
 ---> e8bc081e0890
Step 10/11 : RUN cd ${APPS_DIR} && git clone https://github.com/sergueik/${PROJECT}
 ---> Running in 07aad4d43910
Cloning into 'springboot_study'...
Removing intermediate container 07aad4d43910
 ---> e8fdcf455178
```
the container can be exported or used per its original purpose:

```sh
docker run basic-workspace
```

and used for one command like
```sh
IMAGE_NAME='basic-workspace'
PROJECT='springboot_study'
USER=...
PASSWORD=...
# TODO: url encode
docker exec -it $(docker container ls | grep $IMAGE_NAME | head -1 | awk '{print $1}') sh -c "cd /opt/apps/$PROJECT ; git pull https://${USER}:${PASSWORD}@github.com/${PROJECT}"
```
alternatively use the shell script variation to specify 
an arbitrary branch `$BRANCH`:
```sh
git remote remove origin
git remove add origin "https://${USER}:${PASSWORD}@github.com/${PROJECT}"
git pull origin $BRANCH
git remove remove origin
```
(all concatenated into shell argument) 

### Use Github API

* generate a new token through __Settings/Developer settings__ [Personal access tokens](https://github.com/settings/tokens)
*  use it with `curl`:
```sh
curl -O -J -L -u ${TOKEN}:x-oauth-basic https://github.com/$USER/${PROJECT}/archive/${LATEST_HASH}.zip
```
will download the selected commit of the workspace.
NOTE: it will save under a name it chooses, ignoring the `-O` argument e.g.:
```sh
curl: Saved to filename 'springboot_study-4cc172c5cb4bc7d72641ff9466b8b909a78773ed.zip'
```
NOTE: the variant
```sh
USER=...
PASSWORD=...

LATEST_HASH=$(git --git-dir=${APPS_DIR}/${PROJECT}/.git rev-parse --short HEAD)
curl -O -J -L -u "${USER}:${PASSWORD}" https://github.com/$USER/${PROJECT}/archive/${LATEST_HASH}.zip
```
will lead to the error message and failure
```text
Deprecated authentication method. Create a Personal Access Token to access: https://github.com/settings/token
```
Before the ban of the passwor based API  access the file would be downloaded and written into
`${LATEST_HASH}.zip`

### More  API

Following the steps covered in [document](https://docs.github.com/en/rest/overview/other-authentication-methods#basic-authentication

```sh
curl -u "$(whoami):$TOKEN" https://api.github.com/user
```

gives the list of supported API:
```json
{
  "login": "sergueik",
  "id": 591270,
  "node_id": "MDQ6VXNlcjU5MTI3MA==",
  "avatar_url": "https://avatars.githubusercontent.com/u/591270?v=4",
  "gravatar_id": "",
  "url": "https://api.github.com/users/sergueik",
  "html_url": "https://github.com/sergueik",
  "followers_url": "https://api.github.com/users/sergueik/followers",
  "following_url": "https://api.github.com/users/sergueik/following{/other_user}",
  "gists_url": "https://api.github.com/users/sergueik/gists{/gist_id}",
  "starred_url": "https://api.github.com/users/sergueik/starred{/owner}{/repo}",
  "subscriptions_url": "https://api.github.com/users/sergueik/subscriptions",
  "organizations_url": "https://api.github.com/users/sergueik/orgs",
  "repos_url": "https://api.github.com/users/sergueik/repos",
  "events_url": "https://api.github.com/users/sergueik/events{/privacy}",
  "received_events_url": "https://api.github.com/users/sergueik/received_events",
  "type": "User",
  "site_admin": false,
  "name": "Serguei Kouzmine",
  "company": null,
  "blog": "",
  "location": "Fort Lauderdale",
  "email": "kouzmine_serguei@yahoo.com",
  "hireable": true,
  "bio": null,
  "twitter_username": null,
  "public_repos": 99,
  "public_gists": 0,
  "followers": 32,
  "following": 0,
  "created_at": "2011-01-30T14:35:21Z",
  "updated_at": "2021-10-04T19:11:16Z"
}

```

specifying the repo to clone

```sh
curl -u "$(whoami):$TOKEN" https://api.github.com/users/$(whoami)/repos
```
filter by the DOM node name:
```sh
curl -u "$(whoami):$TOKEN" https://api.github.com/users/$(whoami)/repos | jq '.[]|.html_url'
```
will give the list of browser urls.

repeating the query with `clone_url`
will return the list of clone URLs.
Note if there is too many, the result is truncated

* https://stackoverflow.com/questions/67040794/how-can-i-get-the-commit-hash-of-the-latest-release-from-github-api
* https://stackoverflow.com/questions/62629611/github-rest-api-git-clone
* https://docs.gitlab.com/ee/api/commits.html
```sh
PROJECT=selenium-fluxbox
USER=$(whoami)
curl -H 'Accept: application/vnd.github.v3+json' -u "$USER:$TOKEN" "https://api.github.com/repos/$USER/$PROJECT/commits" |  jq '.[0]'
```
is returning the latest commit in the repository `$PROJECT` -  the commits are shown in reverse chronological order:
```json
{
  "sha": "5d86c3f50dfcbd4844ae148fddf10c7ce4cb5301",
  "node_id": "MDY6Q29tbWl0NzY5OTczMDQ6NWQ4NmMzZjUwZGZjYmQ0ODQ0YWUxNDhmZGRmMTBjN2NlNGNiNTMwMQ==",
  "commit": {
    "author": {
      "name": "Serguei Kouzmine",
      "email": "kouzmine_serguei@yahoo.com",
      "date": "2021-09-04T05:12:19Z"
    },
    "committer": {
      "name": "Serguei Kouzmine",
      "email": "kouzmine_serguei@yahoo.com",
      "date": "2021-09-04T05:12:19Z"
    },
    "message": "pinned and bumped the jsoup depedndency version per dependabot",
    "tree": {
      "sha": "966cfbed4841a3f7bb8a72fb559489d5cb5b436c",
      "url": "https://api.github.com/repos/sergueik/selenium-fluxbox/git/trees/966cfbed4841a3f7bb8a72fb559489d5cb5b436c"
    },
    "url": "https://api.github.com/repos/sergueik/selenium-fluxbox/git/commits/5d86c3f50dfcbd4844ae148fddf10c7ce4cb5301",
    "comment_count": 0,
    "verification": {
      "verified": false,
      "reason": "unsigned",
      "signature": null,
      "payload": null
    }
  },
  "url": "https://api.github.com/repos/sergueik/selenium-fluxbox/commits/5d86c3f50dfcbd4844ae148fddf10c7ce4cb5301",
  "html_url": "https://github.com/sergueik/selenium-fluxbox/commit/5d86c3f50dfcbd4844ae148fddf10c7ce4cb5301",
  "comments_url": "https://api.github.com/repos/sergueik/selenium-fluxbox/commits/5d86c3f50dfcbd4844ae148fddf10c7ce4cb5301/comments",
  "author": {
    "login": "sergueik",
    "id": 591270,
    "node_id": "MDQ6VXNlcjU5MTI3MA==",
    "avatar_url": "https://avatars.githubusercontent.com/u/591270?v=4",
    "gravatar_id": "",
    "url": "https://api.github.com/users/sergueik",
    "html_url": "https://github.com/sergueik",
    "followers_url": "https://api.github.com/users/sergueik/followers",
    "following_url": "https://api.github.com/users/sergueik/following{/other_user}",
    "gists_url": "https://api.github.com/users/sergueik/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/sergueik/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/sergueik/subscriptions",
    "organizations_url": "https://api.github.com/users/sergueik/orgs",
    "repos_url": "https://api.github.com/users/sergueik/repos",
    "events_url": "https://api.github.com/users/sergueik/events{/privacy}",
    "received_events_url": "https://api.github.com/users/sergueik/received_events",
    "type": "User",
    "site_admin": false
  },
  "committer": {
    "login": "sergueik",
    "id": 591270,
    "node_id": "MDQ6VXNlcjU5MTI3MA==",
    "avatar_url": "https://avatars.githubusercontent.com/u/591270?v=4",
    "gravatar_id": "",
    "url": "https://api.github.com/users/sergueik",
    "html_url": "https://github.com/sergueik",
    "followers_url": "https://api.github.com/users/sergueik/followers",
    "following_url": "https://api.github.com/users/sergueik/following{/other_user}",
    "gists_url": "https://api.github.com/users/sergueik/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/sergueik/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/sergueik/subscriptions",
    "organizations_url": "https://api.github.com/users/sergueik/orgs",
    "repos_url": "https://api.github.com/users/sergueik/repos",
    "events_url": "https://api.github.com/users/sergueik/events{/privacy}",
    "received_events_url": "https://api.github.com/users/sergueik/received_events",
    "type": "User",
    "site_admin": false
  },
  "parents": [
    {
      "sha": "5df1dabefb902155cc0deff1f33e2aab453b924f",
      "url": "https://api.github.com/repos/sergueik/selenium-fluxbox/commits/5df1dabefb902155cc0deff1f33e2aab453b924f",
      "html_url": "https://github.com/sergueik/selenium-fluxbox/commit/5df1dabefb902155cc0deff1f33e2aab453b924f"
    }
  ]
}
```

Then can download the archive
```sh
QUERY='.[0].sha'
LATEST_HASH=$(curl -H 'Accept: application/vnd.github.v3+json' -u "$USER:$TOKEN" "https://api.github.com/repos/$USER/$PROJECT/commits" | jq  -cr $QUERY)
```
```sh
curl -O -J -L  -H 'Accept: application/vnd.github.v3+json' -u "${USER}:${TOKEN}" https://github.com/$USER/${PROJECT}/archive/${LATEST_HASH}.zip
unzip "${PROJECT}-${LATEST_HASH}.zip"
cp -R "${PROJECT}-${LATEST_HASH}"/* workspace
rm -rf "${PROJECT}-${LATEST_HASH}.zip" "${PROJECT}-${LATEST_HASH}"
```

Committing the files is  to be done via  (WIP)
```sh
curl -v  -L -H 'Accept: application/vnd.github.v4+json' -u "${USER}:${TOKEN}" "https://github.com/$USER/projects/${PROJECT}/commits" -H 'Content-Type: application/json' -X POST
```
### Cleanup
destroy all orphaned images afterwards
```sh
docker image prune -f
```

[](https://docs.github.com/en/rest/overview/other-authentication-methods#basic-authentication)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
