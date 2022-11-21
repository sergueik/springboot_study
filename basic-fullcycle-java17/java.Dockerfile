FROM openjdk:17

ARG UID=1000
ARG GID=1000

ENV UID=${UID}
ENV GID=${GID}

RUN groupadd --gid $GID app \
  && useradd --create-home --uid $UID --gid app --shell /bin/bash app

USER app

WORKDIR /app

CMD ["java"]