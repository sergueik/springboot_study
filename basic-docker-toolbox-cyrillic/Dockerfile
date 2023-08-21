FROM ubuntu:18.04
RUN apt-get update && apt-get install -y locales

# Locale
RUN sed -i -e 's/# ru_RU.UTF-8 UTF-8/ru_RU.UTF-8 UTF-8/' /etc/locale.gen \
     && sed -i -e 's/# ru_RU.CP1251 UTF-8/ru_RU.CP1251 CP1251/' /etc/locale.gen \
     && locale-gen ru_RU.UTF-8 && locale-gen ru_RU.CP1251
# NOTE: neither one works: 
# ru_RU.UTF-8
# ru_RU.CP1251
ENV LANG ru_RU.UTF-8
ENV LANGUAGE ru_RU:ru
ENV LC_LANG ru_RU.UTF-8
ENV LC_ALL ru_RU.UTF-8
ENV LANG ru_RU.CP1251
ENV LANGUAGE ru_RU:ru
ENV LC_LANG ru_RU.CP1251
ENV LC_ALL ru_RU.CP1251
