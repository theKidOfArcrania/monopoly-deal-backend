FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y libmysqlclient-dev libatomic1

RUN mkdir /app
RUN mkdir /app/config
WORKDIR /app

COPY monopoly-deal .
RUN mkdir -p ./static/tmp

# Server environment vars
ENV YESOD_STATIC_DIR   static
ENV YESOD_HOST         *4 
ENV YESOD_PORT         3000 

# Mysql settings, you can change them in docker command as well
ENV YESOD_MYSQL_USER     dbuser
ENV YESOD_MYSQL_PASSWORD thisisasecurepassword
ENV YESOD_MYSQL_HOST     localhost
ENV YESOD_MYSQL_PORT     3306
ENV YESOD_MYSQL_DATABASE mpd
ENV YESOD_MYSQL_POOLSIZE 10

CMD /app/monopoly-deal
