FROM busybox
ADD index.html /www/index.html
ADD public /www/public

RUN apt-get update && apt install node && npm install && npm build

EXPOSE 8080
CMD trap "exit 0;" TERM INT; httpd -p 8080 -h /www -f & wait