FROM httpd:2.4-alpine

COPY ./ /usr/local/apache2/htdocs/
WORKDIR /usr/local/apache2/htdocs/

RUN apk add --update nodejs-npm && npm install && npm run build
