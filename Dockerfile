FROM node:latest
WORKDIR /usr/app
COPY . /usr/app
RUN npm install elm http-server
RUN ./node_modules/.bin/elm make src/Main.elm --output=build/pamela.js
WORKDIR /usr/app/build
CMD [ "../node_modules/.bin/http-server", "-p", "3000" ]