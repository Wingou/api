FROM node:latest

RUN npm install -g elm http-server

RUN elm make src/Main.elm --output=build/pamela.js

WORKDIR build

CMD [ "http-server" ]
