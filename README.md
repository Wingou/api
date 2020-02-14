# myApp

Starter Kit for create a new project

## Installer ELM 0.19.1

- Downloader le package pour Windows (selon OS)

  - https://guide.elm-lang.org/install/elm.html

- Puis installer le package
  - installer-for-windows

## Générer projet

### Pour génèrer les éléments de base

- le dossier /src et le fichier "elm.json"
  > \$> elm init
- Ajouter dans /src le fichier Main.elm
  - Récupérer un exemple de contenu : https://elm-lang.org/examples/buttons
  - ou récupére le fichier Main.elm de ce projet

### Pour lancer le projet

> \$> elm reactor

- Le projet se lance sur le port 8000 : http://localhost:8000/
- http://localhost:8000/src/Main.elm

### Pour installer les lib classics (optionel)

- \$> elm install elm/http
- \$> elm install elm/json

# Git

## Pour mettre à jour le projet dans Github

> git add -A

> git commit -m 'Message descriptif'

> git push

# Hot Reloading (Une seule fois)

Pour mettre en place le Hot Reloading

> \$> npm install --global elm elm-live

ou

> \$> npm install --save-dev elm elm-live

# API pour Test

https://jsonplaceholder.typicode.com/


# Pour builder le projet afin de le déployer
## > elm make src/Main.elm
> elm make src/Main.elm --output=build/pamela.js 

# Kibana

> https://kibana-test.noc.vpgrp.io/

> Sourcing

> icône : Discover

> sourcing-o2kpi-*

> Add filter : app is Pamela

> Add filter : eventName is click

# Run with docker
## Build image
```
docker build . -t api-image
```
## Run container
```
docker run -it -p 8080:8080 api-image
```