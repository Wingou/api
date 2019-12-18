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

## Pour builder le projet afin de le déployer

> elm make src/Main.elm

# FireBase : mettre en ligne le projet

## Installer FireBase

### Pour installer les outils dans le projet

> \$> npm install -g firebase-tools

### Pour mettre à jour FireBase (Optionel)

> \$> npm i -g firebase-tools

### Pour se logguer

> \$> firebase login

- login : wingou.vp@gmail.com
- password : b^m^f0!

S'il y a le message d'erreur : `Impossible de charger le fichier...`

- Voir la rubrique : `Restriction sous powershell`

### Pour initialiser Firebase dans le projet (Une seule fois)

> \$> firebase init

> Are you ready to proceed? (Y/n)

- `Y`
  > Which Firebase CLI features do you want to set up for this folder?
- `Hosting`

> Use an existing projet

- `Create a new projet`

> Please specigy a unique project id

- `Donner un ID unique de longueur minimum 6 caractères au projet (ex: myApp000)`

> What would you like to call your project ?

- `Donner un nom unique sous lequel le projet va être déployé (ex: myApp)`

> What do you want to use as your public directory? (public)

- `build` (Ne pas prendre le dossier /public)

> Configure as a single-page app (rewrite all urls to /index.html)? (y/N)

- `N`

> File build/index.html already exists. Overwrite?

- `Y`

## Déployer un projet

Pour builder le projet

> \$> elm make src/Main.elm --output build/index.html

Pour déployer le projet

> \$> firebase deploy

# Restriction sous powershell (Optionel)

Pour retirer la restriction sous powershell

> \$> set-executionpolicy unrestricted

Pour remettre la restriction sous powershell

> \$> Set-ExecutionPolicy RemoteSigned

# Hot Reloading (Une seule fois)

Pour mettre en place le Hot Reloading

> \$> npm install --global elm elm-live

ou

> \$> npm install --save-dev elm elm-live

# API

https://jsonplaceholder.typicode.com/
