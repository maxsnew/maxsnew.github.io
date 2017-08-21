# maxsnew

To build new purescript code
```sh
cd src/purescript
bower install
pulp build --to ../../content/js/hubway.js
```

To build
```sh
stack setup
stack build
stack exec maxsnew-exe build
```

To deploy
```sh
./deploy.sh
```
