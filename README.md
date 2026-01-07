# maxsnew

To build
```sh
cabal update
cabal build
cabal run maxsnew-exe build
```

To deploy to gh pages
```sh
./deploy.sh
```


To build from scratch
```sh
npm install -g bower pulp purescript
```

To build new purescript code
```sh
cd src/purescript
bower install
pulp build --to ../../content/js/hubway.js
```

