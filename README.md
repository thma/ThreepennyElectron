# ThreepennyElectron

electron integration:
https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/doc/electron.md

npm install

stack install --local-bin-path build

Now you can simply set relBin to ./build/deva-ui.

Now run your app with Electron: ./node_modules/.bin/electron electron.js


npm install electron-packager

./node_modules/.bin/electron-packager .

./node_modules/.bin/electron-packager . --ignore=app --ignore=src