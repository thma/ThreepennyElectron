# ThreepennyElectron

The Calculator example is based on 
https://bitbucket.org/astynax/threep/src/default/

- fixed issue with entering fraction digits
- extended calculator functions
- added semantic.css stylesheet
- provided Electron integration which allows to generate standalone Desktop app from a Threepenny app

electron integration:
https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/doc/electron.md

npm install

stack install --local-bin-path build

Now you can simply set relBin to ./build/deva-ui.

Now run your app with Electron: ./node_modules/.bin/electron electron.js


npm install electron-packager

./node_modules/.bin/electron-packager .

./node_modules/.bin/electron-packager . --ignore=app --ignore=src


{
  "name": "electron-quick-start",
  "version": "1.0.0",
  "description": "A minimal Electron application",
  "main": "main.js",
  "scripts": {
    "start": "electron ."
  },
  "repository": "https://github.com/electron/electron-quick-start",
  "keywords": [
    "Electron",
    "quick",
    "start",
    "tutorial",
    "demo"
  ],
  "author": "GitHub",
  "license": "CC0-1.0",
  "devDependencies": {
    "electron": "~1.6.2",
    "gulp": "^3.9.1",
    "gulp-sass": "^3.1.0"
  },
  "dependencies": {
    "electron": "^1.6.2",
    "favicon-getter": "^1.1.3",
    "jsonfile": "^2.4.0",
    "uuid": "^3.0.1"
  }
}