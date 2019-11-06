const shell = require('shelljs');
shell.echo('build haskell application:');
shell.exec('stack install --local-bin-path build')