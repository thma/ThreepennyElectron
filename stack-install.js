const shell = require('shelljs');

shell.echo('build haskell application:');
shell.exec('stack install --local-bin-path build');

shell.echo('copy static/ to build/static:');
shell.exec('ncp static build/static');