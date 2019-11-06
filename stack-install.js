import { echo, exec } from 'shelljs';
echo('build haskell application:');
exec('stack install --local-bin-path build')