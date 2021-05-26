const shell = require('shelljs');
const os = require('os');

const plat = os.platform();
const arch = os.arch();

shell.echo(`Running postpack app for ${plat}:${arch}`);
shell.exec(`ncp static ThreepennyElectron-${plat}-${arch}/static`);