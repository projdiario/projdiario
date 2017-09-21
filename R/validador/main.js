const electron = require('electron')
const app = electron.app
const BrowserWindow = electron.BrowserWindow

// rodar shiny
var spawn = require('child_process').spawn,
ls = spawn('cmd.exe', ['/c', 'set PATH=%PATH%;C:\Users\%username%\Documents\R\R-3.4.1\bin\i386 & Rscript app.R']);

ls.stdout.on('data', function (data) {
console.log('stdout: ' + data);
});

ls.stderr.on('data', function (data) {
console.log('stderr: ' + data);
});

ls.on('exit', function (code) {
console.log('child process exited with code ' + code);
});

let win;

app.on('ready', () => {
    win = new BrowserWindow({
        //frame: false,
        height: 850,
        //resizable: false,
        width: 1420,
        icon: 'www/icone.ico'
    })
    win.loadURL(`file://${__dirname}/www/index.html`)
})
