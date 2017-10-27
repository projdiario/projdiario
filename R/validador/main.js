console.log('Inicio do script')
const electron = require('electron')
const app = electron.app
const BrowserWindow = electron.BrowserWindow

// rodar shiny
const abrirShiny = async () => {
    console.log('Começa a rodar o shiny')
    var spawn = require('child_process').spawn
    var ls = await spawn('cmd.exe', ['/c', 'set PATH=%PATH%;C:\Users\%username%\Documents\R\R-3.4.1\bin\i386 & Rscript app.R'])
    console.log('ls é:' + ls)
    
    ls.stdout.on('data', function (data) {
        console.log('stdout: ' + data)
    })

    ls.stderr.on('data', function (data) {
        console.log('stderr: ' + data)
    })

    ls.on('exit', function (code) {
        console.log('child process exited with code ' + code)
    })
}

abrirShiny()

let win;

app.on('ready', () => {
    console.log('Abrindo app...')
    win = new BrowserWindow({
        //frame: false,
        height: 850,
        //resizable: false,
        width: 1420,
        icon: 'www/icone.ico'
    })

    console.log('Carregando página')
    win.loadURL(`file://${__dirname}/www/index.html`)
})

