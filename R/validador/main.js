const electron = require('electron')
const app = electron.app
const BrowserWindow = electron.BrowserWindow

// rodar shiny
const abrirShiny = async () => {
    console.log('Comeca a rodar o shiny')
    var spawn = require('child_process').spawn
    var ls = await spawn('cmd.exe', ['/c', 'Rscript app.R'])
    
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

let win

app.on('ready', () => {
    console.log('Abrindo app ...')
    win = new BrowserWindow({
        //frame: false,
        height: 850,
        //resizable: false,
        width: 1420,
        icon: 'www/icone.ico'
    })

    console.log('Carregando pagina')
    win.loadURL(`file://${__dirname}/www/index.html`)
})

