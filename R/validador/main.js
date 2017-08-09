const {app, BrowserWindow} = requite('electron')
let win

app.on('ready', () => {
    win = new BrowserWindow()
    win.loadURL(`file://${__dirname}/index.html`)
})

