const { app, BrowserWindow, ipcMain } = require('electron/main')
const path = require('node:path')
const pty = require('node-pty');
const os = require('os');

const shell = os.platform() === 'win32' ? 'powershell.exe' : '/bin/bash';

let win;

const createWindow = () => {
  win = new BrowserWindow({
    width: 1500,
    height: 800,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  win.loadFile('index.html')
}

app.whenReady().then(() => {
  const ptyProcess = pty.spawn(shell, [], {
    name: 'xterm-color',
    cols: 80,
    rows: 30,
    cwd: process.env.HOME,
    env: process.env
  });

  ptyProcess.onData((data) => {
    win.webContents.send('terminal-response', data);
  });

  ipcMain.handle('ping', () => 'pong');
  ipcMain.handle('run-command', async (event, command) => {
    try {
      ptyProcess.write(command + '\r');
    }
    catch (error) {
      console.error('Error running command:', error);
      throw error; // Rethrow the error to be caught in the renderer process
    }
    return true; // Return a value to indicate the command was received
  });
  createWindow()

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      createWindow()
    }
  })
})

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {}
  app.quit()
})