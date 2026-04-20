const { app, BrowserWindow, ipcMain } = require('electron/main')
const path = require('node:path')

const createWindow = () => {
  const win = new BrowserWindow({
    width: 1500,
    height: 800,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  win.loadFile('index.html')
}

app.whenReady().then(() => {
  ipcMain.handle('ping', () => 'pong');
  ipcMain.handle('run-command', async (event, command) => {
    let result = "";
    try {
      const { execSync } = require('child_process');
      result = execSync(command, { encoding: 'utf-8' });
    } catch (error) {
      result = `Error: ${error.message}`;
    }
    return result;
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