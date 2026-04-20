const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  node_version: () => process.versions.node,
  chrome_version: () => process.versions.chrome,
  electron_version: () => process.versions.electron,
  ping: () => ipcRenderer.invoke('ping'),
  runCommand: (command) => ipcRenderer.invoke('run-command', command)
  // we can also expose variables, not just functions
})