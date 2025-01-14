const { contextBridge, ipcRenderer } = require('electron');

// Sicherstellen, dass nur definierte APIs verfügbar sind
contextBridge.exposeInMainWorld('electronAPI', {
  startShinyApp: () => ipcRenderer.send('start-shiny-app'),
  stopShinyApp: () => ipcRenderer.send('stop-shiny-app'),
  onAppStatus: (callback) => ipcRenderer.on('app-status', callback),
});
