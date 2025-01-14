const { app, BrowserWindow, ipcMain } = require('electron');
const { exec } = require('child_process');
const path = require('path');
const http = require('http');

let mainWindow;
let shinyProcess;

// Dynamischer Pfad für Ressourcen
const isPackaged = app.isPackaged;
const resourcesPath = isPackaged
  ? process.resourcesPath // Kein zusätzliches 'resources' mehr nötig
  : path.join(__dirname, 'resources'); // Für Entwicklungsmodus

// Umgebungsvariablen für R setzen
process.env.R_HOME = path.join(resourcesPath, 'R'); // R-Installation
process.env.PATH = process.env.PATH + ';' + path.join(resourcesPath, 'R', 'bin'); // R-Binärdateien


// Funktion zum Erstellen des Hauptfensters
const createMainWindow = () => {
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'), // Sicherer Zugriff auf APIs
      contextIsolation: true, // Renderer-Prozess isolieren
      nodeIntegration: false, // Node.js im Renderer deaktivieren
    },
  });

  mainWindow.on('closed', () => {
    mainWindow = null;
    if (shinyProcess && !shinyProcess.killed) {
      shinyProcess.kill('SIGINT'); // Beende den Shiny-Prozess sauber
    }
  });
};

// Funktion zum Starten der Shiny-App
const startShinyApp = (port) => {
  const rBinary = path.join(resourcesPath, 'R', 'bin', 'x64', 'R.exe');
  const pathToShinyApp = path.join(resourcesPath, 'shiny', 'ppr_shiny_res.R').replace(/\\/g, '/');


  // Debugging: Pfade in der Konsole ausgeben
  console.log(`Pfad zu R.exe: ${rBinary}`);
  console.log(`Pfad zum Shiny-Skript: ${pathToShinyApp}`);

  shinyProcess = exec(
    `"${rBinary}" -e "shiny::runApp('${pathToShinyApp}', port = ${port}, host = '127.0.0.1')"`,
    (err, stdout, stderr) => {
      if (err) {
        console.error(`Fehler beim Starten der Shiny-App: ${stderr}`);
        if (mainWindow && mainWindow.webContents) {
          mainWindow.webContents.send('error', 'Die Shiny-App konnte nicht gestartet werden.');
        }
        return;
      }
      console.log(`Shiny-App gestartet: ${stdout}`);
    }
  );

  shinyProcess.on('exit', (code, signal) => {
    console.log(`Shiny-App-Prozess beendet mit Code: ${code}, Signal: ${signal}`);
    shinyProcess = null;
  });

  shinyProcess.on('error', (err) => {
    console.error(`Fehler im Shiny-App-Prozess: ${err.message}`);
  });
};

// Funktion zur Überprüfung, ob der Shiny-Server bereit ist
const waitForServer = (port, retries = 10, delay = 1000) => {
  if (retries === 0) {
    console.error('Shiny-App konnte nicht gestartet werden.');
    if (mainWindow) {
      mainWindow.loadFile('error.html'); // Zeigt eine Fehlermeldung an
    }
    app.quit();
    return;
  }

  http
    .get(`http://127.0.0.1:${port}`, (res) => {
      console.log('Shiny-App ist bereit.');
      if (!mainWindow) {
        createMainWindow();
      }
      mainWindow.loadURL(`http://127.0.0.1:${port}`);
    })
    .on('error', () => {
      console.log('Shiny-App noch nicht bereit. Warte...');
      setTimeout(() => waitForServer(port, retries - 1, delay), delay);
    });
};

// App bereitstellen
app.on('ready', () => {
  const net = require('net');
  const server = net.createServer();
  server.listen(0, () => {
    const port = server.address().port;
    server.close(() => {
      startShinyApp(port);
      waitForServer(port);
    });
  });
});

// Beenden der App
app.on('window-all-closed', () => {
  if (shinyProcess && !shinyProcess.killed) {
    shinyProcess.kill('SIGINT');
  }
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

// IPC-Kommunikation
ipcMain.on('start-shiny-app', () => {
  console.log('Start-Anfrage für Shiny-App empfangen.');
  if (!shinyProcess) {
    const net = require('net');
    const server = net.createServer();
    server.listen(0, () => {
      const port = server.address().port;
      server.close(() => {
        startShinyApp(port);
        waitForServer(port);
      });
    });
  }
});

ipcMain.on('stop-shiny-app', () => {
  console.log('Stop-Anfrage für Shiny-App empfangen.');
  if (shinyProcess && !shinyProcess.killed) {
    shinyProcess.kill('SIGINT');
    shinyProcess = null;
  }
});
