const express = require('express')
const request = require('request')
const app = express()
const fs = require('fs');

const port = process.env.PORT
app.get('/', (req, res) => res.send('Welcome to Express!'))
app.post('/files', (req, res) => {
  fs.readdir('/files/', (err, files) => {
    files.forEach(file => {
      console.log(file);
    });
    res.send(files);
  });
})

app.listen(port, () => console.log(`App listening on port ${port}!`))

module.exports = app

