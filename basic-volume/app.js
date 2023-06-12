const express = require('express')
const request = require('request')
const app = express()
const path = require('path')
const fs = require('fs');
app.use(express.static(path.join(__dirname,'public')))

const port = process.env.PORT
app.get('/', (req, res) => res.send('Welcome to Express!'))
app.post('/app', (req, res) => {
  fs.readdir('/app/', (err, files) => {
    files.forEach(file => {
      console.log(file);
    });
    res.send(files);
  });
})

app.listen(port, () => console.log(`App listening on port ${port}!`))

module.exports = app

