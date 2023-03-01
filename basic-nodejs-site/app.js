const express = require('express')
const cors = require('cors')
const request = require('request')
const app = express()
const path = require('path')

app.use(cors());

app.use(express.urlencoded({
  extended: false
}));
app.use(express.json());
app.use(express.static(path.join(__dirname, 'public')))

make_API_call = function(url) {
  return new Promise((resolve, reject) => {
    request(url, {
      json: true
    }, (err, res, body) => {
      if (err) reject(err)
      resolve(body)
    });
  })
}
const port = process.env.PORT
app.get('/', (req, res) => res.send('Welcome to Express!'))
app.post('/api', (req, res) => {
  make_API_call('https://jsonplaceholder.typicode.com/todos/1')
    .then(response => {
      res.json(response)
    })
    .catch(error => {
      res.send(error)
    })
})

app.listen(port, () => console.log(`App listening on port ${port}!`))

module.exports = app
