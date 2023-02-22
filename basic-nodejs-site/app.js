const express = require('express')
const api = require('./API_helper')
const cors = require('cors')
const app = express()
const path = require('path')

// NOTE: does not work this way
// const env = require('process')
// const port = env.PORT
app.use(cors());
// NOTE: bodyParser is natively supported by express 4.16.x+

app.use(express.urlencoded({
  extended: false
}));
app.use(express.json());
app.use(express.static(path.join(__dirname, 'public')))
// TODO: placing in the same file leads to empty response
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
  api.make_API_call('https://jsonplaceholder.typicode.com/todos/1')
    .then(response => {
      res.json(response)
    })
    .catch(error => {
      res.send(error)
    })
})

app.listen(port, () => console.log(`App listening on port ${port}!`))

module.exports = app