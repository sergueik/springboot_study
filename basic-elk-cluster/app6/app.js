const env = require('node:process')
const apm = require('elastic-apm-node')
if (process.env.MONITOR) {
  var apm_server_url = 'http://apm-server:8200'
  apm.start({
    serviceName: '',
    secretToken: '',
    apiKey: '',
    serverUrl: apm_server_url,
  })
  console.log(`Apm server on ${apm_server_url}!`)
}
const express = require('express')
const api_helper = require('./API_helper')
const app = express()
const port = 3000

app.get('/', (req, res) => res.send('Welcome!'))

app.get('/getAPIResponse', (req, res) => {
  api_helper.make_API_call('http://app2:7000/books/all')
    .then(response => {
      res.json(response)
    })
    .catch(error => {
      res.send(error)
    })
})

app.listen(port, () => console.log(`App listening on port ${port}!`))

module.exports = app
