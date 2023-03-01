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
const bodyParser = require('body-parser');
const request = require('request')
const cors = require('cors')
const app = express()
const path = require('path')

app.use(cors());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.json());
app.use(express.static(path.join(__dirname,'public')))

const port = process.env.PORT
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
app.get('/', (req, res) => res.send('Welcome to Express!'))
app.post('/api', (req, res) => {
  make_API_call('http://app2:7000/books/all')
	.then(response => {
		res.json(response)
	})
	.catch(error => {
		res.send(error)
	})
})

app.listen(port, () => console.log(`App listening on port ${port}!`))

module.exports = app

