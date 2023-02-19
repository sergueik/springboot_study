const apm = require('elastic-apm-node').start({
  serviceName: '',
  secretToken: '',
  apiKey: '',
  // set custom APM Server URL (default: http://127.0.0.1:8200)
  serverUrl: '', // 'http://apm-server:8200',
})
const express = require('express')
const api_helper = require('./API_helper')
const app = express()
const port = 3000

app.get('/', (req, res) => res.send('Welcome to Make REST API Calls In Express!'))

app.get('/getAPIResponse', (req, res) => {
	api_helper.make_API_call('https://jsonplaceholder.typicode.com/todos/1')
	.then(response => {
		res.json(response)
	})
	.catch(error => {
		res.send(error)
	})
})

app.listen(port, () => console.log(`App listening on port ${port}!`))

module.exports = app
