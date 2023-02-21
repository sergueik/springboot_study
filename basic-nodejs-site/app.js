const express = require('express')
const bodyParser = require('body-parser');
const api_helper = require('./API_helper')
const cors = require('cors')
const app = express()
const path = require('path')

// NOTE: does not work this way
// const env = require('process')
// const port = env.PORT
app.use(cors());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.json());
app.use(express.static(path.join(__dirname,'public')))

const port = process.env.PORT
app.get('/', (req, res) => res.send('Welcome to Express!'))
app.post('/api', (req, res) => {
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
