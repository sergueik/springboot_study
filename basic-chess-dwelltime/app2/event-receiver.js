const express = require('express');
const cors = require('cors');
const app = express();
const port = 4000;

app.use(cors({ origin: 'http://localhost:3000' })); // Allow cross-origin from Node A
app.use(express.json());

let eventLog = [];

app.post('/report', (req, res) => {
  const event = req.body;
  console.log('Received event:', event);
  eventLog.push(event);
  res.json({ status: 'ok' });
});

app.get('/events', (req, res) => {
  res.json(eventLog);
});

app.listen(port, () => {
  console.log(`Node B (event receiver) listening at http://localhost:${port}`);
});

