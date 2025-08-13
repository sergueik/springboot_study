// NOTE: node.js  prerequisite (heavy)
// 
const express = require('express');
const app = express();
const port = 3000;

app.get('/', (req, res) => {
  res.send(`
<!DOCTYPE html>
<html>
<head><title>Subject Page</title></head>
<body>
  <h1>Demo Subject Page</h1>
  <button id="btn1">Click me!</button>
  <input id="input1" placeholder="Type here" />
  
  <script>
    // Monitoring JS snippet injected into subject page
    (function() {
      const reportUrl = 'http://localhost:4000/report'; // Node B URL

      function sendEvent(data) {
        fetch(reportUrl, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(data),
          mode: 'cors'
        }).catch(e => console.error('Reporting failed', e));
      }

      // Capture click events
      document.addEventListener('click', e => {
        const eventData = {
          eventType: 'click',
          targetId: e.target.id || null,
          timestamp: Date.now()
        };
        sendEvent(eventData);
      });

      // Capture input changes
      document.addEventListener('input', e => {
        if (e.target.id) {
          const eventData = {
            eventType: 'input',
            targetId: e.target.id,
            value: e.target.value,
            timestamp: Date.now()
          };
          sendEvent(eventData);
        }
      });

      console.log('EUM monitoring injected');
    })();
  </script>
</body>
</html>
  `);
});

app.listen(port, () => {
  console.log(`Node A (subject page) listening at http://localhost:${port}`);
});


