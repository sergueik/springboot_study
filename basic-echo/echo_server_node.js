// based on: match(/Java(Script)/)https://gist.github.com/iainreid820/b42a94c08455a0a251ae5574f37a6df3
// see also: https://debugmode.net/2014/01/14/create-echo-server-in-node-js/
// (web server)
//
const port = process.env.PORT || 10000;
const debug = process.env.DEBUG || false;
const net = require('net')

net.createServer(socket => {
  socket.on('data', function(data){
    var text = data.toString();
    if (debug) {	
      console.log('Received: %s', text)
    }
    socket.write(text);
    if data.match(/QUIT/){
      process.exit(0);
    }
  })
}).listen(port)
