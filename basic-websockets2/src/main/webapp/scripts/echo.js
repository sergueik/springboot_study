"use strict";

var Echo = Echo || {};

Echo.sendMessage = function() {
	var echo = $('#echo');
	var message = echo.val();
	if (message != '') {
		Echo.socket.send(message);
		echo.val('');
	}
};

Echo.connect = function(host) {
	if ('WebSocket' in window) {
		Echo.socket = new WebSocket(host);
	} else if ('MozWebSocket' in window) {
		Echo.socket = new MozWebSocket(host);
	} else {
		console.log('Error: Websocket is not supported by this browser.');
		return;
	}

	Echo.socket.onopen = function() {
		console.log('Info: connection opened');
		$('#echo').keydown(function(evt) {
			if (evt.keyCode == 13) {
				Echo.sendMessage();
			}
		});
	};

	Echo.socket.onclose = function() {
		console.log('Info: connection closed');
	};

	Echo.socket.onmessage = function(message) {
		console.log('message: ' + message.data);
		var echoBack = $('#echoBack');
		echoBack.text(message.data);
	};
};

Echo.initialize = function() {
	var ep = '/websockets-chat-app/echo';
	
	if(window.location.protocol == 'http:') {
		Echo.connect('ws://' + window.location.host + ep);
	} else {
		Echo.connect('wss://' + window.location.host + ep);
	}
};

Echo.initialize();
