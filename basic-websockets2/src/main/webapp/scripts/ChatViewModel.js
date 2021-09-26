var MessageTypes = MessageTypes || {}

MessageTypes.JOIN = 1;
MessageTypes.MESSAGE = 2;
MessageTypes.GETUSERS = 3;
MessageTypes.USERLIST = 4;
MessageTypes.GETMESSAGES = 5;
MessageTypes.MESSAGELIST = 6;

function ChatViewModel(chatClient) {
	// messages
	function JoinMessage(name) {
		this.type = MessageTypes.JOIN;
		this.name = name;
	}

	function NewChatMessage(userName, message) {
		this.type = MessageTypes.MESSAGE;
		this.userName = userName;
		this.message = message;
		this.timeSent = new Date();
	}

	function DisplayChatMessage(userName, message, timeSent, isFirst, isDate) {
		var self = this;
		this.type = MessageTypes.MESSAGE;
		this.userName = userName;
		this.message = message;
		this.isFirst = isFirst;
		this.isDate = isDate;
		this.timeSent = timeSent;

		this.timeSentTimeDisplay = ko.computed(function() {
			return moment(self.timeSent).format('h:mm:ss a');
		});
		this.timeSentDateDisplay = ko.computed(function() {
			return moment(self.timeSent).format('DD/MM/YYYY');
		});
		this.timeSentFullDisplay = ko.computed(function() {
			return moment(self.timeSent).format('DD/MM/YYYY, h:mm:ss a');
		});
	}

	function GetSignedOnUsersMessage() {
		this.type = MessageTypes.GETUSERS;
	}

	function User(name, me) {
		this.name = name;
		this.me = me;
	}

	// 'private' members
	var self = this;
	var SESSION_NAME = "userName";

	var initialise = function(roomName) {
		chatClient.initialise(roomName, self.handler);

// todo is this code needed?
//        if (sessionStorage[SESSION_NAME] && sessionStorage[SESSION_NAME].length > 0) {
//            self.userName(sessionStorage[SESSION_NAME]);
//            self.joined(true);
//        }
	};

	var processMessage = function(msg) {
		console.log(msg);
		if (msg.type == MessageTypes.JOIN) {
			if (self.userName() == msg.name) {
				// do nothing as the list of users will be refreshed when we join 
			} else {
				self.users.push(new User(msg.name));
			}
		} else if (msg.type == MessageTypes.USERLIST) {
			// server has reset
			if (msg.users.length == 0) {
				sessionStorage.clear();
				self.userName('');
				self.joined(false);
			} else {
				$.each(msg.users, function() {
					if (self.userName() == this.name) {
						self.users.push(new User(this.name, true));
					} else {
						self.users.push(new User(this.name));
					}
				});
			}
		} else if (msg.type == MessageTypes.MESSAGELIST) {
			self.messages.removeAll();
			$.each(msg.messages, function() {
				addMessageToCollection(this);
			});
		} else if (msg.type == MessageTypes.MESSAGE) {
			addMessageToCollection(msg);
		}
	};

	var addMessageToCollection = function(msg) {
		var length = self.messages().length;
		if (length > 0) {
			var prev = self.messages()[length - 1];
			var isFirst = !(prev.userName == msg.userName);
			var isDate = !(new moment(prev.timeSent).startOf("day")
					.isSame(new moment(msg.timeSent).startOf("day")));
			self.messages.push(new DisplayChatMessage(msg.userName,
					msg.message, msg.timeSent, isFirst, isDate));
		} else {
			self.messages.push(new DisplayChatMessage(msg.userName,
					msg.message, msg.timeSent, true, true));
		}
	};

	var getSignedOnUsers = function() {
		console.log("getSignedOnUsers");
		self.chat.sendMessage(new GetSignedOnUsersMessage());
	};

	var processBinaryMessage = function(message) {
        message.type = "image/jpg";
        var urlCreator = window.URL || window.webkitURL;
        var imageUrl = urlCreator.createObjectURL(message);
        var img = $('#photo')[0];
        img.src = imageUrl;
	};
	
	// public members
	self.users = ko.observableArray();
	self.messages = ko.observableArray();

	self.chat = chatClient;

	self.roomName = ko.observable("");
	self.userName = ko.observable("");
	self.userImage = ko.observable("");
	self._userImage = ko.computed(function() {
		return self.userImage().replace("C:\\fakepath\\", "");
	});

	self.handler = {
		onopen : function() {
			chat.sendMessage(new JoinMessage(self.userName()));
			getSignedOnUsers();

// todo is this code needed
//			sessionStorage[SESSION_NAME] = self.userName();
		},
		onclose : function() {
			self.joined(false);
		},
		onmessage : function(message) {
			if(message instanceof Blob) {
			    processBinaryMessage(message);
			} else {
				processMessage(JSON.parse(message));
			}
		}
	};

	self.join = function() {
        if(!self.joined() &&
           self.userName() && self.userName().length > 0 &&
           self.roomName() && self.roomName().length > 0) {
            self.joined(true);
            initialise(self.roomName());
        }
	};

	self.uploadImage = function () {
	    var reader = new FileReader();
	    reader.onload = function() {
	        var rawData = reader.result;
	        chat.sendBinary(rawData);
	    };
	    reader.readAsArrayBuffer($('#imageName')[0].files[0]);
	};
	
	self.messages = ko.observableArray();
    self.joined = ko.observable(false);
    self.message = ko.observable('');
    
    self.enableSendMessage = ko.computed(function () {
        return self.joined() && self.message() && self.message().length > 0;
    });
    
    self.sendMessage = function () {
        var msg = new NewChatMessage(self.userName(), self.message());
        chat.sendMessage(msg);
    };
}
