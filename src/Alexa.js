'use strict';

// Alexa

var Alexa = require('alexa-sdk');

exports._init = function(event, context) {
  return Alexa.handler(event, context);
};

exports._registerHandler = function(alexa, label, fn) {
  var handler = {};
  handler[label] = function() {
    fn(this);
    this.emit(':responseReady');
  };
  alexa.registerHandlers(handler);
};

exports._speak = function(say, self) {
  self.response.speak(say);
};

exports._listen = function(listen, self) {
  self.response.listen(listen);
};

exports._execute = function(alexa) {
  alexa.execute();
};
