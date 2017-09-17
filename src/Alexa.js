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
  };
  alexa.registerHandlers(handler);
  return alexa;
};

exports._speak = function(say, self) {
  self.response.speak(say);
  return self;
};

exports._listen = function(listen, self) {
  self.response.listen(listen);
  return self;
};

exports._respond = function(self) {
  self.emit(':responseReady');
};

exports._execute = function(alexa) {
  alexa.execute();
};
