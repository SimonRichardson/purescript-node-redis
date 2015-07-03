/* global exports require process */
'use strict';

// module Database.Redis.Redis

var Redis = require('ioredis'),
    Command = require('ioredis/lib/command');

exports._connect = function(uri, canceler, errback, callback) {
  var client = new Redis({lazyConnect: true});
  client.options.enableReadyCheck = true;
  client.parseOptions(uri);
  client.connect();
  client.once('ready', function(err, _) {
    (err ? errback(err) : callback(client))();
  });
  return canceler(client);
};

exports._handleParseFailure = function(err, canceler, errback) {
  process.nextTick(function() {
    errback(err)();
  });
  var client = new Redis({lazyConnect: true});
  return canceler(client);
};

exports._close = function(client, canceler, errback, callback) {
  process.nextTick(function() {
    client.end();
    callback({})();
  });
  return canceler({});
};

exports._query = function(query, client, canceler, errback, callback) {
  var options = {
        replyEncoding: 'utf8'
      },
      pipeline = client.pipeline(),
      cmd, i,
      snd = function(x) {
        return Array.isArray(x[1]) ? x[1] : [x[1]];
      },
      concatMap = function(f, x) {
        var res = [], i;
        for (i = 0; i < x.length; i++) {
          res.push(f(x[i]));
        }
        return res;
      };
  
  for(i = 0; i < query.length; i++) {
    cmd = query[i];
    pipeline.sendCommand(new Command(cmd.value0, cmd.value1, options));
  }
  
  pipeline.exec(function(err, x) {
    (err ? errback(err) : callback(concatMap(snd, x)))();
  });
  return canceler(client);
};

exports._ignoreCancel = function(any, cancelError, errback, callback) {
  return function() {
    callback(false);
  };
};
