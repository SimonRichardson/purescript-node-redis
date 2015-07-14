'use strict';

// module Examples.Database.Redis.Get

exports.logAny = function(a){
  return function () {
    console.log(a);
    return {};
  };
}