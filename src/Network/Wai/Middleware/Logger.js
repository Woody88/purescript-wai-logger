"use strict";

exports.hrtimeImpl = tuple => () => { 
  const t = process.hrtime();
  return tuple(t[0])(t[1])
}

exports.hrtimeImpl_ = time => tuple => () => { 
  const t = process.hrtime(time);
  return tuple(t[0])(t[1])
}