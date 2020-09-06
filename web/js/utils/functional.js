const partialRight = (fn, presetArgs) => (...laterArgs) =>
  fn(...laterArgs, ...presetArgs);

export { partialRight };
