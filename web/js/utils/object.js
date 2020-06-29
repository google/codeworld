const onObjectPropertyChange = (object, property, callback) => {
  let value = object[property];

  Object.defineProperty(object, property, {
    get() {
      return value;
    },
    set(newValue) {
      value = newValue;
      callback();
    },
  });
};

export { onObjectPropertyChange };
