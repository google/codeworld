const onObjectPropertyChange = (object, property, callback) => {
  if (!object._propertyChangeHandlers) {
    object._propertyChangeHandlers = {};
  }

  if (!object._propertyChangeHandlers[property]) {
    object._propertyChangeHandlers[property] = [];

    let value = object[property];

    Object.defineProperty(object, property, {
      get() {
        return value;
      },
      set(newValue) {
        value = newValue;

        object._propertyChangeHandlers[property].forEach((handler) =>
          handler()
        );
      },
    });
  }

  object._propertyChangeHandlers[property].push(callback);
};

export { onObjectPropertyChange };
