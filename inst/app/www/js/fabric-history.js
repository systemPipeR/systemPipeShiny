
/**
 * Override the initialize function for the _historyInit();
 */
fabric.Canvas.prototype.initialize = (function(originalFn) {
  return function(...args) {
    originalFn.call(this, ...args);
    this._historyInit();
    return this;
  };
})(fabric.Canvas.prototype.initialize);

/**
 * Override the dispose function for the _historyDispose();
 */
fabric.Canvas.prototype.dispose = (function(originalFn) {
  return function(...args) {
    originalFn.call(this, ...args);
    this._historyDispose();
    return this;
  };
})(fabric.Canvas.prototype.dispose);

/**
 * Returns current state of the string of the canvas
 */
fabric.Canvas.prototype._historyNext = function () {
  let state = this.toDatalessJSON(this.extraProps);
  let eventOjbs = [];
  state.objects.map(function(i){
    if(i.evented == false){
      eventOjbs.push(false);
    } else{
      eventOjbs.push(true);
    }
  });
  state.objects = state.objects.filter((d, ind) => eventOjbs[ind]);
  return JSON.stringify(state);
}

/**
 * Returns an object with fabricjs event mappings
 */
fabric.Canvas.prototype._historyEvents = function() {
  return {
    'object:added': this._historySaveAction,
    'object:removed': this._historySaveAction,
    'object:modified': this._historySaveAction,
    'object:skewing': this._historySaveAction
  }
}

/**
 * Initialization of the plugin
 */
fabric.Canvas.prototype._historyInit = function () {
  this.history = [];
  this.historyMaxStep = 49;
  this.historyCurrentStep = 0;
  this.extraProps = ['selectable', 'id', 'evented'];
  this.historyNextState = this._historyNext();
  this.history[this.historyCurrentStep] = this.historyNextState;
  this.on(this._historyEvents());
}

/**
 * Remove the custom event listeners
 */
fabric.Canvas.prototype._historyDispose = function () {
  this.off(this._historyEvents())
}

/**
 * It pushes the state of the canvas into history stack
 */
fabric.Canvas.prototype._historySaveAction = function () {

  if (this.historyProcessing)
    return;
  if (this.historyCurrentStep >= this.historyMaxStep) {
    this.historyCurrentStep = this.historyMaxStep;
    this.history.shift();
  } else {
      this.historyCurrentStep ++;
  }

  this.historyNextState = this._historyNext();
  const json = this.historyNextState;
  this.history[this.historyCurrentStep] = json;
  this.history.splice(this.historyCurrentStep + 1, this.historyMaxStep);
  this.fire('history:append', { json: json, currentStep: this.historyCurrentStep});
}

/**
 * Max steps to save
 */
fabric.Canvas.prototype.setHistoryMax = function (historyMax) {
  this.historyMax = (Number.isInteger(historyMax) && historyMax > 0)? historyMax-1 : 49;
}

/**
 * Undo to latest history.
 */
fabric.Canvas.prototype.undo = function (callback) {
  // The undo process will render the new states of the objects
  // Therefore, object:added and object:modified events will triggered again
  // To ignore those events, we are setting a flag.
  this.historyProcessing = true;
  let history;
  if(this.historyCurrentStep > 0) {
    // min step is 0
    this.historyCurrentStep -= 1;
    this._loadHistory(this.history[this.historyCurrentStep], 'history:undo', callback);
    this.fire('history:undo', {currentStep: this.historyCurrentStep});
  } else {
    this.historyProcessing = false;
  }
}

/**
 * Redo to latest undo history.
 */
fabric.Canvas.prototype.redo = function (callback) {
  // The undo process will render the new states of the objects
  // Therefore, object:added and object:modified events will triggered again
  // To ignore those events, we are setting a flag.
  this.historyProcessing = true;
  if (this.historyCurrentStep < this.historyMaxStep+1 &&
      this.historyCurrentStep < this.history.length - 1) {
    this.historyCurrentStep += 1;
    this._loadHistory(this.history[this.historyCurrentStep], 'history:redo', callback);
    this.historyNextState = this._historyNext();
    this.fire('history:redo', {currentStep: this.historyCurrentStep});
  } else {
    this.historyProcessing = false;
  }
}

fabric.Canvas.prototype._loadHistory = function(history, event, callback) {
  var that = this;

  this.loadFromJSON(history, function() {
    that.renderAll();
    that.fire(event);
    that.historyProcessing = false;

    if (callback && typeof callback === 'function')
      callback();
  });
}

/**
 * Clear history stacks
 */
fabric.Canvas.prototype.clearHistory = function() {
  this.history = [];
  this.historyCurrentStep = 0;
  this.fire('history:clear');
}

/**
 * Off the history
 */
fabric.Canvas.prototype.offHistory = function() {
  this.historyProcessing = true;
}

/**
 * On the history
 */
fabric.Canvas.prototype.onHistory = function() {
  this.historyProcessing = false;
  this.historyNextState = this._historyNext();
  this.history[this.historyCurrentStep] = this.historyNextState;
}
