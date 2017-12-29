'use strict';

var React = require('react');
var DnD = require('react-beautiful-dnd');

var Droppable = function Droppable(props) {
  return function (view) {
    var callback = function callback(provided, snapshot) {
      var children = [view, provided.placeholder];

      return React.createElement("div", { ref: provided.innerRef }, children);
    };

    return React.createElement(DnD.Droppable, props, callback);
  };
};

var Draggable = function Draggable(props) {
  return function (view) {
    var callback = function callback(provided, snapshot) {
      var otherProps = Object.assign({
        ref: provided.innerRef,
        style: provided.draggableStyle

      }, provided.dragHandleProps);

      var children = [React.createElement("div", otherProps, view), provided.placeholder];

      return React.createElement("div", null, children);
    };

    return React.createElement(DnD.Draggable, props, callback);
  };
};

var Context = function Context(fn) {
  return function (view) {
    var onDragEnd = function onDragEnd(result) {
      if (!result.destination) {
        return;
      }

      fn(result.source.index)(result.destination.index); //()
    };

    return React.createElement(DnD.DragDropContext, { onDragEnd: onDragEnd }, view);
  };
};

exports.dragDropContext = Context;
exports.droppable = Droppable;
exports.draggable = Draggable;