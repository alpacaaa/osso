// Can't be imported as is, needs to be transpiled somehow.
// Figure out how to webpack things before feeding them into psc.

const React = require('react')
const DnD = require('react-beautiful-dnd');


const Droppable = props => view => {
  const callback = (provided, snapshot) => {
    const children = [
      view
    , provided.placeholder
    ]

    return React.createElement("div", { ref: provided.innerRef }, children)
  }

  return React.createElement(DnD.Droppable, props, callback)
}


const Draggable = props => view => {
  const callback = (provided, snapshot) => {
    const otherProps = Object.assign({
      ref: provided.innerRef,
      style: provided.draggableStyle,

    }, provided.dragHandleProps)

    const children = [
      React.createElement("div", otherProps, view)
    , provided.placeholder
    ]

    return React.createElement("div", null, children)
  }

  return React.createElement(DnD.Draggable, props, callback)
}


const Context = fn => view => {
  const onDragEnd = result => {
    if (!result.destination) {
      return
    }

    fn(result.source.index)(result.destination.index)//()
  }

  return React.createElement(DnD.DragDropContext, { onDragEnd: onDragEnd }, view)
}

exports.dragDropContext = Context
exports.droppable = Droppable
exports.draggable = Draggable
