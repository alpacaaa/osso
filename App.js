'use strict';

var React = require('react')
var ReactDOM = require('react-dom')

const Counter = require('./src/examples/Counter.purs')
const Benchmark = require('./src/examples/Benchmark.purs')
const Draggable = require('./src/examples/Draggable.purs')

const Html = require('./src/Html.purs')

if (module.hot) {
  module.hot.accept();
}

const App = Html.createComponent(
  Counter.program()
  // Benchmark.program()
  // Draggable.program()
)

ReactDOM.render(
  React.createElement(App),
  document.getElementById('app')
);
