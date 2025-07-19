// cannot use npm style relative when not using npm to compile transpile package
// import { createRoot } from 'react-dom/client'



import { createRoot } from "https://esm.sh/react-dom@18/client";
import React from "https://esm.sh/react";
/*
const root = createRoot(document.getElementById("root"));
const ReactElement =  function() {

  return <h1>Hello from JSX via esm.sh</h1>;

};

console.log(ReactElement());

root.render(<ReactElement />);
*/

function App() {
  return React.createElement("h1", null, "Hello without JSX");
}

const root = createRoot(document.getElementById("root"));
root.render(React.createElement(App));

