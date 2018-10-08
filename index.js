import {Elm} from './src/Main.elm';

console.log(Elm);

Elm.Main.init({
  node: document.getElementById('app'),
  flags: {}
})