import { Elm } from './Main.elm'

Elm.Main.init({
    node: document.getElementById('main'),
    flags: {
        size: 300,
    }
})