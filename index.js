import { Elm } from './src/Main.elm';

Elm.Main.init({
    node: document.getElementById("elm-node"),
    flags: { apiUrl: process.env.ELM_APP_API_URL }, 
});