import { Elm } from "./src/Main.elm";
import "./style.scss";

Elm.Main.init({
  node: document.getElementById("app"),
  flags: [
    { row: 0, col: 0, value: 8 },
    { row: 1, col: 1, value: 7 },
    { row: 2, col: 1, value: 6 },
    { row: 2, col: 2, value: 9 },

    { row: 0, col: 5, value: 6 },
    { row: 1, col: 3, value: 9 },
    { row: 1, col: 4, value: 2 },
    { row: 2, col: 5, value: 5 },

    { row: 0, col: 6, value: 5 },
    { row: 0, col: 8, value: 4 },
    { row: 2, col: 7, value: 3 },

    { row: 3, col: 1, value: 1 },
    { row: 4, col: 1, value: 9 },
    { row: 4, col: 2, value: 5 },
    { row: 5, col: 1, value: 4 },
    { row: 5, col: 2, value: 6 },

    { row: 3, col: 5, value: 3 },
    { row: 5, col: 3, value: 5 },

    { row: 3, col: 6, value: 9 },
    { row: 3, col: 7, value: 2 },
    { row: 4, col: 6, value: 4 },
    { row: 4, col: 7, value: 6 },
    { row: 5, col: 7, value: 7 },

    { row: 6, col: 1, value: 5 },
    { row: 8, col: 0, value: 1 },
    { row: 8, col: 2, value: 7 },

    { row: 6, col: 3, value: 8 },
    { row: 7, col: 4, value: 5 },
    { row: 7, col: 5, value: 7 },
    { row: 8, col: 3, value: 4 },

    { row: 6, col: 6, value: 3 },
    { row: 6, col: 7, value: 4 },
    { row: 7, col: 7, value: 1 },
    { row: 8, col: 8, value: 8 },
  ],
});
