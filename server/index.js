const express = require('express');
const app = express();
const path = require('path');

app.get('/', (req, res) => res.sendFile(path.join(`${__dirname}/../index.html`)));
app.use('/src', express.static(path.join(`${__dirname}/../src`)));
app.use('/dist', express.static(path.join(`${__dirname}/../dist`)));

app.get('/pantry', (req, res) => 
    res.json({
        title: `Sam's Kitchen Pantry`,
        items: [
            { id: 0, name: "Chickpeas", estimateOnHand: 400, maxOnHand: 500, unit: "g" },
            { id: 1, name: "Red Lentils", estimateOnHand: 200, maxOnHand: 700, unit: "g" },
            { id: 2, name: "Cinnamon", estimateOnHand: 10, maxOnHand: 100, unit: "g" },
            { id: 3, name: "Chocolate", estimateOnHand: 40, maxOnHand: 150, unit: "g" },
        ]
    })
);

app.listen(8000);