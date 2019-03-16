const express = require('express');
const app = express();
const path = require('path');
const { prisma } = require('../data-store/generated/prisma-client');
const bodyParser = require('body-parser')

app.use( bodyParser.json() );
app.use( bodyParser.urlencoded({ extended: true })); 
app.use('/src', express.static(path.join(`${__dirname}/../src`)));
app.use('/dist', express.static(path.join(`${__dirname}/../dist`)));

app.get('/', (req, res) => res.sendFile(path.join(`${__dirname}/../index.html`)));

app.get('/pantry', async (req, res) => {
    const items = await prisma.items();
    return res.json({
        title: `Sam's Kitchen Pantry`,
        items: items,
    });
});

app.post('/pantry/new-item', async (req, res) => {
    const {name, onHand, maxOnHand, unit} = req.body;
    res.json(await prisma.createItem({name, estimateOnHand: onHand, maxOnHand, unit}))
})

app.listen(8000);