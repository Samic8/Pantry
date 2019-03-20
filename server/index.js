const express = require('express');
const app = express();
const path = require('path');
const { prisma } = require('../data-store/generated/prisma-client');
const bodyParser = require('body-parser')
const moment = require('moment');
const _ = require('lodash');

app.use( bodyParser.json() );
app.use( bodyParser.urlencoded({ extended: true })); 
app.use('/src', express.static(path.join(`${__dirname}/../src`)));
app.use('/dist', express.static(path.join(`${__dirname}/../dist`)));

app.get('/', (req, res) => res.sendFile(path.join(`${__dirname}/../index.html`)));

app.get('/pantry', async (req, res) => {
    const items = await prisma.items().$fragment(fragmentItemsWithRestocks);

    const viewItems = items.map(buildItemFromResponse);
    
    return res.json({
        title: `Sam's Kitchen Pantry`,
        items: viewItems,
    });
});

app.post('/pantry/items', async (req, res) => {
    const itemUpdates = req.body.map(async item => {
        return await prisma.updateItem({
            data: {
                name: item.name,
                maxOnHand: item.maxOnHand,
                unit: item.unit,
                restocks: {
                    create: [
                        {
                            date: new Date(),
                            newOnHand: item.onHand,
                            didRunOut: new Date(),
                            leftOverFromPrevious: 0,
                        }
                    ]
                }
            },
            where: {
                id: item.id,
            }
        });
    });

    await Promise.all(itemUpdates);
    const items = await prisma.items().$fragment(fragmentItemsWithRestocks);
    res.json(items.map(buildItemFromResponse));
});

function buildItemFromResponse(item) {
    return {
        id: item.id,
        name: item.name,
        unit: item.unit,
        maxOnHand: item.maxOnHand,
        estimateOnHand: bulldEstimateOnHand(item),
    }
}

function bulldEstimateOnHand({maxOnHand, restocks}) {
    if (restocks.length === 1) {
        return bulldEstimateOnHandForIntialRestock(restocks[0]);
    }

    const withoutInitial = restocks.slice(1);
    const averageDays = _.mean(withoutInitial.map((restock, index) => {
        const totalDays = moment(restock.didRunOut).diff(restock.date, 'days');
        const amount = restocks[index].newOnHand;
        // Time to run out of maxOnHand based on this rate
        return (maxOnHand / amount) * Math.max(1, totalDays);
    }));

    const daysElapsedSinceRestock = moment().diff(restocks[restocks.length - 1].date, 'days');
    return Math.max(0, Math.round(((averageDays - daysElapsedSinceRestock) / averageDays) * maxOnHand));
}

function bulldEstimateOnHandForIntialRestock({date, userEstimateRunOut, newOnHand}) {
    const totalDays = moment(userEstimateRunOut).diff(date, 'days');
    const daysElapsedSinceRestock = moment().diff(date, 'days');
    return Math.round(newOnHand - (newOnHand * (daysElapsedSinceRestock / totalDays)));
}

app.post('/pantry/new-item', async (req, res) => {
    const {name, onHand, maxOnHand, unit, userEstimateRunOut} = req.body;
    const [timeValue, timeUnit] = userEstimateRunOut.split(' ');
    const createItemResponse = await prisma.createItem({
        name,
        maxOnHand,
        unit,
        restocks: {
            create: [
                {
                    date: new Date(),
                    newOnHand: onHand,
                    userEstimateRunOut: moment().add(timeValue, timeUnit).toDate(),
                }
            ]
        }
    }).$fragment(fragmentItemsWithRestocks);

    res.json(buildItemFromResponse(createItemResponse))
})

const fragmentItemsWithRestocks = `
    fragment ItemsWithRestocks on Item {
        id
        name
        unit
        maxOnHand
        restocks {
            date
            newOnHand
            userEstimateRunOut
            didRunOut
            leftOverFromPrevious
        }
    }
`;

app.listen(8000);
