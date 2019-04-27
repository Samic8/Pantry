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

app.get('/cupboard/*', (req, res) => res.sendFile(path.join(`${__dirname}/../index.html`)));

app.get('/cupboard', async (req, res) => {
    const urlSlug = getUrlSlugFromReferer(req.headers.referer);
    const doesCupboardExist = await prisma.$exists.cupboard({ urlSlug });
    const cupboard = doesCupboardExist ? await prisma.cupboard({ urlSlug }) : await prisma.createCupboard({title: '', urlSlug});
    const items = await prisma.cupboard({ id: cupboard.id }).items().$fragment(fragmentItemsWithRestocks);

    const viewItems = items.map(buildItemFromResponse);
    
    return res.json({
        title: cupboard.title,
        items: viewItems,
    });
});

app.post('/cupboard', async (req, res) => {
    const urlSlug = getUrlSlugFromReferer(req.headers.referer);
    const cupboard = await prisma.updateCupboard({ data: { title: req.body.title }, where: { urlSlug } });
    res.json({
        title: cupboard.title
    });
});

app.post('/cupboard/items', async (req, res) => {
    if (!req.body.length) return;
    
    const urlSlug = getUrlSlugFromReferer(req.headers.referer);
    const cupboardItems = await prisma.cupboard({ urlSlug }).items().$fragment(fragmentItemsWithRestocks);
    const itemUpdates = req.body.map(item => {
        const updatePromises = [];
        const [previousRestock] = cupboardItems.find(({id}) => id === item.id).restocks;
        const today = getToday();
        const data = {
            name: item.name,
            maxOnHand: item.maxOnHand,
            unit: item.unit,
            restocks: {}
        };

        if (!moment(previousRestock.date).isSame(today, 'day')) {
            data.restocks.create = [
                {
                    date: today,
                    newOnHand: item.onHand,
                    didRunOut: today,
                }
            ];
        } else {
            updatePromises.push(prisma.updateRestock({
                data: { newOnHand: item.onHand },
                where: { id: previousRestock.id }
            }));
        }
        
        updatePromises.push(prisma.updateItem({
            data,
            where: {
                id: item.id,
            }
        }));

        return updatePromises;
    });

    await Promise.all(_.flatten(itemUpdates));
    const items = await prisma.cupboard({ urlSlug }).items().$fragment(fragmentItemsWithRestocks);
    res.json(items.map(buildItemFromResponse));
});

app.post('/cupboard/new-item', async (req, res) => {
    const urlSlug = getUrlSlugFromReferer(req.headers.referer);
    const {name, onHand, maxOnHand, unit, userEstimateRunOut} = req.body;
    const [timeValue, timeUnit] = userEstimateRunOut.split(' ');
    const createItemResponse = await prisma.createItem({
        name,
        maxOnHand,
        unit,
        restocks: {
            create: [
                {
                    date: getToday(),
                    newOnHand: onHand,
                    userEstimateRunOut: getToday().add(timeValue, timeUnit).toDate(),
                }
            ]
        },
        cupboard: { connect: { urlSlug } }
    }).$fragment(fragmentItemsWithRestocks);
    res.json(buildItemFromResponse(createItemResponse));
});

function buildItemFromResponse(item) {
    return {
        id: item.id,
        name: item.name,
        unit: item.unit,
        maxOnHand: item.maxOnHand,
        estimateOnHand: buildEstimateOnHand(item),
        estimateDays: buildEstimateDays(item),
    }
}

function buildEstimateDays({maxOnHand, restocks}) {
    if (restocks.length === 1) {
        const { userEstimateRunOut, date } = restocks[0];
        const totalDays = getTotalDaysForEstimatedRunOut({ userEstimateRunOut, date });
        const daysRemaining = totalDays - getDaysSince({ date });
        return Math.max(0, Math.round(daysRemaining));
    }

    const daysElapsedSinceRestock = getToday().diff(restocks[restocks.length - 1].date, 'days');
    const {newOnHand} = restocks[restocks.length - 1];
    const averageDays = Math.round(buildMeanDays({maxOnHand, restocks}));
    const estimate = Math.round(newOnHand / (maxOnHand / averageDays) - daysElapsedSinceRestock);
    return Math.max(0, estimate);
}

function buildMeanDays({maxOnHand, restocks}) {
    const withoutInitial = restocks.slice(1);
    const averages = withoutInitial.map((restock, index) => {
        const previousRestock = restocks[index];
        const totalDays = moment(restock.didRunOut).diff(previousRestock.date, 'days');
        // Time to run out of maxOnHand based on this restock
        const compareToMaxOnHand = maxOnHand / previousRestock.newOnHand;
        return (Number.isFinite(compareToMaxOnHand) ? compareToMaxOnHand : 0) * Math.max(1, totalDays);
    });
    
    const averageDays = averages.reduce((prev, next) => prev + next, 0);

    return averageDays;
}

function buildEstimateOnHand({maxOnHand, restocks}) {
    if (restocks.length === 1) {
        return buildEstimateOnHandForInitialRestock(restocks[0]);
    }

    const averageDays = buildMeanDays({maxOnHand, restocks});
    const {newOnHand} = restocks[restocks.length - 1];
    const daysElapsedSinceRestock = getToday().diff(restocks[restocks.length - 1].date, 'days');
    return Math.max(0, Math.round(((averageDays - daysElapsedSinceRestock) / averageDays) * newOnHand));
}

function buildEstimateOnHandForInitialRestock({date, userEstimateRunOut, newOnHand}) {
    const totalDays = getTotalDaysForEstimatedRunOut({ userEstimateRunOut, date });
    const estimate = Math.round(newOnHand - (newOnHand * (getDaysSince({ date }) / totalDays)));
    return Math.max(0, estimate);
}

function getTotalDaysForEstimatedRunOut({userEstimateRunOut, date}) {
    return moment(userEstimateRunOut).diff(date, 'days');
}

function getDaysSince({ date }) {
    return getToday().diff(date, 'days');
}

function getUrlSlugFromReferer(referer) {
    return referer.match(/cupboard\/(.*)/)[1]
}

function getToday() {
    return moment();
}

const fragmentItemsWithRestocks = `
    fragment ItemsWithRestocks on Item {
        id
        name
        unit
        maxOnHand
        restocks {
            id
            date
            newOnHand
            userEstimateRunOut
            didRunOut
        }
    }
`;

app.listen(8000);
