# Pantry 🍆
Pantry estimates when you need to restock your kitchen cupboard, it learns more about your usage the more you use it. Pantry is in a prototype state of development so don't expect much in the way of unit tests or documentation yet.

## Design
There early design prototypes [on a Figma prototype](https://www.figma.com/proto/PB3uVQbeSEQ14qQO4hrPionR/Pantry?node-id=103%3A0&viewport=407%2C321%2C0.249057&scaling=min-zoom).

![Preview Screenshot of Pantry](https://github.com/samic8/pantry/raw/master/assets/pantry-preview.png)

## Development
```
npm install
```
### Setup Data-store
Install [Docker](https://docs.docker.com/) and initialize Docker instance
```
cd data-store && docker-compose up -d
```
### Compile Client (Front-end)
```
npm run make:client:dev
```
This needs to be ran on changes to .elm files
### Start Server
```
npm run start:server
```

Then you should be ready to develop at http://localhost:8000/cupboard/[your-cupboard-name-here]. 
