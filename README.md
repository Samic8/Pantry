# Pantry
Estimates when you need to re-stock your kitchen pantry.

![Preview Screenshot of Pantry](https://github.com/samic8/pantry/raw/master/assets/pantry-preview.png)

## Development
This application is in early stages of development. Development will become easier as the install process is simplified (once I figure all the things out).
```
npm install
```
### Setup Data-store
Install [Docker](https://docs.docker.com/) and initalise Docker instance
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
