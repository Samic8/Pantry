"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var prisma_lib_1 = require("prisma-client-lib");
var typeDefs = require("./prisma-schema").typeDefs;

var models = [
  {
    name: "Cupboard",
    embedded: false
  },
  {
    name: "Item",
    embedded: false
  },
  {
    name: "Restock",
    embedded: false
  }
];
exports.Prisma = prisma_lib_1.makePrismaClientClass({
  typeDefs,
  models,
  endpoint: `http://0.0.0.0:4466`
});
exports.prisma = new exports.Prisma();
