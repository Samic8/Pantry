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
  endpoint: `http://pan-try.com:4466/data-store/dev`
});
exports.prisma = new exports.Prisma();
