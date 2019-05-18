module.exports = {
        typeDefs: // Code generated by Prisma (prisma@1.33.0). DO NOT EDIT.
  // Please don't change this file manually but run `prisma generate` to update it.
  // For more information, please read the docs: https://www.prisma.io/docs/prisma-client/

/* GraphQL */ `type AggregateCupboard {
  count: Int!
}

type AggregateItem {
  count: Int!
}

type AggregateRestock {
  count: Int!
}

type BatchPayload {
  count: Long!
}

type Cupboard {
  id: ID!
  title: String!
  urlSlug: String!
  items(where: ItemWhereInput, orderBy: ItemOrderByInput, skip: Int, after: String, before: String, first: Int, last: Int): [Item!]
}

type CupboardConnection {
  pageInfo: PageInfo!
  edges: [CupboardEdge]!
  aggregate: AggregateCupboard!
}

input CupboardCreateInput {
  title: String!
  urlSlug: String!
  items: ItemCreateManyWithoutCupboardInput
}

input CupboardCreateOneWithoutItemsInput {
  create: CupboardCreateWithoutItemsInput
  connect: CupboardWhereUniqueInput
}

input CupboardCreateWithoutItemsInput {
  title: String!
  urlSlug: String!
}

type CupboardEdge {
  node: Cupboard!
  cursor: String!
}

enum CupboardOrderByInput {
  id_ASC
  id_DESC
  title_ASC
  title_DESC
  urlSlug_ASC
  urlSlug_DESC
}

type CupboardPreviousValues {
  id: ID!
  title: String!
  urlSlug: String!
}

type CupboardSubscriptionPayload {
  mutation: MutationType!
  node: Cupboard
  updatedFields: [String!]
  previousValues: CupboardPreviousValues
}

input CupboardSubscriptionWhereInput {
  mutation_in: [MutationType!]
  updatedFields_contains: String
  updatedFields_contains_every: [String!]
  updatedFields_contains_some: [String!]
  node: CupboardWhereInput
  AND: [CupboardSubscriptionWhereInput!]
  OR: [CupboardSubscriptionWhereInput!]
  NOT: [CupboardSubscriptionWhereInput!]
}

input CupboardUpdateInput {
  title: String
  urlSlug: String
  items: ItemUpdateManyWithoutCupboardInput
}

input CupboardUpdateManyMutationInput {
  title: String
  urlSlug: String
}

input CupboardUpdateOneRequiredWithoutItemsInput {
  create: CupboardCreateWithoutItemsInput
  update: CupboardUpdateWithoutItemsDataInput
  upsert: CupboardUpsertWithoutItemsInput
  connect: CupboardWhereUniqueInput
}

input CupboardUpdateWithoutItemsDataInput {
  title: String
  urlSlug: String
}

input CupboardUpsertWithoutItemsInput {
  update: CupboardUpdateWithoutItemsDataInput!
  create: CupboardCreateWithoutItemsInput!
}

input CupboardWhereInput {
  id: ID
  id_not: ID
  id_in: [ID!]
  id_not_in: [ID!]
  id_lt: ID
  id_lte: ID
  id_gt: ID
  id_gte: ID
  id_contains: ID
  id_not_contains: ID
  id_starts_with: ID
  id_not_starts_with: ID
  id_ends_with: ID
  id_not_ends_with: ID
  title: String
  title_not: String
  title_in: [String!]
  title_not_in: [String!]
  title_lt: String
  title_lte: String
  title_gt: String
  title_gte: String
  title_contains: String
  title_not_contains: String
  title_starts_with: String
  title_not_starts_with: String
  title_ends_with: String
  title_not_ends_with: String
  urlSlug: String
  urlSlug_not: String
  urlSlug_in: [String!]
  urlSlug_not_in: [String!]
  urlSlug_lt: String
  urlSlug_lte: String
  urlSlug_gt: String
  urlSlug_gte: String
  urlSlug_contains: String
  urlSlug_not_contains: String
  urlSlug_starts_with: String
  urlSlug_not_starts_with: String
  urlSlug_ends_with: String
  urlSlug_not_ends_with: String
  items_every: ItemWhereInput
  items_some: ItemWhereInput
  items_none: ItemWhereInput
  AND: [CupboardWhereInput!]
  OR: [CupboardWhereInput!]
  NOT: [CupboardWhereInput!]
}

input CupboardWhereUniqueInput {
  id: ID
  urlSlug: String
}

scalar DateTime

type Item {
  id: ID!
  cupboard: Cupboard!
  name: String!
  maxOnHand: Int!
  unit: String!
  restocks(where: RestockWhereInput, orderBy: RestockOrderByInput, skip: Int, after: String, before: String, first: Int, last: Int): [Restock!]
}

type ItemConnection {
  pageInfo: PageInfo!
  edges: [ItemEdge]!
  aggregate: AggregateItem!
}

input ItemCreateInput {
  cupboard: CupboardCreateOneWithoutItemsInput!
  name: String!
  maxOnHand: Int!
  unit: String!
  restocks: RestockCreateManyInput
}

input ItemCreateManyWithoutCupboardInput {
  create: [ItemCreateWithoutCupboardInput!]
  connect: [ItemWhereUniqueInput!]
}

input ItemCreateWithoutCupboardInput {
  name: String!
  maxOnHand: Int!
  unit: String!
  restocks: RestockCreateManyInput
}

type ItemEdge {
  node: Item!
  cursor: String!
}

enum ItemOrderByInput {
  id_ASC
  id_DESC
  name_ASC
  name_DESC
  maxOnHand_ASC
  maxOnHand_DESC
  unit_ASC
  unit_DESC
}

type ItemPreviousValues {
  id: ID!
  name: String!
  maxOnHand: Int!
  unit: String!
}

input ItemScalarWhereInput {
  id: ID
  id_not: ID
  id_in: [ID!]
  id_not_in: [ID!]
  id_lt: ID
  id_lte: ID
  id_gt: ID
  id_gte: ID
  id_contains: ID
  id_not_contains: ID
  id_starts_with: ID
  id_not_starts_with: ID
  id_ends_with: ID
  id_not_ends_with: ID
  name: String
  name_not: String
  name_in: [String!]
  name_not_in: [String!]
  name_lt: String
  name_lte: String
  name_gt: String
  name_gte: String
  name_contains: String
  name_not_contains: String
  name_starts_with: String
  name_not_starts_with: String
  name_ends_with: String
  name_not_ends_with: String
  maxOnHand: Int
  maxOnHand_not: Int
  maxOnHand_in: [Int!]
  maxOnHand_not_in: [Int!]
  maxOnHand_lt: Int
  maxOnHand_lte: Int
  maxOnHand_gt: Int
  maxOnHand_gte: Int
  unit: String
  unit_not: String
  unit_in: [String!]
  unit_not_in: [String!]
  unit_lt: String
  unit_lte: String
  unit_gt: String
  unit_gte: String
  unit_contains: String
  unit_not_contains: String
  unit_starts_with: String
  unit_not_starts_with: String
  unit_ends_with: String
  unit_not_ends_with: String
  AND: [ItemScalarWhereInput!]
  OR: [ItemScalarWhereInput!]
  NOT: [ItemScalarWhereInput!]
}

type ItemSubscriptionPayload {
  mutation: MutationType!
  node: Item
  updatedFields: [String!]
  previousValues: ItemPreviousValues
}

input ItemSubscriptionWhereInput {
  mutation_in: [MutationType!]
  updatedFields_contains: String
  updatedFields_contains_every: [String!]
  updatedFields_contains_some: [String!]
  node: ItemWhereInput
  AND: [ItemSubscriptionWhereInput!]
  OR: [ItemSubscriptionWhereInput!]
  NOT: [ItemSubscriptionWhereInput!]
}

input ItemUpdateInput {
  cupboard: CupboardUpdateOneRequiredWithoutItemsInput
  name: String
  maxOnHand: Int
  unit: String
  restocks: RestockUpdateManyInput
}

input ItemUpdateManyDataInput {
  name: String
  maxOnHand: Int
  unit: String
}

input ItemUpdateManyMutationInput {
  name: String
  maxOnHand: Int
  unit: String
}

input ItemUpdateManyWithoutCupboardInput {
  create: [ItemCreateWithoutCupboardInput!]
  delete: [ItemWhereUniqueInput!]
  connect: [ItemWhereUniqueInput!]
  set: [ItemWhereUniqueInput!]
  disconnect: [ItemWhereUniqueInput!]
  update: [ItemUpdateWithWhereUniqueWithoutCupboardInput!]
  upsert: [ItemUpsertWithWhereUniqueWithoutCupboardInput!]
  deleteMany: [ItemScalarWhereInput!]
  updateMany: [ItemUpdateManyWithWhereNestedInput!]
}

input ItemUpdateManyWithWhereNestedInput {
  where: ItemScalarWhereInput!
  data: ItemUpdateManyDataInput!
}

input ItemUpdateWithoutCupboardDataInput {
  name: String
  maxOnHand: Int
  unit: String
  restocks: RestockUpdateManyInput
}

input ItemUpdateWithWhereUniqueWithoutCupboardInput {
  where: ItemWhereUniqueInput!
  data: ItemUpdateWithoutCupboardDataInput!
}

input ItemUpsertWithWhereUniqueWithoutCupboardInput {
  where: ItemWhereUniqueInput!
  update: ItemUpdateWithoutCupboardDataInput!
  create: ItemCreateWithoutCupboardInput!
}

input ItemWhereInput {
  id: ID
  id_not: ID
  id_in: [ID!]
  id_not_in: [ID!]
  id_lt: ID
  id_lte: ID
  id_gt: ID
  id_gte: ID
  id_contains: ID
  id_not_contains: ID
  id_starts_with: ID
  id_not_starts_with: ID
  id_ends_with: ID
  id_not_ends_with: ID
  cupboard: CupboardWhereInput
  name: String
  name_not: String
  name_in: [String!]
  name_not_in: [String!]
  name_lt: String
  name_lte: String
  name_gt: String
  name_gte: String
  name_contains: String
  name_not_contains: String
  name_starts_with: String
  name_not_starts_with: String
  name_ends_with: String
  name_not_ends_with: String
  maxOnHand: Int
  maxOnHand_not: Int
  maxOnHand_in: [Int!]
  maxOnHand_not_in: [Int!]
  maxOnHand_lt: Int
  maxOnHand_lte: Int
  maxOnHand_gt: Int
  maxOnHand_gte: Int
  unit: String
  unit_not: String
  unit_in: [String!]
  unit_not_in: [String!]
  unit_lt: String
  unit_lte: String
  unit_gt: String
  unit_gte: String
  unit_contains: String
  unit_not_contains: String
  unit_starts_with: String
  unit_not_starts_with: String
  unit_ends_with: String
  unit_not_ends_with: String
  restocks_every: RestockWhereInput
  restocks_some: RestockWhereInput
  restocks_none: RestockWhereInput
  AND: [ItemWhereInput!]
  OR: [ItemWhereInput!]
  NOT: [ItemWhereInput!]
}

input ItemWhereUniqueInput {
  id: ID
}

scalar Long

type Mutation {
  createCupboard(data: CupboardCreateInput!): Cupboard!
  updateCupboard(data: CupboardUpdateInput!, where: CupboardWhereUniqueInput!): Cupboard
  updateManyCupboards(data: CupboardUpdateManyMutationInput!, where: CupboardWhereInput): BatchPayload!
  upsertCupboard(where: CupboardWhereUniqueInput!, create: CupboardCreateInput!, update: CupboardUpdateInput!): Cupboard!
  deleteCupboard(where: CupboardWhereUniqueInput!): Cupboard
  deleteManyCupboards(where: CupboardWhereInput): BatchPayload!
  createItem(data: ItemCreateInput!): Item!
  updateItem(data: ItemUpdateInput!, where: ItemWhereUniqueInput!): Item
  updateManyItems(data: ItemUpdateManyMutationInput!, where: ItemWhereInput): BatchPayload!
  upsertItem(where: ItemWhereUniqueInput!, create: ItemCreateInput!, update: ItemUpdateInput!): Item!
  deleteItem(where: ItemWhereUniqueInput!): Item
  deleteManyItems(where: ItemWhereInput): BatchPayload!
  createRestock(data: RestockCreateInput!): Restock!
  updateRestock(data: RestockUpdateInput!, where: RestockWhereUniqueInput!): Restock
  updateManyRestocks(data: RestockUpdateManyMutationInput!, where: RestockWhereInput): BatchPayload!
  upsertRestock(where: RestockWhereUniqueInput!, create: RestockCreateInput!, update: RestockUpdateInput!): Restock!
  deleteRestock(where: RestockWhereUniqueInput!): Restock
  deleteManyRestocks(where: RestockWhereInput): BatchPayload!
}

enum MutationType {
  CREATED
  UPDATED
  DELETED
}

interface Node {
  id: ID!
}

type PageInfo {
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
  startCursor: String
  endCursor: String
}

type Query {
  cupboard(where: CupboardWhereUniqueInput!): Cupboard
  cupboards(where: CupboardWhereInput, orderBy: CupboardOrderByInput, skip: Int, after: String, before: String, first: Int, last: Int): [Cupboard]!
  cupboardsConnection(where: CupboardWhereInput, orderBy: CupboardOrderByInput, skip: Int, after: String, before: String, first: Int, last: Int): CupboardConnection!
  item(where: ItemWhereUniqueInput!): Item
  items(where: ItemWhereInput, orderBy: ItemOrderByInput, skip: Int, after: String, before: String, first: Int, last: Int): [Item]!
  itemsConnection(where: ItemWhereInput, orderBy: ItemOrderByInput, skip: Int, after: String, before: String, first: Int, last: Int): ItemConnection!
  restock(where: RestockWhereUniqueInput!): Restock
  restocks(where: RestockWhereInput, orderBy: RestockOrderByInput, skip: Int, after: String, before: String, first: Int, last: Int): [Restock]!
  restocksConnection(where: RestockWhereInput, orderBy: RestockOrderByInput, skip: Int, after: String, before: String, first: Int, last: Int): RestockConnection!
  node(id: ID!): Node
}

type Restock {
  id: ID!
  date: DateTime!
  newOnHand: Int!
  previousRestock: Restock
  userEstimateRunOut: DateTime
  didRunOut: DateTime
}

type RestockConnection {
  pageInfo: PageInfo!
  edges: [RestockEdge]!
  aggregate: AggregateRestock!
}

input RestockCreateInput {
  date: DateTime!
  newOnHand: Int!
  previousRestock: RestockCreateOneInput
  userEstimateRunOut: DateTime
  didRunOut: DateTime
}

input RestockCreateManyInput {
  create: [RestockCreateInput!]
  connect: [RestockWhereUniqueInput!]
}

input RestockCreateOneInput {
  create: RestockCreateInput
  connect: RestockWhereUniqueInput
}

type RestockEdge {
  node: Restock!
  cursor: String!
}

enum RestockOrderByInput {
  id_ASC
  id_DESC
  date_ASC
  date_DESC
  newOnHand_ASC
  newOnHand_DESC
  userEstimateRunOut_ASC
  userEstimateRunOut_DESC
  didRunOut_ASC
  didRunOut_DESC
}

type RestockPreviousValues {
  id: ID!
  date: DateTime!
  newOnHand: Int!
  userEstimateRunOut: DateTime
  didRunOut: DateTime
}

input RestockScalarWhereInput {
  id: ID
  id_not: ID
  id_in: [ID!]
  id_not_in: [ID!]
  id_lt: ID
  id_lte: ID
  id_gt: ID
  id_gte: ID
  id_contains: ID
  id_not_contains: ID
  id_starts_with: ID
  id_not_starts_with: ID
  id_ends_with: ID
  id_not_ends_with: ID
  date: DateTime
  date_not: DateTime
  date_in: [DateTime!]
  date_not_in: [DateTime!]
  date_lt: DateTime
  date_lte: DateTime
  date_gt: DateTime
  date_gte: DateTime
  newOnHand: Int
  newOnHand_not: Int
  newOnHand_in: [Int!]
  newOnHand_not_in: [Int!]
  newOnHand_lt: Int
  newOnHand_lte: Int
  newOnHand_gt: Int
  newOnHand_gte: Int
  userEstimateRunOut: DateTime
  userEstimateRunOut_not: DateTime
  userEstimateRunOut_in: [DateTime!]
  userEstimateRunOut_not_in: [DateTime!]
  userEstimateRunOut_lt: DateTime
  userEstimateRunOut_lte: DateTime
  userEstimateRunOut_gt: DateTime
  userEstimateRunOut_gte: DateTime
  didRunOut: DateTime
  didRunOut_not: DateTime
  didRunOut_in: [DateTime!]
  didRunOut_not_in: [DateTime!]
  didRunOut_lt: DateTime
  didRunOut_lte: DateTime
  didRunOut_gt: DateTime
  didRunOut_gte: DateTime
  AND: [RestockScalarWhereInput!]
  OR: [RestockScalarWhereInput!]
  NOT: [RestockScalarWhereInput!]
}

type RestockSubscriptionPayload {
  mutation: MutationType!
  node: Restock
  updatedFields: [String!]
  previousValues: RestockPreviousValues
}

input RestockSubscriptionWhereInput {
  mutation_in: [MutationType!]
  updatedFields_contains: String
  updatedFields_contains_every: [String!]
  updatedFields_contains_some: [String!]
  node: RestockWhereInput
  AND: [RestockSubscriptionWhereInput!]
  OR: [RestockSubscriptionWhereInput!]
  NOT: [RestockSubscriptionWhereInput!]
}

input RestockUpdateDataInput {
  date: DateTime
  newOnHand: Int
  previousRestock: RestockUpdateOneInput
  userEstimateRunOut: DateTime
  didRunOut: DateTime
}

input RestockUpdateInput {
  date: DateTime
  newOnHand: Int
  previousRestock: RestockUpdateOneInput
  userEstimateRunOut: DateTime
  didRunOut: DateTime
}

input RestockUpdateManyDataInput {
  date: DateTime
  newOnHand: Int
  userEstimateRunOut: DateTime
  didRunOut: DateTime
}

input RestockUpdateManyInput {
  create: [RestockCreateInput!]
  update: [RestockUpdateWithWhereUniqueNestedInput!]
  upsert: [RestockUpsertWithWhereUniqueNestedInput!]
  delete: [RestockWhereUniqueInput!]
  connect: [RestockWhereUniqueInput!]
  set: [RestockWhereUniqueInput!]
  disconnect: [RestockWhereUniqueInput!]
  deleteMany: [RestockScalarWhereInput!]
  updateMany: [RestockUpdateManyWithWhereNestedInput!]
}

input RestockUpdateManyMutationInput {
  date: DateTime
  newOnHand: Int
  userEstimateRunOut: DateTime
  didRunOut: DateTime
}

input RestockUpdateManyWithWhereNestedInput {
  where: RestockScalarWhereInput!
  data: RestockUpdateManyDataInput!
}

input RestockUpdateOneInput {
  create: RestockCreateInput
  update: RestockUpdateDataInput
  upsert: RestockUpsertNestedInput
  delete: Boolean
  disconnect: Boolean
  connect: RestockWhereUniqueInput
}

input RestockUpdateWithWhereUniqueNestedInput {
  where: RestockWhereUniqueInput!
  data: RestockUpdateDataInput!
}

input RestockUpsertNestedInput {
  update: RestockUpdateDataInput!
  create: RestockCreateInput!
}

input RestockUpsertWithWhereUniqueNestedInput {
  where: RestockWhereUniqueInput!
  update: RestockUpdateDataInput!
  create: RestockCreateInput!
}

input RestockWhereInput {
  id: ID
  id_not: ID
  id_in: [ID!]
  id_not_in: [ID!]
  id_lt: ID
  id_lte: ID
  id_gt: ID
  id_gte: ID
  id_contains: ID
  id_not_contains: ID
  id_starts_with: ID
  id_not_starts_with: ID
  id_ends_with: ID
  id_not_ends_with: ID
  date: DateTime
  date_not: DateTime
  date_in: [DateTime!]
  date_not_in: [DateTime!]
  date_lt: DateTime
  date_lte: DateTime
  date_gt: DateTime
  date_gte: DateTime
  newOnHand: Int
  newOnHand_not: Int
  newOnHand_in: [Int!]
  newOnHand_not_in: [Int!]
  newOnHand_lt: Int
  newOnHand_lte: Int
  newOnHand_gt: Int
  newOnHand_gte: Int
  previousRestock: RestockWhereInput
  userEstimateRunOut: DateTime
  userEstimateRunOut_not: DateTime
  userEstimateRunOut_in: [DateTime!]
  userEstimateRunOut_not_in: [DateTime!]
  userEstimateRunOut_lt: DateTime
  userEstimateRunOut_lte: DateTime
  userEstimateRunOut_gt: DateTime
  userEstimateRunOut_gte: DateTime
  didRunOut: DateTime
  didRunOut_not: DateTime
  didRunOut_in: [DateTime!]
  didRunOut_not_in: [DateTime!]
  didRunOut_lt: DateTime
  didRunOut_lte: DateTime
  didRunOut_gt: DateTime
  didRunOut_gte: DateTime
  AND: [RestockWhereInput!]
  OR: [RestockWhereInput!]
  NOT: [RestockWhereInput!]
}

input RestockWhereUniqueInput {
  id: ID
}

type Subscription {
  cupboard(where: CupboardSubscriptionWhereInput): CupboardSubscriptionPayload
  item(where: ItemSubscriptionWhereInput): ItemSubscriptionPayload
  restock(where: RestockSubscriptionWhereInput): RestockSubscriptionPayload
}
`
      }
    