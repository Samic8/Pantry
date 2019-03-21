// Code generated by Prisma (prisma@1.28.5). DO NOT EDIT.
// Please don't change this file manually but run `prisma generate` to update it.
// For more information, please read the docs: https://www.prisma.io/docs/prisma-client/

import { DocumentNode } from "graphql";
import {
  makePrismaClientClass,
  BaseClientOptions,
  Model
} from "prisma-client-lib";
import { typeDefs } from "./prisma-schema";

export type AtLeastOne<T, U = { [K in keyof T]: Pick<T, K> }> = Partial<T> &
  U[keyof U];

export interface Exists {
  item: (where?: ItemWhereInput) => Promise<boolean>;
  restock: (where?: RestockWhereInput) => Promise<boolean>;
}

export interface Node {}

export type FragmentableArray<T> = Promise<Array<T>> & Fragmentable;

export interface Fragmentable {
  $fragment<T>(fragment: string | DocumentNode): Promise<T>;
}

export interface Prisma {
  $exists: Exists;
  $graphql: <T = any>(
    query: string,
    variables?: { [key: string]: any }
  ) => Promise<T>;

  /**
   * Queries
   */

  item: (where: ItemWhereUniqueInput) => ItemPromise;
  items: (
    args?: {
      where?: ItemWhereInput;
      orderBy?: ItemOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => FragmentableArray<Item>;
  itemsConnection: (
    args?: {
      where?: ItemWhereInput;
      orderBy?: ItemOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => ItemConnectionPromise;
  restocks: (
    args?: {
      where?: RestockWhereInput;
      orderBy?: RestockOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => FragmentableArray<Restock>;
  restocksConnection: (
    args?: {
      where?: RestockWhereInput;
      orderBy?: RestockOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => RestockConnectionPromise;
  node: (args: { id: ID_Output }) => Node;

  /**
   * Mutations
   */

  createItem: (data: ItemCreateInput) => ItemPromise;
  updateItem: (
    args: { data: ItemUpdateInput; where: ItemWhereUniqueInput }
  ) => ItemPromise;
  updateManyItems: (
    args: { data: ItemUpdateManyMutationInput; where?: ItemWhereInput }
  ) => BatchPayloadPromise;
  upsertItem: (
    args: {
      where: ItemWhereUniqueInput;
      create: ItemCreateInput;
      update: ItemUpdateInput;
    }
  ) => ItemPromise;
  deleteItem: (where: ItemWhereUniqueInput) => ItemPromise;
  deleteManyItems: (where?: ItemWhereInput) => BatchPayloadPromise;
  createRestock: (data: RestockCreateInput) => RestockPromise;
  updateManyRestocks: (
    args: { data: RestockUpdateManyMutationInput; where?: RestockWhereInput }
  ) => BatchPayloadPromise;
  deleteManyRestocks: (where?: RestockWhereInput) => BatchPayloadPromise;

  /**
   * Subscriptions
   */

  $subscribe: Subscription;
}

export interface Subscription {
  item: (
    where?: ItemSubscriptionWhereInput
  ) => ItemSubscriptionPayloadSubscription;
  restock: (
    where?: RestockSubscriptionWhereInput
  ) => RestockSubscriptionPayloadSubscription;
}

export interface ClientConstructor<T> {
  new (options?: BaseClientOptions): T;
}

/**
 * Types
 */

export type RestockOrderByInput =
  | "date_ASC"
  | "date_DESC"
  | "newOnHand_ASC"
  | "newOnHand_DESC"
  | "userEstimateRunOut_ASC"
  | "userEstimateRunOut_DESC"
  | "didRunOut_ASC"
  | "didRunOut_DESC"
  | "leftOverFromPrevious_ASC"
  | "leftOverFromPrevious_DESC"
  | "id_ASC"
  | "id_DESC"
  | "createdAt_ASC"
  | "createdAt_DESC"
  | "updatedAt_ASC"
  | "updatedAt_DESC";

export type ItemOrderByInput =
  | "id_ASC"
  | "id_DESC"
  | "name_ASC"
  | "name_DESC"
  | "maxOnHand_ASC"
  | "maxOnHand_DESC"
  | "unit_ASC"
  | "unit_DESC"
  | "createdAt_ASC"
  | "createdAt_DESC"
  | "updatedAt_ASC"
  | "updatedAt_DESC";

export type MutationType = "CREATED" | "UPDATED" | "DELETED";

export interface ItemCreateInput {
  name: String;
  maxOnHand: Int;
  unit: String;
  restocks?: RestockCreateManyInput;
}

export type ItemWhereUniqueInput = AtLeastOne<{
  id: ID_Input;
}>;

export interface RestockCreateInput {
  date: DateTimeInput;
  newOnHand: Int;
  previousRestock?: RestockCreateOneInput;
  userEstimateRunOut?: DateTimeInput;
  didRunOut?: DateTimeInput;
  leftOverFromPrevious?: Int;
}

export interface RestockWhereInput {
  date?: DateTimeInput;
  date_not?: DateTimeInput;
  date_in?: DateTimeInput[] | DateTimeInput;
  date_not_in?: DateTimeInput[] | DateTimeInput;
  date_lt?: DateTimeInput;
  date_lte?: DateTimeInput;
  date_gt?: DateTimeInput;
  date_gte?: DateTimeInput;
  newOnHand?: Int;
  newOnHand_not?: Int;
  newOnHand_in?: Int[] | Int;
  newOnHand_not_in?: Int[] | Int;
  newOnHand_lt?: Int;
  newOnHand_lte?: Int;
  newOnHand_gt?: Int;
  newOnHand_gte?: Int;
  previousRestock?: RestockWhereInput;
  userEstimateRunOut?: DateTimeInput;
  userEstimateRunOut_not?: DateTimeInput;
  userEstimateRunOut_in?: DateTimeInput[] | DateTimeInput;
  userEstimateRunOut_not_in?: DateTimeInput[] | DateTimeInput;
  userEstimateRunOut_lt?: DateTimeInput;
  userEstimateRunOut_lte?: DateTimeInput;
  userEstimateRunOut_gt?: DateTimeInput;
  userEstimateRunOut_gte?: DateTimeInput;
  didRunOut?: DateTimeInput;
  didRunOut_not?: DateTimeInput;
  didRunOut_in?: DateTimeInput[] | DateTimeInput;
  didRunOut_not_in?: DateTimeInput[] | DateTimeInput;
  didRunOut_lt?: DateTimeInput;
  didRunOut_lte?: DateTimeInput;
  didRunOut_gt?: DateTimeInput;
  didRunOut_gte?: DateTimeInput;
  leftOverFromPrevious?: Int;
  leftOverFromPrevious_not?: Int;
  leftOverFromPrevious_in?: Int[] | Int;
  leftOverFromPrevious_not_in?: Int[] | Int;
  leftOverFromPrevious_lt?: Int;
  leftOverFromPrevious_lte?: Int;
  leftOverFromPrevious_gt?: Int;
  leftOverFromPrevious_gte?: Int;
  AND?: RestockWhereInput[] | RestockWhereInput;
  OR?: RestockWhereInput[] | RestockWhereInput;
  NOT?: RestockWhereInput[] | RestockWhereInput;
}

export interface RestockUpdateManyDataInput {
  date?: DateTimeInput;
  newOnHand?: Int;
  userEstimateRunOut?: DateTimeInput;
  didRunOut?: DateTimeInput;
  leftOverFromPrevious?: Int;
}

export interface RestockCreateManyInput {
  create?: RestockCreateInput[] | RestockCreateInput;
}

export interface RestockUpdateManyWithWhereNestedInput {
  where: RestockScalarWhereInput;
  data: RestockUpdateManyDataInput;
}

export interface RestockCreateOneInput {
  create?: RestockCreateInput;
}

export interface RestockSubscriptionWhereInput {
  mutation_in?: MutationType[] | MutationType;
  updatedFields_contains?: String;
  updatedFields_contains_every?: String[] | String;
  updatedFields_contains_some?: String[] | String;
  node?: RestockWhereInput;
  AND?: RestockSubscriptionWhereInput[] | RestockSubscriptionWhereInput;
  OR?: RestockSubscriptionWhereInput[] | RestockSubscriptionWhereInput;
  NOT?: RestockSubscriptionWhereInput[] | RestockSubscriptionWhereInput;
}

export interface ItemUpdateInput {
  name?: String;
  maxOnHand?: Int;
  unit?: String;
  restocks?: RestockUpdateManyInput;
}

export interface RestockUpdateManyInput {
  create?: RestockCreateInput[] | RestockCreateInput;
  deleteMany?: RestockScalarWhereInput[] | RestockScalarWhereInput;
  updateMany?:
    | RestockUpdateManyWithWhereNestedInput[]
    | RestockUpdateManyWithWhereNestedInput;
}

export interface RestockScalarWhereInput {
  date?: DateTimeInput;
  date_not?: DateTimeInput;
  date_in?: DateTimeInput[] | DateTimeInput;
  date_not_in?: DateTimeInput[] | DateTimeInput;
  date_lt?: DateTimeInput;
  date_lte?: DateTimeInput;
  date_gt?: DateTimeInput;
  date_gte?: DateTimeInput;
  newOnHand?: Int;
  newOnHand_not?: Int;
  newOnHand_in?: Int[] | Int;
  newOnHand_not_in?: Int[] | Int;
  newOnHand_lt?: Int;
  newOnHand_lte?: Int;
  newOnHand_gt?: Int;
  newOnHand_gte?: Int;
  userEstimateRunOut?: DateTimeInput;
  userEstimateRunOut_not?: DateTimeInput;
  userEstimateRunOut_in?: DateTimeInput[] | DateTimeInput;
  userEstimateRunOut_not_in?: DateTimeInput[] | DateTimeInput;
  userEstimateRunOut_lt?: DateTimeInput;
  userEstimateRunOut_lte?: DateTimeInput;
  userEstimateRunOut_gt?: DateTimeInput;
  userEstimateRunOut_gte?: DateTimeInput;
  didRunOut?: DateTimeInput;
  didRunOut_not?: DateTimeInput;
  didRunOut_in?: DateTimeInput[] | DateTimeInput;
  didRunOut_not_in?: DateTimeInput[] | DateTimeInput;
  didRunOut_lt?: DateTimeInput;
  didRunOut_lte?: DateTimeInput;
  didRunOut_gt?: DateTimeInput;
  didRunOut_gte?: DateTimeInput;
  leftOverFromPrevious?: Int;
  leftOverFromPrevious_not?: Int;
  leftOverFromPrevious_in?: Int[] | Int;
  leftOverFromPrevious_not_in?: Int[] | Int;
  leftOverFromPrevious_lt?: Int;
  leftOverFromPrevious_lte?: Int;
  leftOverFromPrevious_gt?: Int;
  leftOverFromPrevious_gte?: Int;
  AND?: RestockScalarWhereInput[] | RestockScalarWhereInput;
  OR?: RestockScalarWhereInput[] | RestockScalarWhereInput;
  NOT?: RestockScalarWhereInput[] | RestockScalarWhereInput;
}

export interface ItemSubscriptionWhereInput {
  mutation_in?: MutationType[] | MutationType;
  updatedFields_contains?: String;
  updatedFields_contains_every?: String[] | String;
  updatedFields_contains_some?: String[] | String;
  node?: ItemWhereInput;
  AND?: ItemSubscriptionWhereInput[] | ItemSubscriptionWhereInput;
  OR?: ItemSubscriptionWhereInput[] | ItemSubscriptionWhereInput;
  NOT?: ItemSubscriptionWhereInput[] | ItemSubscriptionWhereInput;
}

export interface ItemWhereInput {
  id?: ID_Input;
  id_not?: ID_Input;
  id_in?: ID_Input[] | ID_Input;
  id_not_in?: ID_Input[] | ID_Input;
  id_lt?: ID_Input;
  id_lte?: ID_Input;
  id_gt?: ID_Input;
  id_gte?: ID_Input;
  id_contains?: ID_Input;
  id_not_contains?: ID_Input;
  id_starts_with?: ID_Input;
  id_not_starts_with?: ID_Input;
  id_ends_with?: ID_Input;
  id_not_ends_with?: ID_Input;
  name?: String;
  name_not?: String;
  name_in?: String[] | String;
  name_not_in?: String[] | String;
  name_lt?: String;
  name_lte?: String;
  name_gt?: String;
  name_gte?: String;
  name_contains?: String;
  name_not_contains?: String;
  name_starts_with?: String;
  name_not_starts_with?: String;
  name_ends_with?: String;
  name_not_ends_with?: String;
  maxOnHand?: Int;
  maxOnHand_not?: Int;
  maxOnHand_in?: Int[] | Int;
  maxOnHand_not_in?: Int[] | Int;
  maxOnHand_lt?: Int;
  maxOnHand_lte?: Int;
  maxOnHand_gt?: Int;
  maxOnHand_gte?: Int;
  unit?: String;
  unit_not?: String;
  unit_in?: String[] | String;
  unit_not_in?: String[] | String;
  unit_lt?: String;
  unit_lte?: String;
  unit_gt?: String;
  unit_gte?: String;
  unit_contains?: String;
  unit_not_contains?: String;
  unit_starts_with?: String;
  unit_not_starts_with?: String;
  unit_ends_with?: String;
  unit_not_ends_with?: String;
  restocks_every?: RestockWhereInput;
  restocks_some?: RestockWhereInput;
  restocks_none?: RestockWhereInput;
  AND?: ItemWhereInput[] | ItemWhereInput;
  OR?: ItemWhereInput[] | ItemWhereInput;
  NOT?: ItemWhereInput[] | ItemWhereInput;
}

export interface ItemUpdateManyMutationInput {
  name?: String;
  maxOnHand?: Int;
  unit?: String;
}

export interface RestockUpdateManyMutationInput {
  date?: DateTimeInput;
  newOnHand?: Int;
  userEstimateRunOut?: DateTimeInput;
  didRunOut?: DateTimeInput;
  leftOverFromPrevious?: Int;
}

export interface NodeNode {
  id: ID_Output;
}

export interface RestockPreviousValues {
  date: DateTimeOutput;
  newOnHand: Int;
  userEstimateRunOut?: DateTimeOutput;
  didRunOut?: DateTimeOutput;
  leftOverFromPrevious?: Int;
}

export interface RestockPreviousValuesPromise
  extends Promise<RestockPreviousValues>,
    Fragmentable {
  date: () => Promise<DateTimeOutput>;
  newOnHand: () => Promise<Int>;
  userEstimateRunOut: () => Promise<DateTimeOutput>;
  didRunOut: () => Promise<DateTimeOutput>;
  leftOverFromPrevious: () => Promise<Int>;
}

export interface RestockPreviousValuesSubscription
  extends Promise<AsyncIterator<RestockPreviousValues>>,
    Fragmentable {
  date: () => Promise<AsyncIterator<DateTimeOutput>>;
  newOnHand: () => Promise<AsyncIterator<Int>>;
  userEstimateRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
  didRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
  leftOverFromPrevious: () => Promise<AsyncIterator<Int>>;
}

export interface ItemEdge {
  node: Item;
  cursor: String;
}

export interface ItemEdgePromise extends Promise<ItemEdge>, Fragmentable {
  node: <T = ItemPromise>() => T;
  cursor: () => Promise<String>;
}

export interface ItemEdgeSubscription
  extends Promise<AsyncIterator<ItemEdge>>,
    Fragmentable {
  node: <T = ItemSubscription>() => T;
  cursor: () => Promise<AsyncIterator<String>>;
}

export interface Item {
  id: ID_Output;
  name: String;
  maxOnHand: Int;
  unit: String;
}

export interface ItemPromise extends Promise<Item>, Fragmentable {
  id: () => Promise<ID_Output>;
  name: () => Promise<String>;
  maxOnHand: () => Promise<Int>;
  unit: () => Promise<String>;
  restocks: <T = FragmentableArray<Restock>>(
    args?: {
      where?: RestockWhereInput;
      orderBy?: RestockOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => T;
}

export interface ItemSubscription
  extends Promise<AsyncIterator<Item>>,
    Fragmentable {
  id: () => Promise<AsyncIterator<ID_Output>>;
  name: () => Promise<AsyncIterator<String>>;
  maxOnHand: () => Promise<AsyncIterator<Int>>;
  unit: () => Promise<AsyncIterator<String>>;
  restocks: <T = Promise<AsyncIterator<RestockSubscription>>>(
    args?: {
      where?: RestockWhereInput;
      orderBy?: RestockOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => T;
}

export interface RestockSubscriptionPayload {
  mutation: MutationType;
  node: Restock;
  updatedFields: String[];
  previousValues: RestockPreviousValues;
}

export interface RestockSubscriptionPayloadPromise
  extends Promise<RestockSubscriptionPayload>,
    Fragmentable {
  mutation: () => Promise<MutationType>;
  node: <T = RestockPromise>() => T;
  updatedFields: () => Promise<String[]>;
  previousValues: <T = RestockPreviousValuesPromise>() => T;
}

export interface RestockSubscriptionPayloadSubscription
  extends Promise<AsyncIterator<RestockSubscriptionPayload>>,
    Fragmentable {
  mutation: () => Promise<AsyncIterator<MutationType>>;
  node: <T = RestockSubscription>() => T;
  updatedFields: () => Promise<AsyncIterator<String[]>>;
  previousValues: <T = RestockPreviousValuesSubscription>() => T;
}

export interface BatchPayload {
  count: Long;
}

export interface BatchPayloadPromise
  extends Promise<BatchPayload>,
    Fragmentable {
  count: () => Promise<Long>;
}

export interface BatchPayloadSubscription
  extends Promise<AsyncIterator<BatchPayload>>,
    Fragmentable {
  count: () => Promise<AsyncIterator<Long>>;
}

export interface AggregateRestock {
  count: Int;
}

export interface AggregateRestockPromise
  extends Promise<AggregateRestock>,
    Fragmentable {
  count: () => Promise<Int>;
}

export interface AggregateRestockSubscription
  extends Promise<AsyncIterator<AggregateRestock>>,
    Fragmentable {
  count: () => Promise<AsyncIterator<Int>>;
}

export interface ItemConnection {
  pageInfo: PageInfo;
  edges: ItemEdge[];
}

export interface ItemConnectionPromise
  extends Promise<ItemConnection>,
    Fragmentable {
  pageInfo: <T = PageInfoPromise>() => T;
  edges: <T = FragmentableArray<ItemEdge>>() => T;
  aggregate: <T = AggregateItemPromise>() => T;
}

export interface ItemConnectionSubscription
  extends Promise<AsyncIterator<ItemConnection>>,
    Fragmentable {
  pageInfo: <T = PageInfoSubscription>() => T;
  edges: <T = Promise<AsyncIterator<ItemEdgeSubscription>>>() => T;
  aggregate: <T = AggregateItemSubscription>() => T;
}

export interface PageInfo {
  hasNextPage: Boolean;
  hasPreviousPage: Boolean;
  startCursor?: String;
  endCursor?: String;
}

export interface PageInfoPromise extends Promise<PageInfo>, Fragmentable {
  hasNextPage: () => Promise<Boolean>;
  hasPreviousPage: () => Promise<Boolean>;
  startCursor: () => Promise<String>;
  endCursor: () => Promise<String>;
}

export interface PageInfoSubscription
  extends Promise<AsyncIterator<PageInfo>>,
    Fragmentable {
  hasNextPage: () => Promise<AsyncIterator<Boolean>>;
  hasPreviousPage: () => Promise<AsyncIterator<Boolean>>;
  startCursor: () => Promise<AsyncIterator<String>>;
  endCursor: () => Promise<AsyncIterator<String>>;
}

export interface ItemSubscriptionPayload {
  mutation: MutationType;
  node: Item;
  updatedFields: String[];
  previousValues: ItemPreviousValues;
}

export interface ItemSubscriptionPayloadPromise
  extends Promise<ItemSubscriptionPayload>,
    Fragmentable {
  mutation: () => Promise<MutationType>;
  node: <T = ItemPromise>() => T;
  updatedFields: () => Promise<String[]>;
  previousValues: <T = ItemPreviousValuesPromise>() => T;
}

export interface ItemSubscriptionPayloadSubscription
  extends Promise<AsyncIterator<ItemSubscriptionPayload>>,
    Fragmentable {
  mutation: () => Promise<AsyncIterator<MutationType>>;
  node: <T = ItemSubscription>() => T;
  updatedFields: () => Promise<AsyncIterator<String[]>>;
  previousValues: <T = ItemPreviousValuesSubscription>() => T;
}

export interface ItemPreviousValues {
  id: ID_Output;
  name: String;
  maxOnHand: Int;
  unit: String;
}

export interface ItemPreviousValuesPromise
  extends Promise<ItemPreviousValues>,
    Fragmentable {
  id: () => Promise<ID_Output>;
  name: () => Promise<String>;
  maxOnHand: () => Promise<Int>;
  unit: () => Promise<String>;
}

export interface ItemPreviousValuesSubscription
  extends Promise<AsyncIterator<ItemPreviousValues>>,
    Fragmentable {
  id: () => Promise<AsyncIterator<ID_Output>>;
  name: () => Promise<AsyncIterator<String>>;
  maxOnHand: () => Promise<AsyncIterator<Int>>;
  unit: () => Promise<AsyncIterator<String>>;
}

export interface RestockEdge {
  node: Restock;
  cursor: String;
}

export interface RestockEdgePromise extends Promise<RestockEdge>, Fragmentable {
  node: <T = RestockPromise>() => T;
  cursor: () => Promise<String>;
}

export interface RestockEdgeSubscription
  extends Promise<AsyncIterator<RestockEdge>>,
    Fragmentable {
  node: <T = RestockSubscription>() => T;
  cursor: () => Promise<AsyncIterator<String>>;
}

export interface Restock {
  date: DateTimeOutput;
  newOnHand: Int;
  userEstimateRunOut?: DateTimeOutput;
  didRunOut?: DateTimeOutput;
  leftOverFromPrevious?: Int;
}

export interface RestockPromise extends Promise<Restock>, Fragmentable {
  date: () => Promise<DateTimeOutput>;
  newOnHand: () => Promise<Int>;
  previousRestock: <T = RestockPromise>() => T;
  userEstimateRunOut: () => Promise<DateTimeOutput>;
  didRunOut: () => Promise<DateTimeOutput>;
  leftOverFromPrevious: () => Promise<Int>;
}

export interface RestockSubscription
  extends Promise<AsyncIterator<Restock>>,
    Fragmentable {
  date: () => Promise<AsyncIterator<DateTimeOutput>>;
  newOnHand: () => Promise<AsyncIterator<Int>>;
  previousRestock: <T = RestockSubscription>() => T;
  userEstimateRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
  didRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
  leftOverFromPrevious: () => Promise<AsyncIterator<Int>>;
}

export interface AggregateItem {
  count: Int;
}

export interface AggregateItemPromise
  extends Promise<AggregateItem>,
    Fragmentable {
  count: () => Promise<Int>;
}

export interface AggregateItemSubscription
  extends Promise<AsyncIterator<AggregateItem>>,
    Fragmentable {
  count: () => Promise<AsyncIterator<Int>>;
}

export interface RestockConnection {
  pageInfo: PageInfo;
  edges: RestockEdge[];
}

export interface RestockConnectionPromise
  extends Promise<RestockConnection>,
    Fragmentable {
  pageInfo: <T = PageInfoPromise>() => T;
  edges: <T = FragmentableArray<RestockEdge>>() => T;
  aggregate: <T = AggregateRestockPromise>() => T;
}

export interface RestockConnectionSubscription
  extends Promise<AsyncIterator<RestockConnection>>,
    Fragmentable {
  pageInfo: <T = PageInfoSubscription>() => T;
  edges: <T = Promise<AsyncIterator<RestockEdgeSubscription>>>() => T;
  aggregate: <T = AggregateRestockSubscription>() => T;
}

/*
The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1. 
*/
export type Int = number;

/*
The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `"4"`) or integer (such as `4`) input value will be accepted as an ID.
*/
export type ID_Input = string | number;
export type ID_Output = string;

/*
The `Boolean` scalar type represents `true` or `false`.
*/
export type Boolean = boolean;

/*
The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text.
*/
export type String = string;

/*
DateTime scalar input type, allowing Date
*/
export type DateTimeInput = Date | string;

/*
DateTime scalar output type, which is always a string
*/
export type DateTimeOutput = string;

export type Long = string;

/**
 * Model Metadata
 */

export const models: Model[] = [
  {
    name: "Item",
    embedded: false
  },
  {
    name: "Restock",
    embedded: false
  }
];

/**
 * Type Defs
 */

export const prisma: Prisma;