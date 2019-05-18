// Code generated by Prisma (prisma@1.33.0). DO NOT EDIT.
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

export type Maybe<T> = T | undefined | null;

export interface Exists {
  cupboard: (where?: CupboardWhereInput) => Promise<boolean>;
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

  cupboard: (where: CupboardWhereUniqueInput) => CupboardNullablePromise;
  cupboards: (args?: {
    where?: CupboardWhereInput;
    orderBy?: CupboardOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => FragmentableArray<Cupboard>;
  cupboardsConnection: (args?: {
    where?: CupboardWhereInput;
    orderBy?: CupboardOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => CupboardConnectionPromise;
  item: (where: ItemWhereUniqueInput) => ItemNullablePromise;
  items: (args?: {
    where?: ItemWhereInput;
    orderBy?: ItemOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => FragmentableArray<Item>;
  itemsConnection: (args?: {
    where?: ItemWhereInput;
    orderBy?: ItemOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => ItemConnectionPromise;
  restock: (where: RestockWhereUniqueInput) => RestockNullablePromise;
  restocks: (args?: {
    where?: RestockWhereInput;
    orderBy?: RestockOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => FragmentableArray<Restock>;
  restocksConnection: (args?: {
    where?: RestockWhereInput;
    orderBy?: RestockOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => RestockConnectionPromise;
  node: (args: { id: ID_Output }) => Node;

  /**
   * Mutations
   */

  createCupboard: (data: CupboardCreateInput) => CupboardPromise;
  updateCupboard: (args: {
    data: CupboardUpdateInput;
    where: CupboardWhereUniqueInput;
  }) => CupboardPromise;
  updateManyCupboards: (args: {
    data: CupboardUpdateManyMutationInput;
    where?: CupboardWhereInput;
  }) => BatchPayloadPromise;
  upsertCupboard: (args: {
    where: CupboardWhereUniqueInput;
    create: CupboardCreateInput;
    update: CupboardUpdateInput;
  }) => CupboardPromise;
  deleteCupboard: (where: CupboardWhereUniqueInput) => CupboardPromise;
  deleteManyCupboards: (where?: CupboardWhereInput) => BatchPayloadPromise;
  createItem: (data: ItemCreateInput) => ItemPromise;
  updateItem: (args: {
    data: ItemUpdateInput;
    where: ItemWhereUniqueInput;
  }) => ItemPromise;
  updateManyItems: (args: {
    data: ItemUpdateManyMutationInput;
    where?: ItemWhereInput;
  }) => BatchPayloadPromise;
  upsertItem: (args: {
    where: ItemWhereUniqueInput;
    create: ItemCreateInput;
    update: ItemUpdateInput;
  }) => ItemPromise;
  deleteItem: (where: ItemWhereUniqueInput) => ItemPromise;
  deleteManyItems: (where?: ItemWhereInput) => BatchPayloadPromise;
  createRestock: (data: RestockCreateInput) => RestockPromise;
  updateRestock: (args: {
    data: RestockUpdateInput;
    where: RestockWhereUniqueInput;
  }) => RestockPromise;
  updateManyRestocks: (args: {
    data: RestockUpdateManyMutationInput;
    where?: RestockWhereInput;
  }) => BatchPayloadPromise;
  upsertRestock: (args: {
    where: RestockWhereUniqueInput;
    create: RestockCreateInput;
    update: RestockUpdateInput;
  }) => RestockPromise;
  deleteRestock: (where: RestockWhereUniqueInput) => RestockPromise;
  deleteManyRestocks: (where?: RestockWhereInput) => BatchPayloadPromise;

  /**
   * Subscriptions
   */

  $subscribe: Subscription;
}

export interface Subscription {
  cupboard: (
    where?: CupboardSubscriptionWhereInput
  ) => CupboardSubscriptionPayloadSubscription;
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

export type ItemOrderByInput =
  | "id_ASC"
  | "id_DESC"
  | "name_ASC"
  | "name_DESC"
  | "maxOnHand_ASC"
  | "maxOnHand_DESC"
  | "unit_ASC"
  | "unit_DESC";

export type RestockOrderByInput =
  | "id_ASC"
  | "id_DESC"
  | "date_ASC"
  | "date_DESC"
  | "newOnHand_ASC"
  | "newOnHand_DESC"
  | "userEstimateRunOut_ASC"
  | "userEstimateRunOut_DESC"
  | "didRunOut_ASC"
  | "didRunOut_DESC";

export type CupboardOrderByInput =
  | "id_ASC"
  | "id_DESC"
  | "title_ASC"
  | "title_DESC"
  | "urlSlug_ASC"
  | "urlSlug_DESC";

export type MutationType = "CREATED" | "UPDATED" | "DELETED";

export interface ItemUpdateWithoutCupboardDataInput {
  name?: Maybe<String>;
  maxOnHand?: Maybe<Int>;
  unit?: Maybe<String>;
  restocks?: Maybe<RestockUpdateManyInput>;
}

export type CupboardWhereUniqueInput = AtLeastOne<{
  id: Maybe<ID_Input>;
  urlSlug?: Maybe<String>;
}>;

export interface CupboardCreateInput {
  title: String;
  urlSlug: String;
  items?: Maybe<ItemCreateManyWithoutCupboardInput>;
}

export interface ItemScalarWhereInput {
  id?: Maybe<ID_Input>;
  id_not?: Maybe<ID_Input>;
  id_in?: Maybe<ID_Input[] | ID_Input>;
  id_not_in?: Maybe<ID_Input[] | ID_Input>;
  id_lt?: Maybe<ID_Input>;
  id_lte?: Maybe<ID_Input>;
  id_gt?: Maybe<ID_Input>;
  id_gte?: Maybe<ID_Input>;
  id_contains?: Maybe<ID_Input>;
  id_not_contains?: Maybe<ID_Input>;
  id_starts_with?: Maybe<ID_Input>;
  id_not_starts_with?: Maybe<ID_Input>;
  id_ends_with?: Maybe<ID_Input>;
  id_not_ends_with?: Maybe<ID_Input>;
  name?: Maybe<String>;
  name_not?: Maybe<String>;
  name_in?: Maybe<String[] | String>;
  name_not_in?: Maybe<String[] | String>;
  name_lt?: Maybe<String>;
  name_lte?: Maybe<String>;
  name_gt?: Maybe<String>;
  name_gte?: Maybe<String>;
  name_contains?: Maybe<String>;
  name_not_contains?: Maybe<String>;
  name_starts_with?: Maybe<String>;
  name_not_starts_with?: Maybe<String>;
  name_ends_with?: Maybe<String>;
  name_not_ends_with?: Maybe<String>;
  maxOnHand?: Maybe<Int>;
  maxOnHand_not?: Maybe<Int>;
  maxOnHand_in?: Maybe<Int[] | Int>;
  maxOnHand_not_in?: Maybe<Int[] | Int>;
  maxOnHand_lt?: Maybe<Int>;
  maxOnHand_lte?: Maybe<Int>;
  maxOnHand_gt?: Maybe<Int>;
  maxOnHand_gte?: Maybe<Int>;
  unit?: Maybe<String>;
  unit_not?: Maybe<String>;
  unit_in?: Maybe<String[] | String>;
  unit_not_in?: Maybe<String[] | String>;
  unit_lt?: Maybe<String>;
  unit_lte?: Maybe<String>;
  unit_gt?: Maybe<String>;
  unit_gte?: Maybe<String>;
  unit_contains?: Maybe<String>;
  unit_not_contains?: Maybe<String>;
  unit_starts_with?: Maybe<String>;
  unit_not_starts_with?: Maybe<String>;
  unit_ends_with?: Maybe<String>;
  unit_not_ends_with?: Maybe<String>;
  AND?: Maybe<ItemScalarWhereInput[] | ItemScalarWhereInput>;
  OR?: Maybe<ItemScalarWhereInput[] | ItemScalarWhereInput>;
  NOT?: Maybe<ItemScalarWhereInput[] | ItemScalarWhereInput>;
}

export interface ItemCreateManyWithoutCupboardInput {
  create?: Maybe<
    ItemCreateWithoutCupboardInput[] | ItemCreateWithoutCupboardInput
  >;
  connect?: Maybe<ItemWhereUniqueInput[] | ItemWhereUniqueInput>;
}

export interface RestockUpdateDataInput {
  date?: Maybe<DateTimeInput>;
  newOnHand?: Maybe<Int>;
  previousRestock?: Maybe<RestockUpdateOneInput>;
  userEstimateRunOut?: Maybe<DateTimeInput>;
  didRunOut?: Maybe<DateTimeInput>;
}

export interface ItemCreateWithoutCupboardInput {
  name: String;
  maxOnHand: Int;
  unit: String;
  restocks?: Maybe<RestockCreateManyInput>;
}

export interface CupboardWhereInput {
  id?: Maybe<ID_Input>;
  id_not?: Maybe<ID_Input>;
  id_in?: Maybe<ID_Input[] | ID_Input>;
  id_not_in?: Maybe<ID_Input[] | ID_Input>;
  id_lt?: Maybe<ID_Input>;
  id_lte?: Maybe<ID_Input>;
  id_gt?: Maybe<ID_Input>;
  id_gte?: Maybe<ID_Input>;
  id_contains?: Maybe<ID_Input>;
  id_not_contains?: Maybe<ID_Input>;
  id_starts_with?: Maybe<ID_Input>;
  id_not_starts_with?: Maybe<ID_Input>;
  id_ends_with?: Maybe<ID_Input>;
  id_not_ends_with?: Maybe<ID_Input>;
  title?: Maybe<String>;
  title_not?: Maybe<String>;
  title_in?: Maybe<String[] | String>;
  title_not_in?: Maybe<String[] | String>;
  title_lt?: Maybe<String>;
  title_lte?: Maybe<String>;
  title_gt?: Maybe<String>;
  title_gte?: Maybe<String>;
  title_contains?: Maybe<String>;
  title_not_contains?: Maybe<String>;
  title_starts_with?: Maybe<String>;
  title_not_starts_with?: Maybe<String>;
  title_ends_with?: Maybe<String>;
  title_not_ends_with?: Maybe<String>;
  urlSlug?: Maybe<String>;
  urlSlug_not?: Maybe<String>;
  urlSlug_in?: Maybe<String[] | String>;
  urlSlug_not_in?: Maybe<String[] | String>;
  urlSlug_lt?: Maybe<String>;
  urlSlug_lte?: Maybe<String>;
  urlSlug_gt?: Maybe<String>;
  urlSlug_gte?: Maybe<String>;
  urlSlug_contains?: Maybe<String>;
  urlSlug_not_contains?: Maybe<String>;
  urlSlug_starts_with?: Maybe<String>;
  urlSlug_not_starts_with?: Maybe<String>;
  urlSlug_ends_with?: Maybe<String>;
  urlSlug_not_ends_with?: Maybe<String>;
  items_every?: Maybe<ItemWhereInput>;
  items_some?: Maybe<ItemWhereInput>;
  items_none?: Maybe<ItemWhereInput>;
  AND?: Maybe<CupboardWhereInput[] | CupboardWhereInput>;
  OR?: Maybe<CupboardWhereInput[] | CupboardWhereInput>;
  NOT?: Maybe<CupboardWhereInput[] | CupboardWhereInput>;
}

export interface RestockCreateManyInput {
  create?: Maybe<RestockCreateInput[] | RestockCreateInput>;
  connect?: Maybe<RestockWhereUniqueInput[] | RestockWhereUniqueInput>;
}

export interface ItemWhereInput {
  id?: Maybe<ID_Input>;
  id_not?: Maybe<ID_Input>;
  id_in?: Maybe<ID_Input[] | ID_Input>;
  id_not_in?: Maybe<ID_Input[] | ID_Input>;
  id_lt?: Maybe<ID_Input>;
  id_lte?: Maybe<ID_Input>;
  id_gt?: Maybe<ID_Input>;
  id_gte?: Maybe<ID_Input>;
  id_contains?: Maybe<ID_Input>;
  id_not_contains?: Maybe<ID_Input>;
  id_starts_with?: Maybe<ID_Input>;
  id_not_starts_with?: Maybe<ID_Input>;
  id_ends_with?: Maybe<ID_Input>;
  id_not_ends_with?: Maybe<ID_Input>;
  cupboard?: Maybe<CupboardWhereInput>;
  name?: Maybe<String>;
  name_not?: Maybe<String>;
  name_in?: Maybe<String[] | String>;
  name_not_in?: Maybe<String[] | String>;
  name_lt?: Maybe<String>;
  name_lte?: Maybe<String>;
  name_gt?: Maybe<String>;
  name_gte?: Maybe<String>;
  name_contains?: Maybe<String>;
  name_not_contains?: Maybe<String>;
  name_starts_with?: Maybe<String>;
  name_not_starts_with?: Maybe<String>;
  name_ends_with?: Maybe<String>;
  name_not_ends_with?: Maybe<String>;
  maxOnHand?: Maybe<Int>;
  maxOnHand_not?: Maybe<Int>;
  maxOnHand_in?: Maybe<Int[] | Int>;
  maxOnHand_not_in?: Maybe<Int[] | Int>;
  maxOnHand_lt?: Maybe<Int>;
  maxOnHand_lte?: Maybe<Int>;
  maxOnHand_gt?: Maybe<Int>;
  maxOnHand_gte?: Maybe<Int>;
  unit?: Maybe<String>;
  unit_not?: Maybe<String>;
  unit_in?: Maybe<String[] | String>;
  unit_not_in?: Maybe<String[] | String>;
  unit_lt?: Maybe<String>;
  unit_lte?: Maybe<String>;
  unit_gt?: Maybe<String>;
  unit_gte?: Maybe<String>;
  unit_contains?: Maybe<String>;
  unit_not_contains?: Maybe<String>;
  unit_starts_with?: Maybe<String>;
  unit_not_starts_with?: Maybe<String>;
  unit_ends_with?: Maybe<String>;
  unit_not_ends_with?: Maybe<String>;
  restocks_every?: Maybe<RestockWhereInput>;
  restocks_some?: Maybe<RestockWhereInput>;
  restocks_none?: Maybe<RestockWhereInput>;
  AND?: Maybe<ItemWhereInput[] | ItemWhereInput>;
  OR?: Maybe<ItemWhereInput[] | ItemWhereInput>;
  NOT?: Maybe<ItemWhereInput[] | ItemWhereInput>;
}

export interface RestockCreateInput {
  date: DateTimeInput;
  newOnHand: Int;
  previousRestock?: Maybe<RestockCreateOneInput>;
  userEstimateRunOut?: Maybe<DateTimeInput>;
  didRunOut?: Maybe<DateTimeInput>;
}

export interface RestockUpdateManyMutationInput {
  date?: Maybe<DateTimeInput>;
  newOnHand?: Maybe<Int>;
  userEstimateRunOut?: Maybe<DateTimeInput>;
  didRunOut?: Maybe<DateTimeInput>;
}

export interface RestockCreateOneInput {
  create?: Maybe<RestockCreateInput>;
  connect?: Maybe<RestockWhereUniqueInput>;
}

export interface ItemUpdateManyMutationInput {
  name?: Maybe<String>;
  maxOnHand?: Maybe<Int>;
  unit?: Maybe<String>;
}

export interface CupboardUpdateInput {
  title?: Maybe<String>;
  urlSlug?: Maybe<String>;
  items?: Maybe<ItemUpdateManyWithoutCupboardInput>;
}

export interface CupboardUpdateWithoutItemsDataInput {
  title?: Maybe<String>;
  urlSlug?: Maybe<String>;
}

export interface ItemUpdateManyWithoutCupboardInput {
  create?: Maybe<
    ItemCreateWithoutCupboardInput[] | ItemCreateWithoutCupboardInput
  >;
  delete?: Maybe<ItemWhereUniqueInput[] | ItemWhereUniqueInput>;
  connect?: Maybe<ItemWhereUniqueInput[] | ItemWhereUniqueInput>;
  set?: Maybe<ItemWhereUniqueInput[] | ItemWhereUniqueInput>;
  disconnect?: Maybe<ItemWhereUniqueInput[] | ItemWhereUniqueInput>;
  update?: Maybe<
    | ItemUpdateWithWhereUniqueWithoutCupboardInput[]
    | ItemUpdateWithWhereUniqueWithoutCupboardInput
  >;
  upsert?: Maybe<
    | ItemUpsertWithWhereUniqueWithoutCupboardInput[]
    | ItemUpsertWithWhereUniqueWithoutCupboardInput
  >;
  deleteMany?: Maybe<ItemScalarWhereInput[] | ItemScalarWhereInput>;
  updateMany?: Maybe<
    ItemUpdateManyWithWhereNestedInput[] | ItemUpdateManyWithWhereNestedInput
  >;
}

export type ItemWhereUniqueInput = AtLeastOne<{
  id: Maybe<ID_Input>;
}>;

export interface ItemUpdateWithWhereUniqueWithoutCupboardInput {
  where: ItemWhereUniqueInput;
  data: ItemUpdateWithoutCupboardDataInput;
}

export interface CupboardCreateWithoutItemsInput {
  title: String;
  urlSlug: String;
}

export interface ItemUpdateManyWithWhereNestedInput {
  where: ItemScalarWhereInput;
  data: ItemUpdateManyDataInput;
}

export type RestockWhereUniqueInput = AtLeastOne<{
  id: Maybe<ID_Input>;
}>;

export interface RestockUpdateManyInput {
  create?: Maybe<RestockCreateInput[] | RestockCreateInput>;
  update?: Maybe<
    | RestockUpdateWithWhereUniqueNestedInput[]
    | RestockUpdateWithWhereUniqueNestedInput
  >;
  upsert?: Maybe<
    | RestockUpsertWithWhereUniqueNestedInput[]
    | RestockUpsertWithWhereUniqueNestedInput
  >;
  delete?: Maybe<RestockWhereUniqueInput[] | RestockWhereUniqueInput>;
  connect?: Maybe<RestockWhereUniqueInput[] | RestockWhereUniqueInput>;
  set?: Maybe<RestockWhereUniqueInput[] | RestockWhereUniqueInput>;
  disconnect?: Maybe<RestockWhereUniqueInput[] | RestockWhereUniqueInput>;
  deleteMany?: Maybe<RestockScalarWhereInput[] | RestockScalarWhereInput>;
  updateMany?: Maybe<
    | RestockUpdateManyWithWhereNestedInput[]
    | RestockUpdateManyWithWhereNestedInput
  >;
}

export interface CupboardUpdateManyMutationInput {
  title?: Maybe<String>;
  urlSlug?: Maybe<String>;
}

export interface RestockUpdateWithWhereUniqueNestedInput {
  where: RestockWhereUniqueInput;
  data: RestockUpdateDataInput;
}

export interface ItemSubscriptionWhereInput {
  mutation_in?: Maybe<MutationType[] | MutationType>;
  updatedFields_contains?: Maybe<String>;
  updatedFields_contains_every?: Maybe<String[] | String>;
  updatedFields_contains_some?: Maybe<String[] | String>;
  node?: Maybe<ItemWhereInput>;
  AND?: Maybe<ItemSubscriptionWhereInput[] | ItemSubscriptionWhereInput>;
  OR?: Maybe<ItemSubscriptionWhereInput[] | ItemSubscriptionWhereInput>;
  NOT?: Maybe<ItemSubscriptionWhereInput[] | ItemSubscriptionWhereInput>;
}

export interface RestockWhereInput {
  id?: Maybe<ID_Input>;
  id_not?: Maybe<ID_Input>;
  id_in?: Maybe<ID_Input[] | ID_Input>;
  id_not_in?: Maybe<ID_Input[] | ID_Input>;
  id_lt?: Maybe<ID_Input>;
  id_lte?: Maybe<ID_Input>;
  id_gt?: Maybe<ID_Input>;
  id_gte?: Maybe<ID_Input>;
  id_contains?: Maybe<ID_Input>;
  id_not_contains?: Maybe<ID_Input>;
  id_starts_with?: Maybe<ID_Input>;
  id_not_starts_with?: Maybe<ID_Input>;
  id_ends_with?: Maybe<ID_Input>;
  id_not_ends_with?: Maybe<ID_Input>;
  date?: Maybe<DateTimeInput>;
  date_not?: Maybe<DateTimeInput>;
  date_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  date_not_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  date_lt?: Maybe<DateTimeInput>;
  date_lte?: Maybe<DateTimeInput>;
  date_gt?: Maybe<DateTimeInput>;
  date_gte?: Maybe<DateTimeInput>;
  newOnHand?: Maybe<Int>;
  newOnHand_not?: Maybe<Int>;
  newOnHand_in?: Maybe<Int[] | Int>;
  newOnHand_not_in?: Maybe<Int[] | Int>;
  newOnHand_lt?: Maybe<Int>;
  newOnHand_lte?: Maybe<Int>;
  newOnHand_gt?: Maybe<Int>;
  newOnHand_gte?: Maybe<Int>;
  previousRestock?: Maybe<RestockWhereInput>;
  userEstimateRunOut?: Maybe<DateTimeInput>;
  userEstimateRunOut_not?: Maybe<DateTimeInput>;
  userEstimateRunOut_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  userEstimateRunOut_not_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  userEstimateRunOut_lt?: Maybe<DateTimeInput>;
  userEstimateRunOut_lte?: Maybe<DateTimeInput>;
  userEstimateRunOut_gt?: Maybe<DateTimeInput>;
  userEstimateRunOut_gte?: Maybe<DateTimeInput>;
  didRunOut?: Maybe<DateTimeInput>;
  didRunOut_not?: Maybe<DateTimeInput>;
  didRunOut_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  didRunOut_not_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  didRunOut_lt?: Maybe<DateTimeInput>;
  didRunOut_lte?: Maybe<DateTimeInput>;
  didRunOut_gt?: Maybe<DateTimeInput>;
  didRunOut_gte?: Maybe<DateTimeInput>;
  AND?: Maybe<RestockWhereInput[] | RestockWhereInput>;
  OR?: Maybe<RestockWhereInput[] | RestockWhereInput>;
  NOT?: Maybe<RestockWhereInput[] | RestockWhereInput>;
}

export interface RestockUpdateInput {
  date?: Maybe<DateTimeInput>;
  newOnHand?: Maybe<Int>;
  previousRestock?: Maybe<RestockUpdateOneInput>;
  userEstimateRunOut?: Maybe<DateTimeInput>;
  didRunOut?: Maybe<DateTimeInput>;
}

export interface RestockUpdateOneInput {
  create?: Maybe<RestockCreateInput>;
  update?: Maybe<RestockUpdateDataInput>;
  upsert?: Maybe<RestockUpsertNestedInput>;
  delete?: Maybe<Boolean>;
  disconnect?: Maybe<Boolean>;
  connect?: Maybe<RestockWhereUniqueInput>;
}

export interface CupboardUpdateOneRequiredWithoutItemsInput {
  create?: Maybe<CupboardCreateWithoutItemsInput>;
  update?: Maybe<CupboardUpdateWithoutItemsDataInput>;
  upsert?: Maybe<CupboardUpsertWithoutItemsInput>;
  connect?: Maybe<CupboardWhereUniqueInput>;
}

export interface RestockUpsertNestedInput {
  update: RestockUpdateDataInput;
  create: RestockCreateInput;
}

export interface CupboardCreateOneWithoutItemsInput {
  create?: Maybe<CupboardCreateWithoutItemsInput>;
  connect?: Maybe<CupboardWhereUniqueInput>;
}

export interface RestockUpsertWithWhereUniqueNestedInput {
  where: RestockWhereUniqueInput;
  update: RestockUpdateDataInput;
  create: RestockCreateInput;
}

export interface ItemUpdateManyDataInput {
  name?: Maybe<String>;
  maxOnHand?: Maybe<Int>;
  unit?: Maybe<String>;
}

export interface ItemUpsertWithWhereUniqueWithoutCupboardInput {
  where: ItemWhereUniqueInput;
  update: ItemUpdateWithoutCupboardDataInput;
  create: ItemCreateWithoutCupboardInput;
}

export interface RestockUpdateManyDataInput {
  date?: Maybe<DateTimeInput>;
  newOnHand?: Maybe<Int>;
  userEstimateRunOut?: Maybe<DateTimeInput>;
  didRunOut?: Maybe<DateTimeInput>;
}

export interface RestockUpdateManyWithWhereNestedInput {
  where: RestockScalarWhereInput;
  data: RestockUpdateManyDataInput;
}

export interface RestockScalarWhereInput {
  id?: Maybe<ID_Input>;
  id_not?: Maybe<ID_Input>;
  id_in?: Maybe<ID_Input[] | ID_Input>;
  id_not_in?: Maybe<ID_Input[] | ID_Input>;
  id_lt?: Maybe<ID_Input>;
  id_lte?: Maybe<ID_Input>;
  id_gt?: Maybe<ID_Input>;
  id_gte?: Maybe<ID_Input>;
  id_contains?: Maybe<ID_Input>;
  id_not_contains?: Maybe<ID_Input>;
  id_starts_with?: Maybe<ID_Input>;
  id_not_starts_with?: Maybe<ID_Input>;
  id_ends_with?: Maybe<ID_Input>;
  id_not_ends_with?: Maybe<ID_Input>;
  date?: Maybe<DateTimeInput>;
  date_not?: Maybe<DateTimeInput>;
  date_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  date_not_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  date_lt?: Maybe<DateTimeInput>;
  date_lte?: Maybe<DateTimeInput>;
  date_gt?: Maybe<DateTimeInput>;
  date_gte?: Maybe<DateTimeInput>;
  newOnHand?: Maybe<Int>;
  newOnHand_not?: Maybe<Int>;
  newOnHand_in?: Maybe<Int[] | Int>;
  newOnHand_not_in?: Maybe<Int[] | Int>;
  newOnHand_lt?: Maybe<Int>;
  newOnHand_lte?: Maybe<Int>;
  newOnHand_gt?: Maybe<Int>;
  newOnHand_gte?: Maybe<Int>;
  userEstimateRunOut?: Maybe<DateTimeInput>;
  userEstimateRunOut_not?: Maybe<DateTimeInput>;
  userEstimateRunOut_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  userEstimateRunOut_not_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  userEstimateRunOut_lt?: Maybe<DateTimeInput>;
  userEstimateRunOut_lte?: Maybe<DateTimeInput>;
  userEstimateRunOut_gt?: Maybe<DateTimeInput>;
  userEstimateRunOut_gte?: Maybe<DateTimeInput>;
  didRunOut?: Maybe<DateTimeInput>;
  didRunOut_not?: Maybe<DateTimeInput>;
  didRunOut_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  didRunOut_not_in?: Maybe<DateTimeInput[] | DateTimeInput>;
  didRunOut_lt?: Maybe<DateTimeInput>;
  didRunOut_lte?: Maybe<DateTimeInput>;
  didRunOut_gt?: Maybe<DateTimeInput>;
  didRunOut_gte?: Maybe<DateTimeInput>;
  AND?: Maybe<RestockScalarWhereInput[] | RestockScalarWhereInput>;
  OR?: Maybe<RestockScalarWhereInput[] | RestockScalarWhereInput>;
  NOT?: Maybe<RestockScalarWhereInput[] | RestockScalarWhereInput>;
}

export interface RestockSubscriptionWhereInput {
  mutation_in?: Maybe<MutationType[] | MutationType>;
  updatedFields_contains?: Maybe<String>;
  updatedFields_contains_every?: Maybe<String[] | String>;
  updatedFields_contains_some?: Maybe<String[] | String>;
  node?: Maybe<RestockWhereInput>;
  AND?: Maybe<RestockSubscriptionWhereInput[] | RestockSubscriptionWhereInput>;
  OR?: Maybe<RestockSubscriptionWhereInput[] | RestockSubscriptionWhereInput>;
  NOT?: Maybe<RestockSubscriptionWhereInput[] | RestockSubscriptionWhereInput>;
}

export interface ItemCreateInput {
  cupboard: CupboardCreateOneWithoutItemsInput;
  name: String;
  maxOnHand: Int;
  unit: String;
  restocks?: Maybe<RestockCreateManyInput>;
}

export interface ItemUpdateInput {
  cupboard?: Maybe<CupboardUpdateOneRequiredWithoutItemsInput>;
  name?: Maybe<String>;
  maxOnHand?: Maybe<Int>;
  unit?: Maybe<String>;
  restocks?: Maybe<RestockUpdateManyInput>;
}

export interface CupboardUpsertWithoutItemsInput {
  update: CupboardUpdateWithoutItemsDataInput;
  create: CupboardCreateWithoutItemsInput;
}

export interface CupboardSubscriptionWhereInput {
  mutation_in?: Maybe<MutationType[] | MutationType>;
  updatedFields_contains?: Maybe<String>;
  updatedFields_contains_every?: Maybe<String[] | String>;
  updatedFields_contains_some?: Maybe<String[] | String>;
  node?: Maybe<CupboardWhereInput>;
  AND?: Maybe<
    CupboardSubscriptionWhereInput[] | CupboardSubscriptionWhereInput
  >;
  OR?: Maybe<CupboardSubscriptionWhereInput[] | CupboardSubscriptionWhereInput>;
  NOT?: Maybe<
    CupboardSubscriptionWhereInput[] | CupboardSubscriptionWhereInput
  >;
}

export interface NodeNode {
  id: ID_Output;
}

export interface RestockPreviousValues {
  id: ID_Output;
  date: DateTimeOutput;
  newOnHand: Int;
  userEstimateRunOut?: DateTimeOutput;
  didRunOut?: DateTimeOutput;
}

export interface RestockPreviousValuesPromise
  extends Promise<RestockPreviousValues>,
    Fragmentable {
  id: () => Promise<ID_Output>;
  date: () => Promise<DateTimeOutput>;
  newOnHand: () => Promise<Int>;
  userEstimateRunOut: () => Promise<DateTimeOutput>;
  didRunOut: () => Promise<DateTimeOutput>;
}

export interface RestockPreviousValuesSubscription
  extends Promise<AsyncIterator<RestockPreviousValues>>,
    Fragmentable {
  id: () => Promise<AsyncIterator<ID_Output>>;
  date: () => Promise<AsyncIterator<DateTimeOutput>>;
  newOnHand: () => Promise<AsyncIterator<Int>>;
  userEstimateRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
  didRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
}

export interface CupboardEdge {
  node: Cupboard;
  cursor: String;
}

export interface CupboardEdgePromise
  extends Promise<CupboardEdge>,
    Fragmentable {
  node: <T = CupboardPromise>() => T;
  cursor: () => Promise<String>;
}

export interface CupboardEdgeSubscription
  extends Promise<AsyncIterator<CupboardEdge>>,
    Fragmentable {
  node: <T = CupboardSubscription>() => T;
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
  cupboard: <T = CupboardPromise>() => T;
  name: () => Promise<String>;
  maxOnHand: () => Promise<Int>;
  unit: () => Promise<String>;
  restocks: <T = FragmentableArray<Restock>>(args?: {
    where?: RestockWhereInput;
    orderBy?: RestockOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => T;
}

export interface ItemSubscription
  extends Promise<AsyncIterator<Item>>,
    Fragmentable {
  id: () => Promise<AsyncIterator<ID_Output>>;
  cupboard: <T = CupboardSubscription>() => T;
  name: () => Promise<AsyncIterator<String>>;
  maxOnHand: () => Promise<AsyncIterator<Int>>;
  unit: () => Promise<AsyncIterator<String>>;
  restocks: <T = Promise<AsyncIterator<RestockSubscription>>>(args?: {
    where?: RestockWhereInput;
    orderBy?: RestockOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => T;
}

export interface ItemNullablePromise
  extends Promise<Item | null>,
    Fragmentable {
  id: () => Promise<ID_Output>;
  cupboard: <T = CupboardPromise>() => T;
  name: () => Promise<String>;
  maxOnHand: () => Promise<Int>;
  unit: () => Promise<String>;
  restocks: <T = FragmentableArray<Restock>>(args?: {
    where?: RestockWhereInput;
    orderBy?: RestockOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => T;
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

export interface CupboardConnection {
  pageInfo: PageInfo;
  edges: CupboardEdge[];
}

export interface CupboardConnectionPromise
  extends Promise<CupboardConnection>,
    Fragmentable {
  pageInfo: <T = PageInfoPromise>() => T;
  edges: <T = FragmentableArray<CupboardEdge>>() => T;
  aggregate: <T = AggregateCupboardPromise>() => T;
}

export interface CupboardConnectionSubscription
  extends Promise<AsyncIterator<CupboardConnection>>,
    Fragmentable {
  pageInfo: <T = PageInfoSubscription>() => T;
  edges: <T = Promise<AsyncIterator<CupboardEdgeSubscription>>>() => T;
  aggregate: <T = AggregateCupboardSubscription>() => T;
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

export interface Cupboard {
  id: ID_Output;
  title: String;
  urlSlug: String;
}

export interface CupboardPromise extends Promise<Cupboard>, Fragmentable {
  id: () => Promise<ID_Output>;
  title: () => Promise<String>;
  urlSlug: () => Promise<String>;
  items: <T = FragmentableArray<Item>>(args?: {
    where?: ItemWhereInput;
    orderBy?: ItemOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => T;
}

export interface CupboardSubscription
  extends Promise<AsyncIterator<Cupboard>>,
    Fragmentable {
  id: () => Promise<AsyncIterator<ID_Output>>;
  title: () => Promise<AsyncIterator<String>>;
  urlSlug: () => Promise<AsyncIterator<String>>;
  items: <T = Promise<AsyncIterator<ItemSubscription>>>(args?: {
    where?: ItemWhereInput;
    orderBy?: ItemOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => T;
}

export interface CupboardNullablePromise
  extends Promise<Cupboard | null>,
    Fragmentable {
  id: () => Promise<ID_Output>;
  title: () => Promise<String>;
  urlSlug: () => Promise<String>;
  items: <T = FragmentableArray<Item>>(args?: {
    where?: ItemWhereInput;
    orderBy?: ItemOrderByInput;
    skip?: Int;
    after?: String;
    before?: String;
    first?: Int;
    last?: Int;
  }) => T;
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

export interface AggregateCupboard {
  count: Int;
}

export interface AggregateCupboardPromise
  extends Promise<AggregateCupboard>,
    Fragmentable {
  count: () => Promise<Int>;
}

export interface AggregateCupboardSubscription
  extends Promise<AsyncIterator<AggregateCupboard>>,
    Fragmentable {
  count: () => Promise<AsyncIterator<Int>>;
}

export interface CupboardPreviousValues {
  id: ID_Output;
  title: String;
  urlSlug: String;
}

export interface CupboardPreviousValuesPromise
  extends Promise<CupboardPreviousValues>,
    Fragmentable {
  id: () => Promise<ID_Output>;
  title: () => Promise<String>;
  urlSlug: () => Promise<String>;
}

export interface CupboardPreviousValuesSubscription
  extends Promise<AsyncIterator<CupboardPreviousValues>>,
    Fragmentable {
  id: () => Promise<AsyncIterator<ID_Output>>;
  title: () => Promise<AsyncIterator<String>>;
  urlSlug: () => Promise<AsyncIterator<String>>;
}

export interface CupboardSubscriptionPayload {
  mutation: MutationType;
  node: Cupboard;
  updatedFields: String[];
  previousValues: CupboardPreviousValues;
}

export interface CupboardSubscriptionPayloadPromise
  extends Promise<CupboardSubscriptionPayload>,
    Fragmentable {
  mutation: () => Promise<MutationType>;
  node: <T = CupboardPromise>() => T;
  updatedFields: () => Promise<String[]>;
  previousValues: <T = CupboardPreviousValuesPromise>() => T;
}

export interface CupboardSubscriptionPayloadSubscription
  extends Promise<AsyncIterator<CupboardSubscriptionPayload>>,
    Fragmentable {
  mutation: () => Promise<AsyncIterator<MutationType>>;
  node: <T = CupboardSubscription>() => T;
  updatedFields: () => Promise<AsyncIterator<String[]>>;
  previousValues: <T = CupboardPreviousValuesSubscription>() => T;
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

export interface Restock {
  id: ID_Output;
  date: DateTimeOutput;
  newOnHand: Int;
  userEstimateRunOut?: DateTimeOutput;
  didRunOut?: DateTimeOutput;
}

export interface RestockPromise extends Promise<Restock>, Fragmentable {
  id: () => Promise<ID_Output>;
  date: () => Promise<DateTimeOutput>;
  newOnHand: () => Promise<Int>;
  previousRestock: <T = RestockPromise>() => T;
  userEstimateRunOut: () => Promise<DateTimeOutput>;
  didRunOut: () => Promise<DateTimeOutput>;
}

export interface RestockSubscription
  extends Promise<AsyncIterator<Restock>>,
    Fragmentable {
  id: () => Promise<AsyncIterator<ID_Output>>;
  date: () => Promise<AsyncIterator<DateTimeOutput>>;
  newOnHand: () => Promise<AsyncIterator<Int>>;
  previousRestock: <T = RestockSubscription>() => T;
  userEstimateRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
  didRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
}

export interface RestockNullablePromise
  extends Promise<Restock | null>,
    Fragmentable {
  id: () => Promise<ID_Output>;
  date: () => Promise<DateTimeOutput>;
  newOnHand: () => Promise<Int>;
  previousRestock: <T = RestockPromise>() => T;
  userEstimateRunOut: () => Promise<DateTimeOutput>;
  didRunOut: () => Promise<DateTimeOutput>;
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

/*
The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1. 
*/
export type Int = number;

/*
The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `"4"`) or integer (such as `4`) input value will be accepted as an ID.
*/
export type ID_Input = string | number;
export type ID_Output = string;

export type Long = string;

/*
DateTime scalar input type, allowing Date
*/
export type DateTimeInput = Date | string;

/*
DateTime scalar output type, which is always a string
*/
export type DateTimeOutput = string;

/*
The `Boolean` scalar type represents `true` or `false`.
*/
export type Boolean = boolean;

/*
The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text.
*/
export type String = string;

/**
 * Model Metadata
 */

export const models: Model[] = [
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

/**
 * Type Defs
 */

export const prisma: Prisma;
