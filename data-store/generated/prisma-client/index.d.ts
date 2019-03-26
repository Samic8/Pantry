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

  cupboard: (where: CupboardWhereUniqueInput) => CupboardPromise;
  cupboards: (
    args?: {
      where?: CupboardWhereInput;
      orderBy?: CupboardOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => FragmentableArray<Cupboard>;
  cupboardsConnection: (
    args?: {
      where?: CupboardWhereInput;
      orderBy?: CupboardOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => CupboardConnectionPromise;
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
  restock: (where: RestockWhereUniqueInput) => RestockPromise;
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

  createCupboard: (data: CupboardCreateInput) => CupboardPromise;
  updateCupboard: (
    args: { data: CupboardUpdateInput; where: CupboardWhereUniqueInput }
  ) => CupboardPromise;
  updateManyCupboards: (
    args: { data: CupboardUpdateManyMutationInput; where?: CupboardWhereInput }
  ) => BatchPayloadPromise;
  upsertCupboard: (
    args: {
      where: CupboardWhereUniqueInput;
      create: CupboardCreateInput;
      update: CupboardUpdateInput;
    }
  ) => CupboardPromise;
  deleteCupboard: (where: CupboardWhereUniqueInput) => CupboardPromise;
  deleteManyCupboards: (where?: CupboardWhereInput) => BatchPayloadPromise;
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
  updateRestock: (
    args: { data: RestockUpdateInput; where: RestockWhereUniqueInput }
  ) => RestockPromise;
  updateManyRestocks: (
    args: { data: RestockUpdateManyMutationInput; where?: RestockWhereInput }
  ) => BatchPayloadPromise;
  upsertRestock: (
    args: {
      where: RestockWhereUniqueInput;
      create: RestockCreateInput;
      update: RestockUpdateInput;
    }
  ) => RestockPromise;
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
  | "unit_DESC"
  | "createdAt_ASC"
  | "createdAt_DESC"
  | "updatedAt_ASC"
  | "updatedAt_DESC";

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
  | "didRunOut_DESC"
  | "beforeRestock_ASC"
  | "beforeRestock_DESC"
  | "createdAt_ASC"
  | "createdAt_DESC"
  | "updatedAt_ASC"
  | "updatedAt_DESC";

export type CupboardOrderByInput =
  | "id_ASC"
  | "id_DESC"
  | "title_ASC"
  | "title_DESC"
  | "urlSlug_ASC"
  | "urlSlug_DESC"
  | "createdAt_ASC"
  | "createdAt_DESC"
  | "updatedAt_ASC"
  | "updatedAt_DESC";

export type MutationType = "CREATED" | "UPDATED" | "DELETED";

export interface ItemUpdateWithoutCupboardDataInput {
  name?: String;
  maxOnHand?: Int;
  unit?: String;
  restocks?: RestockUpdateManyInput;
}

export type CupboardWhereUniqueInput = AtLeastOne<{
  id: ID_Input;
  urlSlug?: String;
}>;

export interface CupboardCreateInput {
  title: String;
  urlSlug: String;
  items?: ItemCreateManyWithoutCupboardInput;
}

export interface ItemScalarWhereInput {
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
  AND?: ItemScalarWhereInput[] | ItemScalarWhereInput;
  OR?: ItemScalarWhereInput[] | ItemScalarWhereInput;
  NOT?: ItemScalarWhereInput[] | ItemScalarWhereInput;
}

export interface ItemCreateManyWithoutCupboardInput {
  create?: ItemCreateWithoutCupboardInput[] | ItemCreateWithoutCupboardInput;
  connect?: ItemWhereUniqueInput[] | ItemWhereUniqueInput;
}

export interface RestockUpdateDataInput {
  date?: DateTimeInput;
  newOnHand?: Int;
  previousRestock?: RestockUpdateOneInput;
  userEstimateRunOut?: DateTimeInput;
  didRunOut?: DateTimeInput;
  beforeRestock?: Int;
}

export interface ItemCreateWithoutCupboardInput {
  name: String;
  maxOnHand: Int;
  unit: String;
  restocks?: RestockCreateManyInput;
}

export interface CupboardWhereInput {
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
  title?: String;
  title_not?: String;
  title_in?: String[] | String;
  title_not_in?: String[] | String;
  title_lt?: String;
  title_lte?: String;
  title_gt?: String;
  title_gte?: String;
  title_contains?: String;
  title_not_contains?: String;
  title_starts_with?: String;
  title_not_starts_with?: String;
  title_ends_with?: String;
  title_not_ends_with?: String;
  urlSlug?: String;
  urlSlug_not?: String;
  urlSlug_in?: String[] | String;
  urlSlug_not_in?: String[] | String;
  urlSlug_lt?: String;
  urlSlug_lte?: String;
  urlSlug_gt?: String;
  urlSlug_gte?: String;
  urlSlug_contains?: String;
  urlSlug_not_contains?: String;
  urlSlug_starts_with?: String;
  urlSlug_not_starts_with?: String;
  urlSlug_ends_with?: String;
  urlSlug_not_ends_with?: String;
  items_every?: ItemWhereInput;
  items_some?: ItemWhereInput;
  items_none?: ItemWhereInput;
  AND?: CupboardWhereInput[] | CupboardWhereInput;
  OR?: CupboardWhereInput[] | CupboardWhereInput;
  NOT?: CupboardWhereInput[] | CupboardWhereInput;
}

export interface RestockCreateManyInput {
  create?: RestockCreateInput[] | RestockCreateInput;
  connect?: RestockWhereUniqueInput[] | RestockWhereUniqueInput;
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
  cupboard?: CupboardWhereInput;
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

export interface RestockCreateInput {
  date: DateTimeInput;
  newOnHand: Int;
  previousRestock?: RestockCreateOneInput;
  userEstimateRunOut?: DateTimeInput;
  didRunOut?: DateTimeInput;
  beforeRestock?: Int;
}

export interface RestockUpdateManyMutationInput {
  date?: DateTimeInput;
  newOnHand?: Int;
  userEstimateRunOut?: DateTimeInput;
  didRunOut?: DateTimeInput;
  beforeRestock?: Int;
}

export interface RestockCreateOneInput {
  create?: RestockCreateInput;
  connect?: RestockWhereUniqueInput;
}

export interface ItemUpdateManyMutationInput {
  name?: String;
  maxOnHand?: Int;
  unit?: String;
}

export interface CupboardUpdateInput {
  title?: String;
  urlSlug?: String;
  items?: ItemUpdateManyWithoutCupboardInput;
}

export interface CupboardUpdateWithoutItemsDataInput {
  title?: String;
  urlSlug?: String;
}

export interface ItemUpdateManyWithoutCupboardInput {
  create?: ItemCreateWithoutCupboardInput[] | ItemCreateWithoutCupboardInput;
  delete?: ItemWhereUniqueInput[] | ItemWhereUniqueInput;
  connect?: ItemWhereUniqueInput[] | ItemWhereUniqueInput;
  set?: ItemWhereUniqueInput[] | ItemWhereUniqueInput;
  disconnect?: ItemWhereUniqueInput[] | ItemWhereUniqueInput;
  update?:
    | ItemUpdateWithWhereUniqueWithoutCupboardInput[]
    | ItemUpdateWithWhereUniqueWithoutCupboardInput;
  upsert?:
    | ItemUpsertWithWhereUniqueWithoutCupboardInput[]
    | ItemUpsertWithWhereUniqueWithoutCupboardInput;
  deleteMany?: ItemScalarWhereInput[] | ItemScalarWhereInput;
  updateMany?:
    | ItemUpdateManyWithWhereNestedInput[]
    | ItemUpdateManyWithWhereNestedInput;
}

export type ItemWhereUniqueInput = AtLeastOne<{
  id: ID_Input;
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
  id: ID_Input;
}>;

export interface RestockUpdateManyInput {
  create?: RestockCreateInput[] | RestockCreateInput;
  update?:
    | RestockUpdateWithWhereUniqueNestedInput[]
    | RestockUpdateWithWhereUniqueNestedInput;
  upsert?:
    | RestockUpsertWithWhereUniqueNestedInput[]
    | RestockUpsertWithWhereUniqueNestedInput;
  delete?: RestockWhereUniqueInput[] | RestockWhereUniqueInput;
  connect?: RestockWhereUniqueInput[] | RestockWhereUniqueInput;
  set?: RestockWhereUniqueInput[] | RestockWhereUniqueInput;
  disconnect?: RestockWhereUniqueInput[] | RestockWhereUniqueInput;
  deleteMany?: RestockScalarWhereInput[] | RestockScalarWhereInput;
  updateMany?:
    | RestockUpdateManyWithWhereNestedInput[]
    | RestockUpdateManyWithWhereNestedInput;
}

export interface CupboardUpdateManyMutationInput {
  title?: String;
  urlSlug?: String;
}

export interface RestockUpdateWithWhereUniqueNestedInput {
  where: RestockWhereUniqueInput;
  data: RestockUpdateDataInput;
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

export interface RestockWhereInput {
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
  beforeRestock?: Int;
  beforeRestock_not?: Int;
  beforeRestock_in?: Int[] | Int;
  beforeRestock_not_in?: Int[] | Int;
  beforeRestock_lt?: Int;
  beforeRestock_lte?: Int;
  beforeRestock_gt?: Int;
  beforeRestock_gte?: Int;
  AND?: RestockWhereInput[] | RestockWhereInput;
  OR?: RestockWhereInput[] | RestockWhereInput;
  NOT?: RestockWhereInput[] | RestockWhereInput;
}

export interface RestockUpdateInput {
  date?: DateTimeInput;
  newOnHand?: Int;
  previousRestock?: RestockUpdateOneInput;
  userEstimateRunOut?: DateTimeInput;
  didRunOut?: DateTimeInput;
  beforeRestock?: Int;
}

export interface RestockUpdateOneInput {
  create?: RestockCreateInput;
  update?: RestockUpdateDataInput;
  upsert?: RestockUpsertNestedInput;
  delete?: Boolean;
  disconnect?: Boolean;
  connect?: RestockWhereUniqueInput;
}

export interface CupboardUpdateOneRequiredWithoutItemsInput {
  create?: CupboardCreateWithoutItemsInput;
  update?: CupboardUpdateWithoutItemsDataInput;
  upsert?: CupboardUpsertWithoutItemsInput;
  connect?: CupboardWhereUniqueInput;
}

export interface RestockUpsertNestedInput {
  update: RestockUpdateDataInput;
  create: RestockCreateInput;
}

export interface CupboardCreateOneWithoutItemsInput {
  create?: CupboardCreateWithoutItemsInput;
  connect?: CupboardWhereUniqueInput;
}

export interface RestockUpsertWithWhereUniqueNestedInput {
  where: RestockWhereUniqueInput;
  update: RestockUpdateDataInput;
  create: RestockCreateInput;
}

export interface ItemUpdateManyDataInput {
  name?: String;
  maxOnHand?: Int;
  unit?: String;
}

export interface ItemUpsertWithWhereUniqueWithoutCupboardInput {
  where: ItemWhereUniqueInput;
  update: ItemUpdateWithoutCupboardDataInput;
  create: ItemCreateWithoutCupboardInput;
}

export interface RestockUpdateManyDataInput {
  date?: DateTimeInput;
  newOnHand?: Int;
  userEstimateRunOut?: DateTimeInput;
  didRunOut?: DateTimeInput;
  beforeRestock?: Int;
}

export interface RestockUpdateManyWithWhereNestedInput {
  where: RestockScalarWhereInput;
  data: RestockUpdateManyDataInput;
}

export interface RestockScalarWhereInput {
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
  beforeRestock?: Int;
  beforeRestock_not?: Int;
  beforeRestock_in?: Int[] | Int;
  beforeRestock_not_in?: Int[] | Int;
  beforeRestock_lt?: Int;
  beforeRestock_lte?: Int;
  beforeRestock_gt?: Int;
  beforeRestock_gte?: Int;
  AND?: RestockScalarWhereInput[] | RestockScalarWhereInput;
  OR?: RestockScalarWhereInput[] | RestockScalarWhereInput;
  NOT?: RestockScalarWhereInput[] | RestockScalarWhereInput;
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

export interface ItemCreateInput {
  cupboard: CupboardCreateOneWithoutItemsInput;
  name: String;
  maxOnHand: Int;
  unit: String;
  restocks?: RestockCreateManyInput;
}

export interface ItemUpdateInput {
  cupboard?: CupboardUpdateOneRequiredWithoutItemsInput;
  name?: String;
  maxOnHand?: Int;
  unit?: String;
  restocks?: RestockUpdateManyInput;
}

export interface CupboardUpsertWithoutItemsInput {
  update: CupboardUpdateWithoutItemsDataInput;
  create: CupboardCreateWithoutItemsInput;
}

export interface CupboardSubscriptionWhereInput {
  mutation_in?: MutationType[] | MutationType;
  updatedFields_contains?: String;
  updatedFields_contains_every?: String[] | String;
  updatedFields_contains_some?: String[] | String;
  node?: CupboardWhereInput;
  AND?: CupboardSubscriptionWhereInput[] | CupboardSubscriptionWhereInput;
  OR?: CupboardSubscriptionWhereInput[] | CupboardSubscriptionWhereInput;
  NOT?: CupboardSubscriptionWhereInput[] | CupboardSubscriptionWhereInput;
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
  beforeRestock?: Int;
}

export interface RestockPreviousValuesPromise
  extends Promise<RestockPreviousValues>,
    Fragmentable {
  id: () => Promise<ID_Output>;
  date: () => Promise<DateTimeOutput>;
  newOnHand: () => Promise<Int>;
  userEstimateRunOut: () => Promise<DateTimeOutput>;
  didRunOut: () => Promise<DateTimeOutput>;
  beforeRestock: () => Promise<Int>;
}

export interface RestockPreviousValuesSubscription
  extends Promise<AsyncIterator<RestockPreviousValues>>,
    Fragmentable {
  id: () => Promise<AsyncIterator<ID_Output>>;
  date: () => Promise<AsyncIterator<DateTimeOutput>>;
  newOnHand: () => Promise<AsyncIterator<Int>>;
  userEstimateRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
  didRunOut: () => Promise<AsyncIterator<DateTimeOutput>>;
  beforeRestock: () => Promise<AsyncIterator<Int>>;
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
  cupboard: <T = CupboardSubscription>() => T;
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
  items: <T = FragmentableArray<Item>>(
    args?: {
      where?: ItemWhereInput;
      orderBy?: ItemOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => T;
}

export interface CupboardSubscription
  extends Promise<AsyncIterator<Cupboard>>,
    Fragmentable {
  id: () => Promise<AsyncIterator<ID_Output>>;
  title: () => Promise<AsyncIterator<String>>;
  urlSlug: () => Promise<AsyncIterator<String>>;
  items: <T = Promise<AsyncIterator<ItemSubscription>>>(
    args?: {
      where?: ItemWhereInput;
      orderBy?: ItemOrderByInput;
      skip?: Int;
      after?: String;
      before?: String;
      first?: Int;
      last?: Int;
    }
  ) => T;
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
  beforeRestock?: Int;
}

export interface RestockPromise extends Promise<Restock>, Fragmentable {
  id: () => Promise<ID_Output>;
  date: () => Promise<DateTimeOutput>;
  newOnHand: () => Promise<Int>;
  previousRestock: <T = RestockPromise>() => T;
  userEstimateRunOut: () => Promise<DateTimeOutput>;
  didRunOut: () => Promise<DateTimeOutput>;
  beforeRestock: () => Promise<Int>;
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
  beforeRestock: () => Promise<AsyncIterator<Int>>;
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
