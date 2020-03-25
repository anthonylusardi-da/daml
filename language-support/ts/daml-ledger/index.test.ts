// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

import { Template, ContractId, Choice } from "@daml/types";
import WS from "jest-websocket-mock";
import Ledger from "./index";
import { Event, CreateEvent } from "./index";
import * as jtv from "@mojotech/json-type-validation";
import mockConsole from "jest-mock-console";


type Foo = {};

const mockChange = jest.fn();

const Foo: Template<Foo, undefined, "foo-id"> = {
  templateId: "foo-id",
  keyDecoder: () => jtv.constant(undefined),
  decoder: () => jtv.object({}),
  Archive: {} as unknown as Choice<Foo, {}, {}, undefined>,
};

const fooCreateEvent = (
  coid: number
): CreateEvent<Foo, undefined, "foo-id"> => {
  return {
    templateId: "foo-id",
    contractId: coid.toString() as ContractId<object>,
    signatories: [],
    observers: [],
    agreementText: "fooAgreement",
    key: undefined,
    payload: {},
  };
};

const fooEvent = (coid: number): Event<Foo, undefined, "foo-id"> => {
  return { created: fooCreateEvent(coid) };
};


const fooArchiveEvent = (coid: number): Event<Foo, undefined, "foo-id"> => {
  return {
    archived: {
      templateId: "foo-id",
      contractId: coid.toString() as ContractId<object>,
    },
  };
};

const mockOptions = {
  token: "dummyToken",
  httpBaseUrl: "http://localhost:5000/",
  wsBaseUrl: "ws://localhost:4000/",
};

afterEach(() => {
  WS.clean();
  mockChange.mockClear();
});

describe("streamQuery", () => {
  // we only check that nothing crashes.
  test("receive unknown message", async () => {
    const restoreConsole = mockConsole();
    const server = new WS("ws://localhost:4000/v1/stream/query", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamQuery(Foo);
    await server.connected;
    console.log("connected");
    stream.on("change", evs => console.log(evs));
    stream.on("close", ev => console.log(ev));
    server.send("mickey mouse");
    expect(console.error).toHaveBeenCalledWith("Ledger.streamQuery unknown message", "mickey mouse");
    restoreConsole();
  });

  // we only check that nothing crashes.
  test("receive warnings", async () => {
    const restoreConsole = mockConsole();
    const server = new WS("ws://localhost:4000/v1/stream/query", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamQuery(Foo);
    await server.connected;
    console.log("connected");
    stream.on("change", evs => console.log(evs));
    stream.on("close", ev => console.log(ev));
    stream.off("change", evs => console.log(evs));
    stream.off("close", ev => console.log(ev));
    server.send({ warnings: ["oh oh"] });
    expect(console.warn).toHaveBeenCalledWith("Ledger.streamQuery warnings", {"warnings": ["oh oh"]});
    restoreConsole();
  });

  // we only check that nothing crashes.
  test("receive errors", async () => {
    const restoreConsole = mockConsole();
    const server = new WS("ws://localhost:4000/v1/stream/query", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamQuery(Foo);
    await server.connected;
    console.log("connected");
    stream.on("change", evs => console.log(evs));
    stream.on("close", ev => console.log(ev));
    stream.off("change", evs => console.log(evs));
    stream.off("close", ev => console.log(ev));
    server.send({ errors: ["not good!"] });
    expect(console.error).toHaveBeenCalledWith("Ledger.streamQuery errors", { errors: ["not good!"] });
    restoreConsole();
  });

  test("receive empty events", async () => {
    const server = new WS("ws://localhost:4000/v1/stream/query", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamQuery(Foo);
    await server.connected;
    console.log("connected");
    stream.on("change", mockChange);
    stream.on("close", ev => console.log(ev));
    stream.off("change", evs => console.log(evs));
    stream.off("close", ev => console.log(ev));
    server.send({ events: [] });
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenLastCalledWith([], []);
  });

  test("receive one event", async () => {
    const server = new WS("ws://localhost:4000/v1/stream/query", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamQuery(Foo);
    await server.connected;
    console.log("connected");
    stream.on("change", evs => mockChange(evs));
    stream.on("close", ev => console.log(ev));
    stream.off("change", evs => console.log(evs));
    stream.off("close", ev => console.log(ev));
    server.send({ events: [fooEvent(1)] });
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenLastCalledWith([fooCreateEvent(1)]);
  });

  test("receive several events", async () => {
    const server = new WS("ws://localhost:4000/v1/stream/query", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamQuery(Foo);
    await server.connected;
    console.log("connected");
    stream.on("change", evs => mockChange(evs));
    stream.on("close", ev => console.log(ev));
    stream.off("change", evs => console.log(evs));
    stream.off("close", ev => console.log(ev));
    server.send({ events: [1, 2, 3].map(fooEvent) });
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenCalledWith([1, 2, 3].map(fooCreateEvent));
  });

  test("drop matching created and archived events", async () => {
    const server = new WS("ws://localhost:4000/v1/stream/query", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamQuery(Foo);
    await server.connected;
    console.log("connected");
    stream.on("change", evs => mockChange(evs));
    stream.on("close", ev => console.log(ev));
    stream.off("change", evs => console.log(evs));
    stream.off("close", ev => console.log(ev));
    server.send({ events: [fooEvent(1), fooEvent(2)] });
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenCalledWith([fooCreateEvent(1), fooCreateEvent(2)]);
    mockChange.mockClear();
    server.send({ events: [fooArchiveEvent(1)]});
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenCalledWith([fooCreateEvent(2)]);
  });
});

describe("streamFetchByKey", () => {
  test("receive no event", async () => {
    const server = new WS("ws://localhost:4000/v1/stream/fetch", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamFetchByKey(Foo, undefined);
    await server.connected;
    console.log("connected");
    stream.on("change", ev => mockChange(ev));
    stream.on("close", ev => console.log(ev));
    stream.off("change", ev => console.log(ev));
    stream.off("close", ev => console.log(ev));
    server.send({ events: [] });
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenCalledWith(null);
  });

  test("receive one event", async () => {
    const server = new WS("ws://localhost:4000/v1/stream/fetch", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamFetchByKey(Foo, undefined);
    await server.connected;
    console.log("connected");
    stream.on("change", ev => mockChange(ev));
    stream.on("close", ev => console.log(ev));
    stream.off("change", ev => console.log(ev));
    stream.off("close", ev => console.log(ev));
    server.send({ events: [fooEvent(1)] });
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenCalledWith(fooCreateEvent(1));
  });

  test("receive several events", async () => {
    const server = new WS("ws://localhost:4000/v1/stream/fetch", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamFetchByKey(Foo, undefined);
    await server.connected;
    console.log("connected");
    stream.on("change", ev => mockChange(ev));
    stream.on("close", ev => console.log(ev));
    stream.off("change", ev => console.log(ev));
    stream.off("close", ev => console.log(ev));
    server.send({ events: [fooEvent(1), fooEvent(2), fooEvent(3)] });
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenCalledWith(fooCreateEvent(3));
  });

  test("drop matching created and archived events", async () => {
    const server = new WS("ws://localhost:4000/v1/stream/fetch", {
      jsonProtocol: true,
    });
    const ledger = new Ledger(mockOptions);
    const stream = ledger.streamFetchByKey(Foo, undefined);
    await server.connected;
    console.log("connected");
    stream.on("change", ev => mockChange(ev));
    stream.on("close", ev => console.log(ev));
    stream.off("change", evs => console.log(evs));
    stream.off("close", ev => console.log(ev));
    server.send({ events: [fooEvent(1)] });
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenCalledWith(fooCreateEvent(1));
    mockChange.mockClear();
    server.send({ events: [fooArchiveEvent(1)] });
    expect(mockChange).toHaveBeenCalledTimes(1);
    expect(mockChange).toHaveBeenCalledWith(null);
  });
});
