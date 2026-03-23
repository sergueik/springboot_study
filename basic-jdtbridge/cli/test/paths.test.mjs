import { describe, it, expect } from "vitest";
import { stripProject, toWsPath } from "../src/paths.mjs";

describe("stripProject", () => {
  it("strips leading slash", () => {
    expect(stripProject("/m8-server/src/main/java/Foo.java")).toBe(
      "m8-server/src/main/java/Foo.java",
    );
  });

  it("returns path as-is if no leading slash", () => {
    expect(stripProject("m8-server/src/Foo.java")).toBe(
      "m8-server/src/Foo.java",
    );
  });
});

describe("toWsPath", () => {
  it("adds leading slash", () => {
    expect(toWsPath("m8-server/src/Foo.java")).toBe(
      "/m8-server/src/Foo.java",
    );
  });

  it("keeps existing leading slash", () => {
    expect(toWsPath("/m8-server/src/Foo.java")).toBe(
      "/m8-server/src/Foo.java",
    );
  });
});
