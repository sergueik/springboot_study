import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdtempSync, writeFileSync, readFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import { tmpdir } from "node:os";
import {
  getHome,
  instancesDir,
  readConfig,
  writeConfig,
  resetHome,
} from "../src/home.mjs";

describe("home", () => {
  let testDir;
  let origEnv;

  beforeEach(() => {
    testDir = mkdtempSync(join(tmpdir(), "jdtbridge-test-"));
    origEnv = process.env.JDTBRIDGE_HOME;
    process.env.JDTBRIDGE_HOME = testDir;
    resetHome();
  });

  afterEach(() => {
    if (origEnv !== undefined) {
      process.env.JDTBRIDGE_HOME = origEnv;
    } else {
      delete process.env.JDTBRIDGE_HOME;
    }
    resetHome();
  });

  describe("getHome", () => {
    it("returns JDTBRIDGE_HOME env var", () => {
      expect(getHome()).toBe(testDir);
    });

    it("caches the result", () => {
      const first = getHome();
      process.env.JDTBRIDGE_HOME = "/something/else";
      expect(getHome()).toBe(first);
    });

    it("uses default when env not set", () => {
      delete process.env.JDTBRIDGE_HOME;
      resetHome();
      const home = getHome();
      expect(home).toContain(".jdtbridge");
    });
  });

  describe("instancesDir", () => {
    it("creates instances subdirectory", () => {
      const dir = instancesDir();
      expect(dir).toBe(join(testDir, "instances"));
      expect(existsSync(dir)).toBe(true);
    });
  });

  describe("readConfig / writeConfig", () => {
    it("returns empty object when no config exists", () => {
      expect(readConfig()).toEqual({});
    });

    it("writes and reads config", () => {
      writeConfig({ port: 8080, debug: true });
      const config = readConfig();
      expect(config.port).toBe(8080);
      expect(config.debug).toBe(true);
    });

    it("merges with existing config", () => {
      writeConfig({ a: 1, b: 2 });
      writeConfig({ b: 3, c: 4 });
      const config = readConfig();
      expect(config).toEqual({ a: 1, b: 3, c: 4 });
    });

    it("returns empty object for corrupt config", () => {
      writeFileSync(join(testDir, "config.json"), "not json{{{");
      expect(readConfig()).toEqual({});
    });
  });
});
