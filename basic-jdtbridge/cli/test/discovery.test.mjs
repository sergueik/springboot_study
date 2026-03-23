import { describe, it, expect, beforeEach, afterEach } from "vitest";
import {
  mkdtempSync,
  mkdirSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { resetHome } from "../src/home.mjs";
import {
  discoverInstances,
  findInstance,
  isPidAlive,
} from "../src/discovery.mjs";

describe("discovery", () => {
  let testDir;
  let instDir;
  let origEnv;

  beforeEach(() => {
    testDir = mkdtempSync(join(tmpdir(), "jdtbridge-disc-"));
    instDir = join(testDir, "instances");
    mkdirSync(instDir, { recursive: true });
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

  function writeInstance(name, data) {
    writeFileSync(join(instDir, name), JSON.stringify(data));
  }

  describe("isPidAlive", () => {
    it("returns true for current process", () => {
      expect(isPidAlive(process.pid)).toBe(true);
    });

    it("returns false for non-existent PID", () => {
      // PID 99999999 is extremely unlikely to exist
      expect(isPidAlive(99999999)).toBe(false);
    });
  });

  describe("discoverInstances", () => {
    it("returns empty array when no instances", () => {
      expect(discoverInstances()).toEqual([]);
    });

    it("reads valid instance file with alive PID", () => {
      writeInstance("abc123.json", {
        port: 7891,
        token: "tok",
        pid: process.pid,
        workspace: "D:/ws",
      });
      const instances = discoverInstances();
      expect(instances).toHaveLength(1);
      expect(instances[0].port).toBe(7891);
      expect(instances[0].token).toBe("tok");
      expect(instances[0].workspace).toBe("D:/ws");
      expect(instances[0].file).toContain("abc123.json");
    });

    it("filters out instances with dead PID", () => {
      writeInstance("dead.json", {
        port: 7891,
        token: "tok",
        pid: 99999999,
        workspace: "D:/ws",
      });
      expect(discoverInstances()).toEqual([]);
    });

    it("skips files without port", () => {
      writeInstance("noport.json", {
        token: "tok",
        pid: process.pid,
        workspace: "D:/ws",
      });
      expect(discoverInstances()).toEqual([]);
    });

    it("skips files without pid", () => {
      writeInstance("nopid.json", {
        port: 7891,
        token: "tok",
        workspace: "D:/ws",
      });
      expect(discoverInstances()).toEqual([]);
    });

    it("skips corrupt JSON files", () => {
      writeFileSync(join(instDir, "corrupt.json"), "{broken json");
      expect(discoverInstances()).toEqual([]);
    });

    it("ignores non-.json files", () => {
      writeFileSync(join(instDir, "readme.txt"), "not an instance");
      expect(discoverInstances()).toEqual([]);
    });

    it("returns multiple live instances", () => {
      writeInstance("a.json", { port: 1001, token: "t1", pid: process.pid, workspace: "/ws/a" });
      writeInstance("b.json", { port: 1002, token: "t2", pid: process.pid, workspace: "/ws/b" });
      const instances = discoverInstances();
      expect(instances).toHaveLength(2);
    });
  });

  describe("findInstance", () => {
    it("returns null when no instances", () => {
      expect(findInstance()).toBeNull();
    });

    it("returns single instance without hint", () => {
      writeInstance("only.json", { port: 7891, token: "t", pid: process.pid, workspace: "/ws/main" });
      const inst = findInstance();
      expect(inst.port).toBe(7891);
    });

    it("matches workspace hint", () => {
      writeInstance("a.json", { port: 1001, token: "t1", pid: process.pid, workspace: "/ws/alpha" });
      writeInstance("b.json", { port: 1002, token: "t2", pid: process.pid, workspace: "/ws/beta" });
      const inst = findInstance("beta");
      expect(inst.port).toBe(1002);
    });

    it("matches workspace hint case-insensitively", () => {
      writeInstance("a.json", { port: 1001, token: "t1", pid: process.pid, workspace: "/ws/MyProject" });
      writeInstance("b.json", { port: 1002, token: "t2", pid: process.pid, workspace: "/ws/other" });
      const inst = findInstance("myproject");
      expect(inst.port).toBe(1001);
    });

    it("normalizes backslashes in hint", () => {
      writeInstance("a.json", { port: 1001, token: "t1", pid: process.pid, workspace: "D:/ws/proj" });
      writeInstance("b.json", { port: 1002, token: "t2", pid: process.pid, workspace: "D:/ws/other" });
      const inst = findInstance("D:\\ws\\proj");
      expect(inst.port).toBe(1001);
    });

    it("falls back to first when hint doesn't match", () => {
      writeInstance("a.json", { port: 1001, token: "t1", pid: process.pid, workspace: "/ws/alpha" });
      writeInstance("b.json", { port: 1002, token: "t2", pid: process.pid, workspace: "/ws/beta" });
      const inst = findInstance("nonexistent");
      expect(inst).not.toBeNull();
    });

    it("falls back to first when no hint given with multiple instances", () => {
      writeInstance("a.json", { port: 1001, token: "t1", pid: process.pid, workspace: "/ws/alpha" });
      writeInstance("b.json", { port: 1002, token: "t2", pid: process.pid, workspace: "/ws/beta" });
      const inst = findInstance();
      expect(inst).not.toBeNull();
    });
  });
});
