import { describe, it, expect, beforeEach, afterEach } from "vitest";
import {
  isColorEnabled,
  setColorEnabled,
  red,
  green,
  yellow,
  bold,
  dim,
} from "../src/color.mjs";

describe("color", () => {
  let origNoColor, origForceColor, origJdtColor, origArgv, origIsTTY;

  beforeEach(() => {
    origNoColor = process.env.NO_COLOR;
    origForceColor = process.env.FORCE_COLOR;
    origJdtColor = process.env.JDTBRIDGE_COLOR;
    origArgv = process.argv;
    origIsTTY = process.stdout.isTTY;
    // Reset to force re-detection
    setColorEnabled(undefined);
  });

  afterEach(() => {
    restoreEnv("NO_COLOR", origNoColor);
    restoreEnv("FORCE_COLOR", origForceColor);
    restoreEnv("JDTBRIDGE_COLOR", origJdtColor);
    process.argv = origArgv;
    Object.defineProperty(process.stdout, "isTTY", {
      value: origIsTTY,
      writable: true,
      configurable: true,
    });
    setColorEnabled(undefined);
  });

  function restoreEnv(key, orig) {
    if (orig !== undefined) process.env[key] = orig;
    else delete process.env[key];
  }

  describe("auto-detection", () => {
    it("disables on NO_COLOR env", () => {
      delete process.env.FORCE_COLOR;
      delete process.env.JDTBRIDGE_COLOR;
      process.env.NO_COLOR = "1";
      process.argv = ["node", "jdt"];
      setColorEnabled(undefined);
      expect(isColorEnabled()).toBe(false);
    });

    it("disables on --no-color flag", () => {
      delete process.env.NO_COLOR;
      delete process.env.FORCE_COLOR;
      delete process.env.JDTBRIDGE_COLOR;
      process.argv = ["node", "jdt", "--no-color"];
      setColorEnabled(undefined);
      expect(isColorEnabled()).toBe(false);
    });

    it("enables on FORCE_COLOR env", () => {
      delete process.env.NO_COLOR;
      delete process.env.JDTBRIDGE_COLOR;
      process.env.FORCE_COLOR = "1";
      process.argv = ["node", "jdt"];
      setColorEnabled(undefined);
      expect(isColorEnabled()).toBe(true);
    });

    it("enables on JDTBRIDGE_COLOR env", () => {
      delete process.env.NO_COLOR;
      delete process.env.FORCE_COLOR;
      process.env.JDTBRIDGE_COLOR = "1";
      process.argv = ["node", "jdt"];
      setColorEnabled(undefined);
      expect(isColorEnabled()).toBe(true);
    });

    it("enables on --color flag", () => {
      delete process.env.NO_COLOR;
      delete process.env.FORCE_COLOR;
      delete process.env.JDTBRIDGE_COLOR;
      process.argv = ["node", "jdt", "--color"];
      setColorEnabled(undefined);
      expect(isColorEnabled()).toBe(true);
    });

    it("falls back to isTTY when no env/flags", () => {
      delete process.env.NO_COLOR;
      delete process.env.FORCE_COLOR;
      delete process.env.JDTBRIDGE_COLOR;
      process.argv = ["node", "jdt"];
      Object.defineProperty(process.stdout, "isTTY", {
        value: true,
        writable: true,
        configurable: true,
      });
      setColorEnabled(undefined);
      expect(isColorEnabled()).toBe(true);
    });

    it("disables when not a TTY and no env/flags", () => {
      delete process.env.NO_COLOR;
      delete process.env.FORCE_COLOR;
      delete process.env.JDTBRIDGE_COLOR;
      process.argv = ["node", "jdt"];
      Object.defineProperty(process.stdout, "isTTY", {
        value: undefined,
        writable: true,
        configurable: true,
      });
      setColorEnabled(undefined);
      expect(isColorEnabled()).toBe(false);
    });
  });

  describe("wrap functions", () => {
    it("all pass through when disabled", () => {
      setColorEnabled(false);
      expect(red("x")).toBe("x");
      expect(green("x")).toBe("x");
      expect(yellow("x")).toBe("x");
      expect(bold("x")).toBe("x");
      expect(dim("x")).toBe("x");
    });

    it("all apply ANSI codes when enabled", () => {
      setColorEnabled(true);
      for (const fn of [red, green, yellow, bold, dim]) {
        const result = fn("test");
        expect(result).not.toBe("test");
        expect(result).toContain("test");
      }
    });
  });
});
