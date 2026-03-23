// Colored output with TTY auto-detection.
// Enable: --color flag, JDTBRIDGE_COLOR=1, or FORCE_COLOR=1
// Disable: --no-color flag, NO_COLOR=1

import pc from "picocolors";

let _enabled;

/** Check if color output is enabled. */
export function isColorEnabled() {
  if (_enabled !== undefined) return _enabled;

  if (process.env.NO_COLOR || process.argv.includes("--no-color")) {
    _enabled = false;
  } else if (
    process.env.FORCE_COLOR ||
    process.env.JDTBRIDGE_COLOR ||
    process.argv.includes("--color")
  ) {
    _enabled = true;
  } else {
    _enabled = process.stdout.isTTY === true;
  }
  return _enabled;
}

/** Set color enabled/disabled explicitly. */
export function setColorEnabled(enabled) {
  _enabled = enabled;
}

function wrap(fn) {
  return (s) => (isColorEnabled() ? fn(s) : s);
}

export const red = wrap(pc.red);
export const green = wrap(pc.green);
export const yellow = wrap(pc.yellow);
export const bold = wrap(pc.bold);
export const dim = wrap(pc.dim);
