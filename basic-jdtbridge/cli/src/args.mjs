// CLI argument parsing utilities.

/**
 * Parse --flag and --key value pairs from args array.
 * Boolean flags (standalone --flag) get value `true`.
 * Key-value pairs (--key value) get the string value.
 */
export function parseFlags(args) {
  const flags = {};
  for (let i = 0; i < args.length; i++) {
    if (args[i].startsWith("--")) {
      const key = args[i].slice(2);
      if (i + 1 < args.length && !args[i + 1].startsWith("--")) {
        flags[key] = args[++i];
      } else {
        flags[key] = true;
      }
    }
  }
  return flags;
}

/**
 * Extract positional arguments (non-flag values) from args array.
 * Skips --flag and --key value pairs.
 */
export function extractPositional(args) {
  const result = [];
  for (let i = 0; i < args.length; i++) {
    if (args[i].startsWith("--")) {
      if (i + 1 < args.length && !args[i + 1].startsWith("--")) i++;
    } else {
      result.push(args[i]);
    }
  }
  return result;
}
