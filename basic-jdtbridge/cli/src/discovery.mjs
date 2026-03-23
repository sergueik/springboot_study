// Eclipse instance discovery — find running instances via bridge files and process list.

import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { instancesDir } from "./home.mjs";

/**
 * @typedef {Object} Instance
 * @property {number} port
 * @property {string} token
 * @property {number} pid
 * @property {string} workspace
 * @property {string} file - path to the instance file
 */

/**
 * Read all instance files from JDTBRIDGE_HOME/instances/.
 * Filters out stale instances (PID not alive).
 * @returns {Instance[]}
 */
export function discoverInstances() {
  const dir = instancesDir();
  let files;
  try {
    files = readdirSync(dir).filter((f) => f.endsWith(".json"));
  } catch {
    return [];
  }

  const instances = [];
  for (const file of files) {
    const filePath = join(dir, file);
    try {
      const data = JSON.parse(readFileSync(filePath, "utf8"));
      if (!data.port || !data.pid) continue;
      if (!isPidAlive(data.pid)) continue;
      instances.push({ ...data, file: filePath });
    } catch {
      // corrupt file — skip
    }
  }
  return instances;
}

/**
 * Find a single instance. If multiple are running, prefer one matching
 * the given workspace hint (cwd or explicit). If none match, return the
 * first one found (or null).
 * @param {string} [workspaceHint] - substring to match against workspace path
 * @returns {Instance|null}
 */
export function findInstance(workspaceHint) {
  const instances = discoverInstances();
  if (instances.length === 0) return null;
  if (instances.length === 1) return instances[0];

  if (workspaceHint) {
    const normalized = workspaceHint.replace(/\\/g, "/").toLowerCase();
    const match = instances.find((i) =>
      i.workspace.replace(/\\/g, "/").toLowerCase().includes(normalized),
    );
    if (match) return match;
  }

  // Default to first
  return instances[0];
}

/**
 * Check if a PID is still alive.
 * Sends signal 0 which checks existence without killing.
 */
export function isPidAlive(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}
