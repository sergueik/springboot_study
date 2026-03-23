// Path utilities for workspace-relative paths.

/**
 * Strip leading slash from workspace-relative path.
 * Eclipse returns paths like /m8-server/src/... — we want m8-server/src/...
 */
export function stripProject(wsPath) {
  return wsPath.startsWith("/") ? wsPath.slice(1) : wsPath;
}

/**
 * Ensure path starts with / for workspace-relative API calls.
 * Accepts: m8-server/src/... or /m8-server/src/...
 */
export function toWsPath(p) {
  return p.startsWith("/") ? p : "/" + p;
}
