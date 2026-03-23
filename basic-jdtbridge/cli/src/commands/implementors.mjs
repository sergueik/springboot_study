import { get } from "../client.mjs";
import { extractPositional, parseFlags } from "../args.mjs";
import { stripProject } from "../paths.mjs";

export async function implementors(args) {
  const pos = extractPositional(args);
  const flags = parseFlags(args);
  const [fqn, method] = pos;
  if (!fqn || !method) {
    console.error("Usage: implementors <FQN> <method> [--arity n]");
    process.exit(1);
  }
  let url = `/implementors?class=${encodeURIComponent(fqn)}&method=${encodeURIComponent(method)}`;
  if (flags.arity !== undefined && flags.arity !== true)
    url += `&arity=${flags.arity}`;
  const results = await get(url, 30_000);
  if (results.error) {
    console.error(results.error);
    process.exit(1);
  }
  if (results.length === 0) {
    console.log("(no implementors)");
    return;
  }
  for (const r of results) {
    console.log(`${r.fqn}  ${stripProject(r.file)}:${r.line}`);
  }
}

export const help = `Find implementations of an interface method across all implementing classes.

Usage:  jdt implementors <FQN> <method> [--arity <n>]

Example:  jdt implementors app.m8.web.shared.core.HasId getId`;
