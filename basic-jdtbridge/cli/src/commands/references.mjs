import { get } from "../client.mjs";
import { extractPositional, parseFlags } from "../args.mjs";
import { formatReferences } from "../format/references.mjs";

export async function references(args) {
  const pos = extractPositional(args);
  const flags = parseFlags(args);
  const fqn = pos[0];
  if (!fqn) {
    console.error("Usage: references <FQN> [method] [--field name] [--arity n]");
    process.exit(1);
  }
  let url = `/references?class=${encodeURIComponent(fqn)}`;
  if (flags.field) {
    url += `&field=${encodeURIComponent(flags.field)}`;
  } else {
    const method = pos[1];
    if (method) url += `&method=${encodeURIComponent(method)}`;
  }
  if (flags.arity !== undefined && flags.arity !== true)
    url += `&arity=${flags.arity}`;
  const results = await get(url, 30_000);
  if (results.error) {
    console.error(results.error);
    process.exit(1);
  }
  if (results.length === 0) {
    console.log("(no references)");
    return;
  }
  formatReferences(results);
}

export const help = `Find all references to a type, method, or field across the workspace.

Usage:  jdt references <FQN> [method]
        jdt references <FQN> --field <name>
        jdt references <FQN> [method] --arity <n>

Arguments:
  FQN       fully qualified class name
  method    method name (optional)

Flags:
  --field <name>   find references to a field
  --arity <n>      disambiguate overloaded methods by parameter count

Examples:
  jdt references app.m8.dto.web.core.IdOrgRoot
  jdt references app.m8.dao.StaffDaoImpl getStaff
  jdt references app.m8.dao.StaffDaoImpl --field staffCache`;
