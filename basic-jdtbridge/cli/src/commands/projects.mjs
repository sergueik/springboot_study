import { get } from "../client.mjs";

export async function projects() {
  const results = await get("/projects");
  if (results.error) {
    console.error(results.error);
    process.exit(1);
  }
  for (const p of results) console.log(p);
}

export const help = `List all Java projects in the Eclipse workspace.

Usage:  jdt projects

Output: one project name per line.`;
