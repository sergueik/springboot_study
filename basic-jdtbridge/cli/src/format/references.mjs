// References output formatter.
// Groups source references by file, binary references by project+jar.

import { stripProject } from "../paths.mjs";

export function formatReferences(results) {
  let lastGroup = "";
  for (const r of results) {
    const f = stripProject(r.file);
    const isBinary = r.line <= 0;
    const group = isBinary ? `${r.project || "?"}  ${f}` : f;
    if (group !== lastGroup) {
      if (lastGroup) console.log();
      if (isBinary) {
        console.log(`${r.project || "?"}  (${f.split(/[/\\]/).pop()})`);
      }
      lastGroup = group;
    }
    if (isBinary) {
      if (r.in) console.log(`  ${r.in}`);
      if (r.content) console.log(`    | ${r.content}`);
    } else {
      console.log(`${f}:${r.line}`);
      if (r.in) console.log(`  in ${r.in}`);
      if (r.content) console.log(`  | ${r.content}`);
    }
  }
}
